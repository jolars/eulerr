use extendr_api::prelude::*;
use extendr_api::throw_r_error;
use eunoia::{
    geometry::{
        primitives::Point,
        shapes::{Circle, Ellipse, Polygon},
        traits::DiagramShape,
    },
    loss::LossType,
    plotting::{decompose_regions, polygon_clip, ClipOperation},
    spec::DiagramSpec,
    Combination, DiagramSpecBuilder, Fitter, InputType,
};
use std::collections::{HashMap, HashSet};

/// Extract unique set names from combination labels in order of first appearance.
fn extract_set_names(combo_names: &[String]) -> Vec<String> {
    let mut seen: HashSet<String> = HashSet::new();
    let mut names: Vec<String> = Vec::new();
    for name in combo_names {
        for part in name.split('&') {
            if !part.is_empty() && seen.insert(part.to_string()) {
                names.push(part.to_string());
            }
        }
    }
    names
}

fn parse_input_type(input: &str) -> std::result::Result<InputType, Error> {
    match input {
        "disjoint" => Ok(InputType::Exclusive),
        "union" => Ok(InputType::Inclusive),
        other => Err(format!("Unknown input type: {}", other).into()),
    }
}

fn parse_loss_type(loss: &str, agg: &str) -> std::result::Result<LossType, Error> {
    match (loss, agg) {
        ("square", "sum") => Ok(LossType::SumSquared),
        ("square", "max") => Ok(LossType::MaxSquared),
        ("abs", "sum") => Ok(LossType::SumAbsoute),
        ("abs", "max") => Ok(LossType::MaxAbsolute),
        ("region", "sum") => Ok(LossType::SumAbsoluteRegionError),
        ("region", "max") => Ok(LossType::DiagError),
        _ => Err(format!("Unknown loss combination: {}/{}", loss, agg).into()),
    }
}

/// Determine which sets in the spec are non-empty (inclusive area >= epsilon).
fn non_empty_set_names(spec: &DiagramSpec) -> Vec<String> {
    const EPS: f64 = 1e-10;
    spec.set_names()
        .iter()
        .filter(|name| {
            let combo = Combination::new(&[name.as_str()]);
            spec.inclusive_areas()
                .get(&combo)
                .copied()
                .unwrap_or(0.0)
                >= EPS
        })
        .cloned()
        .collect()
}

/// Enumerate all 2^n - 1 non-empty subset bitmasks in legacy cardinality-first
/// order (sorted by `(popcount, mask)`).
///
/// Matches the legacy C++ `bit_index` enumeration in `eulerr/src/helpers.cpp`,
/// which looped over cardinality `i = 1..=n` and permuted a vector with `i`
/// true bits via `std::prev_permutation`. For n = 3 this produces:
/// `[0b001, 0b010, 0b100, 0b011, 0b101, 0b110, 0b111]`
/// (i.e. A, B, C, A&B, A&C, B&C, A&B&C).
///
/// eulerr's plotting/fill code is positional — `fills.grob.<i>` is indexed by
/// `bit_indexr(n)` rows — so the row order is part of the public contract.
fn legacy_subset_masks(n: usize) -> Vec<usize> {
    if n == 0 {
        return Vec::new();
    }
    let total = (1usize << n) - 1;
    let mut masks: Vec<usize> = (1..=total).collect();
    masks.sort_by_key(|&m| (m.count_ones(), m));
    masks
}

/// Enumerate all 2^n - 1 non-empty subsets of `all_set_names` in legacy
/// cardinality-first order. Returns (combo_labels, combination_keys) —
/// labels use input order of set names; keys use alphabetically-sorted
/// names (Combination::new sorts internally).
fn enumerate_combinations(all_set_names: &[String]) -> (Vec<String>, Vec<Combination>) {
    let n = all_set_names.len();
    let masks = legacy_subset_masks(n);
    let mut labels = Vec::with_capacity(masks.len());
    let mut keys = Vec::with_capacity(masks.len());

    for &mask in &masks {
        let subset: Vec<&str> = (0..n)
            .filter(|&i| (mask >> i) & 1 == 1)
            .map(|i| all_set_names[i].as_str())
            .collect();
        labels.push(subset.join("&"));
        keys.push(Combination::new(&subset));
    }

    (labels, keys)
}

/// Build the R list result from fitted shape params + metric HashMaps.
#[allow(clippy::too_many_arguments)]
fn build_result_list(
    all_set_names: &[String],
    fitted_set_names: Vec<String>,
    h: Vec<f64>,
    k: Vec<f64>,
    a: Vec<f64>,
    b: Vec<f64>,
    phi: Vec<f64>,
    requested: &HashMap<Combination, f64>,
    fitted: &HashMap<Combination, f64>,
    residuals: &HashMap<Combination, f64>,
    region_error: &HashMap<Combination, f64>,
    diag_error: f64,
    stress: f64,
) -> List {
    let (combo_labels, combo_keys) = enumerate_combinations(all_set_names);
    let n_combos = combo_keys.len();

    let mut orig_vec = Vec::with_capacity(n_combos);
    let mut fit_vec = Vec::with_capacity(n_combos);
    let mut resid_vec = Vec::with_capacity(n_combos);
    let mut region_err_vec = Vec::with_capacity(n_combos);

    for combo in &combo_keys {
        orig_vec.push(requested.get(combo).copied().unwrap_or(0.0));
        fit_vec.push(fitted.get(combo).copied().unwrap_or(0.0));
        resid_vec.push(residuals.get(combo).copied().unwrap_or(0.0));
        region_err_vec.push(region_error.get(combo).copied().unwrap_or(0.0));
    }

    list!(
        all_set_names = all_set_names.to_vec(),
        fitted_set_names = fitted_set_names,
        h = h,
        k = k,
        a = a,
        b = b,
        phi = phi,
        combo_labels = combo_labels,
        original_values = orig_vec,
        fitted_values = fit_vec,
        residuals = resid_vec,
        region_error = region_err_vec,
        diag_error = diag_error,
        stress = stress,
    )
}

/// Build result for edge cases (0 or 1 non-empty sets). Uses spec's exclusive
/// areas for `original_values`; fitted/residuals/region_error are all zero.
fn build_edge_case_list(
    all_set_names: &[String],
    spec: &DiagramSpec,
    non_empty: &[String],
) -> List {
    let (combo_labels, combo_keys) = enumerate_combinations(all_set_names);
    let n_combos = combo_keys.len();

    let mut orig_vec = Vec::with_capacity(n_combos);
    for combo in &combo_keys {
        orig_vec.push(
            spec.exclusive_areas()
                .get(combo)
                .copied()
                .unwrap_or(0.0),
        );
    }

    let fit_vec = vec![0.0f64; n_combos];
    let resid_vec = vec![0.0f64; n_combos];
    let region_err_vec = vec![0.0f64; n_combos];

    let (h, k, a, b, phi, fitted_set_names) = if non_empty.len() == 1 {
        // Single set: circle at origin with radius sqrt(area/π)
        let name = &non_empty[0];
        let combo = Combination::new(&[name.as_str()]);
        let area = spec
            .inclusive_areas()
            .get(&combo)
            .copied()
            .unwrap_or(0.0);
        let r = (area / std::f64::consts::PI).sqrt();
        (
            vec![0.0],
            vec![0.0],
            vec![r],
            vec![r],
            vec![0.0],
            vec![name.clone()],
        )
    } else {
        // All empty: no fitted shapes
        (vec![], vec![], vec![], vec![], vec![], vec![])
    };

    // For single-set case, fitted == requested (perfect fit)
    let fit_vec = if non_empty.len() == 1 {
        orig_vec.clone()
    } else {
        fit_vec
    };

    list!(
        all_set_names = all_set_names.to_vec(),
        fitted_set_names = fitted_set_names,
        h = h,
        k = k,
        a = a,
        b = b,
        phi = phi,
        combo_labels = combo_labels,
        original_values = orig_vec,
        fitted_values = fit_vec,
        residuals = resid_vec,
        region_error = region_err_vec,
        diag_error = 0.0,
        stress = 0.0,
    )
}

/// Convert a shape's params into [h, k, a, b, phi].
fn shape_to_euler_params<S: DiagramShape>(shape: &S, is_circle: bool) -> [f64; 5] {
    let p = shape.to_params();
    if is_circle {
        [p[0], p[1], p[2], p[2], 0.0]
    } else {
        [p[0], p[1], p[2], p[3], p[4]]
    }
}

/// Bit-index matrix: (2^n - 1) × n integer matrix where row i's column j
/// indicates whether set j is included in subset i. Rows are in legacy
/// cardinality-first order (see `legacy_subset_masks`) so that downstream
/// positional code (`fills.grob.<i>`, plotter region indexing) keeps
/// matching the legacy Rcpp/C++ behavior.
/// @keywords internal
#[extendr]
fn bit_index_rust(n: i32) -> Robj {
    let n = n as usize;
    if n == 0 {
        let empty: Vec<i32> = vec![];
        let mut robj: Robj = empty.into_robj();
        robj.set_attrib("dim", [0i32, 0i32]).unwrap();
        return robj;
    }

    let masks = legacy_subset_masks(n);
    let nrows = masks.len();
    // Column-major (R matrix convention).
    let data: Vec<i32> = (0..n)
        .flat_map(|col| masks.iter().map(move |&mask| ((mask >> col) & 1) as i32))
        .collect();

    let mut robj: Robj = data.into_robj();
    robj.set_attrib("dim", [nrows as i32, n as i32]).unwrap();
    robj
}

/// Fit an Euler diagram using the eunoia Rust library.
/// @keywords internal
#[extendr]
fn fit_euler_diagram(
    combo_names: Vec<String>,
    combo_values: Vec<f64>,
    input: &str,
    shape: &str,
    loss: &str,
    loss_aggregator: &str,
    extraopt_threshold: Robj,
    tolerance: Robj,
    seed: i32,
) -> extendr_api::Result<List> {
    if combo_names.len() != combo_values.len() {
        return Err("combo_names and combo_values must be the same length".into());
    }

    let all_set_names = extract_set_names(&combo_names);
    let n = all_set_names.len();

    // Edge case: no sets at all
    if n == 0 {
        // Build an empty spec-equivalent result
        let combo_labels: Vec<String> = vec![];
        let empty_vec: Vec<f64> = vec![];
        let empty_names: Vec<String> = vec![];
        return Ok(list!(
            all_set_names = empty_names.clone(),
            fitted_set_names = empty_names,
            h = empty_vec.clone(),
            k = empty_vec.clone(),
            a = empty_vec.clone(),
            b = empty_vec.clone(),
            phi = empty_vec.clone(),
            combo_labels = combo_labels,
            original_values = empty_vec.clone(),
            fitted_values = empty_vec.clone(),
            residuals = empty_vec.clone(),
            region_error = empty_vec,
            diag_error = 0.0,
            stress = 0.0,
        ));
    }

    let input_type = parse_input_type(input)?;
    let loss_type = parse_loss_type(loss, loss_aggregator)?;

    let cmaes_threshold: Option<f64> = if extraopt_threshold.is_null() {
        None
    } else {
        extraopt_threshold.as_real()
    };

    let tolerance_opt: Option<f64> = if tolerance.is_null() {
        None
    } else {
        tolerance.as_real()
    };

    let seed_u64 = seed as u32 as u64;

    // Build DiagramSpec
    let mut builder = DiagramSpecBuilder::new();
    for (name, &value) in combo_names.iter().zip(combo_values.iter()) {
        let sets: Vec<&str> = name.split('&').filter(|s| !s.is_empty()).collect();
        if sets.is_empty() {
            continue;
        }
        if sets.len() == 1 {
            builder = builder.set(sets[0], value);
        } else {
            builder = builder.intersection(&sets, value);
        }
    }
    let spec = builder
        .input_type(input_type)
        .build()
        .unwrap_or_else(|e| throw_r_error(&format!("eunoia spec error: {}", e)));

    // Determine non-empty sets and handle edge cases
    let non_empty = non_empty_set_names(&spec);

    if non_empty.len() <= 1 {
        return Ok(build_edge_case_list(&all_set_names, &spec, &non_empty));
    }

    // Main fitting path: >= 2 non-empty sets
    match shape {
        "circle" => {
            let mut fitter = Fitter::<Circle>::new(&spec)
                .loss_type(loss_type)
                .seed(seed_u64);
            if let Some(t) = cmaes_threshold {
                fitter = fitter.cmaes_fallback_threshold(t);
            }
            if let Some(tol) = tolerance_opt {
                fitter = fitter.tolerance(tol);
            }
            let layout = fitter
                .fit()
                .unwrap_or_else(|e| throw_r_error(&format!("eunoia fit error: {}", e)));

            let mut h_vec = Vec::with_capacity(non_empty.len());
            let mut k_vec = Vec::with_capacity(non_empty.len());
            let mut a_vec = Vec::with_capacity(non_empty.len());
            let mut b_vec = Vec::with_capacity(non_empty.len());
            let mut phi_vec = Vec::with_capacity(non_empty.len());

            for name in &non_empty {
                let shape_ref = layout
                    .shape_for_set(name)
                    .ok_or_else(|| Error::from(format!("missing shape for set {}", name)))?;
                let p = shape_to_euler_params(shape_ref, true);
                h_vec.push(p[0]);
                k_vec.push(p[1]);
                a_vec.push(p[2]);
                b_vec.push(p[3]);
                phi_vec.push(p[4]);
            }

            let requested = layout.requested().clone();
            let fitted = layout.fitted().clone();
            let residuals = layout.residuals();
            let region_error = layout.region_error();
            let diag_error = layout.diag_error();
            let stress = layout.stress();

            Ok(build_result_list(
                &all_set_names,
                non_empty,
                h_vec,
                k_vec,
                a_vec,
                b_vec,
                phi_vec,
                &requested,
                &fitted,
                &residuals,
                &region_error,
                diag_error,
                stress,
            ))
        }
        "ellipse" => {
            let mut fitter = Fitter::<Ellipse>::new(&spec)
                .loss_type(loss_type)
                .seed(seed_u64);
            if let Some(t) = cmaes_threshold {
                fitter = fitter.cmaes_fallback_threshold(t);
            }
            if let Some(tol) = tolerance_opt {
                fitter = fitter.tolerance(tol);
            }
            let layout = fitter
                .fit()
                .unwrap_or_else(|e| throw_r_error(&format!("eunoia fit error: {}", e)));

            let mut h_vec = Vec::with_capacity(non_empty.len());
            let mut k_vec = Vec::with_capacity(non_empty.len());
            let mut a_vec = Vec::with_capacity(non_empty.len());
            let mut b_vec = Vec::with_capacity(non_empty.len());
            let mut phi_vec = Vec::with_capacity(non_empty.len());

            for name in &non_empty {
                let shape_ref = layout
                    .shape_for_set(name)
                    .ok_or_else(|| Error::from(format!("missing shape for set {}", name)))?;
                let p = shape_to_euler_params(shape_ref, false);
                h_vec.push(p[0]);
                k_vec.push(p[1]);
                a_vec.push(p[2]);
                b_vec.push(p[3]);
                phi_vec.push(p[4]);
            }

            let requested = layout.requested().clone();
            let fitted = layout.fitted().clone();
            let residuals = layout.residuals();
            let region_error = layout.region_error();
            let diag_error = layout.diag_error();
            let stress = layout.stress();

            Ok(build_result_list(
                &all_set_names,
                non_empty,
                h_vec,
                k_vec,
                a_vec,
                b_vec,
                phi_vec,
                &requested,
                &fitted,
                &residuals,
                &region_error,
                diag_error,
                stress,
            ))
        }
        other => Err(format!("Unknown shape: {}", other).into()),
    }
}

/// Compute the input-order bitmask of a region's set membership over
/// `set_names`. Each region is keyed by a `Combination` whose set names are
/// alphabetically sorted; we recover eulerr's positional row index by ORing
/// `1 << j` for each set's input-order index `j`.
fn region_mask_in_input_order(combo: &Combination, set_names: &[String]) -> Option<usize> {
    let mut mask = 0usize;
    for s in combo.sets() {
        let j = set_names.iter().position(|n| n == s)?;
        mask |= 1usize << j;
    }
    Some(mask)
}

/// Flatten a list of polygons into eulerr's `(x, y, id_lengths)` triple.
/// Empty polygons are skipped; coordinate vectors are concatenated in order.
fn polygons_to_xy_list(polygons: &[Polygon]) -> List {
    let mut x: Vec<f64> = Vec::new();
    let mut y: Vec<f64> = Vec::new();
    let mut id_lengths: Vec<i32> = Vec::new();
    for poly in polygons {
        let verts = poly.vertices();
        if verts.is_empty() {
            continue;
        }
        for v in verts {
            x.push(v.x());
            y.push(v.y());
        }
        id_lengths.push(verts.len() as i32);
    }
    list!(x = x, y = y, id_lengths = id_lengths)
}

/// Build a minimal `DiagramSpec` from a list of set names. The plotting
/// path's `decompose_regions` ignores the spec's areas and only consults the
/// set names, so dummy values are fine.
fn build_dummy_spec(set_names: &[String]) -> std::result::Result<DiagramSpec, Error> {
    let mut builder = DiagramSpecBuilder::new();
    for name in set_names {
        builder = builder.set(name.as_str(), 1.0);
    }
    builder
        .input_type(InputType::Exclusive)
        .build()
        .map_err(|e| format!("eunoia spec build error: {}", e).into())
}

/// Compute polygon geometry and label anchors for plotting a fitted Euler
/// diagram.
///
/// Inputs are the fitted shape parameters for the **non-empty** sets only,
/// in the order eulerr stores them (`x$ellipses` rows after dropping rows
/// with NA). The output is positional over `2^n - 1` rows in eulerr's
/// `bit_indexr(n)` order, with `NULL` entries for regions the fitted
/// geometry doesn't populate.
///
/// @keywords internal
#[extendr]
#[allow(clippy::too_many_arguments)]
fn euler_plot_data(
    set_names: Vec<String>,
    h: Vec<f64>,
    k: Vec<f64>,
    a: Vec<f64>,
    b: Vec<f64>,
    phi: Vec<f64>,
    n_vertices: i32,
    label_precision: f64,
) -> extendr_api::Result<List> {
    let n = set_names.len();
    if n == 0 {
        return Ok(list!(
            set_polygons = List::new(0),
            region_polygons = List::new(0),
            region_centers_x = Vec::<f64>::new(),
            region_centers_y = Vec::<f64>::new(),
        ));
    }
    if h.len() != n || k.len() != n || a.len() != n || b.len() != n || phi.len() != n {
        return Err(
            "set_names and shape parameter vectors must all have the same length".into(),
        );
    }

    let n_vertices = n_vertices.max(3) as usize;

    let ellipses: Vec<Ellipse> = (0..n)
        .map(|i| Ellipse::new(Point::new(h[i], k[i]), a[i], b[i], phi[i]))
        .collect();

    // Per-set polygon outlines (input order).
    let set_polygons: Vec<Robj> = ellipses
        .iter()
        .map(|e| {
            use eunoia::geometry::traits::Polygonize;
            let p = e.polygonize(n_vertices);
            let mut x: Vec<f64> = Vec::with_capacity(p.vertices().len());
            let mut y: Vec<f64> = Vec::with_capacity(p.vertices().len());
            for v in p.vertices() {
                x.push(v.x());
                y.push(v.y());
            }
            list!(x = x, y = y).into()
        })
        .collect();

    let spec = build_dummy_spec(&set_names)?;
    let regions = decompose_regions(&ellipses, &set_names, &spec, n_vertices);
    let anchors = regions.label_points(label_precision);

    let masks = legacy_subset_masks(n);
    let n_id = masks.len();

    // Map mask -> row index in legacy positional order.
    let mut mask_to_row: HashMap<usize, usize> = HashMap::with_capacity(n_id);
    for (row, &m) in masks.iter().enumerate() {
        mask_to_row.insert(m, row);
    }

    let mut region_polygons: Vec<Robj> = (0..n_id).map(|_| Robj::from(())).collect();
    let mut centers_x: Vec<f64> = vec![f64::na(); n_id];
    let mut centers_y: Vec<f64> = vec![f64::na(); n_id];

    for (combo, polys) in regions.iter() {
        let Some(mask) = region_mask_in_input_order(combo, &set_names) else {
            continue;
        };
        let Some(&row) = mask_to_row.get(&mask) else {
            continue;
        };
        region_polygons[row] = polygons_to_xy_list(polys).into();
        if let Some(point) = anchors.get(combo) {
            centers_x[row] = point.x();
            centers_y[row] = point.y();
        }
    }

    Ok(list!(
        set_polygons = List::from_values(set_polygons),
        region_polygons = List::from_values(region_polygons),
        region_centers_x = centers_x,
        region_centers_y = centers_y,
    ))
}

/// Clip a (possibly multi-polygon) subject path against a single clip
/// polygon. Mirrors the slice of `polyclip::polyclip` behavior eulerr
/// actually uses at the stripe-pattern site.
///
/// @keywords internal
#[extendr]
fn polygon_clip_rust(
    subject_x: Vec<f64>,
    subject_y: Vec<f64>,
    subject_id_lengths: Vec<i32>,
    clip_x: Vec<f64>,
    clip_y: Vec<f64>,
    op: &str,
) -> extendr_api::Result<List> {
    if subject_x.len() != subject_y.len() {
        return Err("subject_x and subject_y must have the same length".into());
    }
    if clip_x.len() != clip_y.len() {
        return Err("clip_x and clip_y must have the same length".into());
    }
    let total_subject: usize = subject_id_lengths.iter().map(|&n| n as usize).sum();
    if total_subject != subject_x.len() {
        return Err(
            "sum(subject_id_lengths) must equal length(subject_x)".into(),
        );
    }

    let operation = match op {
        "intersection" => ClipOperation::Intersection,
        "union" => ClipOperation::Union,
        "minus" | "difference" => ClipOperation::Difference,
        "xor" => ClipOperation::Xor,
        other => return Err(format!("Unknown clip operation: {}", other).into()),
    };

    let clip_poly = if clip_x.is_empty() {
        Polygon::new(Vec::new())
    } else {
        Polygon::new(
            clip_x
                .iter()
                .zip(clip_y.iter())
                .map(|(&x, &y)| Point::new(x, y))
                .collect(),
        )
    };

    // Split the subject into individual polygons by id_lengths.
    let mut result_polys: Vec<Polygon> = Vec::new();
    let mut cursor = 0usize;
    for &len in &subject_id_lengths {
        let len = len as usize;
        let verts: Vec<Point> = (cursor..cursor + len)
            .map(|i| Point::new(subject_x[i], subject_y[i]))
            .collect();
        cursor += len;
        let subject_poly = Polygon::new(verts);
        let pieces = polygon_clip(&subject_poly, &clip_poly, operation);
        result_polys.extend(pieces);
    }

    Ok(polygons_to_xy_list(&result_polys))
}

// Macro to generate exports.
extendr_module! {
    mod eulerr;
    fn bit_index_rust;
    fn fit_euler_diagram;
    fn euler_plot_data;
    fn polygon_clip_rust;
}

#[cfg(test)]
mod tests {
    use super::legacy_subset_masks;

    #[test]
    fn legacy_subset_masks_n3_matches_cardinality_first_order() {
        // Locked-down contract: positional plotter code in eulerr (`fills.grob.<i>`)
        // depends on this exact order matching the legacy C++ `bit_index`.
        // Order: cardinality 1 (A, B, C), then 2 (A&B, A&C, B&C), then 3 (A&B&C).
        assert_eq!(
            legacy_subset_masks(3),
            vec![0b001, 0b010, 0b100, 0b011, 0b101, 0b110, 0b111]
        );
    }

    #[test]
    fn legacy_subset_masks_n0_is_empty() {
        assert_eq!(legacy_subset_masks(0), Vec::<usize>::new());
    }

    #[test]
    fn legacy_subset_masks_n1() {
        assert_eq!(legacy_subset_masks(1), vec![0b1]);
    }

    #[test]
    fn legacy_subset_masks_n4_first_and_last() {
        let m = legacy_subset_masks(4);
        // 4 singletons, then 6 pairs, then 4 triples, then 1 quadruple = 15
        assert_eq!(m.len(), 15);
        // First 4: singletons in increasing-bit order
        assert_eq!(&m[..4], &[0b0001, 0b0010, 0b0100, 0b1000]);
        // Last: full set
        assert_eq!(*m.last().unwrap(), 0b1111);
    }
}
