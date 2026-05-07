use extendr_api::prelude::*;
use extendr_api::throw_r_error;
use eunoia::{
    constants::{MAX_SETS, MAX_SETS_HARD_CAP},
    geometry::{
        primitives::Point,
        shapes::{Circle, Ellipse, Polygon, Rectangle},
        traits::{DiagramShape, Polygonize},
    },
    loss::LossType,
    plotting::{decompose_regions, polygon_clip, ClipOperation, RegionPiece},
    spec::DiagramSpec,
    Combination, DiagramError, DiagramSpecBuilder, Fitter, InputType,
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

fn parse_loss_type(loss: &str) -> std::result::Result<LossType, Error> {
    match loss {
        "sum_squared" => Ok(LossType::SumSquared),
        "sum_absolute" => Ok(LossType::SumAbsoute),
        "sum_absolute_region_error" => Ok(LossType::SumAbsoluteRegionError),
        "sum_squared_region_error" => Ok(LossType::SumSquaredRegionError),
        "max_absolute" => Ok(LossType::MaxAbsolute),
        "max_squared" => Ok(LossType::MaxSquared),
        "root_mean_squared" => Ok(LossType::RootMeanSquared),
        "stress" => Ok(LossType::Stress),
        "diag_error" => Ok(LossType::DiagError),
        other => Err(format!("Unknown loss type: {}", other).into()),
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

/// Build a label for a Combination using set names ordered by their first
/// appearance in `input_order`. Returns None if any set in the combination is
/// missing from `input_order`.
fn combo_label(combo: &Combination, input_order: &[String]) -> Option<String> {
    let mut indices: Vec<usize> = combo
        .sets()
        .iter()
        .map(|s| input_order.iter().position(|n| n == s))
        .collect::<Option<Vec<_>>>()?;
    indices.sort_unstable();
    Some(
        indices
            .into_iter()
            .map(|i| input_order[i].as_str())
            .collect::<Vec<_>>()
            .join("&"),
    )
}

/// Build a stable, deterministic ordering of the union of `requested` and
/// `fitted` keys: by cardinality (ascending), then by input-order indices
/// (lexicographic). The empty combination (complement region) is skipped
/// here — eulerr surfaces the complement via a dedicated result field.
fn ordered_combo_keys<'a>(
    requested: &'a HashMap<Combination, f64>,
    fitted: &'a HashMap<Combination, f64>,
    input_order: &[String],
) -> Vec<&'a Combination> {
    let mut seen: HashSet<&Combination> = HashSet::new();
    let mut keys: Vec<&Combination> = Vec::new();
    for k in requested.keys() {
        if !k.is_empty() && seen.insert(k) {
            keys.push(k);
        }
    }
    for k in fitted.keys() {
        if !k.is_empty() && seen.insert(k) {
            keys.push(k);
        }
    }
    keys.sort_by_key(|c| {
        let mut indices: Vec<usize> = c
            .sets()
            .iter()
            .map(|s| input_order.iter().position(|n| n == s).unwrap_or(usize::MAX))
            .collect();
        indices.sort_unstable();
        (indices.len(), indices)
    });
    keys
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
    container: Option<&Rectangle>,
) -> List {
    let combo_keys = ordered_combo_keys(requested, fitted, all_set_names);
    let n_combos = combo_keys.len();

    let mut combo_labels = Vec::with_capacity(n_combos);
    let mut orig_vec = Vec::with_capacity(n_combos);
    let mut fit_vec = Vec::with_capacity(n_combos);
    let mut resid_vec = Vec::with_capacity(n_combos);
    let mut region_err_vec = Vec::with_capacity(n_combos);

    for combo in &combo_keys {
        let label = combo_label(combo, all_set_names)
            .unwrap_or_else(|| combo.sets().join("&"));
        combo_labels.push(label);
        orig_vec.push(requested.get(combo).copied().unwrap_or(0.0));
        fit_vec.push(fitted.get(combo).copied().unwrap_or(0.0));
        resid_vec.push(residuals.get(combo).copied().unwrap_or(0.0));
        region_err_vec.push(region_error.get(combo).copied().unwrap_or(0.0));
    }

    let (container_h, container_k, container_w, container_height) = match container {
        Some(rect) => (
            rect.center().x(),
            rect.center().y(),
            rect.width(),
            rect.height(),
        ),
        None => (f64::NAN, f64::NAN, f64::NAN, f64::NAN),
    };
    let has_container = container.is_some();

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
        container_h = container_h,
        container_k = container_k,
        container_width = container_w,
        container_height = container_height,
        has_container = has_container,
    )
}

/// Build result for edge cases (0 or 1 non-empty sets). Sparse: only the
/// combinations present in the spec's exclusive areas are returned.
fn build_edge_case_list(
    all_set_names: &[String],
    spec: &DiagramSpec,
    non_empty: &[String],
) -> List {
    // Treat the spec's exclusive areas as the "requested" map and produce an
    // empty fitted map for the 0-set case (or fitted == requested for the
    // single-set case below).
    let requested: HashMap<Combination, f64> = spec.exclusive_areas().clone();

    let (h, k, a, b, phi, fitted_set_names, fitted_map) = if non_empty.len() == 1 {
        let name = &non_empty[0];
        let combo = Combination::new(&[name.as_str()]);
        let area = spec
            .inclusive_areas()
            .get(&combo)
            .copied()
            .unwrap_or(0.0);
        let r = (area / std::f64::consts::PI).sqrt();
        // Fitted equals requested for the single-set perfect-fit case.
        (
            vec![0.0],
            vec![0.0],
            vec![r],
            vec![r],
            vec![0.0],
            vec![name.clone()],
            requested.clone(),
        )
    } else {
        (
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
        )
    };

    let zero_map: HashMap<Combination, f64> = HashMap::new();
    build_result_list(
        all_set_names,
        fitted_set_names,
        h,
        k,
        a,
        b,
        phi,
        &requested,
        &fitted_map,
        &zero_map,
        &zero_map,
        0.0,
        0.0,
        None,
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

/// Fit an Euler diagram using the eunoia Rust library.
/// @keywords internal
#[extendr]
#[allow(clippy::too_many_arguments)]
fn fit_euler_diagram(
    combo_names: Vec<String>,
    combo_values: Vec<f64>,
    input: &str,
    shape: &str,
    loss: &str,
    extraopt_threshold: Robj,
    tolerance: Robj,
    max_sets: Robj,
    complement: Robj,
    seed: i32,
) -> extendr_api::Result<List> {
    if combo_names.len() != combo_values.len() {
        return Err("combo_names and combo_values must be the same length".into());
    }

    let all_set_names = extract_set_names(&combo_names);
    let n = all_set_names.len();

    if n == 0 {
        let empty_vec: Vec<f64> = vec![];
        let empty_names: Vec<String> = vec![];
        let combo_labels: Vec<String> = vec![];
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
            container_h = f64::NAN,
            container_k = f64::NAN,
            container_width = f64::NAN,
            container_height = f64::NAN,
            has_container = false,
        ));
    }

    let input_type = parse_input_type(input)?;
    let loss_type = parse_loss_type(loss)?;

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

    let max_sets_opt: Option<usize> = if max_sets.is_null() {
        None
    } else {
        max_sets
            .as_real()
            .filter(|v| v.is_finite() && *v >= 1.0)
            .map(|v| v as usize)
    };

    let complement_opt: Option<f64> = if complement.is_null() {
        None
    } else {
        complement.as_real()
    };

    let seed_u64 = seed as u32 as u64;

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
    if let Some(m) = max_sets_opt {
        builder = builder.max_sets(m);
    }
    if let Some(c) = complement_opt {
        builder = builder.complement(c);
    }
    let spec = builder
        .input_type(input_type)
        .build()
        .unwrap_or_else(|e| throw_r_error(&format!("eunoia spec error: {}", e)));

    let non_empty = non_empty_set_names(&spec);

    // Edge cases (0 or 1 non-empty sets) have a perfect closed-form fit, so
    // we bypass the optimizer. The current eunoia fitter rejects single-set
    // specs even with a complement, so the bypass also covers that case;
    // the R wrapper synthesises a container around the lone disk for the
    // n_e == 1 path.
    if non_empty.len() <= 1 {
        return Ok(build_edge_case_list(&all_set_names, &spec, &non_empty));
    }

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
            let container = layout.container().copied();

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
                container.as_ref(),
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
            let container = layout.container().copied();

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
                container.as_ref(),
            ))
        }
        other => Err(format!("Unknown shape: {}", other).into()),
    }
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

/// Flatten a region's pieces (each an outer ring + zero-or-more holes) into
/// eulerr's `(x, y, id_lengths)` triple. Outer rings are CCW and holes are
/// CW (eunoia normalises them), so `grid::pathGrob` with the default
/// `rule = "winding"` (nonzero) renders outer-minus-holes correctly.
fn region_pieces_to_xy_list(pieces: &[RegionPiece]) -> List {
    let mut x: Vec<f64> = Vec::new();
    let mut y: Vec<f64> = Vec::new();
    let mut id_lengths: Vec<i32> = Vec::new();
    let push_ring = |verts: &[Point], x: &mut Vec<f64>, y: &mut Vec<f64>, id_lengths: &mut Vec<i32>| {
        if verts.is_empty() {
            return;
        }
        for v in verts {
            x.push(v.x());
            y.push(v.y());
        }
        id_lengths.push(verts.len() as i32);
    };
    for piece in pieces {
        push_ring(piece.outer.vertices(), &mut x, &mut y, &mut id_lengths);
        for hole in &piece.holes {
            push_ring(hole.vertices(), &mut x, &mut y, &mut id_lengths);
        }
    }
    list!(x = x, y = y, id_lengths = id_lengths)
}

fn empty_region_xy_list() -> List {
    list!(
        x = Vec::<f64>::new(),
        y = Vec::<f64>::new(),
        id_lengths = Vec::<i32>::new()
    )
}

/// Read an `Option<f64>` from a possibly-NULL R scalar.
fn read_optional_f64(obj: &Robj) -> Option<f64> {
    if obj.is_null() {
        None
    } else {
        obj.as_real()
    }
}

/// Build the optional fitted container rectangle from raw R scalars. Returns
/// `None` when any field is NULL/non-finite/non-positive — matches the
/// "no container" branch on the R side.
fn build_container(
    container_h: &Robj,
    container_k: &Robj,
    container_width: &Robj,
    container_height: &Robj,
) -> Option<Rectangle> {
    let ch = read_optional_f64(container_h)?;
    let ck = read_optional_f64(container_k)?;
    let cw = read_optional_f64(container_width)?;
    let chh = read_optional_f64(container_height)?;
    if !ch.is_finite() || !ck.is_finite() || !cw.is_finite() || !chh.is_finite() {
        return None;
    }
    if cw <= 0.0 || chh <= 0.0 {
        return None;
    }
    Some(Rectangle::new(Point::new(ch, ck), cw, chh))
}

/// Compute polygon geometry and label anchors for plotting a fitted Euler
/// diagram, including the optional complement region inside a fitted
/// container.
///
/// Inputs are the fitted shape parameters for the **non-empty** sets only,
/// in the order eulerr stores them (`x$ellipses` rows after dropping rows
/// with NA). When `container_*` are non-NULL they describe the fitted
/// universe-box rectangle; in that case the result also carries the
/// complement region geometry (the area inside the rectangle outside every
/// shape) and a label anchor for it. Eunoia's `decompose_regions` emits this
/// region under the empty `Combination` whenever the spec carries a
/// complement and a container is supplied — so eulerr no longer needs a
/// hand-rolled rectangle-minus-shapes pass.
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
    container_h: Robj,
    container_k: Robj,
    container_width: Robj,
    container_height: Robj,
    n_vertices: i32,
    label_precision: f64,
) -> extendr_api::Result<List> {
    let n = set_names.len();
    if n == 0 {
        return Ok(list!(
            set_polygons = List::new(0),
            region_labels = Vec::<String>::new(),
            region_polygons = List::new(0),
            region_centers_x = Vec::<f64>::new(),
            region_centers_y = Vec::<f64>::new(),
            has_complement = false,
            complement_polygon = empty_region_xy_list(),
            complement_label_x = f64::NAN,
            complement_label_y = f64::NAN,
            container_outline_x = Vec::<f64>::new(),
            container_outline_y = Vec::<f64>::new(),
        ));
    }
    if h.len() != n || k.len() != n || a.len() != n || b.len() != n || phi.len() != n {
        return Err(
            "set_names and shape parameter vectors must all have the same length".into(),
        );
    }

    let n_vertices = n_vertices.max(3) as usize;

    // Validate at the FFI boundary: shape params come from R, so use
    // `try_new` and surface eunoia's `InvalidShapeParameter` as a readable R
    // error rather than panicking inside `new`.
    let ellipses: Vec<Ellipse> = (0..n)
        .map(|i| {
            Ellipse::try_new(Point::new(h[i], k[i]), a[i], b[i], phi[i]).map_err(|e| match e {
                DiagramError::InvalidShapeParameter {
                    shape,
                    param,
                    value,
                } => Error::from(format!(
                    "invalid {} parameter `{}` for set `{}`: {}",
                    shape, param, set_names[i], value
                )),
                other => Error::from(format!("eunoia ellipse error: {}", other)),
            })
        })
        .collect::<extendr_api::Result<Vec<_>>>()?;

    let container = build_container(
        &container_h,
        &container_k,
        &container_width,
        &container_height,
    );
    let has_complement = container.is_some();

    // Per-set polygon outlines (input order). eunoia doesn't repeat the first
    // vertex; close the ring here so polylineGrob (used for edges) draws the
    // closing segment instead of leaving a visible gap.
    let set_polygons: Vec<Robj> = ellipses
        .iter()
        .map(|e| {
            let p = e.polygonize(n_vertices);
            let verts = p.vertices();
            let mut x: Vec<f64> = Vec::with_capacity(verts.len() + 1);
            let mut y: Vec<f64> = Vec::with_capacity(verts.len() + 1);
            for v in verts {
                x.push(v.x());
                y.push(v.y());
            }
            if let Some(first) = verts.first() {
                x.push(first.x());
                y.push(first.y());
            }
            list!(x = x, y = y).into()
        })
        .collect();

    // Build a spec just for region decomposition. The areas are dummies —
    // `decompose_regions` reads only set names and `spec.complement()`. When a
    // container is supplied, flag the spec with `.complement(...)` so eunoia
    // emits the complement region under the empty `Combination`.
    let mut builder = DiagramSpecBuilder::new();
    for name in &set_names {
        builder = builder.set(name.as_str(), 1.0);
    }
    if has_complement {
        builder = builder.complement(1.0);
    }
    let spec = builder
        .input_type(InputType::Exclusive)
        .build()
        .map_err(|e| Error::from(format!("eunoia spec build error: {}", e)))?;

    let regions = decompose_regions(
        &ellipses,
        &set_names,
        &spec,
        container.as_ref(),
        n_vertices,
    );
    let region_anchors = regions.label_points(label_precision);

    // Walk regions in canonical input order (singletons → pairs → triples,
    // ties broken by input-order indices) so the output is deterministic
    // without the R side having to sort. Skip the empty (complement)
    // combination here — it's surfaced separately via the dedicated
    // `complement_*`/`container_outline_*` fields.
    let mut region_labels: Vec<String> = Vec::new();
    let mut region_polygons: Vec<Robj> = Vec::new();
    let mut centers_x: Vec<f64> = Vec::new();
    let mut centers_y: Vec<f64> = Vec::new();

    for (combo, polys) in regions.iter_in_input_order(&set_names) {
        if combo.is_empty() {
            continue;
        }
        let Some(label) = combo_label(combo, &set_names) else {
            continue;
        };
        region_labels.push(label);
        region_polygons.push(region_pieces_to_xy_list(polys).into());
        if let Some(point) = region_anchors.get(combo) {
            centers_x.push(point.x());
            centers_y.push(point.y());
        } else {
            centers_x.push(f64::na());
            centers_y.push(f64::na());
        }
    }

    let empty_combo = Combination::new(&[]);
    let (complement_polygon, complement_label_x, complement_label_y, outline_x, outline_y) =
        match container.as_ref() {
            Some(rect) => {
                let pieces = regions.get(&empty_combo);
                let polygon_list = match pieces {
                    Some(p) => region_pieces_to_xy_list(p),
                    None => empty_region_xy_list(),
                };
                let cx = rect.center().x();
                let cy = rect.center().y();
                let half_w = rect.width() / 2.0;
                let half_h = rect.height() / 2.0;
                let (anchor_x, anchor_y) = match region_anchors.get(&empty_combo) {
                    Some(p) => (p.x(), p.y()),
                    None => {
                        // No complement area / POI failed: fall back to a
                        // top-right inset so the count label still has a
                        // sensible anchor.
                        let inset = rect.width().min(rect.height()) * 0.05;
                        (cx + half_w - inset, cy + half_h - inset)
                    }
                };
                let outline_x = vec![
                    cx - half_w,
                    cx + half_w,
                    cx + half_w,
                    cx - half_w,
                    cx - half_w,
                ];
                let outline_y = vec![
                    cy - half_h,
                    cy - half_h,
                    cy + half_h,
                    cy + half_h,
                    cy - half_h,
                ];
                (polygon_list, anchor_x, anchor_y, outline_x, outline_y)
            }
            None => (
                empty_region_xy_list(),
                f64::NAN,
                f64::NAN,
                Vec::new(),
                Vec::new(),
            ),
        };

    Ok(list!(
        set_polygons = List::from_values(set_polygons),
        region_labels = region_labels,
        region_polygons = List::from_values(region_polygons),
        region_centers_x = centers_x,
        region_centers_y = centers_y,
        has_complement = has_complement,
        complement_polygon = complement_polygon,
        complement_label_x = complement_label_x,
        complement_label_y = complement_label_y,
        container_outline_x = outline_x,
        container_outline_y = outline_y,
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

/// Default number of sets that `eunoia` accepts before rejecting a spec.
/// Used by the R-side input validator so the cap is not hardcoded.
/// @keywords internal
#[extendr]
fn max_sets_default() -> i32 {
    MAX_SETS as i32
}

/// Absolute upper bound on the number of sets that `eunoia` can represent
/// in a single diagram. Used by the R-side input validator so the cap is
/// not hardcoded.
/// @keywords internal
#[extendr]
fn max_sets_hard_cap() -> i32 {
    MAX_SETS_HARD_CAP as i32
}

// Macro to generate exports.
extendr_module! {
    mod eulerr;
    fn fit_euler_diagram;
    fn euler_plot_data;
    fn polygon_clip_rust;
    fn max_sets_default;
    fn max_sets_hard_cap;
}
