use eunoia::{
    Combination, DiagramError, DiagramSpecBuilder, Fitter, InputType, Optimizer,
    constants::{MAX_SETS, MAX_SETS_HARD_CAP},
    geometry::{
        primitives::Point,
        shapes::{Circle, Ellipse, Polygon, Rectangle, RotatedRectangle, Square},
        traits::{DiagramShape, Polygonize},
    },
    loss::LossType,
    plotting::{
        ClipOperation, ElbowOptions, ExteriorPolicy, LeaderStrategy, PlacementKind,
        PlacementStrategy, RegionPiece, RegionPolygons, TetherSource, decompose_regions,
        place_labels, placements_bbox, polygon_clip,
    },
    spec::DiagramSpec,
};
use extendr_api::prelude::*;
use extendr_api::throw_r_error;
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

fn parse_loss_type(loss: &str, eps: f64) -> std::result::Result<LossType, Error> {
    match loss {
        "sum_squared" => Ok(LossType::SumSquared),
        "sum_absolute" => Ok(LossType::SumAbsolute),
        "sum_absolute_region_error" => Ok(LossType::SumAbsoluteRegionError),
        "sum_squared_region_error" => Ok(LossType::SumSquaredRegionError),
        "max_absolute" => Ok(LossType::MaxAbsolute),
        "max_squared" => Ok(LossType::MaxSquared),
        "root_mean_squared" => Ok(LossType::RootMeanSquared),
        "stress" => Ok(LossType::Stress),
        "diag_error" => Ok(LossType::DiagError),
        "log_sum_absolute" => Ok(LossType::LogSumAbsolute),
        // Smooth (Huber) surrogates: each replaces |x| / max(x) with a
        // gradient-friendly approximation controlled by `eps`. eunoia
        // expresses `eps` as a struct-variant payload.
        "smooth_sum_absolute" => Ok(LossType::SmoothSumAbsolute { eps }),
        "smooth_sum_absolute_region_error" => Ok(LossType::SmoothSumAbsoluteRegionError { eps }),
        "smooth_max_absolute" => Ok(LossType::SmoothMaxAbsolute { eps }),
        "smooth_max_squared" => Ok(LossType::SmoothMaxSquared { eps }),
        "smooth_diag_error" => Ok(LossType::SmoothDiagError { eps }),
        "smooth_log_sum_absolute" => Ok(LossType::SmoothLogSumAbsolute { eps }),
        other => Err(format!("Unknown loss type: {}", other).into()),
    }
}

/// Parse an optimizer name into the eunoia [`Optimizer`] variant. Returns
/// `Ok(None)` for `"auto"` (or an empty string), which leaves the Fitter on
/// its automatic, capability-driven default pool.
fn parse_optimizer(opt: &str) -> std::result::Result<Option<Optimizer>, Error> {
    let chosen = match opt {
        "auto" | "" => return Ok(None),
        "levenberg_marquardt" => Optimizer::LevenbergMarquardt,
        "lbfgs" => Optimizer::Lbfgs,
        "nelder_mead" => Optimizer::NelderMead,
        "mads" => Optimizer::Mads,
        "cma_es" => Optimizer::CmaEs,
        "cma_es_lm" => Optimizer::CmaEsLm,
        "trf" => Optimizer::Trf,
        "cma_es_trf" => Optimizer::CmaEsTrf,
        other => return Err(format!("Unknown optimizer: {}", other).into()),
    };
    Ok(Some(chosen))
}

/// Parse a placement-strategy string into the eunoia [`LeaderStrategy`]
/// variant, applying optional per-strategy overrides. `iterations` only
/// applies to `"force_directed"`; `min_gap` only applies to `"elbow"`.
fn parse_placement(
    placement: &str,
    margin: Option<f64>,
    iterations: Option<usize>,
    min_gap: Option<f64>,
) -> std::result::Result<LeaderStrategy, Error> {
    match placement {
        "raycast" => Ok(LeaderStrategy::Straight(ExteriorPolicy::Raycast { margin })),
        "force_directed" => Ok(LeaderStrategy::Straight(ExteriorPolicy::ForceDirected {
            margin,
            iterations,
        })),
        "elbow" => Ok(LeaderStrategy::Elbow(
            ElbowOptions::default().margin(margin).min_gap(min_gap),
        )),
        other => Err(format!("Unknown placement strategy: {}", other).into()),
    }
}

/// Parse a tether-source string into the eunoia [`TetherSource`] variant.
fn parse_tether(tether: &str) -> std::result::Result<TetherSource, Error> {
    match tether {
        "poi" => Ok(TetherSource::Poi),
        "boundary" => Ok(TetherSource::Boundary),
        other => Err(format!("Unknown tether source: {}", other).into()),
    }
}

/// Determine which sets in the spec are non-empty (inclusive area >= epsilon).
fn non_empty_set_names(spec: &DiagramSpec) -> Vec<String> {
    const EPS: f64 = 1e-10;
    spec.set_names()
        .iter()
        .filter(|name| {
            let combo = Combination::new(&[name.as_str()]);
            spec.inclusive_areas().get(&combo).copied().unwrap_or(0.0) >= EPS
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
            .map(|s| {
                input_order
                    .iter()
                    .position(|n| n == s)
                    .unwrap_or(usize::MAX)
            })
            .collect();
        indices.sort_unstable();
        (indices.len(), indices)
    });
    keys
}

/// Per-set fitted shape parameters in the wide eulerr schema. NaN means the
/// field doesn't apply to the chosen shape (e.g. `width`/`height` are NaN
/// for ellipse fits, `phi` is NaN for axis-aligned rectangle/square fits).
struct ShapeParams {
    h: Vec<f64>,
    k: Vec<f64>,
    a: Vec<f64>,
    b: Vec<f64>,
    phi: Vec<f64>,
    width: Vec<f64>,
    height: Vec<f64>,
    side: Vec<f64>,
}

impl ShapeParams {
    fn with_capacity(n: usize) -> Self {
        Self {
            h: Vec::with_capacity(n),
            k: Vec::with_capacity(n),
            a: vec![f64::NAN; n],
            b: vec![f64::NAN; n],
            phi: vec![f64::NAN; n],
            width: vec![f64::NAN; n],
            height: vec![f64::NAN; n],
            side: vec![f64::NAN; n],
        }
    }

    /// Empty parallel vectors for the 0-set edge case.
    fn empty() -> Self {
        Self {
            h: Vec::new(),
            k: Vec::new(),
            a: Vec::new(),
            b: Vec::new(),
            phi: Vec::new(),
            width: Vec::new(),
            height: Vec::new(),
            side: Vec::new(),
        }
    }
}

/// Pull fitted parameters out of a generic `Layout<S>` into the wide eulerr
/// schema. The mapping from `DiagramShape::to_params()` to the schema is
/// shape-specific: ellipse uses (a, b, phi); circle collapses to (a=b=r,
/// phi=0); rectangle uses (width, height); square uses (side, with
/// width=height=side mirrored in for convenience). All unused columns stay
/// NaN.
fn unpack_layout_params<S: DiagramShape + Copy + 'static>(
    layout: &eunoia::Layout<S>,
    non_empty: &[String],
    shape: ShapeKind,
) -> extendr_api::Result<ShapeParams> {
    let n = non_empty.len();
    let mut out = ShapeParams::with_capacity(n);
    for (i, name) in non_empty.iter().enumerate() {
        let shape_ref = layout
            .shape_for_set(name)
            .ok_or_else(|| Error::from(format!("missing shape for set {}", name)))?;
        let p = shape_ref.to_params();
        out.h.push(p[0]);
        out.k.push(p[1]);
        match shape {
            ShapeKind::Circle => {
                out.a[i] = p[2];
                out.b[i] = p[2];
                out.phi[i] = 0.0;
            }
            ShapeKind::Ellipse => {
                out.a[i] = p[2];
                out.b[i] = p[3];
                out.phi[i] = p[4];
            }
            ShapeKind::Rectangle => {
                out.width[i] = p[2];
                out.height[i] = p[3];
            }
            ShapeKind::Square => {
                out.side[i] = p[2];
                out.width[i] = p[2];
                out.height[i] = p[2];
            }
            ShapeKind::RotatedRectangle => {
                // RotatedRectangle::to_params() -> [x, y, width, height, phi]
                out.width[i] = p[2];
                out.height[i] = p[3];
                out.phi[i] = p[4];
            }
        }
    }
    Ok(out)
}

#[derive(Clone, Copy)]
enum ShapeKind {
    Circle,
    Ellipse,
    Rectangle,
    Square,
    RotatedRectangle,
}

impl ShapeKind {
    fn parse(s: &str) -> std::result::Result<Self, Error> {
        match s {
            "circle" => Ok(ShapeKind::Circle),
            "ellipse" => Ok(ShapeKind::Ellipse),
            "rectangle" => Ok(ShapeKind::Rectangle),
            "square" => Ok(ShapeKind::Square),
            "rotated_rectangle" => Ok(ShapeKind::RotatedRectangle),
            other => Err(format!("Unknown shape: {}", other).into()),
        }
    }
}

/// Build the R list result from fitted shape params + metric HashMaps.
#[allow(clippy::too_many_arguments)]
fn build_result_list(
    all_set_names: &[String],
    fitted_set_names: Vec<String>,
    params: ShapeParams,
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
        let label = combo_label(combo, all_set_names).unwrap_or_else(|| combo.sets().join("&"));
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
        h = params.h,
        k = params.k,
        a = params.a,
        b = params.b,
        phi = params.phi,
        width = params.width,
        height = params.height,
        side = params.side,
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
/// combinations present in the spec's exclusive areas are returned. The
/// single-set closed-form fit always emits a circle (a = b = radius,
/// phi = 0); for rectangle/square shapes the R wrapper substitutes the
/// equivalent square representation when reading the result.
fn build_edge_case_list(
    all_set_names: &[String],
    spec: &DiagramSpec,
    non_empty: &[String],
    shape: ShapeKind,
) -> List {
    // Treat the spec's exclusive areas as the "requested" map and produce an
    // empty fitted map for the 0-set case (or fitted == requested for the
    // single-set case below).
    let requested: HashMap<Combination, f64> = spec.exclusive_areas().clone();

    let (params, fitted_set_names, fitted_map) = if non_empty.len() == 1 {
        let name = &non_empty[0];
        let combo = Combination::new(&[name.as_str()]);
        let area = spec.inclusive_areas().get(&combo).copied().unwrap_or(0.0);
        let mut p = ShapeParams::with_capacity(1);
        p.h.push(0.0);
        p.k.push(0.0);
        match shape {
            ShapeKind::Circle | ShapeKind::Ellipse => {
                let r = (area / std::f64::consts::PI).sqrt();
                p.a[0] = r;
                p.b[0] = r;
                p.phi[0] = 0.0;
            }
            ShapeKind::Rectangle => {
                // Single-set rectangle has infinite valid (width, height)
                // pairs for a fixed area; pick the square that matches
                // the per-set area so the geometry is well-defined.
                let side = area.sqrt();
                p.width[0] = side;
                p.height[0] = side;
            }
            ShapeKind::Square => {
                let side = area.sqrt();
                p.width[0] = side;
                p.height[0] = side;
                p.side[0] = side;
            }
            ShapeKind::RotatedRectangle => {
                // A single-set rotated rectangle is unconstrained in aspect
                // and angle; emit the axis-aligned square (phi = 0) matching
                // the area, mirroring the rectangle/square closed forms.
                let side = area.sqrt();
                p.width[0] = side;
                p.height[0] = side;
                p.phi[0] = 0.0;
            }
        }
        // Fitted equals requested for the single-set perfect-fit case.
        (p, vec![name.clone()], requested.clone())
    } else {
        (ShapeParams::empty(), Vec::new(), HashMap::new())
    };

    let zero_map: HashMap<Combination, f64> = HashMap::new();
    build_result_list(
        all_set_names,
        fitted_set_names,
        params,
        &requested,
        &fitted_map,
        &zero_map,
        &zero_map,
        0.0,
        0.0,
        None,
    )
}

/// Run a shape-typed Fitter and project its layout back into the wide eulerr
/// result list. Factored out so the four shape arms in `fit_euler_diagram`
/// can share configuration and result-assembly code.
fn fit_and_collect<S: DiagramShape + Copy + 'static>(
    spec: &DiagramSpec,
    non_empty: &[String],
    all_set_names: &[String],
    shape: ShapeKind,
    loss_type: LossType,
    seed: u64,
    cmaes_threshold: Option<f64>,
    tolerance_opt: Option<f64>,
    jobs: usize,
    optimizer: Option<Optimizer>,
    n_restarts: Option<usize>,
) -> extendr_api::Result<List> {
    let mut fitter = Fitter::<S>::new(spec)
        .loss_type(loss_type)
        .seed(seed)
        .jobs(jobs);
    if let Some(t) = cmaes_threshold {
        fitter = fitter.cmaes_fallback_threshold(t);
    }
    if let Some(tol) = tolerance_opt {
        fitter = fitter.tolerance(tol);
    }
    if let Some(opt) = optimizer {
        fitter = fitter.optimizer(opt);
    }
    if let Some(n) = n_restarts {
        fitter = fitter.n_restarts(n);
    }
    let layout = fitter
        .fit()
        .unwrap_or_else(|e| throw_r_error(&format!("eunoia fit error: {}", e)));

    let params = unpack_layout_params(&layout, non_empty, shape)?;
    let requested = layout.requested().clone();
    let fitted = layout.fitted().clone();
    let residuals = layout.residuals();
    let region_error = layout.region_error();
    let diag_error = layout.diag_error();
    let stress = layout.stress();
    let container = layout.container().copied();

    Ok(build_result_list(
        all_set_names,
        non_empty.to_vec(),
        params,
        &requested,
        &fitted,
        &residuals,
        &region_error,
        diag_error,
        stress,
        container.as_ref(),
    ))
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
    loss_eps: f64,
    optimizer: &str,
    n_restarts: Robj,
    extraopt_threshold: Robj,
    tolerance: Robj,
    max_sets: Robj,
    complement: Robj,
    seed: i32,
    n_threads: Robj,
) -> extendr_api::Result<List> {
    if combo_names.len() != combo_values.len() {
        return Err("combo_names and combo_values must be the same length".into());
    }

    let shape_kind = ShapeKind::parse(shape)?;
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
            width = empty_vec.clone(),
            height = empty_vec.clone(),
            side = empty_vec.clone(),
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
    let loss_type = parse_loss_type(loss, loss_eps)?;
    let optimizer_opt = parse_optimizer(optimizer)?;

    let n_restarts_opt: Option<usize> = if n_restarts.is_null() {
        None
    } else {
        n_restarts
            .as_real()
            .filter(|v| v.is_finite() && *v >= 1.0)
            .map(|v| v as usize)
    };

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

    // Thread count for the restart loop. NULL means "automatic" (eunoia's
    // `jobs(0)` defers to the global rayon pool = all logical cores); an
    // explicit positive integer pins a private, scoped pool of that size.
    // Results are identical regardless of thread count, so this is purely a
    // wall-time knob and only has an effect when eunoia is built with the
    // `parallel` feature.
    let jobs: usize = if n_threads.is_null() {
        0
    } else {
        n_threads
            .as_real()
            .filter(|v| v.is_finite() && *v >= 1.0)
            .map(|v| v as usize)
            .unwrap_or(1)
    };

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
        return Ok(build_edge_case_list(
            &all_set_names,
            &spec,
            &non_empty,
            shape_kind,
        ));
    }

    macro_rules! fit {
        ($shape:ty) => {
            fit_and_collect::<$shape>(
                &spec,
                &non_empty,
                &all_set_names,
                shape_kind,
                loss_type,
                seed_u64,
                cmaes_threshold,
                tolerance_opt,
                jobs,
                optimizer_opt,
                n_restarts_opt,
            )
        };
    }

    match shape_kind {
        ShapeKind::Circle => fit!(Circle),
        ShapeKind::Ellipse => fit!(Ellipse),
        ShapeKind::Rectangle => fit!(Rectangle),
        ShapeKind::Square => fit!(Square),
        ShapeKind::RotatedRectangle => fit!(RotatedRectangle),
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
    let push_ring =
        |verts: &[Point], x: &mut Vec<f64>, y: &mut Vec<f64>, id_lengths: &mut Vec<i32>| {
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
    if obj.is_null() { None } else { obj.as_real() }
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

/// Map the wide eulerr per-set parameter schema (NaN-padded) into the
/// shape-specific eunoia objects needed by the plotting pipeline. Surfaces
/// `InvalidShapeParameter` errors as readable R-level messages rather than
/// panicking at the FFI boundary.
fn build_shapes(
    shape: ShapeKind,
    set_names: &[String],
    h: &[f64],
    k: &[f64],
    a: &[f64],
    b: &[f64],
    phi: &[f64],
    width: &[f64],
    height: &[f64],
    side: &[f64],
) -> extendr_api::Result<DiagramShapes> {
    let n = set_names.len();
    let invalid_param = |i: usize, e: DiagramError| -> Error {
        match e {
            DiagramError::InvalidShapeParameter {
                shape,
                param,
                value,
            } => Error::from(format!(
                "invalid {} parameter `{}` for set `{}`: {}",
                shape, param, set_names[i], value
            )),
            other => Error::from(format!("eunoia shape error: {}", other)),
        }
    };
    match shape {
        ShapeKind::Circle | ShapeKind::Ellipse => {
            if h.len() != n || k.len() != n || a.len() != n || b.len() != n || phi.len() != n {
                return Err(
                    "set_names and ellipse parameter vectors must all have the same length".into(),
                );
            }
            let ellipses: Vec<Ellipse> = (0..n)
                .map(|i| {
                    Ellipse::try_new(Point::new(h[i], k[i]), a[i], b[i], phi[i])
                        .map_err(|e| invalid_param(i, e))
                })
                .collect::<extendr_api::Result<Vec<_>>>()?;
            Ok(DiagramShapes::Ellipses(ellipses))
        }
        ShapeKind::Rectangle => {
            if h.len() != n || k.len() != n || width.len() != n || height.len() != n {
                return Err(
                    "set_names and rectangle parameter vectors must all have the same length"
                        .into(),
                );
            }
            let rects: Vec<Rectangle> = (0..n)
                .map(|i| {
                    Rectangle::try_new(Point::new(h[i], k[i]), width[i], height[i])
                        .map_err(|e| invalid_param(i, e))
                })
                .collect::<extendr_api::Result<Vec<_>>>()?;
            Ok(DiagramShapes::Rectangles(rects))
        }
        ShapeKind::Square => {
            if h.len() != n || k.len() != n || side.len() != n {
                return Err(
                    "set_names and square parameter vectors must all have the same length".into(),
                );
            }
            let squares: Vec<Square> = (0..n)
                .map(|i| {
                    Square::try_new(Point::new(h[i], k[i]), side[i])
                        .map_err(|e| invalid_param(i, e))
                })
                .collect::<extendr_api::Result<Vec<_>>>()?;
            Ok(DiagramShapes::Squares(squares))
        }
        ShapeKind::RotatedRectangle => {
            if h.len() != n
                || k.len() != n
                || width.len() != n
                || height.len() != n
                || phi.len() != n
            {
                return Err(
                    "set_names and rotated rectangle parameter vectors must all have the same length"
                        .into(),
                );
            }
            let rects: Vec<RotatedRectangle> = (0..n)
                .map(|i| {
                    RotatedRectangle::try_new(Point::new(h[i], k[i]), width[i], height[i], phi[i])
                        .map_err(|e| invalid_param(i, e))
                })
                .collect::<extendr_api::Result<Vec<_>>>()?;
            Ok(DiagramShapes::RotatedRectangles(rects))
        }
    }
}

/// Per-shape collection of fitted shapes feeding the plotting pipeline.
/// eunoia's `decompose_regions` / `place_labels` are generic over a single
/// shape type, so we dispatch on this enum once and reuse the same downstream
/// region/label code for all shape kinds.
enum DiagramShapes {
    Ellipses(Vec<Ellipse>),
    Rectangles(Vec<Rectangle>),
    Squares(Vec<Square>),
    RotatedRectangles(Vec<RotatedRectangle>),
}

impl DiagramShapes {
    /// Per-set polygon outlines (input order). eunoia doesn't repeat the
    /// first vertex; close the ring here so polylineGrob (used for edges)
    /// draws the closing segment instead of leaving a visible gap.
    fn set_polygon_outlines(&self, n_vertices: usize) -> Vec<Robj> {
        match self {
            DiagramShapes::Ellipses(v) => v
                .iter()
                .map(|s| polygonize_outline(s, n_vertices))
                .collect(),
            DiagramShapes::Rectangles(v) => v
                .iter()
                .map(|s| polygonize_outline(s, n_vertices))
                .collect(),
            DiagramShapes::Squares(v) => v
                .iter()
                .map(|s| polygonize_outline(s, n_vertices))
                .collect(),
            DiagramShapes::RotatedRectangles(v) => v
                .iter()
                .map(|s| polygonize_outline(s, n_vertices))
                .collect(),
        }
    }

    fn decompose(
        &self,
        set_names: &[String],
        spec: &DiagramSpec,
        container: Option<&Rectangle>,
        n_vertices: usize,
    ) -> RegionPolygons {
        match self {
            DiagramShapes::Ellipses(v) => {
                decompose_regions(v, set_names, spec, container, n_vertices)
            }
            DiagramShapes::Rectangles(v) => {
                decompose_regions(v, set_names, spec, container, n_vertices)
            }
            DiagramShapes::Squares(v) => {
                decompose_regions(v, set_names, spec, container, n_vertices)
            }
            DiagramShapes::RotatedRectangles(v) => {
                decompose_regions(v, set_names, spec, container, n_vertices)
            }
        }
    }
}

fn polygonize_outline<S: Polygonize>(shape: &S, n_vertices: usize) -> Robj {
    let p = shape.polygonize(n_vertices);
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
}

/// Compute polygon geometry and label anchors for plotting a fitted Euler
/// diagram, including the optional complement region inside a fitted
/// container.
///
/// Inputs are the fitted shape parameters for the **non-empty** sets only,
/// in the order eulerr stores them (`x$shapes` rows after dropping rows
/// with NA). The leading `shape` argument selects which per-set columns are
/// active: `"circle"`/`"ellipse"` consume `(h, k, a, b, phi)`,
/// `"rectangle"` consumes `(h, k, width, height)`, `"square"` consumes
/// `(h, k, side)`. The remaining vectors must still be provided (NaN
/// padding is acceptable). When `container_*` are non-NULL they describe
/// the fitted universe-box rectangle; in that case the result also carries
/// the complement region geometry (the area inside the rectangle outside
/// every shape) and a label anchor for it. Eunoia's `decompose_regions`
/// emits this region under the empty `Combination` whenever the spec
/// carries a complement and a container is supplied.
///
/// @keywords internal
#[extendr]
#[allow(clippy::too_many_arguments)]
fn euler_plot_data(
    set_names: Vec<String>,
    shape: &str,
    h: Vec<f64>,
    k: Vec<f64>,
    a: Vec<f64>,
    b: Vec<f64>,
    phi: Vec<f64>,
    width: Vec<f64>,
    height: Vec<f64>,
    side: Vec<f64>,
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

    let shape_kind = ShapeKind::parse(shape)?;
    let n_vertices = n_vertices.max(3) as usize;

    let shapes = build_shapes(
        shape_kind, &set_names, &h, &k, &a, &b, &phi, &width, &height, &side,
    )?;

    let container = build_container(
        &container_h,
        &container_k,
        &container_width,
        &container_height,
    );
    let has_complement = container.is_some();

    let set_polygons: Vec<Robj> = shapes.set_polygon_outlines(n_vertices);

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

    let regions = shapes.decompose(&set_names, &spec, container.as_ref(), n_vertices);
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

fn placement_kind_str(kind: PlacementKind) -> &'static str {
    match kind {
        PlacementKind::Interior => "interior",
        PlacementKind::ExteriorRaycast => "exterior_raycast",
        PlacementKind::ExteriorForceDirected => "exterior_force_directed",
        PlacementKind::ExteriorElbow => "exterior_elbow",
        // `PlacementKind` is #[non_exhaustive]; treat any future variant as
        // an interior placement (the no-leader default).
        _ => "interior",
    }
}

/// Place per-region labels using eunoia's `place_labels` API.
///
/// Inputs mirror `euler_plot_data` for shape geometry and add per-region
/// label sizes plus placement-strategy options. Returns, parallel to
/// `label_combos`:
///
/// * `anchor_x` / `anchor_y` — placed label anchor (NA on miss);
/// * `kind` — one of `"interior"`, `"exterior_raycast"`,
///   `"exterior_force_directed"`, `"exterior_elbow"`; `""` if no
///   placement was produced;
/// * `tether_x` / `tether_y` — tether point for the leader line (NA for
///   interior placements / misses).
/// * `leader_end_x` / `leader_end_y` — point on the label box AABB where
///   the leader terminates (NA for interior placements / misses).
/// * `leader_waypoints_x` / `leader_waypoints_y` / `leader_waypoints_lengths`
///   — concatenated waypoint coordinates and per-label counts. Empty for
///   straight leaders; carries one knee point per elbow placement.
///
/// Plus a canvas bbox (`canvas_bbox_h/k/width/height`) from eunoia's
/// `placements_bbox` — NaN when no placements were produced — for the R
/// side to grow `xlim/ylim` so exterior labels are never clipped.
///
/// The complement region is requested with `""` in `label_combos`; when
/// `container_*` are non-NULL the spec is built with `.complement(1.0)`
/// so eunoia emits the empty `Combination` from `decompose_regions`.
///
/// @keywords internal
#[extendr]
#[allow(clippy::too_many_arguments)]
fn place_euler_labels(
    set_names: Vec<String>,
    shape: &str,
    h: Vec<f64>,
    k: Vec<f64>,
    a: Vec<f64>,
    b: Vec<f64>,
    phi: Vec<f64>,
    width: Vec<f64>,
    height: Vec<f64>,
    side: Vec<f64>,
    container_h: Robj,
    container_k: Robj,
    container_width: Robj,
    container_height: Robj,
    n_vertices: i32,
    label_combos: Vec<String>,
    label_widths: Vec<f64>,
    label_heights: Vec<f64>,
    placement: &str,
    placement_margin: Robj,
    placement_iterations: Robj,
    placement_min_gap: Robj,
    placement_tether: &str,
    placement_leader_gap: Robj,
    label_precision: f64,
) -> extendr_api::Result<List> {
    let n = set_names.len();
    let n_labels = label_combos.len();

    if label_widths.len() != n_labels || label_heights.len() != n_labels {
        return Err(
            "label_combos, label_widths and label_heights must have the same length".into(),
        );
    }

    // Empty diagram: return NA-filled parallel vectors so the R side
    // doesn't have to special-case.
    if n == 0 || n_labels == 0 {
        let nan_vec = vec![f64::NAN; n_labels];
        let kind_vec = vec![String::new(); n_labels];
        let zero_lengths = vec![0i32; n_labels];
        return Ok(list!(
            anchor_x = nan_vec.clone(),
            anchor_y = nan_vec.clone(),
            kind = kind_vec,
            tether_x = nan_vec.clone(),
            tether_y = nan_vec.clone(),
            leader_end_x = nan_vec.clone(),
            leader_end_y = nan_vec,
            leader_waypoints_x = Vec::<f64>::new(),
            leader_waypoints_y = Vec::<f64>::new(),
            leader_waypoints_lengths = zero_lengths,
            canvas_bbox_h = f64::NAN,
            canvas_bbox_k = f64::NAN,
            canvas_bbox_width = f64::NAN,
            canvas_bbox_height = f64::NAN,
        ));
    }

    let shape_kind = ShapeKind::parse(shape)?;
    let n_vertices = n_vertices.max(3) as usize;

    let shapes = build_shapes(
        shape_kind, &set_names, &h, &k, &a, &b, &phi, &width, &height, &side,
    )?;

    let container = build_container(
        &container_h,
        &container_k,
        &container_width,
        &container_height,
    );

    // Build a region-decomposition spec. Areas are dummies; eunoia reads
    // only set names and `spec.complement()`. Setting `.complement(1.0)`
    // when a container is provided makes `decompose_regions` emit the
    // empty `Combination` (the complement region), which is what the
    // caller needs for placement of the `""` key.
    let mut builder = DiagramSpecBuilder::new();
    for name in &set_names {
        builder = builder.set(name.as_str(), 1.0);
    }
    if container.is_some() {
        builder = builder.complement(1.0);
    }
    let spec = builder
        .input_type(InputType::Exclusive)
        .build()
        .map_err(|e| Error::from(format!("eunoia spec build error: {}", e)))?;

    let regions = shapes.decompose(&set_names, &spec, container.as_ref(), n_vertices);

    // Build the size HashMap keyed by canonical Combination string form so
    // place_labels' internal `key.parse::<Combination>()` finds each
    // region. Input-order keys ("A&B" where A,B are in input order) round-
    // trip cleanly because Combination::from_str sorts internally.
    let mut sizes: HashMap<String, (f64, f64)> = HashMap::with_capacity(n_labels);
    // Track the canonical key per caller-order index so we can read back
    // placements in the same order the caller passed.
    let mut canonical_keys: Vec<Option<String>> = Vec::with_capacity(n_labels);
    for (i, raw) in label_combos.iter().enumerate() {
        let w = label_widths[i];
        let hh = label_heights[i];
        if !(w.is_finite() && hh.is_finite()) || w <= 0.0 || hh <= 0.0 {
            canonical_keys.push(None);
            continue;
        }
        let combo: Combination = raw.parse().unwrap_or_else(|_| Combination::new(&[]));
        let key = combo.to_string();
        sizes.insert(key.clone(), (w, hh));
        canonical_keys.push(Some(key));
    }

    let margin_opt: Option<f64> = read_optional_f64(&placement_margin);
    let iterations_opt: Option<usize> = if placement_iterations.is_null() {
        None
    } else {
        placement_iterations
            .as_real()
            .filter(|v| v.is_finite() && *v >= 1.0)
            .map(|v| v as usize)
    };
    let min_gap_opt: Option<f64> =
        read_optional_f64(&placement_min_gap).filter(|v| v.is_finite() && *v >= 0.0);

    let leader = parse_placement(placement, margin_opt, iterations_opt, min_gap_opt)?;
    let tether_source = parse_tether(placement_tether)?;
    let leader_gap = read_optional_f64(&placement_leader_gap)
        .filter(|v| v.is_finite())
        .map(|v| v.max(0.0))
        .unwrap_or(0.0);
    // `PlacementStrategy` is #[non_exhaustive]; build it via its fluent setters
    // rather than a struct literal.
    let strategy = PlacementStrategy::default()
        .leader(leader)
        .precision(label_precision.max(f64::EPSILON))
        .tether(tether_source)
        .leader_gap(leader_gap);

    let placements = place_labels(&regions, &sizes, container.as_ref(), &strategy);

    let mut anchor_x = Vec::with_capacity(n_labels);
    let mut anchor_y = Vec::with_capacity(n_labels);
    let mut kind = Vec::with_capacity(n_labels);
    let mut tether_x = Vec::with_capacity(n_labels);
    let mut tether_y = Vec::with_capacity(n_labels);
    let mut leader_end_x = Vec::with_capacity(n_labels);
    let mut leader_end_y = Vec::with_capacity(n_labels);
    let mut waypoints_x: Vec<f64> = Vec::new();
    let mut waypoints_y: Vec<f64> = Vec::new();
    let mut waypoints_lengths: Vec<i32> = Vec::with_capacity(n_labels);

    for canon in &canonical_keys {
        match canon.as_ref().and_then(|k| placements.get(k)) {
            Some(p) => {
                anchor_x.push(p.anchor.x());
                anchor_y.push(p.anchor.y());
                kind.push(placement_kind_str(p.kind).to_string());
                match p.tether {
                    Some(t) => {
                        tether_x.push(t.x());
                        tether_y.push(t.y());
                    }
                    None => {
                        tether_x.push(f64::NAN);
                        tether_y.push(f64::NAN);
                    }
                }
                match p.leader_end {
                    Some(le) => {
                        leader_end_x.push(le.x());
                        leader_end_y.push(le.y());
                    }
                    None => {
                        leader_end_x.push(f64::NAN);
                        leader_end_y.push(f64::NAN);
                    }
                }
                for w in &p.leader_waypoints {
                    waypoints_x.push(w.x());
                    waypoints_y.push(w.y());
                }
                waypoints_lengths.push(p.leader_waypoints.len() as i32);
            }
            None => {
                anchor_x.push(f64::NAN);
                anchor_y.push(f64::NAN);
                kind.push(String::new());
                tether_x.push(f64::NAN);
                tether_y.push(f64::NAN);
                leader_end_x.push(f64::NAN);
                leader_end_y.push(f64::NAN);
                waypoints_lengths.push(0);
            }
        }
    }

    let (cbb_h, cbb_k, cbb_w, cbb_height) = match placements_bbox(&placements, &sizes) {
        Some(rect) => (
            rect.center().x(),
            rect.center().y(),
            rect.width(),
            rect.height(),
        ),
        None => (f64::NAN, f64::NAN, f64::NAN, f64::NAN),
    };

    Ok(list!(
        anchor_x = anchor_x,
        anchor_y = anchor_y,
        kind = kind,
        tether_x = tether_x,
        tether_y = tether_y,
        leader_end_x = leader_end_x,
        leader_end_y = leader_end_y,
        leader_waypoints_x = waypoints_x,
        leader_waypoints_y = waypoints_y,
        leader_waypoints_lengths = waypoints_lengths,
        canvas_bbox_h = cbb_h,
        canvas_bbox_k = cbb_k,
        canvas_bbox_width = cbb_w,
        canvas_bbox_height = cbb_height,
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
        return Err("sum(subject_id_lengths) must equal length(subject_x)".into());
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

/// Canonical (non-proportional) Venn layout for a given shape.
///
/// Returns per-set geometry for a true Venn diagram of `set_names.len()` sets,
/// where every one of the `2^n - 1` regions is present. Used by the R `venn()`
/// path for shapes whose layout is supplied by eunoia rather than by eulerr's
/// precomputed ellipse table. Currently only `"rotated_rectangle"` is wired
/// here (it supports `n` in 1..=4); the rotation is what lets four rectangles
/// open all 15 regions, which axis-aligned rectangles cannot.
///
/// @keywords internal
#[extendr]
fn venn_layout(set_names: Vec<String>, shape: &str) -> extendr_api::Result<List> {
    let n = set_names.len();
    let shapes = match shape {
        "rotated_rectangle" => RotatedRectangle::canonical_venn_layout(n).ok_or_else(|| {
            Error::from(format!(
                "rotated-rectangle Venn diagrams support 1 to 4 sets, but {} were given",
                n
            ))
        })?,
        other => {
            return Err(format!("venn_layout: unsupported shape `{}`", other).into());
        }
    };

    let mut h = Vec::with_capacity(n);
    let mut k = Vec::with_capacity(n);
    let mut width = Vec::with_capacity(n);
    let mut height = Vec::with_capacity(n);
    let mut phi = Vec::with_capacity(n);
    for s in &shapes {
        // RotatedRectangle::to_params() -> [x, y, width, height, phi]
        let p = s.to_params();
        h.push(p[0]);
        k.push(p[1]);
        width.push(p[2]);
        height.push(p[3]);
        phi.push(p[4]);
    }

    Ok(list!(
        set_names = set_names,
        h = h,
        k = k,
        width = width,
        height = height,
        phi = phi,
    ))
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
    fn place_euler_labels;
    fn polygon_clip_rust;
    fn venn_layout;
    fn max_sets_default;
    fn max_sets_hard_cap;
}
