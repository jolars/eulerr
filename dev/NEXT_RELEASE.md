# NA

This is a major milestone for eulerr. The 8.0.0 release introduces a
complete rewrite of the underlying C++ codebase, now relying instead on
the Rust library [Eunoia](https://eunoia.bz), which is a library I have
developed. Here are some of the highlights this brings:

- We have added support for squares and rectangles too!
- Eunoia uses analytical gradients for all the smooth loss functions,
  which leads to significantly improved performance. We have also
  swapped the default optimization algorithm to a Levendberg–Marquardt
  (LM) method plus CMA-ES fallback. The LM method is invoked only when
  the loss is squared residuals, but is then much faster than the
  previous [`nlm()`](https://rdrr.io/r/stats/nlm.html) method. The
  CMA-ES fallback replaces the previous use of the **GenSA** package,
  and is invoked whenever the LM method reaches a minimum that’s above a
  level of error (configurable by the user).
- There are now several labeling algorithms that ensure labels do not
  overlap each other or other parts of the diagram.
- As a result of the above, we have been able to drop multiple
  dependencies: **Rcpp**, **RcppArmadillo**, **GenSA**, **polylabelr**,
  and **polyclip** are all no longer required, since **rextendr** and
  Eunoia handle the Compiled code and algorithms natively.
- The calculated intersections are now *sparse*, which means that only
  the intersections that actually appear in either input or output (the
  diagram) are calculated and stored. This is a **breaking change**
  because it means that you cannot index the intersections by their
  position or intersection name and guarantee that you will get a value.
  Instead, you must check if the intersection exists first, and then
  implicitly it will have a value of zero. But this allows eulerr to
  handle much larger combinations, since we never store a
  `2^n - 1`-sized vector anywhere.
