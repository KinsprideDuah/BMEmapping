# BMEmapping 2.0.0

# BMEmapping 2.0.0 (2026-07)

## Major Features
* **Quantile-Based BME (QBME) Integration:** Added full pipeline support for the parameter-free QBME interpolation engine via `q_bme_predict()`, `q_bme_predict_ci()`, `q_bme_cv()`, and `q_prob_zk()`. 
* **Native S3 Plotting Architecture:** Extended the base S3 `plot()` and `summary()` methods to accept prediction, cross-validation, and density collection objects directly, enabling continuous surface and uncertainty field mapping.

## Minor Improvements & Bug Fixes
* Explicitly namespaced base statistical functions (`stats::approx()`, `stats::density()`) to eliminate namespace collision warnings during package check.
* Replaced direct unquoted column selection in **ggplot2** internal aesthetics with the `rlang::.data` pronoun wrapper to prevent unbound global variable flags.
* Pruned obsolete package dependencies (`utils`) from the `Imports` profile in the `DESCRIPTION` namespace to satisfy tidy deployment standards.
* Fixed an issue in R Markdown compilation where raw LaTeX `\url{}` expressions failed to generate clean hyperlinks across HTML and docx output engines.

# BMEmapping 1.2.2

* Fixed minor namespace exports and structural input validations for the Classical BME pipeline.

# BMEmapping 1.2.0

* Optimization of the integration windows during continuous spatial probability support profiling.

# BMEmapping 1.0.0

* Stable baseline release of the classical BME interpolation suite.

# BMEmapping 0.3.0

## Major Features
* Extended the internal integration `zk_range` window bounds by 20% to prevent density truncations and provide more robust non-Gaussian localized support estimates.
* Re-engineered covariance matrix construction functions to structurally preserve spatial continuity signatures under heavy nugget-effect penalties.

## Minor Improvements & Bug Fixes
* Flattened multi-dimensional distance arrays to eliminate intensive Kronecker-product tensor operations, generating massive execution speedups during global coordinate matrix evaluations.
* Tightened input sanitization checks to reject inconsistent dimensions or incomplete spatial objects prior to entropy maximization.
* Refined standard logging profiles to provide clear error messages for degenerate data or ill-conditioned covariance structures.

# BMEmapping 0.2.0

* Refined input validation pipelines and expanded warning triggers for singular matrices.

# BMEmapping 0.1.0

## Major Features
* Initial core release providing full computational framework support for Bayesian Maximum Entropy spatial interpolation across hard observations and soft-interval data domains.

## Minor Improvements & Bug Fixes
* Initial CRAN-ready codebase structures.
* Implemented clean custom printing and tabular text summary methods for BME model class assignments.
* Restructured package boundaries to streamline the package vignette footprint and drop bloated optional dependencies.
* Added Dr. Yan Sun as package author.
* Hardened internal numerical stability tolerances during matrix decompressions and localized density profiling.
