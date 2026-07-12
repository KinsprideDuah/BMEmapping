## Major Changes

1. **Integrated Quantile-Based BME Framework (QBME)**
Introduced a suite of core functions (`q_bme_predict()`, `q_bme_predict_ci()`, `q_bme_cv()`, and `q_prob_zk()`) that execute spatial continuity modeling via internal, quantile-specific variogram ensembles, bypassing manual parameter selection.
2. **Added User-Specified Parameter Ranges for Density Functions**
Users can now selectively pass customized bounds via the `zk_range` argument to explicitly set the support domain for localized posterior density estimations.
3. **Added Granular Resolution Scaling (`n` Vector Length)**
Users can now specify the length of the internal `zk_vector` evaluation space via the `n` parameter, allowing for direct control over computational speed versus localized integration precision.
4. **Added Comprehensive S3 Methods**
Implemented native `plot()` and `summary()` extensions for predictions, cross-validations, and density collections to automatically build diagnostic statistics (ME, MAE, RMSE, $R^2$) and spatial maps.
5. **Formalized Object Pipelines via `bme_map()**`
Introduced a unified initialization constructor to systematically validate coordinate structures, hard measurements, and soft intervals into strict `BMEmapping` class objects prior to execution.

## Minor Patches

1. **Namespace & Global Variable Auditing**
Explicitly namespaced standard statistical functions (`stats::approx()`, `stats::density()`) and mapped unquoted ggplot columns to the `rlang::.data` pronoun to clean up strict compilation bindings.
2. **Input Sanitization & Error Handling**
Hardened sanity checks for structural data dimensionality across cross-validation routines to output clear, human-readable logging errors for degenerate or ill-conditioned covariance matrices.
3. **URL Endpoint & Documentation Overhaul**
Corrected permanently moved NOAA external data paths and expanded vignette tutorials to showcase both Classical and Quantile-Based geostatistical mapping tracks.

---

## Resubmission / Response to CRAN Notes

This is a resubmission.

### CRAN Incoming Feasibility Response:

> *NOTE: Version contains large components (1.2.2.9000)*

* **Correction:** The version number has been officially bumped and structured out of development component suffixes (`.9000`) to match CRAN production specifications. This major framework expansion is submitted cleanly as version **`2.0.0`**.

---

## R CMD check results

Checked on:

* Local Machine (Windows 11 / R 4.5.0)
* win-builder (devel) via `devtools::check_win_devel()`

Results: **0 errors | 0 warnings | 0 notes**

---

## Downstream dependencies

There are currently no known downstream dependencies affected by this update. Changes are designed to maintain full backward compatibility with the classical baseline pipeline while expanding capabilities natively.
