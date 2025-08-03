## Major Changes

1. **Added an argument to specify the range for posterior density estimation.**  
   Users can now choose their own range for posterior density estimation based on their data.

2. **Added an argument to specify the lenth of `zk_vector` for posterior density estimation.**  
   Users can now specify the length of the `zk_vector` for posterior density estimation.

3. **Added a function to compute the posterior density estimation**  
   Users can now compute the posterior density for various locations using the `prob_zk()`.
   
4. **Added S3 method functions**  
   Available S3 method functions for plotting and summarizing prediction results.
   
5. **Added `bme_map` function**  
   Included a `bme_map()` to create `BMEmapping` class objects and store data in a specific way.

## Minor Patches

1. **Improved error handling and messaging**  
   Enhanced internal checks for input consistency and added user-friendly error messages in key functions like `bme_predict()` and `bme_cv()`.

2. **Documentation and vignette improvements**  
   Updated function documentation and expanded the main vignette to include a complete real-world example and best practices for model tuning.



## Resubmission

This is a resubmission.



## R CMD check results

Checked on:

- Local machine (macOS/Linux, R 4.4.0)
- win-builder (devel) via `devtools::check_win_devel()`

Results:

0 errors | 0 warnings | 0 notes



## Downstream dependencies

There are currently no known downstream dependencies affected by this update.  
Changes are backward-compatible where possible and enhancements are clearly documented.



