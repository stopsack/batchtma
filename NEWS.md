# batchtma 0.2.0

* Breaking change: Require R >= 4.1
* No other user-visible changes.
* Internal changes:
  + Replace long-deprecated `filter(across())` with `if_all()` to be {dplyr} 
    1.2.0-compatible (#3, thanks to @DavisVaughan).
  + Split code into multiple files.
  + Use modern tidyselect, pipe, and anonymous functions.
  + Add some unit tests.


# batchtma 0.1.7

* Separately call tidyverse packages
* Update remaining references to manuscript


# batchtma 0.1.6

* Add reference to accompanying [manuscript on bioRxiv](https://doi.org/10.1101/2021.06.29.450369)


# batchtma 0.1.5

* Expand documentation for `diagnose_models()`.


# batchtma 0.1.4

* Bioconductor limma dependency added for `remotes::install_github()`
* Code review, updates to curly-curly, and tidyverse style changes,
  thanks to @tgerke


# batchtma 0.1.3

* Compose formula in `batchrq()` using `stats::reformulate()`.


# batchtma 0.1.2

* Consistently allow for missing biomarker and confounder values as well as empty batches.
* Add parameter `quantreg_method` to allow for choosing the Barrodale--Roberts algorithm (`br`). 
  The default remains `fn`.
* Rename parameters `truncate` to `ipw_truncate`; `tau` to `quantreg_tau` to indicate their scope.


# batchtma 0.1.1

* Remove old version of quantile regression without standardization.


# batchtma 0.1.0

* This is the first released version of batchtma.
