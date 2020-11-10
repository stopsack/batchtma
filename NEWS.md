# batchtma 0.1.2

* Consistently allow for missing biomarker and confounder values as well as empty batches.
* Add parameter `quantreg_method` to allow for choosing the Barrodale--Roberts algorithm (`br`). 
  The default remains `fn`.
* Rename parameters `truncate` to `ipw_truncate`; `tau` to `quantreg_tau` to indicate their scope.


# batchtma 0.1.1

* Remove old version of quantile regression without standardization.


# batchtma 0.1.0

* This is the first released version of batchtma.
