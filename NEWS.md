# CHANGES IN conquestr VERSION 1.9.2

## NEW FEATURES

* `pvRegression()` now returns an object of class `"pvRegression"` (still a
  data frame) with `print()`, `summary()`, `coef()`, `vcov()`, `confint()`,
  `predict()` and `nobs()` methods. `vcov()` gives the full Rubin-combined
  covariance matrix of the coefficients.

## DEPRECATED AND DEFUNCT

* `pvMeanVar()` has been removed. 
  Use `pvMean()` and `pvVar()` (or `pvDescribe()`) instead, which also
  support weights and replicate standard errors.

# CHANGES IN conquestr VERSION 1.8.3

## NEW FEATURES

* `genResponses()` gains a `debug` argument. When `debug = TRUE` it returns a
  list holding the responses and the intermediate data from each step: the
  standardised inputs, the parsed perturbations, the perturbed item
  parameters, which perturbation was applied to each case and item, the
  random draws, and the category probabilities used (and those under the
  unperturbed items). Responses are identical to `debug = FALSE` for the same
  seed.
* New `genResponsesProbs()` tabulates the response probabilities used for
  each case, item, and category alongside the unperturbed probabilities, to
  check that perturbations were applied to the intended cases. See the
  "Generate item responses" vignette.
* Perturbations in `genResponses(perturbR = )` can now apply to all cases
  (omit `group`, or use `NULL` or `NA`), one group, or several groups
  (e.g., `group = c("g1", "g2")`). Elements can be given by name.
* The same item can now be perturbed differently for different groups. If two
  perturbations apply to the same case and item, the later one is used, with a
  warning (previously, the last was used silently).
* "discrimination" perturbations gain `pivotAt = "item"`, which changes the
  discrimination around a point `pivot` logits from the item's location, so
  that the item location does not move (category probabilities are unchanged
  at the pivot point). The default, `pivotAt = "theta"`, keeps the previous
  behaviour. A separate `shift` can be added to move the item location without
  moving the discrimination pivot.
* New family of functions for population statistics from plausible values
  (PVs) with replicate-weight standard errors. Each statistic is computed for
  every PV under the full-sample weight and each replicate weight (in C++),
  then combined with Rubin's rules. Supported replication methods are BRR,
  Fay's variant of BRR (`repMethod = "Fay"`, with `fayK`), and the delete-one
  (`"JK1"`) and paired (`"JK2"`) jackknife. All support a grouping variable
  (`by`). See the "Population statistics from plausible values" section of
  the introductory vignette.
    * `pvStat()` is the core function: mean, variance, SD, median, quantiles,
      IQR, sum of weights, or a user-supplied `function(x, w)`.
    * `pvMean()`, `pvVar()`, `pvSd()`, `pvMedian()`, `pvQuantile()`, `pvIqr()`
      and `pvDescribe()` are wrappers for common descriptive statistics.
    * `pvDiff()` estimates differences between groups (e.g., girls minus
      boys), computing the difference within each PV and replicate so the
      standard error reflects covariance induced by the sampling design.
    * `pvCor()` estimates correlations between a measure and one or more
      other measures, combined on the r scale (default) or the Fisher-z scale
      (`combine = "fisherZ"`), with confidence intervals. Each measure may be
      latent (a set of PVs) or manifest (a single observed column).
    * `pvCorMatrix()` and `pvCovMatrix()` estimate the correlation and
      covariance matrices of several measures, e.g., the outcome domains of
      IELS, PISA or TIMSS. Latent and manifest measures can be mixed: latent
      measures are paired by PV draw, and a manifest variable's column is
      reused for every draw. With only manifest variables, the ordinary
      weighted matrix is returned with replicate standard errors. Results are
      available in long or matrix form. Cases are dropped listwise so the
      matrix is valid; a warning is given if Fisher-z combining produces a
      matrix that is not positive semi-definite.
    * `pvRegression()` fits a weighted linear regression of PVs on
      background variables given as a one-sided formula, and returns
      coefficients and R-squared with standard errors and confidence
      intervals.

## MINOR CHANGES

* `pvMeanVar()` is now a thin wrapper around `pvMean()` and `pvVar()`. Results
  are unchanged. Its documentation now correctly describes the input as n
  cases by m PVs.
* `genResponses()` has been split into steps (`genRespInputs()`,
  `parsePerturbations()`, `perturbItem()`, `buildPerturbMap()`, and
  `genMissingMCAR()`), each of which can be called on its own.
* `genResponses()` now gives informative errors for invalid perturbations
  (e.g., a missing `factor` or `pivot`, or a `group` not found in `groups`) and
  for `mcarP` outside 0 to 1.
* The documentation of `pivot` in `genResponses()` has been corrected: it is
  added to the delta dot. For a dichotomous item, perturbed and unperturbed
  curves cross at theta = pivot / (a * (factor - 1)).

## BUG FIXES

* `getCqParams()` (and so `getCqRespModel()`) removed the wrong parameters
  when a model had unidentified (problem) gins, misaligning parameter
  estimates with their gins and, in some cases, failing with "arguments imply
  differing number of rows".

# CHANGES IN conquestr VERSION 1.8.1

## MAJOR CHANGES

* The readers for 'ACER ConQuest' system files and rout files (`ReadSys` and the
  `Read*` helpers) have been rewritten in C++. The file is read into memory once
  and parsed in C++; `ConQuestSys()` is roughly 10-20 times faster on the
  included examples, and the results are identical.
* Truncated or corrupt files now give an informative "unexpected end of file"
  error instead of silently returning short reads or trying to allocate huge vectors.
* The low-level `Read*` helpers now take a 'cqReader' (an in-memory file created
  internally by `ConQuestSys()` and `ConQuestRout()`) rather than a connection.
  `DecompressSys()` now takes and returns a raw vector, and no longer writes a
  temporary file.

# CHANGES IN conquestr VERSION 1.6.1

## NEW FEATURES

* Added cpp helper functions to handle complex strings in Itanal helpers 
  see `conquestr::lcs` and `conquestr::lcs_vec`
* This allows more complex support for `conquestr::getCqItanal`, and 
  `conquestr::getCqItanalSummary` for example when group variables
  or levels within groups contain punctuation (e.g., "my-group_variable.",
  and "my-group_level_1")

## MAJOR CHANGES

* Patched issue with getCqRespModel when the model has both anchors
  and problem (unidentified) items 
* Patched internal tests for getCqRespModel. 

# CHANGES IN conquestr VERSION 1.5.5

## NEW FEATURES

* Added support for ACER ConQuest system files version 30 which are created by
  the command put (https://conquestmanual.acer.org/s4-00.html#put) in ACER ConQuest
  Version > 5.47.
* Improved handling of missing values or items where all responses are in one category
  itanal output (see e.g., `conquestr::getCqItanal`, and `conquestr::getCqItanalSummary`)

## MAJOR CHANGES

* Patched internal tests for upstream dependencies. 


# CHANGES IN conquestr VERSION 1.5.0

## NEW FEATURES

* Added support for ACER ConQuest system files version 29 which are created by
  the command put (https://conquestmanual.acer.org/s4-00.html#put) in ACER ConQuest
  Version > 5.42.

## MAJOR CHANGES

* conquestr::getCqItan now returns estimated scores in the item tables rather than
  raw scores. Previous versions would the pre-key value and the raw score associated
  with the pre-key value (these two values are identical when no key or recode is 
  used). The values are now the pre-key value and the estimated score. 
  In the case of models without estimated scores (e.g., Rasch models) the raw score
  is the same as the estimated score. 
  
  
# CHANGES IN conquestr VERSION 1.4.4

## NEW FEATURES

* Added support reading uncompressed system files in markdown and shiny workflows 
  previously, the function that worked out if a system file was compressed threw
  an invisible error that would break, for example, knitr workflows. 
* Added support for polytomies and scores in genItems.
* added support for itanals that include groups.

## MAJOR CHANGES

* conquesrt::getCqItan now returns a list of list of lists. The top level
  list is the return object. Each element of this list is a group from the itanal.
  In most use cases this is "all cases" and the length of this list is 1. Therefore,
  to get the 4th item analysis table for the first group is now accessed 
  by `returnObject[[1]][[4]]` instead of `returnObject[[4]]`


# CHANGES IN conquestr VERSION 1.4.0

## NEW FEATURES

* added support for ACER ConQuest system file v27 (ConQuest V5.40.14)
* added gTokenList

# CHANGES IN conquestr VERSION 1.3.6

## NEW FEATURES

* added support for long labels in history files. See `?conquestr::getCqHistory`.
* added gYBetaAll - the regression mean for the cases.

# CHANGES IN conquestr VERSION 1.3.0

## NEW FEATURES

* added support for reading compressed system files.

## MAJOR CHANGES

* 

## Bug fixes

* 

# CHANGES IN conquestr VERSION 1.2.0

## NEW FEATURES

* added support for reading "mini" system file.

## MAJOR CHANGES

* 

## Bug fixes

* 

# CHANGES IN conquestr VERSION 1.1.1

## NEW FEATURES

* added support for plotting item characteristics curves, see e.g., `plotCCC`


## MAJOR CHANGES

* 
## Bug fixes

* 

# CHANGES IN conquestr VERSION 1.0.0

## NEW FEATURES

* added support for producing and plotting model expected curves related to items and tests, see e.g., `plotModelICC`
* added support for extracting item parameters, variances, fits, labels in `getCqRespModel`

## MAJOR CHANGES

* functions that deal with items scores are more general and apply to any model that scores categories including those
  that do not assume integer scoring.

## Bug fixes

* many.


# CHANGES IN conquestr VERSION 0.9.4

## NEW FEATURES

* added support for producing and plotting test information functions

## MAJOR CHANGES

* `ConQuestCall` now will call "cmd.exe" via `system2` on Windows. This is morre consistent with behaviour on Mac.

## Bug fixes

* compiled (win binary) version previously could cause error when using `ConQuestCall` with paths with spaces. Fixed.


# CHANGES IN conquestr VERSION 0.9.1

## NEW FEATURES

* added support for analysing standardised residuals as per DOI: 10.1177/0013164410379322 

## MAJOR CHANGES

* `ConQuestCall` now will try and find the ConQuest executable rather than requiring explicit declaration

## Bug fixes


# CHANGES IN conquestr VERSION 0.8.5

## NEW FEATURES

* added support for ACER ConQuest system files from BTL/pairwise models 
* added generic S3 function, `plotRout` to support plotting rout obejcts

## MAJOR CHANGES

* conquestr now requires ACER ConQuest > 5.12.3
* `plotRout` has methods for ICC and for Infomap objects.

## Bug fixes

* fix in getCqHist: matrix was previously the transpose of history (truncated where n iter > n params and vice-versa), now corrected.

# CHANGES IN conquestr VERSION 0.4.0

## NEW FEATURES

* added support for ACER ConQuest system files from MCMC models (estimate ! method = patz...)
* added `searchConQuestSys` to help users search names in system file objects

## MAJOR CHANGES

* conquestr now requires ACER ConQuest > 4.34.0
* `replaceInDataFrame` replaces `zapSystemMissing` in calls to `createDfFromSys` - this uses Rccp in place of base R and is approx 25 times faster making this function usable for large analyses.

# CHANGES IN conquestr VERSION 0.3.7

## NEW FEATURES

* Initial CRAN release.
