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
