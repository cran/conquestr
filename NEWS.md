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
