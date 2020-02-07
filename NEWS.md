# CHANGES IN bookdown VERSION 0.4.0

## NEW FEATURES

* conquestr now requires ACER ConQuest > 4.34.0
* added support for ACER ConQuest MCMC objects
* added `searchConQuestSys` to help users search names in system file objects

## MAJOR CHANGES

* `replaceInDataFrame` replaces `zapSystemMissing` in calls to `createDfFromSys` - this uses Rccp in place of base R and is approx 25 times faster making this function usable for large analyses.

## BUG FIXES

* 

# CHANGES IN bookdown VERSION 0.3.7

## NEW FEATURES

* Initial CRAN release.