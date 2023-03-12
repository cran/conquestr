## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library("conquestr")

## -----------------------------------------------------------------------------
# set up
oldWd <- getwd()
myTmpDir <- tempdir()
setwd(myTmpDir)
file.copy(from = c(
  system.file("extdata", "ex1.cqc", package = "conquestr"),
  system.file("extdata", "ex1.dat", package = "conquestr"),
  system.file("extdata", "mysysfile.cqs", package = "conquestr")
  ), to = myTmpDir)

# run ConQuest
# ConQuestCall is not run here, as it requires a local install of ConQuest 
# ConQuestCall(cqc = file.path(myTmpDir, "ex1.cqc"), stdout = NULL) # searches for valid install of CQ
myExSys <- ConQuestSys(myCqs = file.path(myTmpDir, "mysysfile.cqs"))

# see the content of the sysfile
str(myExSys)

# tidy up
file.remove(
  list.files(myTmpDir, all.files = TRUE, full.names = TRUE, pattern = "^myi|^myS|ex1"),
  recursive=TRUE
)
setwd(oldWd)

## -----------------------------------------------------------------------------
# if no argument is provided to ConQuestSys, the example system file is read in by default.
myCqs <- ConQuestSys()

## -----------------------------------------------------------------------------
# search for objects named history in myCqs
searchConQuestSys("history", myCqs)

