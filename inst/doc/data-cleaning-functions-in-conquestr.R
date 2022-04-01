## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library("conquestr")

## -----------------------------------------------------------------------------

# get default sys file
myEx1Sys <- ConQuestSys()

# get itanal lists
myEx1Sys_itanal <- getCqItanal(myEx1Sys)

# show unformatted list objects for first item
print(myEx1Sys_itanal[[1]])


## -----------------------------------------------------------------------------

# set statistical criteria for conditional formatting

easyFlag <- 85 # highlight if facility is GREATER than this value
hardFlag <- 15 # highlight if facility is LESS than this value
irestFlag <- 0.2 # highlight if item-rest r is LESS than this value
underfitFlag <- 1.2 # highlight if weighted MNSQ is GREATER than this value
overfitFlag <- 0.8 # highlight if weighted MNSQ is LESS than this value
ptBisFlag <- 0.0 # highlight if non-key ptBis r is MORE than this value


## -----------------------------------------------------------------------------

# return a conditionally formatted item category statistics table for the fourth item
myEx1Sys_itanal_f <- fmtCqItanal(myEx1Sys_itanal, ptBisFlag = ptBisFlag, textColHighlight = "red")

# print table
myEx1Sys_itanal_f[[4]]$table

# print summary
myEx1Sys_itanal_f[[length(myEx1Sys_itanal_f)]] # the last object is always the summary


