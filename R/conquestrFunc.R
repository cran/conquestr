#' @include conquestr.R
packageStartupMessage("\nConQuestR requires a copy of ACER ConQuest version <= 4.33.1")

#
# required functions
#

#' @title zapNulls
#'
#' @description Zaps NULL values embedded in ConQuest data objects.
#'
#' @param x a data frame.
#' @return x.
#' @keywords internal
zapNulls <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#' @title zapSystemMissing
#'
#' @description Coerce ConQuest system missing values to NA.
#'
#' @param x a data frame.
#' @return x
#' @keywords internal
zapSystemMissing<- function(x){
  if(!(class(x) == "data.frame")){
    stop("x must be a data.frame")
  } else {
    for(i in seq_along(x)){

      # if column is not numeric
      if(!(class(x[[i]]) == "numeric")){
        next
      } else {
        # print(names(myDemo$myConQuestData[i]))
        for(j in seq_along(x[[i]])){
          if(all.equal(x[[i]][j], -1.7976931348e+308) == TRUE){ # this is the required level of precision to get this to return true, -1.797693e+308 will retunr FALSE
            x[[i]][j]<- NA
          } else {
            next
          }
        }
      }
    }
  }
  return(x)
}

#' @title searchConQuestSys
#'
#' @description Search for object names within a ConQuest System file object.
#'
#' @param searchString A string to search within the names of mySys.
#' @param mySys An 'ACER ConQuest' system file object created using the conquestr::ConQuestSys function.
#' @param value Should searchConQuestSys return the name of the object or its index.
#' @param ignore.case Should searchConQuestSys ignore the case of the search term.
#' @return a string including object names mathching the search term
searchConQuestSys<- function(searchString, mySys, value = TRUE, ignore.case = TRUE){

  if(!("conQuestSysFile" %in% class(mySys))){
    stop("mySys must be an 'ACER ConQuest' system file object created using the conquestr::ConQuestSys function")
  } else {
    x<- grep(searchString, names(mySys), value = value, ignore.case = ignore.case)
  }
  return(x)
}



#' @title transformPvs
#'
#' @description Helper function to Transform PVs onto a new metric (e.g., PISA Mean = 500, SD = 100). Uses the method described in the PISA 2012 technical manual.
#'
#' @param x A concatenated vector of varnames in data, PV1, PV2, ..., PVm.
#' @param mT The desired mean of the PVs
#' @param sdT The desired sd of the PVs
#' @param weights The name of the weight variable in 'data' used to cauclate the mean and SD accross the PVs
#' @param data The data frame that contains the PVs and weights.
#' @param addToDf A Boolean, if TRUE, the transformed PVs are coerced into the DF, data, with name data$x_T (not yet implmented).
#' @param debug A temporary flag to spit-out objects to global env for chekcing. Will be removed when pushed to CRAN
#' @return a List of transofrmed PVs with as many elements as PVs were listed in 'x'.
transformPvs<- function(x, mT = 0, sdT = 1, weights, data, addToDf = FALSE, debug = TRUE){
  # setup
  results<- list()
  pvDataList<- list()
  weightDataList<- list()
  m<- length(x)
  dataName<- deparse(substitute(data)) # name of data frame

  # put the PVs and weights in a list to calculate the pooled mean and var
  i<- 1
  for(pv in x){
    # add cehcking that i is less than m
    pvDataList[[i]]<- eval(parse(text = paste0(dataName, "$", pv)))
    weightDataList[[i]]<- eval(parse(text = paste0(dataName, "$", weights)))
    pvData<- unlist(pvDataList)
    pvWeights<- unlist(weightDataList)
    i<- i + 1
  }

  if(debug == TRUE) {
    print(utils::str(pvDataList))
    # tmpCheckMe<<- pvDataList
    print("object tmpCheckMe added to global envrionemt for debugging")
  }

  # calc mean and var pooled over PVs
  pvM<- stats::weighted.mean(pvData, pvWeights)
  pvVar<- (sum(pvWeights) / (sum(pvWeights)^2 - sum(pvWeights^2))) * sum(pvWeights * (pvData - pvM)^2)
  pvSd<- sqrt(pvVar)

  # use values to create tranformed PVs in results
  # such that PV_Ti = A × PV_Ui + B, where T = transformed, U = untransofrmed,PV = verctor of all PVs combined
  # SD = desired SD
  # M = desired mean
  # A = SD/sd(PV), B = M - A*mean(PV_U)
  myA<- sdT/pvSd
  myB<- mT - myA* pvM
  i<- 1
  for(pv in x){
    results[[i]]<- eval(parse(text = paste0(myA, "*", dataName, "$", pv, "+", myB)))
    i <- i + 1
  }

  # results["pvM"]<- pvM
  # results["pvVar"]<- pvVar
  # results["pvSd"]<- pvSd
  return(results)

}







