#' @include conquestr.R
packageStartupMessage("\nConQuestR requires a copy of ACER ConQuest version <= 5.5.11")

#
# required helper functions
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
#' @description Coerce ConQuest system missing values to NA. Note this is very slow and users should use the internal function conquestr::replaceInDataFrame where possible.
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


#' @title createConQuestProject
#'
#' @description creates a standard folder structure to work with 'ACER ConQuest' Projects.
#'
#' @param prefix a valid file path where to create project folders.
#' @param ... optional params, including "setDebug"
#' @return Boolean TRUE.
#' @examples
#' \dontrun{
#' createConQuestProject()
#' }
#' @importFrom methods hasArg
createConQuestProject<- function(prefix = getwd(), ...){
  # debug
  myDebug<- FALSE
  setDebug<- FALSE
  if(hasArg(setDebug)){
    myArgs<- c(...) # have to get the optional arguments first!
    myDebug<- myArgs["setDebug"]
  }

  if(is.null(prefix)) stop("prefix must be a valid dir") # mostly in case getwd() returns NULL (e.g.., if you delete your wd)

  # print message
  message(paste("creating project folders in ", prefix))

  # create alist of file paths to create
  myFilePathsList<- list()

  myFilePathsVec<- c(
    file.path("data"),
    file.path("syntax"),
    file.path("syntax", "R"),
    file.path("syntax", "cq"),
    file.path("input"),
    file.path("input", "labels"),
    file.path("input", "params"),
    file.path("input", "params", "xsi"),
    file.path("input", "params", "tau"),
    file.path("input", "params", "sigma"),
    file.path("input", "params", "beta"),
    file.path("output"),
    file.path("output", "show"),
    file.path("output", "itanal"),
    file.path("output", "plot"),
    file.path("output", "params"),
    file.path("output", "params", "xsi"),
    file.path("output", "params", "tau"),
    file.path("output", "params", "sigma"),
    file.path("output", "params", "beta"),
    file.path("output", "cases"),
    file.path("output", "log"),
    file.path("output", "resdiuals"),
    file.path("output", "history"),
    file.path("submission")
  )

  if(prefix == getwd()){
    # put paths in a list
    for(myFilePath in myFilePathsVec){
      if(myDebug) print(myFilePath)
      myFilePathsList[[myFilePath]]<- myFilePath
    }
  } else {
    # put paths in a list with the prefix in front
    for(myFilePath in myFilePathsVec){
      tmpPath<- file.path(prefix, myFilePath)
      myFilePathsList[[myFilePath]]<- tmpPath
    }
  }

  # create dirs
  for(i in seq_along(myFilePathsList)){
    if(myDebug){
      print(i)
      print(myFilePathsList[[i]])
    }
    dir.create(myFilePathsList[[i]], recursive = TRUE)
  }

  return(invisible(TRUE))

}


#' @title getCqHist
#'
#' @description creates a data frame representation of the iteration history for all parameters.
#'
#' @param myCqs A system file.
#' @return A data frame.
#' @examples
#' \dontrun{
#' getCqHist(ConQuestSys())
#' }
getCqHist<- function(myCqs){

  IterHistTmp<- data.frame(
    Iter = unlist(myCqs$gHistory$Iter),
    Likelihood = unlist(myCqs$gHistory$Likelihood)
  )


  ParamTypesTmp<- c("Beta", "Variance", "Xsi" , "Tau", "RanTermVariance")

  for(paramType in ParamTypesTmp){
    whichParam<- as.logical(match(names(myCqs$gHistory), paramType, nomatch = 0))
    if(!all(is.na(unlist(myCqs$gHistory[whichParam])))){
      tmpMat<- matrix(
          unlist(myCqs$gHistory[whichParam]),
          nrow = length(myCqs$gHistory[whichParam][[1]]),
          byrow = TRUE
        )

      tmpDf<- as.data.frame(tmpMat)
      names(tmpDf)<- paste0(paramType, 1:ncol(tmpDf))
      IterHistTmp<- cbind(IterHistTmp, tmpDf)
    }
  }
  IterHistTmp<- replaceInDataFrame(IterHistTmp, -1.797693e+308, NA)
  # manually remove RanTermVar if not used
  if(
    all(is.na(unlist(
      IterHistTmp[ , grep("^Ran", names(IterHistTmp))]
      )))
  ) IterHistTmp<- IterHistTmp[ , -grep("^Ran", names(IterHistTmp))]
  return(IterHistTmp)

}


#' @title getCqVars
#'
#' @description creates a data frame representation of the variables in the model statement.
#'     Note that steps are not variables.
#' @param myCqs A system file.
#' @return A data frame.
#' @examples
#' \dontrun{
#' getCqVars(ConQuestSys())
#' }
getCqVars<- function(myCqs){
  myVars<- data.frame(
    VariableType = c(rep("E", length(myCqs$gModelVariables[[1]])), rep("I", length(myCqs$gModelVariables[[2]]))),
    VariableNumber = unlist(myCqs$gModelVariables),
    VariableLevels = unlist(myCqs$gLevel),
    row.names = NULL
  )
  return(myVars)
}

#' @title getCqTerms
#'
#' @description creates a data frame representation of the terms of the model statement, including interactions.
#' @param myCqs A system file.
#' @return A data frame.
#' @examples
#' \dontrun{
#' getCqTerms(ConQuestSys())
#' }
getCqTerms<- function(myCqs){
  termList<- list()
  tmpVars<- getCqVars(myCqs)

  for(term in seq_len(length(myCqs$gTerms))){
    stepInvolved<- any(unlist(myCqs$gTerms[[term]][c("VariableNumber", "VariableType")]) == 2) # does this term involve steps?
    thisVarType<- unlist(myCqs$gTerms[[term]][c("VariableType")])
    termList[[term]]<-data.frame(
      # what variables are involved in this term matrix(unlist(myGroupSys$gTerms[[4]][c("VariableNumber", "VariableType")]), ncol = 2)
      VariableNumber = unlist(myCqs$gTerms[[term]][c("VariableNumber")]),
      VariableType = ifelse (thisVarType == 0, "I", ifelse(thisVarType == 1, "E", "S")),
      TermNumber = term,
      TermSign = unlist(myCqs$gTerms[[term]]["Sign"]),
      TermLabel = unlist(myCqs$gTerms[[term]]["Label"]),
      TermStepInvolved = stepInvolved,
      row.names = NULL
    )
  }

  termDf<- do.call("rbind", termList)
  # termDf<- merge(termDf, tmpVars, by.x = c("VariableNumber", "VariableType"), by.y = c("VariableNumber", "VariableType"), all.x = TRUE)
  return(termDf)
}


#' @title getCqParams
#'
#' @description creates a data frame representation of the parameters of the model, including both estimated and constrained parameters.
#'    Parameters are either freely estimated ('ParamType' == 0) or constrained ('ParamType' == 0).
#'    Parameters are indexed (0 offset) by the column 'ParamNumber'. There is a seperate index for free and constrained parameters.
#' @param myCqs A system file.
#' @return A data frame.
#' @examples
#' \dontrun{
#' getCqParams(ConQuestSys())
#' }
getCqParams<- function(myCqs){

  if(myCqs$gPairWise) stop("pairwise is not yet supported by getCqParams")
  # will want to have, for each param, est, SE, unwFit, unw95CIlow, unw95CIhigh, unwY, wFit, nw95CIlow, w95CIhigh, wT,
  # neeed to know, are the SE? are there fits
  # myCqs$gIFit # bool
  mySe<- myCqs$gStdError < 3 # bool SE is calculated, 3 = none

  # at the moment, we simply get back a list of param nums, param types, signs and labels
  # first get info about terms
  termList<- list()

  for(term in seq_len(length(myCqs$gTerms))){
    stepInvolved<- any(unlist(myCqs$gTerms[[term]][c("VariableType")]) == 2) # does this term involve steps?
    termList[[term]]<-matrix(
      unlist(myCqs$gTerms[[term]][c("ParamNumber", "ParamType")]),
      nrow = length(myCqs$gTerms[[term]][["ParamNumber"]]),
      byrow = FALSE
    )

    tempDf<- data.frame(
        TermSign = myCqs$gTerms[[term]]["Sign"],
        TermLabel = myCqs$gTerms[[term]]["Label"],
        TermNum = term,
        TermStepInvolved = stepInvolved, # does this term involve steps?
        ParamNumber =  termList[[term]][ , 1],
        ParamType =  termList[[term]][ , 2],
        ParamXsi = NA,
        ParamSe = NA,
        ParamAnchored = 0,
        ParamDim = NA,
        ParamStep = NA
    )

    if(term == 1){
      tmpParamListDf<- tempDf
    } else {
      tmpParamListDf<- rbind(tmpParamListDf, tempDf)
    }

  }

  # add in anchor xsi
  if(length(myCqs$gImportParameters) > 0){
    anyItemAnchors<- FALSE
    for(i in seq_len(length(myCqs$gImportParameters))){
      if(myCqs$gImportParameters[[i]]$Type == 0){
        anyItemAnchors<- TRUE
        tmpParamNo<- myCqs$gImportParameters[[i]]$I1 #I2 not used for anchor Xsi
        tmpParamAnchorVal<- myCqs$gImportParameters[[i]]$Value
        tmpParamListDf$ParamXsi[tmpParamListDf$ParamNumber == tmpParamNo & tmpParamListDf$ParamType == 0]<- tmpParamAnchorVal
        tmpParamListDf$ParamAnchored[tmpParamListDf$ParamNumber == tmpParamNo & tmpParamListDf$ParamType == 0]<- 1
      }
    }
  }

  # add in estimated xsi
  thisXsiIndex<- 1
  thisParamNum<- 0
  thisParamNumSe<- 1
  for(i in seq_len(length(tmpParamListDf$ParamNumber))){
    if(tmpParamListDf$ParamAnchored[i] == 1){
      thisXsiIndex<- thisXsiIndex + 1 # use up an index, because imported anchored Xsi appear in gXsi
      thisParamNum<- thisParamNum + 1
    } else if(tmpParamListDf$ParamType[i] == 1){
      next # this param is a location constraint
    } else {
      tmpParamListDf$ParamXsi[tmpParamListDf$ParamNumber == thisParamNum & tmpParamListDf$ParamType == 0]<- myCqs$gXsi[thisXsiIndex]
      if(mySe){
        tmpParamListDf$ParamSe[tmpParamListDf$ParamNumber == thisParamNum & tmpParamListDf$ParamType == 0]<- sqrt(myCqs$gDeriv2nd[thisParamNumSe,thisParamNumSe])
      } else {
        tmpParamListDf$ParamSe[tmpParamListDf$ParamNumber == thisParamNum & tmpParamListDf$ParamType == 0]<- NA
      }
      thisXsiIndex<- thisXsiIndex + 1
      thisParamNum<- thisParamNum + 1
      thisParamNumSe<- thisParamNumSe + 1 # if the param is anchored, then it is skipped in gDeriv2nd and thisXsiIndex and thisParamNumSe get out of sync
    }

  }

  return(tmpParamListDf)
  # to get the value of an anchored xsi, need to find out which Dim it is on, and calculate -1*(sum(Xsi_id)) (for d = d, and i = i in d)
}

#' @title getCqFit
#'
#' @description creates a data frame representation of the fit of parameters in the item reponse model
#' @param myCqs A system file.
#' @return A data frame.
#' @examples
#' \dontrun{
#' getCqFit(ConQuestSys())
#' }
getCqFit<- function(myCqs){
  if(!myCqs$gIFit) stop("fit has not been estimated")
  myTempNames<- data.frame(fitName = matrix(unlist(myCqs$gFitStatistics[[1]]), ncol = 1))
  myTempFits<-  matrix(unlist(myCqs$gFitStatistics[[2]]), nrow = length(myCqs$gFitStatistics[[2]]), byrow = TRUE)
  myTempFits<- data.frame(myTempFits)
  names(myTempFits)<- names(myCqs$gFitStatistics$Value[[1]])

  myFit<- cbind(myTempNames, myTempFits)
  return(myFit)
}



#' @title getCqParams2
#'
#' @description returns a list of data frames, one for each term in the model.
#'    The data frames include the the estimate, location constraint, or anchored value for each parameter.
#'    The standard error of estimated parameters is also included if it has been estimated.
#'    The fit (un/weighted MNSQ, sometimes called outfit and infit) of all parameters is alre included if has been estimated.
#' @param myCqs A system file.
#' @return A list of data frames of length `gTerms`.
#' @examples
#' \dontrun{
#' getCqParams2(ConQuestSys())
#' }
getCqParams2<- function(myCqs){

  if(myCqs$gPairWise) stop("pairwise is not yet supported by getCqParams")

  paramList<- list() # this is returned
  mySeExist<- myCqs$gStdError < 3 # bool, is SE available? (3 = none)
  myFitExist<- myCqs$gIFit

  myVars<- getCqVars(myCqs)
  myTerms<- getCqTerms(myCqs)
  myXsiCounter<- 1 # counter to iterate over `gXsi`

  for(i in 1:max(myTerms$TermNumber)){
    myTermsSub<- myTerms[myTerms$TermNumber == i ,]
    myTermVarN<- length(unique(paste0(myTermsSub$VariableNumber, myTermsSub$VariableType))) # how many variables in this term?
    myVarsSub<- myVars[paste0(myVars$VariableNumber, myVars$VariableType) %in% paste0(myTermsSub$VariableNumber, myTermsSub$VariableType), ]
    print(myVarsSub)
    # do terms without steps first - it's easier!
    if(all(!myTermsSub$TermStepInvolved)){
      # how many variables involved?
      myTotalLevels<- prod(myVarsSub$VariableLevels)
      myTmpRes<- matrix(NA, ncol = 10+(2*myTermVarN), nrow = myTotalLevels) # e.g., for one variable in term, we will have variable level number, variable level label, xsi, stderr, (outfit x4), (infit x 4) = 12 cols AND product of levels rows
      print(myTmpRes)
    } else {
      myStepCounter<- myCqs$gItemSteps[[myXsiCounter]] #how many steps for the Xsi?
      myTotalLevels<- prod(myVarsSub$VariableLevels)
      myTmpRes<- matrix(NA, ncol = 10+(2*myTermVarN), nrow = myTotalLevels) # e.g., for one variable in term, we will have variable level number, variable level label, xsi, stderr, (outfit x4), (infit x 4) = 12 cols AND product of levels rows
      print(myTmpRes)
    }

  }

  return(TRUE)

  # will want to have, for each param, est, SE, unwFit, unw95CIlow, unw95CIhigh, unwY, wFit, nw95CIlow, w95CIhigh, wT,
  # neeed to know, are the SE? are there fits
  # myCqs$gIFit # bool
  mySeExist<- myCqs$gStdError < 3 # bool SE is calculated, 3 = none

  # at the moment, we simply get back a list of param nums, param types, signs and labels
  # first get info about terms
  termList<- list()

  for(term in seq_len(length(myCqs$gTerms))){
    stepInvolved<- any(unlist(myCqs$gTerms[[term]][c("VariableType")]) == 2) # does this term involve steps?
    termList[[term]]<-matrix(
      unlist(myCqs$gTerms[[term]][c("ParamNumber", "ParamType")]),
      nrow = length(myCqs$gTerms[[term]][["ParamNumber"]]),
      byrow = FALSE
    )

    tempDf<- data.frame(
      TermSign = myCqs$gTerms[[term]]["Sign"],
      TermLabel = myCqs$gTerms[[term]]["Label"],
      TermNum = term,
      TermStepInvolved = stepInvolved, # does this term involve steps?
      ParamNumber =  termList[[term]][ , 1],
      ParamType =  termList[[term]][ , 2],
      ParamXsi = NA,
      ParamSe = NA,
      ParamAnchored = 0,
      ParamDim = NA,
      ParamStep = NA
    )

    if(term == 1){
      tmpParamListDf<- tempDf
    } else {
      tmpParamListDf<- rbind(tmpParamListDf, tempDf)
    }

  }

  # add in anchor xsi
  if(length(myCqs$gImportParameters) > 0){
    anyItemAnchors<- FALSE
    for(i in seq_len(length(myCqs$gImportParameters))){
      if(myCqs$gImportParameters[[i]]$Type == 0){
        anyItemAnchors<- TRUE
        tmpParamNo<- myCqs$gImportParameters[[i]]$I1 #I2 not used for anchor Xsi
        tmpParamAnchorVal<- myCqs$gImportParameters[[i]]$Value
        tmpParamListDf$ParamXsi[tmpParamListDf$ParamNumber == tmpParamNo & tmpParamListDf$ParamType == 0]<- tmpParamAnchorVal
        tmpParamListDf$ParamAnchored[tmpParamListDf$ParamNumber == tmpParamNo & tmpParamListDf$ParamType == 0]<- 1
      }
    }
  }

  # add in estimated xsi
  thisXsiIndex<- 1
  thisParamNum<- 0
  thisParamNumSe<- 1
  for(i in seq_len(length(tmpParamListDf$ParamNumber))){
    if(tmpParamListDf$ParamAnchored[i] == 1){
      thisXsiIndex<- thisXsiIndex + 1 # use up an index, because imported anchored Xsi appear in gXsi
      thisParamNum<- thisParamNum + 1
    } else if(tmpParamListDf$ParamType[i] == 1){
      next # this param is a location constraint
    } else {
      tmpParamListDf$ParamXsi[tmpParamListDf$ParamNumber == thisParamNum & tmpParamListDf$ParamType == 0]<- myCqs$gXsi[thisXsiIndex]
      if(mySeExist){
        tmpParamListDf$ParamSe[tmpParamListDf$ParamNumber == thisParamNum & tmpParamListDf$ParamType == 0]<- sqrt(myCqs$gDeriv2nd[thisParamNumSe,thisParamNumSe])
      } else {
        tmpParamListDf$ParamSe[tmpParamListDf$ParamNumber == thisParamNum & tmpParamListDf$ParamType == 0]<- NA
      }
      thisXsiIndex<- thisXsiIndex + 1
      thisParamNum<- thisParamNum + 1
      thisParamNumSe<- thisParamNumSe + 1 # if the param is anchored, then it is skipped in gDeriv2nd and thisXsiIndex and thisParamNumSe get out of sync
    }

  }

  return(tmpParamListDf)
  # to get the value of an anchored xsi, need to find out which Dim it is on, and calculate -1*(sum(Xsi_id)) (for d = d, and i = i in d)
}


