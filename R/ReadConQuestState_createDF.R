# todo: getCqGroupData; add group to getCqData/Df; give cqYData sensible column labs (currently Y1:YgNReg)


#' @title getCqDataDf
#'
#' @description Takes a list object returned by `conquestr::getCqData` and coerces it to a wide data frame.
#'   This can sometimes cause issues in complex data, for example where there are multiple response
#'   vectors for each case (for example a many-facets model). This is because it is assumed that the data
#'   can be reduced to a matrix of _gNCases x m variables_ (where _m_ is the number of id, item, estimate and
#'   regression variables in the analysis). For more complex data, the user should use the outputs of
#'   `conquestr::getCqData` to manually merge together a data frame.
#'
#' @param cqData An R object of class list, returned by the function conquestr::getCqData
#' @return A data frame containing R data frames based on the list objects in
#'   the ConQuest system file that has been read in.
#' @seealso conquestr::ConQuestSys()
#' @seealso conquestr::getCqData
#' @examples
#'   mySys <- ConQuestSys()
#'   myData <- getCqData(mySys)
#'   myDataDf <- getCqDataDf(myData)
getCqDataDf <-function(cqData) {

  isCqData <- any(grepl("^cqData", class(cqData)))
  isMini <- any(grepl("Mini$", class(cqData)))

  if (!isCqData) {
    stop("'cqData' must be a list created by 'conquestr::getCqData'")
  }

  if (isMini) {
    myConQuestData <- cqData[["Estimates"]]
    return(myConQuestData)
  }

  tmpRespData <- cqData[["Responses"]]

  tmpRespData <- tryCatch(
    #try this
    reshape(tmpRespData, timevar = "Item", idvar = "Pid", direction = "wide"),
    # if there's a warning, handle it like this
    warning = function(w) {
      print(
        "converting gResponseData from long to wide has thrown a warning.
        This is usually caused by duplicate PIDs in the response data.
        Some data loss may have occurred"
      )
      (reshape(tmpRespData, timevar = "Item", idvar = "Pid", direction = "wide"))
    },
    # finally, do this
    #   don't need anything here as reshape will 
    #   always return the result from reshape
    finally = { } 
  )
  # reorder resp data cols
  tmpColNames <- names(tmpRespData)
  tmpColNames[-c(1)] # remove "pid"
  myOrder <- as.numeric(
    gsub(
      "^\\w+\\.+(\\d+)$",
      "\\1",
      tmpColNames
    )
  )
  tmpColNames <- tmpColNames[order(myOrder)]
  tmpRespData <- tmpRespData[ , c("Pid", tmpColNames)]

  myConQuestData <- cqData[["PID"]]
  # merge response data on PID lookup, this gives us the right link between seqNum and PID
  myConQuestData <- merge(myConQuestData, tmpRespData, by.x = "seqNum", by.y ="Pid", all.x = TRUE)
  # myConQuestData <- merge(myConQuestData, gGroupDataDf, by.x = "seqNum", by.y ="CaseNum", all.x = TRUE) # merge group data on response data, there will always be at least 1 vector of group vars (can be all NA)
  # merge gYData
  myConQuestData <- merge(myConQuestData, cqData[["Regression"]], by.x = "seqNum", by.y ="seqNum", all.x = TRUE)
  # merge estimates  (note some cases could be missing from gAllCaseEstimatesDf IF they are missing all response data and are missing regression data - e.g., missing regressors result in deletion)
  myConQuestData <- merge(myConQuestData, cqData[["Estimates"]], by.x = "seqNum", by.y ="pid", all.x = TRUE)

  return(myConQuestData)

}

# TODO: function to return matrix sampler
#
#
#   # make nice DF out of matrix sampler  matricies iF they exist
#   if (any(grep("_raw|_fit", names(ReadSysList$gMatrixList))))
#   {
#     # get user defined prefix
#     myMatrixoutPrefix <- strsplit(grep("_raw|_fit", names(ReadSysList$gMatrixList), value = TRUE)[1], split = "_")[[1]][1]
#
#     matrixSampler_fit <- as.data.frame(eval(parse(text=paste0("ReadSysList$gMatrixList$", myMatrixoutPrefix, "_fit"))))
#     matrixSampler_raw <- as.data.frame(eval(parse(text=paste0("ReadSysList$gMatrixList$", myMatrixoutPrefix, "_raw"))))
#
#     # add to system file
#     systemFile[["matrixSampler_fit"]] <- matrixSampler_fit
#     systemFile[["matrixSampler_raw"]] <- matrixSampler_raw
#   }
#
#   # make nice DF out of item fit to use with matrix sampler matricies iF they exist
#   if (any(grep("_userfit", names(ReadSysList$gMatrixList))))
#   {
#     # get user defined prefix
#     myMatrixoutPrefix <- strsplit(grep("_userfit", names(ReadSysList$gMatrixList), value = TRUE)[1], split = "_")[[1]][1]
#
#     matrix_userfit <- as.data.frame(eval(parse(text=paste0("ReadSysList$gMatrixList$", myMatrixoutPrefix, "_userfit"))))
#     matrix_userfit$gin <- c(1:ReadSysList$gNGins)
#     # add to system file
#     systemFile[["matrix_userfit"]] <- matrix_userfit
#   }
#
#   return(systemFile)
#
# }

#' @title getCqData
#'
#' @description Get data objects from an R object of class ConQuestSys.
#'   This function returns person IDs, response data, case estimates, regression and weight data.
#'   Each data type is stored as a data frame, and each data frame is a named element of a list.
#'
#'   1. PID,
#'   2. Responses,
#'   3. Estimates,
#'   4. Regression.
#'
#' @param mySys An R object of class ConQuestSys, returned by the function conquestr::ConQuestSys
#' @return A List of data frames.
#' @seealso conquestr::ConQuestSys()
#' @examples
#'   mySys <- ConQuestSys()
#'   myData <- getCqData(mySys)
getCqData <- function(mySys) {
  
  isSysFile <- any(grepl("^conQuestSysFile", class(mySys)))
  isMini <- any(grepl("Mini$", class(mySys)))

  if (!isSysFile) {
    stop("
      'mySys' must be a ConQuest system file object created by 
      'conquestr::ConQuestSys'"
    )
  }
  tmpList <- list()

  # get case ests
  tmpList[["Estimates"]] <- getCqEsts(mySys)
  if (!isMini) {
    # get person IDs
    tmpList[["PID"]] <- getCqPid(mySys)
    # get responses
    tmpList[["Responses"]] <- getCqResp(mySys)
    # get weight and regression vars
    tmpList[["Regression"]] <- getCqYData(mySys)
  }
  if (isMini) {
    class(tmpList) <- append(class(tmpList), "cqDataMini")
  } else {
    class(tmpList) <- append(class(tmpList), "cqData")
  }
  return(tmpList)
}

#' @title getCqPid
#'
#' @description Return PID as a data frame.
#'
#' @param mySys An R object of class conQuestSysFile, returned by the function conquestr::ConQuestSys
#' @return A data frame containing sequence number and PID (if no PID is declared, this is the sequence number).
#' @keywords internal
getCqPid <- function(mySys) {
  # if no PID is declared in datafile/format, gPIDLookUp is empty
  tmpSeq <- c(seq(mySys$gNCases[[1]]))
  if (!length(mySys$gPIDLookUp) == 0) {
    # note PID can be string
    tmpPid <- as.character(unlist(mySys$gPIDLookUp))
    gPidDf <- data.frame(
      pid = tmpPid,
      seqNum = tmpSeq
    )
  } else
  {
    gPidDf <- data.frame(
      pid = as.character(tmpSeq),
      seqNum = tmpSeq
    )
  }
  gPidDf <- replaceInDataFrame(gPidDf, -1.797693e+308, NA)
  return(gPidDf)
}

#' @title getCqResp
#'
#' @description Return item responses as a data frame.
#'
#' @param mySys An R object of class conQuestSysFile, returned by the function conquestr::ConQuestSys
#' @return A data frame containing raw item responses (pre key) and scored item response (post key).
#' @importFrom stats reshape
#' @keywords internal
getCqResp <- function(mySys) {

  # note: "pid" is really seqNum
  tmpColNames <- names(mySys$gResponseData[[1]])
  tmpNCol <- length(tmpColNames)
  tmpRespData <- matrix(
    unlist(mySys$gResponseData),
    ncol = tmpNCol, byrow = TRUE
  )
  tmpRespData <- as.data.frame(tmpRespData)
  names(tmpRespData) <- tmpColNames

  # get preKey lookup table
  tmpKeys <- unlist(mySys$gPreKeyLookUp[[2]])
  preKeyLU <- data.frame(
    lookup = 0:(length(tmpKeys) - 1),
    PreKeyRsp_char = tmpKeys # should always be char
  )
  tmpRespData <- merge(tmpRespData, preKeyLU, by.x = "PreKeyRsp", by.y = "lookup", all.x = TRUE)
  # order by PID and item
  tmpRespData <- tmpRespData[order(tmpRespData$Pid, tmpRespData$Item), ]

  tmpRespData <- replaceInDataFrame(tmpRespData, -1.797693e+308, NA)

  tmpRespData$Rsp[tmpRespData$RspFlag == 10] <- NA # these are dummies for drawing PVs

  return(tmpRespData)
}


#' @title getCqEsts
#'
#' @description Return ability estimates as a data frame.
#'
#' @param mySys An R object of class conQuestSysFile, returned by the function conquestr::ConQuestSys
#' @return A data frame containing ability estimates (missing if not estimated).
#' @keywords internal
getCqEsts <- function(mySys) {
  tmpNames <- names(unlist(mySys$gAllCaseEstimates[[1]]))
  tmpNCol <- length(tmpNames)
  # rename PVs - dimensions cycle faster than PVs
  tmpNames[grep("^pvs", tmpNames)] <- paste0(
    rep("PV", mySys$gNDim * mySys$gNPlausibles),
    rep(1:mySys$gNPlausibles, each = mySys$gNDim),
    "_D",
    rep(1:mySys$gNDim, mySys$gNPlausibles)
  )
  gAllCaseEstimatesDf <- matrix(
    unlist(mySys$gAllCaseEstimates),
    ncol = tmpNCol, byrow = TRUE
  )
  gAllCaseEstimatesDf <- as.data.frame(gAllCaseEstimatesDf)
  gAllCaseEstimatesDf <- replaceInDataFrame(gAllCaseEstimatesDf, -1.797693e+308, NA)
  names(gAllCaseEstimatesDf) <- tmpNames

  return(gAllCaseEstimatesDf)
}


#' @title getCqYData
#'
#' @description Return weight and regression data as a data frame.
#'
#' @param mySys An R object of class conQuestSysFile, returned by the function conquestr::ConQuestSys
#' @return A data frame containing weight and regression data.
#' @keywords internal
getCqYData <- function(mySys) {
  tmpNCol <- length(unlist(mySys$gYData[[1]]))
  if (tmpNCol > 2)
  {
    tmpNames <- c("Weight", "Constant", paste0("Y", 1:(tmpNCol-2)))
  } else
  {
    tmpNames <- c("Weight", "Constant")
  }

  gYDataDf <- matrix(
    unlist(mySys$gYData),
    ncol = tmpNCol, byrow = TRUE
  )
  gYDataDf <- as.data.frame(gYDataDf)
  names(gYDataDf) <- tmpNames
  gYDataDf$seqNum <- 1:mySys$gNCases[[1]]

  gYDataDf <- replaceInDataFrame(gYDataDf, -1.797693e+308, NA)

  return(gYDataDf)
}
