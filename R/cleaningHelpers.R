#' @title checkVarsExist
#'
#' @description Check that all required variables exist in raw data.
#' @param data Raw data, a data frame.
#' @param varNames Vector of valid variable names.
#' @return A list.
#' @keywords internal
checkVarsExist <- function(data, varNames) {
  #create return object
  tmpList <- list()
  # validation of inputs
  if (!is.data.frame(data)) stop("Data must be a data frame")
  if (!is.character(varNames)) stop("varNames must be a vector of variable names")
  tmpDataNames <- names(data)
  if (!all(varNames %in% tmpDataNames)) {
    tmpList[[1]] <- varNames[!varNames %in% tmpDataNames]
  }
  return(tmpList)
}

#' @title checkNoExtraVars
#'
#' @description Check that the raw data does not include extraneous variables.
#' @param data Raw data, a data frame.
#' @param varNames Vector of valid variable names.
#' @param except A vector of variable names to be excluded form the check. 
#' @return A list.
#' @keywords internal
checkNoExtraVars <- function(data, varNames, except=NULL) {
  #create return object
  tmpList <- list()
  # validation of inputs
  if (!is.data.frame(data)) stop("Data must be a data frame")
  if (!is.character(varNames)) stop("varNames must be a vector of variable names")
  if (!is.null(except)) {
    if (!is.character(except)) stop("Except must be a vector of variable names")
  }
  tmpDataNames <- names(data)
  if (length(except > 0)) varNames <- c(varNames, except)
  if (!all(tmpDataNames %in% varNames)) {
    tmpList[[1]] <- tmpDataNames[!tmpDataNames %in% varNames]
  }
  return(tmpList)
}

#' @title checkVars
#'
#' @description Check raw data: are all required variables preseent and 
#'     ensure there are no extraneous variables.
#' @param data Raw data, a data frame.
#' @param varNames Vector of valid variable names.
#' @param except A vector of variable names to be excluded form the check.
#' @return A list.
checkVars <- function(data, varNames, except=NULL) {
  #create return object
  tmpList <- list()
  tmpList[[1]] <- checkVarsExist(data, varNames)
  tmpList[[2]] <- checkNoExtraVars(data, varNames, except)
  return(tmpList)
}

#' @title checkItemRespValid
#'
#' @description Check that the item responses in raw data are: (1) valid, (2) each valid response mapped to an
#'   item appears at least once, and (3) each valid reponse mapped to an item has sufficently many responses
#'   (defaults to a minimum of 10 observations for each response category) 
#' @param data Raw data, a data frame.
#' @param caseID A string indicating the name of the case id variable in the data.
#' @param validMap A data frame which contains a mapping of valid responses to item lables.
#'   This data frame shoudl be in long format, with each valis response * item conbination representing a row.
#' @param varLabel A string indicating the name of the variable in validMap that identidies the valis items names/lables.
#' @param validLabel A string indicating the name of the variable in validMap that contains the 
#'      valid codes/responses for each item. This shoudl include missing values (e.g., "99")
#' @return A list of lists: one list per item in validMap$varLabel. Within each list, there can be up to three dfs: 
#'     (1) the case ids and invalid responses for the item, (2) the valid codes not observed in the data set, and 
#'     (3) the valid codes observed fewer than 10 times in the data.
#'     NOTE: a wanring is thrown if the validMap$varLabel is not found in the data. 
checkItemRespValid <- function(data, caseID, validMap, varLabel, validLabel) {
  #create return object
  tmpList <- list()
  # Validation of inputs
  if (!is.data.frame(data)) stop("Data must be a data frame")
  if (any(class(data) %in% c("tbl_df", "tbl")))
  {
    data <- as.data.frame(data)
  }
  if (!is.character(caseID)) stop("caseID must be a case ID")
  if (!is.data.frame(validMap)) stop("validResps must be a data frame")
  if (any(class(validMap) %in% c("tbl_df", "tbl")))
  {
    validMap <- as.data.frame(validMap)
  }
  if (!is.character(varLabel)) stop("varLabel must be the name of the vector which contains the variable names")
  if (!is.character(validLabel)) stop("validLabel must be the name of the vector which contains the valid responses")
  tmpUniqueVar <- unique(validMap[ , grep(paste0("^",varLabel,"$"), names(validMap))])
  # is the list of vars valid?
  if (any(duplicated(tmpUniqueVar))) stop("variable names to be validated are not unique")
  # loop over variables to validate
  for (i in seq(length(tmpUniqueVar)))
  {
    thisItem <- tmpUniqueVar[i]
    tmpList[[thisItem]] <- list()
    tC <- 1
    # is this variable in the data?
    if (!tmpUniqueVar[i] %in% names(data))
    {
      warning(paste0("variable: ", tmpUniqueVar[i], " not found in the data!"))
      next
    }
    # set up the indexes of item variable and caseID for data
    varIndex <- grep(paste0("^",tmpUniqueVar[i],"$"), names(data))
    idIndex <- grep(paste0("^",caseID,"$"), names(data))
    # set up the indexes of item variable and response values for valid mapping data frame
    validItemIndex <- grep(paste0("^",varLabel,"$"), names(validMap))
    validCodeIndex <- grep(paste0("^",validLabel,"$"), names(validMap))
    # get all the responses
    tmpResps <- data[ , varIndex]
    # get the valid responses
    tmpValidRows <- grep(paste0("^",tmpUniqueVar[i],"$"), validMap[,validItemIndex])
    tmpValid <- validMap[tmpValidRows, grep(paste0("^",validLabel,"$"), names(validMap))]
    # are there any invalid responses?
    if (any(!tmpResps %in% tmpValid))
    {
      # get the smallest practical subset
      tmpDf <- data[!tmpResps %in% tmpValid , c(idIndex, varIndex)]
      # (tmpDf)
      tmpList[[thisItem]][[tC]] <- tmpDf
    } else
    {
      tmpList[[thisItem]][[tC]] <- data.frame()
    }
    tC <- tC + 1
    #are there any valid responses that don't appear in the dataset?
    if (any(!tmpValid %in% tmpResps))
    {
      # get the smallest practical subset
      tmpDf <- validMap[validMap[,validItemIndex] %in% tmpUniqueVar[i], ]
      tmpDf <- tmpDf[!tmpValid %in% tmpResps , c(validItemIndex, validCodeIndex)]
      # print(tmpDf)
      tmpList[[thisItem]][[tC]] <- tmpDf
    } else
    {
      tmpList[[thisItem]][[tC]] <- data.frame()
    }
    tC <- tC + 1
    # are there any valid responses that appear less than 10 times?
    tmpTable <- as.data.frame(table(tmpResps))
    # remove invalid resps from table
    # subset table by those less than 10
    # work with that new table for if statement using dimNames
    if (any(tmpValid %in% unique(Filter(function (elem) length(which(tmpResps == elem)) < 10, tmpResps))))
    {
      tmpDf <- validMap[validMap[,validItemIndex] %in% tmpUniqueVar[i], ]
      tmpDf <- tmpDf[tmpValid %in% unique(Filter(function (elem) length(which(tmpResps == elem)) < 10, tmpResps)), c(validItemIndex, validCodeIndex)]
      # print(tmpDf)
      tmpList[[thisItem]][[tC]] <- tmpDf

    } else
    {
      tmpList[[thisItem]][[tC]] <- data.frame()
    }
    tC <- tC + 1
  }
  return(tmpList)
}

#' @title recodeResps
#'
#' @description Recode raw item responses for analyses.
#' @param data Raw data, a data frame.
#' @param recodeMap A data frame which contains the raw responses and corresponding recoded responses 
#' for each of the items in long form.
#' @param varLabel A variable name in recodeMap that identifes the item label.
#' @param rawLabel A variable name in recodeMap that identifies the raw item responses to be recoded.
#' @param recodeLabel A variable name in recodeMap that idenitfies the new values to recode to.
#' @return a data frame with raw data recoded according to recodeMap.
recodeResps <- function(data, recodeMap, varLabel, rawLabel, recodeLabel) {
  # Validation of inputs
  if (!is.data.frame(data)) stop("Data must be a data frame")
  if (any(class(data) %in% c("tbl_df", "tbl")))
  {
    data <- as.data.frame(data)
  }
  if (!is.data.frame(recodeMap)) stop("validResps must be a data frame")
  if (any(class(recodeMap) %in% c("tbl_df", "tbl")))
  {
    recodeMap <- as.data.frame(recodeMap)
  }
  if (!is.character(varLabel)) stop("varLabel must be the name of the vector which contains the variable names")
  if (!is.character(rawLabel)) stop("rawLabel must be the name of the vector which contains the raw responses")
  if (!is.character(recodeLabel)) stop("recodeLabel must be the name of the vector which contains the corresponding recoded responses")
  if (!is.numeric(recodeMap[ , grep(paste0("^",recodeLabel,"$"), names(recodeMap))])) stop("vector which contains the corresponding recoded responses must be numeric")
  tmpUniqueVar <- unique(recodeMap[ , grep(paste0("^",varLabel,"$"), names(recodeMap))])
  # is the list of vars valid?
  if (any(duplicated(tmpUniqueVar))) stop("variable names to be validated are not unique")
  # loop over variables to validate
  for (i in seq(length(tmpUniqueVar)))
  {
    # is this variable in the data?
    if (!tmpUniqueVar[i] %in% names(data))
    {
      warning(paste0("variable: ", tmpUniqueVar[i], " not found in the data!"))
      next
    }
    # what is index of this variable in the data?
    varIndex <- grep(paste0("^",tmpUniqueVar[i],"$"), names(data))
    # set up the indexes of item variable, raw responses and recode responses for recodeMap
    validItemIndex <- grep(paste0("^",varLabel,"$"), names(recodeMap))
    rawIndex <- grep(paste0("^",rawLabel,"$"), names(recodeMap))
    recodeIndex <- grep(paste0("^",recodeLabel,"$"), names(recodeMap))
    # subset data by item
    dataTmp <- data[ , varIndex]
    # subset recode mapping
    tmpMap <- subset(recodeMap, rawLabel!=recodeLabel | is.na(recodeLabel))
    # set up the index of item variable for tmpMap
    tmpMapItemIndex <- grep(paste0("^",varLabel,"$"), names(tmpMap))
    # subset rows of tmpMap by item
    tmpMap <- tmpMap[grep(paste0("^",tmpUniqueVar[i],"$"), tmpMap[,tmpMapItemIndex]) , ]
    # set up the indexes of raw responses and recode responses for tmpMap
    tmpMapRawIndex <- grep(paste0("^",rawLabel,"$"), names(tmpMap))
    tmpMapRecodeIndex <- grep(paste0("^",recodeLabel,"$"), names(tmpMap))
    #handle NA
    tmpNa <- max(tmpMap[, tmpMapRecodeIndex]+100, na.rm=TRUE)
    tmpMap[is.na(tmpMap[, tmpMapRecodeIndex]) , tmpMapRecodeIndex] <- tmpNa
    for (val in tmpMap[ , tmpMapRawIndex])
    {
      # replacement values
      replaceW <- tmpMap[tmpMap[ , tmpMapRawIndex] == val, tmpMapRecodeIndex]
      # replace values in data
      dataTmp <- replace(dataTmp, data[, varIndex] %in% val, replaceW)
    }
    # handle NA
    dataTmp[dataTmp == tmpNa] <- NA
    data[ , varIndex] <- dataTmp
  }
  return(data)
}