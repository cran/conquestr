#
# required helper functions
#

#' @title readCharSafe
#'
#' @description reads `n` bytes as raw from a binary connection.
#'     Removes any embedded nuls, replacing them with `replace`.
#'
#' @param con A file connection - usually a binary file.
#' @param n The number of bytes to read.
#' @param replace a character to replace embedded nulls with.
#'
#' @return character vector.
#' @keywords internal
readCharSafe <- function(con, n, replace = " ") {
    tS <- readBin(con = con, what = "raw", n = n)
    if (any(tS == as.raw(0))) {
        tS <- charToRaw(paste0(rep(replace, n), collapse = ""))
    }
    tS <- rawToChar(tS)
    return(tS)
}

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
#' @description Coerce ConQuest system missing values to NA. Note this is very slow and users should use the internal 
#'   function conquestr::replaceInDataFrame where possible.
#'
#' @param x a data frame.
#' @return x
#' @keywords internal
zapSystemMissing <- function(x) {
  if (!inherits(x, "data.frame")) {
    stop("x must be a data.frame")
  } else {
    for (i in seq_along(x)) {

      # if column is not numeric
      if (!inherits(x[[i]], "numeric")) {
        next
      } else {
        for (j in seq_along(x[[i]])) {
          # this is the required level of precision to get this to return true, -1.797693e+308 will return FALSE
          if (all.equal(x[[i]][j], -1.7976931348e+308) == TRUE) {
            x[[i]][j] <- NA
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
#' @return a string including object names matching the search term
searchConQuestSys <- function(searchString, mySys, value = TRUE, ignore.case = TRUE) {

  if (!("conQuestSysFile" %in% class(mySys))) {
    stop("mySys must be an 'ACER ConQuest' system file object created using the conquestr::ConQuestSys function")
  } else {
    x <- grep(searchString, names(mySys), value = value, ignore.case = ignore.case)
  }
  return(x)
}



#' @title transformPvs
#'
#' @description Helper function to Transform PVs onto a new metric
#'     (e.g., PISA Mean = 500, SD = 100).
#'     Uses the method described in the PISA 2012 technical manual.
#'
#' @param data A data frame or matrix that contains the PVs
#' @param mT The desired mean of the PVs
#' @param sdT The desired sd of the PVs
#' @param weights a vector of weights, the same length as `data[1]` used to
#'     calculate the mean and SD across the PVs
#' @return a List of transformed PVs with as many elements as PVs were listed in 'x'.
transformPvs <- function(data, mT = 0, sdT = 1, weights = 1) {
  # setup and checking
  if (!(is.data.frame(data)||is.matrix(data))) stop("`data` must be matrix or data frame")
  mPVs <- ncol(data)
  for (i in seq(mPVs)) {
    tmpV <- data[ , i]
    if (!is.numeric((tmpV))) stop (paste0("Vectors in `data` must be numeric. Problem with column: ", i))
    if (i == 1) {
      tmpM <- tmpV
    } else {
      tmpM <- cbind (tmpM, tmpV)
    }
  }
  tData <- as.matrix(tmpM)
  if (ncol(tData) != mPVs) stop ("Something funky happened")
  
  # check all PVs are not missing
  tDataL <- as.vector(tData)
  if (any(is.na(tDataL))) stop("PVs must be complete data - missing not allowed")
  # set any NA weights to 0
  if(weights == 1) weights <- rep(1, length(tDataL))
  if (!is.numeric(weights)) stop("weights must be numeric")
  if (any(is.na(weights))) {
    message("missing weights set to 0")
    weights[is.na(weights)] <- 0
  }
  if (length(weights) != length(tDataL)) stop ("weights must be the same length as PVs")
  
  # calc mean and var pooled over PVs
  pvM <- stats::weighted.mean(tDataL, weights)
  pvVar <- (sum(weights) / (sum(weights)^2 - sum(weights^2))) * sum(weights * (tDataL - pvM)^2)
  pvSd <- sqrt(pvVar)

  # use values to create transformed PVs in results
  # such that PV_Ti = A × PV_Ui + B, where T = transformed, U = untransformed,PV = vector of all PVs combined
  # SD = desired SD
  # M = desired mean
  # A = SD/sd(PV), B = M - A*mean(PV_U)
  results <- list()
  myA <- sdT/pvSd
  myB <- mT - myA * pvM
  results <- myA * tDataL + myB
  results <- matrix(results, ncol = mPVs)

  # results["pvM"] <- pvM
  # results["pvVar"] <- pvVar
  # results["pvSd"] <- pvSd
  return(results)

}

#' @title findConQuestExe
#'
#' @description Searches in common install paths to find ConQuest executable.
#' This is called by `ConQuestCall` when no executable is passed explicitly.
#'
#' @return Char with path to ConQuest executable.
#' @keywords internal
#' @examples
#' \dontrun{
#' findConQuestExe()
#' }
findConQuestExe <- function() {
  message("no path to ConQuest Executable provided: searching common install locations.")
  # FIRST we look for a folder (not recursive) that has the string ConQuest in it in commonInstallLocs
  # then we loop through each and is we find a possible folder, we search recursively for a file called
  #  ConQuest and see if it is an exe
  if (Sys.info()["sysname"] == "Windows")
  {
    commonInstallLocs <- c(
      file.path(Sys.getenv("PROGRAMFILES")),
      file.path(Sys.getenv("ProgramFiles(x86)")),
      file.path(Sys.getenv("APPDATA")),
      # normalizePath(file.path(Sys.getenv("HOME"), "..", "Desktop")),
      file.path(Sys.getenv("HOME"))
    )

  } else if (Sys.info()["sysname"] == "Darwin") {
    commonInstallLocs <- c(
        file.path("", "Applications"),
        file.path("~", "Applications"),
        file.path("~", "Desktop"),
        file.path("~", "Downloads")
    )

  } else {
    commonInstallLocs <- c("/") # placeholder for Linux
  }
  for (path1 in commonInstallLocs) {
    mytmp <- list.dirs(path1, recursive = FALSE)
    mytmp <- mytmp[grep("conquest", mytmp, ignore.case = TRUE)]
    if (length(mytmp) == 0) {
      message(paste0("searched in ", path1, ". No ConQuest directory found"))
    } else {
      for (dir in mytmp) {
        myFiles <- list.files(dir, recursive = TRUE, pattern = "ConQuest",  full.names=TRUE)
        if (length(myFiles) == 0) {
          message(paste0("searched in ", dir, ". No ConQuest executable found"))
          break
        } else {
          for (myFile in myFiles) {
            if (file.access(myFile, 1) == 0) { # file.access, mode = 1 (execute), 0 = TRUE, -1 FAIL
              message(
                paste0(
                  "found executable ", normalizePath(myFile, mustWork = FALSE), 
                  ". This will be used to try to call ACER ConQuest"
                )
              )
              if (Sys.info()["sysname"] == "Windows") myFile <- normalizePath(myFile, winslash = "/", mustWork = TRUE)
              return(myFile)
            }
          }
          message(paste0("searched in", dir, ". No ConQuest executable found"))
        }
      }
    }
  }
  stop("No executable found: you must specify where the ConQuest executable is. This error is fatal")
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
createConQuestProject <- function(prefix = getwd(), ...) {
  # debug
  myDebug <- FALSE
  setDebug <- FALSE
  if (hasArg(setDebug)) {
    myArgs <- c(...) # have to get the optional arguments first!
    myDebug <- myArgs["setDebug"]
  }

  # mostly in case getwd() returns NULL (e.g.., if you delete your wd)
  if (is.null(prefix)) stop("prefix must be a valid dir")

  # print message
  message(paste("creating project folders in ", prefix))

  # create alist of file paths to create
  myFilePathsList <- list()

  myFilePathsVec <- c(
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
    file.path("output", "residuals"),
    file.path("output", "history"),
    file.path("submission")
  )

  if (prefix == getwd()) {
    # put paths in a list
    for (myFilePath in myFilePathsVec) {
      if (myDebug) print(myFilePath)
      myFilePathsList[[myFilePath]] <- myFilePath
    }
  } else {
    # put paths in a list with the prefix in front
    for (myFilePath in myFilePathsVec) {
      tmpPath <- file.path(prefix, myFilePath)
      myFilePathsList[[myFilePath]] <- tmpPath
    }
  }

  # create dirs
  for (i in seq_along(myFilePathsList)) {
    if (myDebug) {
      print(i)
      print(myFilePathsList[[i]])
    }
    dir.create(myFilePathsList[[i]], recursive = TRUE)
  }

  return(invisible(TRUE))

}


#' @title getCqHist
#'
#' @description creates a data frame representation of the iteration history 
#'   for all parameters.
#'
#' @param myCqs An ACER ConQuest system file created using the conquest 
#'   command, [put](https://conquestmanual.acer.org/s4-00.html#put).
#' @param labelParams A boolean. When true, and if long (user) 
#'   parameter labels are available, replace default history column names 
#'   (e.g., "Xsi1") with user labels (e.g., "Item one"). 
#'   Currently only available for Xsi and Tau.
#' @return A data frame.
#' @seealso [conquestr::getCqChain()] which is a wrapper for this function
#'   to use with models estimated by Markov chain Monte Carlo (MCMC) methods.
#' @examples
#' myHist <- getCqHist(ConQuestSys(), labelParams = TRUE)
#' str(myHist)
getCqHist <- function(myCqs, labelParams = FALSE) {

  IterHistTmp <- data.frame(
    RunNo = unlist(myCqs$gHistory$RunNo),
    Iter = unlist(myCqs$gHistory$Iter),
    Likelihood = unlist(myCqs$gHistory$Likelihood)
  )

  IterHistTmp <- replaceInDataFrame(IterHistTmp, -1.797693e+308, NA)

# todo - clear NA likelihoods for JML

  ParamTypesTmp <- c("Beta", "Variance", "Xsi", "Tau", "RanTermVariance")
  histList <- list()
  history <- list()
  history[["Likelihood"]] <- IterHistTmp

  # iterate over each param type and unlist into a named list
  for (paramType in ParamTypesTmp) {
    # which lists in gHistory are we working with?
    whichParam <- as.logical(match(names(myCqs$gHistory), paramType, nomatch = 0)) 
    histList[[paramType]] <- unlist(myCqs$gHistory[whichParam])
    # if this paramType is not used in this model (all NA) move on to the next param
    if (all(is.na(histList[[paramType]]))) next
    
    # Deal with "Xsi" , "Tau", "RanTermVariance"
    # beta is special case, 1 row per dim, var is special case, 
    # (1,1); (1,2), ... , (1,gNDim), ... , (2, 1), ... (gNDim, gNDim)
    if (paramType != "Beta" && paramType != "Variance")
    {
      history[[paramType]] <- as.data.frame(
        matrix(
          histList[[paramType]],
          nrow = length(IterHistTmp$Iter),
          byrow = TRUE
        )
      )
      # add names based on param type
      names(history[[paramType]]) <- paste0(paramType, 1:ncol(history[[paramType]])) 
      if (paramType == "Xsi") {
        if (labelParams) {
          if (length(myCqs$gXsiParameterLabels) > 0) {
            names(history[["Xsi"]]) <- paste0(
              names(history[["Xsi"]]),
              "_", 
              trimws(unlist(myCqs$gXsiParameterLabels))
            )
          }
        }
      }
      if (paramType == "Tau") {
        if (labelParams) {
          if (length(myCqs$gTauParameterLabels) > 0) {
            names(history[["Tau"]]) <- paste0(
              names(history[["Tau"]]),
              "_", 
              trimws(unlist(myCqs$gTauParameterLabels))
            )
          } 
        }
      }
    }
    # Deal with Betas
    if (paramType == "Beta") # beta is special case, each entry has 1 row per dim
    {
      myIter <- length(IterHistTmp$Iter) # length because iters may recycle over RunNo
      for (iter in seq(myIter))
      {
        myBetaIter <- unlist(myCqs$gHistory[[paramType]][[iter]])
        myBetaVec <- vector()
        for (i in seq(myCqs$gNDim))
        {
          for (j in seq(myCqs$gNReg))
          {
            myBetaVec <- c(myBetaVec, myBetaIter[i,j])
          }
        }
        if (iter == 1)
        {
          myBetaMatrix <- myBetaVec
        } else
        {
          myBetaMatrix <- rbind(myBetaMatrix, myBetaVec)
        }
      }
      history[[paramType]] <- as.data.frame(myBetaMatrix)
      # add names based on param type
      names(history[[paramType]]) <- paste0(paramType, "_Est", 1:myCqs$gNReg, "_D", rep(1:myCqs$gNDim, each = myCqs$gNReg)) 
    }
    # Deal with Variance
    # var is special case, (1,1); (1,2), ... , (1,gNDim), ... , (2, 1), ... (gNDim, gNDim)
    if (paramType == "Variance")
    {
      myIter <- length(IterHistTmp$Iter) # length because iters may recycle over RunNo
      for (iter in seq(myIter))
      {
        myVarIter <- unlist(myCqs$gHistory[[paramType]][[iter]])
        # get Variances
        myVariances <- diag(myVarIter)
        # get covars (only use in length myCovars > 0)
        myCovars <- myVarIter[lower.tri(myVarIter)]
        myVarIter <- c(myVariances, myCovars)
        if (iter == 1)
        {
          myVarMatrix <- myVarIter
        } else
        {
          myVarMatrix <- rbind(myVarMatrix, myVarIter)
        }
      }
      history[[paramType]] <- as.data.frame(myVarMatrix)
      myVarNames <- paste0(paramType, "_D", 1:myCqs$gNDim)
      if (length(myCovars) > 0)
      {
        # if there are covariances, lets use the indices of the var-covar matrix as names
        tmpMat <- unlist(myCqs$gHistory[[paramType]][[1]]) # grab the first var-covar matrix
        myCovarInd <- matrix(which(lower.tri(tmpMat), arr.ind=T), ncol = 2)
        myCovarInd <- myCovarInd[order(myCovarInd[ , 1]) , ]
        if (!is.null(nrow(myCovarInd))) # is there more than 1 covariance?
        {
          myCovarIndTxt <- apply(myCovarInd,1,paste,collapse="")
        } else
        {
          myCovarIndTxt <- paste(myCovarInd,collapse="")
        }
        myCovarNames <- paste0("Co", tolower(paramType), myCovarIndTxt)
        myVarNames <- c(myVarNames, myCovarNames)
      }
      names(history[[paramType]])<- myVarNames # add names based on param type
    }
    history[[paramType]] <- replaceInDataFrame(history[[paramType]], -1.797693e+308, NA)
  }
  # concatenate list into single DF
  myHistoryDf <- Reduce(cbind, history)
  row.names(myHistoryDf) <- NULL
  return(myHistoryDf)
}

#' @title getCqChain
#'
#' @description creates a data frame representation of the estimation chain from an 
#'   MCMC model.
#'   For example the Patz estimator in ACER ConQuest.
#'   The burn is discarded and only the un-skipped iterations in MCMC chain are retained.
#'
#' @param myCqs A system file.
#' @return A data frame.
#' @examples
#' \dontrun{
#' getCqChain(ConQuestSys())
#' }
getCqChain <- function(myCqs) {
  if (!myCqs$gIntegrationMethod %in% c(7:8)) stop("getCqHist is for models using MCMC integration only, try getCqHist instead")
  tmpHist <- getCqHist(myCqs)
  tmpBurn <- myCqs$gBurn
  if (tmpBurn > 0)
  {
    # "myHist$Iter[1] == 0" checks that iter 1 is the first burn iteration,
    # and that this function hasn't been called multiple times
    tmpBurn <- tmpBurn+1 # note that gBurn is 1-offset, and iter is 0-offset
    if (tmpHist$Iter[1] == 0) tmpHist <- tmpHist[ -c(1:tmpBurn), ]
  }
  tmpHist <- tmpHist[ , -c(grep("^Iter", names(tmpHist))) ]
  return(tmpHist)
}

#' @title summariseCqChain
#'
#' @description takes a data frame created by getCqChain and returns a list reporting the mean and 
#'   variance for each parameter
#'
#' @param myChain A data frame returned from getCqChain.
#' @return A list.
#' @examples
#' \dontrun{
#' summariseCqChain(getCqChain(ConQuestSys()))
#' }
#' @importFrom stats var
summariseCqChain <- function(myChain)
{
  mySummary <- list()

  tmp <- as.data.frame(colMeans(myChain))
  names(tmp)<- c("est")
  mySummary[["mean"]] <- tmp

  tmp <- as.data.frame(sapply(myChain, var)) # manual alg. (sum(myHist$Xsi1^2) - (sum(myHist$Xsi1)^2) / length(myHist$Xsi1)) / (length(myHist$Xsi1) - 1)
  names(tmp)<- c("est")
  mySummary[["var"]] <- tmp

  return(mySummary)
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
getCqVars <- function(myCqs) {
  myVars <- data.frame(
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
getCqTerms <- function(myCqs) {
  termList <- list()
  tmpVars <- getCqVars(myCqs)

  for (term in seq_len(length(myCqs$gTerms))) {
    stepInvolved <- any(unlist(myCqs$gTerms[[term]][c("VariableNumber", "VariableType")]) == 2) # does this term involve steps?
    thisVarType <- unlist(myCqs$gTerms[[term]][c("VariableType")])
    termList[[term]] <- data.frame(
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

  termDf <- do.call("rbind", termList)
  # termDf <- merge(termDf, tmpVars, by.x = c("VariableNumber", "VariableType"), by.y = c("VariableNumber", "VariableType"), all.x = TRUE)
  return(termDf)
}


#' @title getCqParams
#'
#' @description creates a data frame representation of the parameters of the model, 
#'   including both estimated, constrained, and anchored parameters.
#'
#' @param sysFile An ACER ConQuest system file read into R using conquestr::ConQuestSys.
#' @return A data frame.
#' @keywords internal
#' @examples
#' \dontrun{
#' getCqParams(ConQuestSys())
#' }
getCqParams <- function(sysFile) {

  isDebug <- FALSE
  # check sysfile is okay
  defaultSys <- FALSE
  if (missing(sysFile)) {
    sysFile <- conquestr::ConQuestSys()
    defaultSys <- TRUE
  }
  sysFileOk(sysFile, defaultSys)

  # checks
  if (any(unlist(sysFile$gEstimationAllMethods)>20)) stop("pairwise is not yet supported") # actually don't know if this will work
  isFit <- sysFile$gIFit # is fit available?
  isSe <- sysFile$gStdError < 3 # bool SE is calculated, 3 = none

  # get terms, and params associated with each term
  tmpNames <- c("ParamNumber", "ParamType")
  for (i in seq_along(sysFile$gTerms)) {
    tmp1 <- sysFile$gTerms[[i]]
    tmpParamNo <- unlist(tmp1$ParamNumber)
    tmpParamtype <- unlist(tmp1$ParamType)
    tmpResult <- as.data.frame(
      cbind(tmpParamNo, tmpParamtype)
    )
    tmpResultL <- nrow(tmpResult)
    if (tmpResultL > 0) {
      names(tmpResult) <- tmpNames
      tmpResult$label <- tmp1$Label
      # there is a var type for each variable involved in the term
      tmpResult$variable_type <- paste0(unlist(tmp1$VariableType), collapse = ",")
      tmpResult$variable_number <- paste0(unlist(tmp1$VariableNumber), collapse = ",")
      if (i == 1) {
        tempTerms <- tmpResult
      } else
      {
        tempTerms <- rbind(tempTerms, tmpResult)
      }
    } else {
      warning(
        paste0(
          tmp1$Label,
          " is defined in model statement but no parameters are associated with it"
        )
      )
    }
  }

  # See https://github.com/acerorg/ACER-ConQuest/issues/10
  tmpFlag <- (length(unlist(sysFile$gParam)) %% 3 > 0) || (unlist(sysFile$gParam)[1] == -1)
  if (tmpFlag) {
    tmpDim <- 4
    tmpTrimReq <- TRUE
  } else {
    tmpDim <- 3
    tmpTrimReq <- FALSE
  }

  if (isDebug) {
    print(paste0("tmpTrimReq: ", tmpTrimReq))
    print("tmpResult: got terms, and params associated with each term")
    print(tmpResult)
    print(str(tmpResult))
  }
  # get param est values and associated info
  tmpNames <- c("gin_no", "step_involved", "sign") #, "constrained", "anchor")
  # est params or anchors
  tmpParams <- as.data.frame(
    matrix(unlist(sysFile$gParam), ncol = tmpDim, byrow = TRUE)
  )
  if (tmpTrimReq) tmpParams <- tmpParams[ , 2:tmpDim]
  names(tmpParams) <- tmpNames
  tmpParams$gin_no <- as.integer(tmpParams$gin_no)

  # which gins are not identified?
  MyProblemGins <- unlist(sysFile$gProblemGins)+1
  tmpParams <- tmpParams[!(tmpParams$gin_no %in% (MyProblemGins)) , ] #

  if (isDebug) {
    print("tmpParams: removed MyProblemGins")
    print(tmpParams)
    print(str(tmpParams))
  }

  tmpParams$constrained <- FALSE
  TmpAnchor <- unlist(sysFile$gXsiAnchor)
  if (length(MyProblemGins) > 0) TmpAnchor <- TmpAnchor[-MyProblemGins]

  if (isDebug) {
    print("tmpParams$anchor: trying to create after removing MyProblemGins")
    print(TmpAnchor)
    print(str(TmpAnchor))
  }

  tmpParams$anchor <- TmpAnchor # later we put this back in tmpParams$anchor

  if (isDebug) print(str(tmpParams))

  tmpParams$xsi <- as.vector(unlist(sysFile$gXsi))
  tmpParams$se <- rep(NA, length(tmpParams$xsi))
  tmpCounter <- 1
  tmpErrVar <- as.vector(diag(sysFile$gDeriv2nd))
  
  if (isDebug) {
    print(paste0("length(tmpParams$xsi): ", length(tmpParams$xsi)))
    print(paste0("tmpErrVar: ", tmpErrVar))
  }

  # TODO: build test for this
  # can gNXsiAnchors be length(gXsiAnchor)? if so it could be > length(tmpParams$xsi)
  if(length(tmpParams$xsi) == sysFile$gNXsiAnchors) { 
    # nothing to do, all xsi anchored
  } else {
    for (i in seq_along(tmpParams$anchor)) {
      if(!tmpParams$anchor[i]) {
        tmpParams$se[i] <- tmpErrVar[tmpCounter]
        tmpCounter <- tmpCounter + 1
      }
    }
  }
  tmpParams$se <- sqrt(tmpParams$se)

  if (length(sysFile$gParamConstrained) > 0) {
    tmpParamsCons <- as.data.frame(
      matrix(unlist(sysFile$gParamConstrained), ncol = tmpDim, byrow = TRUE)
    )
    if (tmpTrimReq) tmpParamsCons <- tmpParamsCons[ , 2:tmpDim]
    names(tmpParamsCons) <- tmpNames
    tmpParamsCons$constrained <- TRUE
    tmpParamsCons$anchor <- FALSE
    tmpParamsCons$xsi <- NA
    tmpParamsCons$se <- NA
  } else {
   tmpParamsCons <- data.frame(
    gin_no = NULL,
    step_involved = NULL,
    sign = NULL,
    constrained = NULL,
    anchor = NULL,
    xsi = NULL,
    se = NULL
    )
  }

  if (isDebug) {
    print(paste0("tmpParamsCons: ", tmpParamsCons))
  }

  # put params in same order as tempTerms
  tmpCount1 <- 1 # which unconstrained param to get
  tmpCount2 <- 1 # which constrained param to get

  myParams <- list()
  for (i in seq(length(tempTerms$ParamNumber))) {
    if (tempTerms$ParamType[i] == 0) { # unconstrained
      myParams[[i]] <- tmpParams[tmpCount1 , ]
      tmpCount1 <- tmpCount1 + 1
    } else if (tempTerms$ParamType[i] == 1) { # constrained
      myParams[[i]] <- tmpParamsCons[tmpCount2 , ]
      tmpCount2 <- tmpCount2 + 1
    } else
    {
      stop("unknown param type encountered")
    }
  }

  for (i in seq(length(myParams))) {
    if (i == 1) {
      myParamsDf <- myParams[[i]]
    } else
    {
      myParamsDf <- rbind(myParamsDf, myParams[[i]])
    }
  }

  myResult <- cbind(tempTerms, myParamsDf)

  return(myResult)
}

#' @title getCqFit
#'
#' @description creates a data frame representation of the fit of parameters in the item response model
#' @param myCqs A system file.
#' @return A data frame.
#' @examples
#' \dontrun{
#' getCqFit(ConQuestSys())
#' }
getCqFit <- function(myCqs) {
  if (!myCqs$gIFit) stop("fit has not been estimated")
  myTempNames <- data.frame(fitName = matrix(unlist(myCqs$gFitStatistics[[1]]), ncol = 1))
  myTempFits <-  matrix(unlist(myCqs$gFitStatistics[[2]]), nrow = length(myCqs$gFitStatistics[[2]]), byrow = TRUE)
  myTempFits <- data.frame(myTempFits)
  names(myTempFits)<- names(myCqs$gFitStatistics$Value[[1]])

  myFit <- cbind(myTempNames, myTempFits)
  return(myFit)
}

#' @title getCqLongLabs
#'
#' @description returns a vector of long gin labels returns vector of length 0
#'  if there are no labels used (see command labels in conquest)
#'
#' @param sysFile An ACER ConQuest system file read into R using conquestr::ConQuestSys.
#' @return A vector.
#' @keywords internal
#' @examples
#' \dontrun{
#' getCqLongLabs(ConQuestSys())
#' }
getCqLongLabs <- function(sysFile) {
  # check sysfile is okay
  defaultSys <- FALSE
  if (missing(sysFile)) {
    sysFile <- conquestr::ConQuestSys()
    defaultSys <- TRUE
  }
  sysFileOk(sysFile, defaultSys)

  return(unlist(sysFile$gGinLongLabels))
}

#' @title isCqConverged
#'
#' @description returns true is the ConQuest model has converged normally (system file).
#'
#' @param myCqs A system file.
#' @return A boolean.
#' @keywords internal
#' @examples
#' \dontrun{
#' isCqConverged(ConQuestSys())
#' }
isCqConverged <- function(myCqs) {
  if (!"conQuestSysFile" %in% class(myCqs))
  {
    stop("'myCqs' must be a ConQuest system file object created by 'conquestr::ConQuestSys'")
  }

  modConverged <- FALSE
  tmpHist <- getCqHist(myCqs)
  lastIter <- length(tmpHist$Iter)
  paramCriterion <- myCqs$gParameterConvCriterion
  devCriterion <- myCqs$gDevianceConvCriterion

  # Params
  lastParams <- tmpHist[lastIter, grep("^Lik", names(tmpHist))]
  lastParams <- lastParams[!is.na(lastParams)]
  prevParams <- tmpHist[lastIter - 1, grep("^Lik", names(tmpHist))]
  prevParams <- prevParams[!is.na(prevParams)]
  paramsConv <- all(abs(lastParams - prevParams) < paramCriterion)
  # Dev
  lastDev <- tmpHist[lastIter, grep("^Lik", names(tmpHist))]
  lastDev <- lastDev[!is.na(lastDev)]
  prevDev <- tmpHist[lastIter - 1, grep("^Lik", names(tmpHist))]
  prevDev <- prevDev[!is.na(prevDev)]
  devConv <- all(abs(lastDev - prevDev) < devCriterion)

  if (all(c(paramsConv, devConv))) modConverged <- TRUE

  return(modConverged)
}