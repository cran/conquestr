#' @title getCqRespModel
#'
#' @description produces a table of model parameter estimates, errors, fits, and
#'   scaled 2PL estimates if available.
#'
#' @param sysFile An ACER ConQuest system file read into R using conquestr::ConQuestSys
#'
#' @return A List of data frames. Each data frame is a term in the response model
#' @examples
#' \dontrun{
#' myShowRespMod <- getCqRespModel(conquestr::ConQuestSys())
#' }
getCqRespModel <- function(sysFile) {

  isDebug <- FALSE
  # test if system file is passed in
  defaultSys <- FALSE
  if (missing(sysFile)) {
    sysFile <- conquestr::ConQuestSys()
    defaultSys <- TRUE
  }
  sysFileOk(sysFile, defaultSys)

  # setup
  resultL <- list() # put results in here
  termsN <- length(sysFile$gTerms) # how many terms in model?
  isTwoPl <- sysFile$gTwoPL

  # get the model terms and params
  termsParams <- getCqParams(sysFile)

  # pad the model terms and params to account for steps
  tempList <- list()
  tmpCount <- 1
  for (i in seq_along(termsParams[, 1])) {
    if (termsParams$step_involved[i] < 0) { # no step involved
      tempList[[tmpCount]] <- termsParams[i , ]
      tmpCount <- tmpCount + 1
    } else {
      # prepare dummy row for step 0 and last step (constraint)
      tmpy <- termsParams[i , ]
      tmpy$step_involved <- 0
      tmpy$constrained <- FALSE
      tmpy$anchor <- NA
      tmpy$xsi <- NA
      tmpy$se <- NA

      if (termsParams$step_involved[i] == 1) { # first step
        tempList[[tmpCount]] <- tmpy
        tmpCount <- tmpCount + 1
        #if (isDebug) print("this is step 0")
      }

      tempList[[tmpCount]] <- termsParams[i , ]
      tmpCount <- tmpCount + 1
      #if (isDebug) print("this is an estimated step")

      if (i < length(termsParams[, 1])) { # check this isn't the last row
        if (termsParams$step_involved[i+1] == 1) { # next row is a different gin
          tmpy$step_involved <- as.numeric(termsParams$step_involved[i])+1
          tmpy$constrained <- TRUE
          tempList[[tmpCount]] <- tmpy
          tmpCount <- tmpCount + 1
          #if (isDebug)  print("this is the last step")
        }
      } else if (i == length(termsParams[, 1])) {
        tmpy$step_involved <- as.numeric(termsParams$step_involved[i])+1
        tmpy$constrained <- TRUE
        tempList[[tmpCount]] <- tmpy
        tmpCount <- tmpCount + 1
        #if (isDebug)  print("this is the very last step")
      }
    }
  }

  for (i in seq(length(tempList))) {
    if (i == 1) {
      showRespMod <- tempList[[i]]
    } else
    {
      showRespMod <- rbind(showRespMod, tempList[[i]])
    }
  }

  # calculate constrained params
  tTerms <- unique(showRespMod$label)
  for (term in tTerms) {
    tShowRespMod <- showRespMod[showRespMod$label == term , ]
    if (isDebug) {
      print(paste0("calculate constrained params for term: ", term))
      print(paste0("step_involved: ", any(tShowRespMod$step_involved > -1)))
    }
    # does this term only involve explicit vars? That is, it is not a gin
    isExplicit <- all(tShowRespMod$gin_no == "-1")
    # does the term involve an explicit var, for example item*rater? this term includes gins
    involvesExplicit <- all(grepl("1", tShowRespMod$variable_type))


    if (any(tShowRespMod$step_involved > -1)) { #step involved
      if (any(tShowRespMod$constrained)) {
        tGins <- unique(tShowRespMod$gin_no) # gins involved with this term
        for (thisGin in tGins) {
          # first category is omitted
          tShowRespMod_sub <- tShowRespMod[(
            # when tShowRespMod$step_involved == 0, this is the first step within the gin
            tShowRespMod$gin_no == thisGin & tShowRespMod$step_involved > 0
          ), ]

          tstSub <- tShowRespMod_sub[
            tShowRespMod_sub$constrained == TRUE & (is.na(tShowRespMod_sub$xsi)|is.na(tShowRespMod_sub$se)) , 
          ]
          tmpCheck <- nrow(tstSub)
          if (isDebug) {
            print("subset of this term that involves a step with 0th cat ommited")
            print(tShowRespMod_sub) #debug
            print(paste0("how many rows need updating?", tmpCheck))
          }
          # check, do we need to calculate this value?
          if (tmpCheck > 0) {
            tmpXsi <- -1 * sum(tShowRespMod_sub$xsi, na.rm = TRUE)
            tVar <- tShowRespMod_sub$se^2
            tmpXsiVar <- sqrt(sum(tVar, na.rm = TRUE))
            # replace values
            showRespMod[
              (
                showRespMod$label == term &
                showRespMod$gin_no == thisGin &
                showRespMod$constrained == TRUE
              ), grep("^xsi", names(showRespMod))
            ] <- tmpXsi
            showRespMod[
              (
                showRespMod$label == term &
                showRespMod$gin_no == thisGin &
                showRespMod$constrained == TRUE
              ), grep("^se$", names(showRespMod))
            ] <- tmpXsiVar

            if (isDebug) {
            print(paste0("updated Xsi is ", tmpXsi))
            print(paste0("updated SE is ", tmpXsiVar))
          }
          }
        }
      }
    } else if (isExplicit) {
      if (any(tShowRespMod$constrained)) {
        # which row in tShowRespMod (none of the items are gins)
        # is it more than one? can it be?
        constrGins <- tShowRespMod$ParamNumber[which(tShowRespMod$constrained, isTRUE)]
        if (isDebug) print(constrGins)
        if (length(constrGins) > 1) stop("Too many contrainted params in term made up of only explict vars")
        tmpXsi <- -1 * sum(tShowRespMod$xsi, na.rm = TRUE)
        tVar <- tShowRespMod$se^2
        tmpXsiVar <- sqrt(sum(tVar, na.rm = TRUE))

        # replace NA in showRespMod
        # this gets the wrong gin number - do we need to get a new "constrGins[i]"

        showRespMod[
          (
            showRespMod$label == term &
            showRespMod$ParamNumber == constrGins[1] &
            showRespMod$constrained == TRUE
          ), grep("^xsi", names(showRespMod))
        ] <- tmpXsi
        showRespMod[
          (
            showRespMod$label == term &
            showRespMod$ParamNumber == constrGins[1] &
            showRespMod$constrained == TRUE
          ), grep("^se$", names(showRespMod))
        ] <- tmpXsiVar
        if (isDebug) {
          print(paste0("constrGins: ", constrGins))
          print(paste0("tmpXsi: ", tmpXsi))
          print(paste0("tmpXsiVar: ", tmpXsiVar))
          warning("check this is working with explcit vars")
        }
      }

    } else { # no step involved, not Explicit
      # involves an implicit var? Then only include gins on dim
      if (length(grep("0", tShowRespMod$variable_type)) > 0) {
        # for this term, find the constrained gins (can be many when dims > 1), 
        # find which dim each constrained gin is on
        # then find the gins involved with each constrained gin
        constrGins <- tShowRespMod[
          tShowRespMod$constrained == TRUE ,
          grep("^gin_no", names(tShowRespMod))
        ]
        for (i in seq_along(constrGins)) {
          for (j in seq_len(sysFile$gNDim)) {
            if (constrGins[i] %in% unlist(sysFile$gGeneraliseditemList_D[[j]])) {
              # this constrained param is in this dim
              invGins <- unlist(sysFile$gGeneraliseditemList_D[[j]])
              tShowRespMod_sub <- tShowRespMod[tShowRespMod$gin_no %in% invGins , ]
              tmpXsi <- -1 * sum(tShowRespMod_sub$xsi, na.rm = TRUE)
              tVar <- tShowRespMod_sub$se^2
              tmpXsiVar <- sqrt(sum(tVar, na.rm = TRUE))
              # replace NA in showRespMod
              showRespMod[
                (
                  showRespMod$gin_no == constrGins[i] &
                  showRespMod$constrained == TRUE
                ), grep("^xsi", names(showRespMod))
              ] <- tmpXsi
              showRespMod[
                (
                  showRespMod$gin_no == constrGins[i] &
                  showRespMod$constrained == TRUE
                ), grep("^se$", names(showRespMod))
              ] <- tmpXsiVar
            }
          }
        }
      }
    }
  } # end loop over terms

   # get fit
  if (any(showRespMod$variable_type == "1")) {
    warning("fit is not available when explicit vars in model")
  } else {
    if (sysFile$gIFit) {
    tmpFit <- getCqFit(sysFile)
    showRespMod <- cbind(showRespMod, tmpFit)
    } else {
      warning("fit is not available and is omitted from Show Response Model")
    }
  }

  # add long gin labels if available
    if (any(showRespMod$variable_type == "1")) {
      # not sure why by even when model is something like "item+item*step+studyYear"
      # gGinLongLabels is in the format "item+item*studyYear"
      # note it is called GinLabel internally
      warning("labels not available when explicit vars in model")
    } else {
      tLongLabs <- getCqLongLabs(sysFile)
      tLongLabs <- tLongLabs[as.numeric(showRespMod$gin_no)+1] # only keep gins included in model - note 0 index
      if (length(tLongLabs) > 0) {
        if (length(tLongLabs) != length(showRespMod[ , 1])) {
          warning("labels not same length as item params")
        } else {
          showRespMod <- cbind(showRespMod, tLongLabs)
          names(showRespMod)[length(showRespMod)] <- "GinLongLabel"
        }
      }
    }

  return(showRespMod)
}

#' @title sysFileOk
#'
#' @description checks
#'
#' @param sysFile An ACER ConQuest system file read into R using conquestr::ConQuestSys
#' @param defaultSys A Boolean indicating if sysFile is the defualt system file
#'   created by an empty call to conquestr::ConQuestSys
#'
#' @return NULL
#' @examples
#' \dontrun{
#' sysFileOkResult <- sysFileOk(conquestr::ConQuestSys())
#' }
sysFileOk <- function(sysFile, defaultSys) {

  if (defaultSys)
  {
    warning(
      "No System File provided function. Using default
      (inbuilt) system file instead"
    )
  }
  if (!is(sysFile, "conQuestSysFile"))
  {
    stop(
      "The System File is not a valid ACER ConQuest system
      file read-into R using `conquestr::ConQuestSys`"
    )
  }

}

#' @title ginsOnDims
#'
#' @description returns a list of length gNDims. Each element of the list contains
#'  a vector of the gins on this dim.
#'
#' @param sysFile An ACER ConQuest system file read into R using conquestr::ConQuestSys
#'
#' @return a list
#' @examples
#' \dontrun{
#' myResult <- ginsOnDims(conquestr::ConQuestSys())
#' }
ginsOnDims <- function(sysFile) {

  defaultSys <- FALSE
  if (missing(sysFile)) {
    sysFile <- conquestr::ConQuestSys()
    defaultSys <- TRUE
  }
  sysFileOk(sysFile, defaultSys)

  tmpResult <- list()
  for (i in seq_len(sysFile$gNDim)) {
    tmpResult[[i]] <- unlist(sysFile$gGeneraliseditemList_D[[i]])
  }

  return(tmpResult)

}
