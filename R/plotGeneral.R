#
# these are plotting functions that do not use the Rout object. That is, these are plots in addition to the methods specified for the plotRout generic function
#

#' @title plotCqHist
#'
#' @description generates a plot from a history object.
#' Use `getCqHist` to create a history object from an 'ACER ConQuest' system file.
#'
#' @param myHist an R object created by the `getCqHist` function.
#' @param centre a Boolean representing whether the iteration history should be mean centred (within parameter).
#'   This is helpful for plots that include all parameters to ensure the Y axis is sensible.
#'   Consider, for example, the readability of a plot with raw values of the Likelihood _and_ item parameters on it.
#' @param params A string of which params to plot.
#'   Must be one or more of "all", "Likelihood", "Beta", Variance", "Xsi", "Tau".
#'   Note the match when using "Beta", Variance", "Xsi", "Tau" is by regular expression,
#'   so "Xsi1" will plot item location parameter 1, 10-19, 100-199 and so on.
#' @param legend Should a legend be plotted?
#' @param plotProblems an optional list defining which potential problem parameters to plot.
#'
#'   - Iters: The first element of the list is an integer defining how many of the final iterations
#'     to consider (e.g., identify parameters that are moving the most over the final 20 iterations).
#'     if NA, the default is to consider the last 10% of iterations.
#'   - Magnitude: The second element of the list is number indicating the magnitude of change over the last
#'     n iterations. if NA, and _Type_ is "relative", defaults to 30 times the largest change at the final iteration.
#'     if NA, and _Type_ is "absolute", defaults to 0.05 logits.
#'   - Type: The third element of the list is a string, either "relative" or "absolute":
#'     -  "relative" indicates that _Magnitude_ is the multiple of the change between the final
#'        iteration and the second-to-last iteration that indicates a potential problem.
#'     - "absolute" indicates that _Magnitude_ refers to change between the the final
#'        iteration and the value in _Iters_ that indicates a potential problem.
#' @return A ggplot2 object.
#' @examples
#' \dontrun{
#' myHistPlot <- plotCqHist(getCqHist(ConQuestSys()))
#' }
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate group_by matches
plotCqHist <- function(myHist, centre = TRUE, params = c("all"), legend = FALSE, plotProblems = NULL) {
  myDebug <- FALSE
  # remove cols that all NA
  myHist <- myHist[ , !is.na(colSums(myHist))]
  # there is no iter column in object returned from getCqChain
  if (!"Iter" %in% names(myHist)) myHist$Iter <- seq_along(myHist[ , 1])

  if ("all" %in% params) {
    # note myParams is index of names vector, NOT string
    myParams <- grep("^Likelihood|^Beta|^Variance|^Xsi|^Tau", names(myHist))
  } else {
    myParams <- grep(paste0("^", params), names(myHist))
    if (length(myParams) < 1) {
      stop(paste0("could not find and model parameters matching the regular expression, ", params))
    }
  }
  myParams_string <- names(myHist)[myParams]
  if (myDebug) {
    print("myParams_string: ")
    print(myParams_string)
  }

  if (!is.null(plotProblems)) {
    # catch errors
    if (length(plotProblems) != 3) stop("plotProblems must be of length three")
    if (!is.integer(plotProblems[[1]])) stop("plotProblems[[1]], 'Iters', must be an integer")
    if (!is.numeric(plotProblems[[2]])) stop("plotProblems[[2]], 'Magnitude', must be a number")
    if (!is.character(plotProblems[[3]])) stop("plotProblems[[3]], 'Type', must be either 'relative', or 'absolute'")

    lastRunNo <- max(myHist$RunNo)
    lastIter <- max(myHist$Iter)

    if (is.na(plotProblems[[1]])) {
      # get the last 10% of iters
      firstIter <- lastIter - ceiling(max(myHist$Iter) / 100 * 10)
      } else {
      if (plotProblems[[1]] > max(myHist[, 2])) plotProblems[[1]] <- max(myHist[, 2])
      firstIter <- plotProblems[[1]]
      }

    if (plotProblems[[3]] == "relative") {
      # over all included params, collect largest change at final iter
      largestChange <- {
        max(
          abs(
            myHist[myHist$Iter == lastIter, myParams] -
             myHist[myHist$Iter == (lastIter - 1), myParams]
          )
        )
      }
      if (is.na(plotProblems[[2]])) {
        tmpCriteria <- largestChange * 30
      } else  {
        tmpCriteria <- largestChange * plotProblems[[2]]
      }
    } else if (plotProblems[[3]] == "absolute") {
      if (is.na(plotProblems[[2]])) {
        tmpCriteria <- 0.05
      } else  {
        tmpCriteria <- plotProblems[[2]]
      }
    }
    # update myParams based on tmpCriteria over span from firstIter to lastIter
    # which rows in myHist
    tmpWhichLastRow <- myHist[myHist$Iter == lastIter & myHist$RunNo == lastRunNo, grep("^Iter", names(myHist))]
    tmpWhichFirstRow <- myHist[myHist$Iter == firstIter & myHist$RunNo == lastRunNo, grep("^Iter", names(myHist))]
    if (myDebug) {
      print("Last row in history: ")
      print(tmpWhichLastRow)
      print("Comparison row in history: ")
      print(tmpWhichFirstRow)
      print("myParams before reducing to problems: ")
      print(myParams_string)
    }

    t_diff <- abs((myHist[myHist$Iter == tmpWhichLastRow, ]) - (myHist[myHist$Iter == tmpWhichFirstRow, ]))
    t_names <- names(myHist)[t_diff > tmpCriteria]
    t_names_str <- paste0("^", paste0(t_names, collapse = "|^"))
    t_problems <- grep(t_names_str, names(myHist))
    whichParams <- myParams[myParams %in% t_problems]

    if (myDebug) {
      print("difference between last row and comparator row for all columns in history, t_diff: ")
      print(t_diff)
      print("which differences are bigger than the criteria for all columns in history, t_names: ")
      print(t_names)
      print("index of names of columns from above, t_names_str: ")
      print(t_names_str)
      print("string of index of column names from above, t_problems: ")
      print(t_problems)
    }
    if (length(whichParams) == 0) {
      warning("criteria in plotProblems found 0 potential issues, plotting all params instead.")
    } else {
      myParams <- whichParams
      myParams_string <- names(myHist)[myParams]
    }
    if (myDebug) {
      print("myParams after reducing to problems: ")
      print(myParams_string)
      print(paste0("tmpCriteria: ", tmpCriteria))
    }
  }

  # need "RunNo", "Iter" plus myParams_string for plot
  t_index <- grep("^RunNo|^Iter", names(myHist))
  t_index_1 <- c(t_index, myParams)
  myHist <- myHist[ , t_index_1]

  myHist_L <- {
    myHist |>
    pivot_longer(
      cols = matches(myParams_string),
      names_to = "param",
      values_to = "estimate"
    )
  }
  yAxisLabel <- "Estimate"
  if (centre) {
    yAxisLabel <- "Mean-centred estimate"
    myHist_L <- {
      myHist_L |>
      group_by(.data$param)  |>
      mutate(
        estimate = scale(.data$estimate)
      )
    }
  }

  if (legend) {
    LegendPos <- "right"
  } else {
    LegendPos <- "none"
  }

  myHistPlot <- ggplot2::ggplot(myHist_L, ggplot2::aes(x = .data$Iter, y = .data$estimate, colour = .data$param)) +
    ggplot2::geom_line(size = 0.5) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Iteration", y = yAxisLabel, colour = "Parameter") +
    ggplot2::theme(legend.position = LegendPos)

  return(myHistPlot)
}

#' @title plotDif
#'
#' @description Creates a plot (ggplot2 object) of item parameter estimates common to two system files
#'   (e.g., a DIF analysis).
#'
#' @param mySysToItemDifDf An R object of class data frame returned from conquestr::sysToItemDifDf
#' @param myScale A string specifying if the item parameter estimates displayed should be "centred" (default),
#'   "scaled" (z scores), or "none" (raw).
#' @param mySuffixes a vector of strings specifying the names for the two groups being analysed,
#'   e.g., if the two system files are an analysis of boys and girls, the vector may be `c(_male", "_female")`.
#' @return A ggplot2 object.
#' @seealso conquestr::sysToItemDifDf()
#' @examples
#' mySys1 <- ConQuestSys()
#' mySys2 <- ConQuestSys()
#' mySysList <- list(mySys1, mySys2)
#' myDifDf <- sysToItemDifDf(mySysList, mySuffixes = c("_male", "_female"), myDims = "all")
#' myDifPlot <- plotDif (myDifDf,myScale = "centred", mySuffixes = c("_male", "_female"))
#' \dontrun{
#' # if you run the above example you will have the plot in the object `myDifPlot`.
#' plot(myDifPlot)
#' }
plotDif <- function(mySysToItemDifDf, myScale = "centred", mySuffixes) {
  myScaleVal <- ifelse(myScale == "none", "", ifelse(myScale == "centred", "C", "Z"))
  myPlot <- ggplot2::ggplot(
      mySysToItemDifDf,
      ggplot2::aes(
        x = eval(parse(text = paste0("xsi", myScaleVal, mySuffixes[1]))),
        y = eval(parse(text = paste0("xsi", myScaleVal, mySuffixes[2])))
      )
    ) +
    ggplot2::geom_point(ggplot2::aes(size = .data$myZedTest)) +
    ggplot2::theme_bw() +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggrepel::geom_text_repel(
      ggplot2::aes(
        label = ifelse(.data$myZedTest > 1.96, as.character(.data$label), '')
      ),
      box.padding = 0.5
    ) +
    ggplot2::coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) +
    ggplot2::labs(x = paste0("xsi", myScaleVal, mySuffixes[1]), y = paste0("xsi", myScaleVal, mySuffixes[2]))
  return(myPlot)
}

#' @title sysToItemDifDf
#'
#' @description Creates a data frame that includes the common item parameter estimates from two (or more)
#'   system files (e.g., a DIF analysis).
#'
#' @param listOfSysFiles A list of system files returned from conquestr::ConQuestSys
#' @param mySuffixes a vector of strings specifying the names for the two groups being analysed,
#'   e.g., if the two system files are an analysis of boys and girls, the vector may be `c(_male", "_female")`.
#' @param myDims A string specifying if all or specific dimensions should be included. The default is
#'   "all", Specific dimensions are specified by the label "D1" for dimensions 1 etc.
#' @return A data frame object.
#' @seealso conquestr::plotDif ()
sysToItemDifDf <- function(listOfSysFiles, mySuffixes, myDims = "all") {
  # TODO - error handling
  outList <- list()
  for (i in seq_along(1:length(listOfSysFiles))) {
    outList[[i]] <- ItemDfStdZ(sysToItems(listOfSysFiles[[i]], myDims))
  }
  outDf <- ItemDfStdZMerge(outList[[1]], outList[[2]], mySuffixes)
  return(outDf)
}

#' @title sysToItems
#'
#' @description Read an R object of class ConQuestSys and create a labelled
#'   representation of the B matrix (scoring matrix).
#'   This maps item response categories to items and dimensions.
#'
#' @param mySys An R object of class ConQuestSys,
#'   returned by the function conquestr::ConQuestSys
#' @param myDims A string specifying if all or specific dimensions
#'   should be included. The default is "all", Specific dimensions are
#'   specified by the label "D1" for dimensions 1 etc.
#' @return A data frame containing R the labelled B matrix.
#' @keywords internal
#' @seealso conquestr::sysToItemDifDf()
sysToItems <- function(mySys, myDims) {
  if (is.null(mySys$gLabels[1][[1]]$Label)) stop("System files must contain items that are labelled")
  myGins <- mySys$gNGins
  myDf <- data.frame(
    item = unlist(mySys$gLabels[1][[1]]$Code),
    label = unlist(mySys$gLabels[1][[1]]$Label),
    xsi = mySys$gXsi[seq_along(1:myGins) , 1],
    se = mySys$gQuickErrorsXsi[seq_along(1:myGins), 1]
  )
  if (!(myDims == "all")) {
    myBMatrix <- sysToBMatrixDf(mySys)
    # which item codes in this DIM?
    # get 2 cols, "myDims... and ItemCode"
    myBMatrixThisDim <- myBMatrix[ , grep(paste0(myDims, "|ItemCode"), names(myBMatrix))]
    myItemsThisDim <- myBMatrixThisDim[myBMatrixThisDim[1] > 0 , 2]
    myDf <- subset(myDf, myDf$item %in% myItemsThisDim)
  }
  return(myDf)
}


#' @title ItemDfStdZ
#'
#' @description Calculates centred and scaled item parameter estimates.
#'   Also calculates standardised standard errors of item parameter estimates
#'   to complement scaled item parameter estimates.
#'
#' @param myDf a data frame.
#' @return a data frame.
#' @keywords internal
#' @seealso conquestr::sysToItemDifDf()
ItemDfStdZ <- function(myDf) {
  tmpDf <- myDf
  if (length(grep("item", names(tmpDf))) > 0) {
    tmpDf <- tmpDf[ , -c(grep("item", names(tmpDf)))]
  }
  tmpDf$xsiC <- scale(tmpDf$xsi, scale = FALSE)
  tmpDf$xsiZ <- scale(tmpDf$xsi)
  tmpDf$seZ <- tmpDf$se * 1/stats::sd(tmpDf$xsi, na.rm= TRUE)
  return(tmpDf)

}

#' @title ItemDfStdZMerge
#'
#' @description Calculates Z test on the common items from 2 data frames
#'   returned from `conquestr::ItemDfStdZ`.
#'
#' @param myDf1 a data frame.
#' @param myDf2 a data frame.
#' @param mySuffixes a vector of strings specifying the names for the two
#'   groups being analysed, e.g., if the two system files are an analysis of boys and girls, the vector may be `c(_male", "_female")`.
#' @return a data frame.
#' @keywords internal
#' @seealso conquestr::sysToItemDifDf()
ItemDfStdZMerge <- function(myDf1, myDf2, mySuffixes) {
  tmpDfMerge <- merge(myDf1, myDf2, by = "label", suffixes = c(mySuffixes))
  # So the z test is (mu1 - mu2 - expected diff (e.g., 0))/sqrt(sd1 + sd2)
  tmpDfMerge$myZedTest <- abs(
    eval(
      parse(
        text = paste0(
          "(tmpDfMerge$xsiZ", mySuffixes[1],
          "- tmpDfMerge$xsiZ", mySuffixes[2],
          ")/sqrt(tmpDfMerge$seZ", mySuffixes[1],
          "+ tmpDfMerge$seZ", mySuffixes[2],
          ")"
        )
      )
    )
  )
  return(tmpDfMerge)
}


#' @title sysToBMatrixDf
#'
#' @description Read an R object of class ConQuestSys and create a labelled
#'   representation of the B matrix (scoring matrix). This maps item response
#'   categories to items and dimensions. Returns long data frame, where items are
#'   duplicated if they are in many dimensions.
#'
#' @param mySys An R object of class ConQuestSys,
#'   returned by the function conquestr::ConQuestSys
#' @param applyLabels A bool indicating whether labels
#'   (e.g., dimension labels) should be appended.
#' @return A data frame containing R the labelled B matrix.
#' @examples
#' myBMatrix <- sysToBMatrixDf(ConQuestSys())
#' \dontrun{
#' # if you run the above example you will have the B Matrix from the example system file.
#' str(myBMatrix)
#' }
sysToBMatrixDf <- function(mySys, applyLabels = TRUE) {
  myTempMat <- matrix(unlist(mySys$gBMatrices), ncol = mySys$gNDim, byrow = TRUE)
  myTempDf <- as.data.frame(myTempMat)
  names(myTempDf)<- paste0(rep("D", mySys$gNDim), c(1:mySys$gNDim))
  if (isTRUE(applyLabels)) {
    if (length(mySys$gLabels) == 0) {
    } else {
      for (i in seq_along(length(mySys$gLabels))) {
        if (mySys$gLabels[[i]]$VarNum == 0 & mySys$gLabels[[i]]$VarType == 0) { # these are item labels
          # do something
          if (length(mySys$gLabels[[i]]$code) > length(unlist(mySys$gItemListByD))) { # too many labels!
          }
        } else {

        }
      }
    }
    tmpList <- list() # list to put steps into
    myDimLabs <- sysToDimLabels(mySys, myWarn = FALSE)
    names(myTempDf)<- paste0(names(myTempDf), "_", myDimLabs$Label)
    myItemLabs <- sysToItemLabels(mySys, myWarn = FALSE)
    # this expands the DF of item labels by each element in gItemSteps
    myItemLabsExp <- myItemLabs[rep(seq_len(nrow(myItemLabs)), unlist(mySys$gItemSteps)), ]
    myTempDf$ItemCode <- as.numeric(as.character(myItemLabsExp$Code))
    for (i in seq(unlist(mySys$gItemSteps))) {
      tmpList[[i]] <- seq(unlist(mySys$gItemSteps)[i])
    }
    myTempDf$ItemStep <- unlist(tmpList)
    myTempDf$ItemLabel <- myItemLabsExp$Label
  }
  return(myTempDf)
}


#' @title sysToDimLabels
#'
#' @description Gets dimensions labels from a ConQuest system file.
#'
#' @param mySys An R object of class ConQuestSys, returned by the function conquestr::ConQuestSys.
#' @param myWarn a bool indicating whether a warning should be printed if there are no dimension labels.
#' @return a data frame.
#' @keywords internal
#' @seealso conquestr::sysToBMatrixDf()
sysToDimLabels <- function(mySys, myWarn = TRUE) {
  tmpFlag <- FALSE
  # cycle through labels - if there are DIM labs, set flag to true
  for (i in seq_len(length(mySys$gLabels))) {
    if (mySys$gLabels[[i]]$VarType == 2) {
      tmpFlag <- TRUE
    }
  }
  # if there are labels, go find them and make DF
  if (isTRUE(tmpFlag)) {
    for (i in seq_len(length(mySys$gLabels))) {
      if (mySys$gLabels[[i]]$VarType == 2) {
        myTmpMat <- matrix(unlist(mySys$gLabels[[i]][c("Code", "Label")]), ncol = 2)
        myTmpDf <- as.data.frame(myTmpMat)
        names(myTmpDf)<- c("Code", "Label")
        return(myTmpDf)
      }
    }
  } else {
    if (isTRUE(myWarn)) warning("This system file does not contain labels for dimensions, labels set to NA")
    myTmpDf <- data.frame(
      Code = seq_len(mySys$gNDim),
      Label = NA
    )
    return(myTmpDf)
  }
}

#' @title sysToItemLabels
#'
#' @description Gets item labels from a ConQuest system file.
#'
#' @param mySys An R object of class ConQuestSys, returned by the function conquestr::ConQuestSys.
#' @param myWarn a bool indicating whether a warning should be printed if there are no item labels.
#' @return a data frame.
#' @keywords internal
#' @seealso conquestr::sysToBMatrixDf()
sysToItemLabels <- function(mySys, myWarn = TRUE) {
  tmpFlag <- FALSE
  # cycle through labels - if there are ITEM labs, set flag to true
  for (i in seq_len(length(mySys$gLabels))) {
    if (mySys$gLabels[[i]]$VarType == 0) {
      tmpFlag <- TRUE
    }
  }
  # if there are labels, go find them and make DF
  if (isTRUE(tmpFlag)) {
    for (i in seq_len(length(mySys$gLabels))) {
      if (mySys$gLabels[[i]]$VarType == 0) {
        myTmpMat <- matrix(unlist(mySys$gLabels[[i]][c("Code", "Label")]), ncol = 2)
        myTmpDf <- as.data.frame(myTmpMat)
        names(myTmpDf)<- c("Code", "Label")
        return(myTmpDf)
      }
    }
  } else {
    if (isTRUE(myWarn)) warning("This system file does not contain labels for items, labels set to NA")
    myTmpDf <- data.frame(
      Code = seq_len(mySys$gNDim),
      Label = NA
    )
    return(myTmpDf)
  }
}

#' @title plotItemMap
#'
#' @description Creates a plot (ggplot2 object) of item parameter
#'   estimates and abilities on latent trait. Note this is not for
#'   use with `rout` files. See the method method plotRout.itemMap
#'   to the generic function `plotRout`
#'
#' @param mySys An 'ACER ConQuest' system file object created using
#'   the conquestr::ConQuestSys function.
#' @param myDims A string specifying which specific dimensions should be
#'   included. The default is "D1", Specific dimensions are specified by
#'   the label "D1" for dimensions 1 etc.
#' @param ginLabs A string specifying whether short or long gin labels should
#'   be used. Default to "short".
#' @param abilityType What kind of person ability estimate should be used?
#'   Defaults to plausible values. Alternatively WLE, MLE, EAP.
#' @param ... Optional arguments, mostly for debugging, e.g., `setDebug = TRUE`
#'   will print temporary data frames.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' mySys1 <- ConQuestSys()
#' myItemMap <- plotItemMap(mySys1)
#' \dontrun{
#' # if you run the above example you will have the plot in the object `myItemMap`.
#' plot(myItemMap)
#' }
plotItemMap <- function(mySys, myDims = "D1", ginLabs = "short", abilityType = "PV", ...) {
  # for debug
  myDebug <- FALSE
  setDebug <- FALSE
  if (hasArg(setDebug)) {
    myArgs <- c(...) # have to get the optional arguments first!
    myDebug <- myArgs["setDebug"]
  }

  # handle myDims
  tmpReqDims <- as.numeric(gsub("D(\\d)", "\\1", myDims))
  if (myDebug) print(tmpReqDims)
  if (!is.na(tmpReqDims) && length(tmpReqDims) == 1) {
    myDimsVec <- tmpReqDims
    if (max(myDimsVec) > mySys$gNDim || min(myDimsVec) < 1) stop("myDims must be a valid dimension")
  } else {
    stop(
      "myDims must be in the format 'Dd', where 'd' is the number
      of the dimension"
    )
  }

  if (abilityType == "PV") {
    if (!mySys$gPlausibleExist) {
      stop(
        "You must have generated PVs in ConQuest to plot,
        try, `estimate ! ...abilities = yes ...;`"
      )
    }
    abilityType <- paste0("PV1_", myDims) # we use this in call to ggplot2
  }
  if (abilityType == "EAP") {
    if (!mySys$gEAPExist) {
      stop(
        "You must have generated EAPs in ConQuest to plot, try,
        `estimate ! ...abilities = yes ...;`"
      )
    }
    abilityType <- paste0("eap", substr(myDims, 2, 2))
  }
  if (abilityType == "WLE")
  {
    if (!mySys$gWLEExist) {
      stop(
        "You must have generated WLEs in ConQuest to plot, try,
        `estimate ! ...abilities = yes ...;`"
      )
    }
    abilityType <- paste0("wle", substr(myDims,2,2))
  }
  # get gins on the requested dims
  gOnD <- ginsOnDims(mySys)
  ginList <- vector()
  if (length(myDimsVec) == 1) {
    ginList <- gOnD[[myDimsVec]]
  } else {
    for (dim in myDimsVec) {
      ginList <- c(ginList, gOnD[[dim]])
    }
  }

  # get item params (currently ONLY delta dots)
  myTmpXsi <- getCqRespModel(mySys)
  myTmpXsi <- myTmpXsi[(myTmpXsi$gin_no %in% ginList & myTmpXsi$label == "item"), ]
  myTmpXsi$`Generalised Item` <- as.numeric(myTmpXsi$gin_no) + 1
  # get gin labs
  ginList2 <- ginList + 1
  shortGinLabs <- vector()
  longGinLabs <- vector()
  for (gin in ginList2) {
    shortGinLabs <- c(shortGinLabs, mySys$gGinShortLabels[[gin]])
    longGinLabs <- c(longGinLabs, mySys$gGinLongLabels[[gin]])
  }
  myTmpXsi$shortGinLabs <- shortGinLabs
  myTmpXsi$longGinLabs <- longGinLabs
  if (ginLabs == "long") myTmpXsi$`Generalised Item` <- myTmpXsi$longGinLabs

  if (myDebug) print (myTmpXsi)
  # merge gGinShortLabels and gGinLongLabels?
  # get abilities (prefer PV, then WLE, then none)
  myTmpAbil <- getCqData(mySys)$Estimates

  myPlot <- ggplot2::ggplot(myTmpAbil, ggplot2::aes(x = eval(parse(text = paste0(".data$", abilityType))))) +
    ggplot2::geom_density() +
    ggplot2::theme_bw() +
    ggplot2::geom_point(
      data = myTmpXsi, ggplot2::aes(x = .data$xsi, y = 0),
      fill = "red", shape = 21, alpha = 0.5, size = 3
    ) +
    ggrepel::geom_text_repel(data = myTmpXsi,
      # TODO, test for no labels and use gin instead.
      # consider above and below dodge for many items
      ggplot2::aes(x = .data$xsi, y = 0, label = .data$`Generalised Item`),
      nudge_y = -0.1,
      direction = "y",
      # angle = 90, # angle of text
      vjust = 0,
      segment.size = 0.2,
      ylim = c(NA, -0.1)
    ) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::coord_flip(ylim = c(-0.2, NA)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")

  return(myPlot)
}



#' @title plotCCC
#'
#' @description Creates a plot of an item characteristic curve (by response category).
#'   For a dichotomous item, this will yield a single curve, for polytomous items this
#'   will produce a curve for each response category.
#'   Note this is not for use with `rout` files. See the generic function `plotRout` for plotting rout files.
#'
#' @param item A matrix of item parameters for a single item. Matrix should be of
#'   the form used in `simplef`
#' @param abilities A vector of doubles  estimated person abilities.
#' @param responses A vector of integers giving the observed person responses to this item.
#' @param weights A vector of doubles of sampling weights.
#' @param groups A factor vector indicating groups.
#' @param range Lower and upper bounds to plot over (defaults to `c(-6, 6)`).
#' @param by A double. The increment to the sequence along `range` used to plot the model lines.
#' @param linetype A string. Should the empirical lines be based on "bins", or "regression". Defaults to "bins"
#' @param bins If _linetype_ is "bins", how many bins should be used to chunk the empirical lines?
#'     defaults to 10. Ignored otherwise.
#' @param plotZero Should the zero category be plotted?
#'   Defaults to `FALSE` when item is dichotomous and `TRUE` otherwise.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw ggplot_build labs guides guide_legend
#' @importFrom stats complete.cases loess weighted.mean
#' @examples
#' myRout <- ConQuestRout()
#' myPlot <- plotRout(myRout)
#' \dontrun{
#' # if you run the above example you will have an ICC plot in the object `myPlot`.
#' plot(myPlot)
#' }
plotCCC <- function(
    item, abilities, responses, weights = NULL, groups = NULL,
    range = c(-6, 6), by = 0.1, linetype = "bins", bins = 10,
    plotZero
) {

  isDebug <- FALSE

  # check inputs are okay
  if (missing(plotZero)) {
    if (length(item[, 1]) == 2) {
      plotZero <- FALSE
    } else {
      plotZero <- TRUE
    }
  }

  if (is.null(groups)) {
    isGroups <- FALSE
  } else {
    isGroups <- TRUE # used in building ggplot object
  }
  if (is.null(weights)) weights <- rep(1, length(abilities))
  if (is.null(groups)) groups <- factor(rep(1, length(abilities)))
  if (
    (length(abilities) != length(responses)) &&
    (length(abilities) != length(groups)) &&
    (length(abilities) != length(weights))
  ) {
    stop("abilities, responses, groups, and weights (if used) must be of equal length")
  }
  if(any(weights < 0)) stop("weights must be non-negative")

  # how many missing responses and abilities?
  NMissResp <- sum(is.na(responses))
  NMissAbility <- sum(is.na(abilities))
  NMissWeights <- sum(is.na(weights))
  NMissGroups <- sum(is.na(groups))

  # create data for empirical lines
    # one series per group
    # when linetype == "bins" for each group, there are b `bins` and for each bin there are k cats
    # when linetype == "regress" ...?
  if (linetype == "bins" || linetype == "regression") {
    weightedN <- sum(weights)
    casesInBins <- weightedN/bins # take every "casesInBins" value of ordered abilities
    myData <- data.frame(
      abilities = abilities,
      responses = responses,
      weights = weights,
      groups = groups,
      expectedP = NA
    )

    for (i in seq(myData$abilities)) {
      # select the expected probability associated with the response
      tmpExpP <- simplep(theta = myData$abilities[i], item = item)
      thisResp <- myData$responses[i]
      tmpExpP_resp <- tmpExpP[(thisResp+1)] # e.g., the response 0 is the first elelemt in tmpExpP
      myData$expectedP[i] <- tmpExpP_resp
    }

    myData <- myData[complete.cases(myData), ]
    if (isDebug) print("myData after complete cases: ")
    if (isDebug) print(str(myData))

    # order data, calc cumulative sum of weights, then workout cut points for quantiles
    myData <- myData[order(myData$abilities) , ]
    myData$cumSumWeight <- cumsum(myData$weights)
    myData$bin <- NA
    sumWt <- 0
    thisBin <- 1
    for (i in seq(myData$weights)) {
      if (thisBin > bins) break
      myData$bin[i] <- thisBin
      sumWt <- sumWt + myData$weights[i]
      if (sumWt > thisBin * casesInBins) thisBin <- thisBin + 1
    }
    myData$bin <- factor(myData$bin)

    # one set of summary data per group
    summaryDataL <- list()
    for(g in seq(unique(myData$group))) {
      thisGroup <- unique(myData$group)[g]
      tData <- myData[myData$groups == thisGroup, ]
      mySummaryData <- data.frame(
        # this data frame is 1 row per response cat, per bin
        bin = rep(unique(myData$bin), length(item[ , 1])),
        cat = rep(item[ , 1], each = bins),
        itemSampleSize = NA, # sum of weights for all cases in the item
        avgAbility = NA, # avg ability of cases in this bin, in this resp cat
        categorySampleSize = NA, # sum of the weights for the cat, for th group
        avgExpP = NA, # average expected P for this category
        avgObsProp = NA, #observed proportion of cases in this cetegory
        group = unique(myData$group)[g] # when we collapse the list to 1 DF, this is the series indicator
      )
      # if (isDebug) print("mySummaryData after group")
      # if (isDebug) print(mySummaryData)
      for (b in seq(unique(myData$bin))) {
        # subset myData for bin b - note that myData$bin is a factor
        thisBin <- unique(myData$bin)[b]
        tData_bin <- tData[tData$bin == thisBin, ]
        # indices of rows and cols to be updated in mySummaryData[[g]] based on subset of raw data, tData_bin
        whichRows <- (mySummaryData$bin == thisBin)
        whichCol1 <- grep("itemSampleSize", names(mySummaryData))
        whichCol2 <- grep("categorySampleSize", names(mySummaryData))
        whichCol3 <- grep("avgAbility", names(mySummaryData))
        whichCol4 <- grep("avgExpP", names(mySummaryData))
        whichCol5 <- grep("avgObsProp", names(mySummaryData))
        # update values based on this bin (ind of cat)
        # if no cases in this bin for this group, skip
        tSum <- sum(tData_bin$weights)
        if (!is.na(tSum)) {
          mySummaryData[whichRows , whichCol1] <-  tSum # itemSampleSize
          mySummaryData[whichRows , whichCol3] <- weighted.mean(tData_bin$abilities, tData_bin$weights) # avgAbility (x coord for this bin)
          #if (isDebug) print(paste0("mySummaryData after bin, ", b))
          #if (isDebug) print(mySummaryData)
          # now work over cats
          # i'm a little worried about this - there is a chance there are unidentified response cats within the group
          for (k in seq(unique(tData_bin$responses))) {
            thisK <- unique(tData_bin$responses)[k]
            tData_cat <- tData_bin[tData_bin$responses == thisK , ]
            whichRows_sub <- (mySummaryData$bin == thisBin & mySummaryData$cat == thisK)
            tSum_sub <- sum(tData_cat$weights)
            # if no cases for this cat in this bin for this group, skip
            if (!is.na(tSum_sub)) {
              mySummaryData[whichRows_sub , whichCol2] <- tSum_sub # cat sample size
              mySummaryData[whichRows_sub , whichCol4] <- weighted.mean(tData_cat$expectedP, tData_cat$weights) # avgExpectedP for this cat
              mySummaryData[whichRows_sub , whichCol5] <- tSum_sub/tSum # avgExpectedP for this cat
              #if (isDebug) print(paste0("mySummaryData after cat, ", k))
              #if (isDebug) print(mySummaryData)
            }
          }
        }
      }
      summaryDataL[[g]] <- mySummaryData
    }

    summaryData <- do.call(rbind.data.frame, summaryDataL)

    # if not plotzero, chuck out the data for the zero cat
    if (!plotZero) summaryData <- summaryData[summaryData$cat > 0 , ]

    if (isDebug) {
      # print(summaryData)
      message("object `tmpSummaryData` created in global env")
      .GlobalEnv$tmpSummaryData <- summaryData
    }

  } else { # end if linetype == "bins"
    stop("linetype must be regression or bins")
  }
  # create plot
  modelPlot <- plotModelCCC(item = item, range = range, by = by, plotZero = plotZero)
  # get plot info to extract colours etc
  g <- ggplot_build(modelPlot)
  tCols <- unique(g$data[[1]]$colour)
  myLineCols <- list()
  for (i in seq(summaryData$cat)) {
    thisCat <- summaryData$cat[i]
    if(plotZero) thisCat <- thisCat + 1
    myLineCols[[i]] <- tCols[(thisCat)]
  }
  summaryData$lineCols <- unlist(myLineCols)

  modelPlot <- modelPlot + labs(colour = "Response \nCategory")

  # add empirical points
  # helper functions below

  modelPlot <- modelPlot +
    geom_point(
      data = summaryData,
      aes(
        x = .data$avgAbility, y = .data$avgObsProp,
        shape = .data$group
      ),
      colour = summaryData$lineCols, alpha = 0.4
    ) +
    guide_remove_groups(isGroups)

  tmpLine <- linetype
  modelPlot <- modelPlot +
    geom_line_or_smooth(thisData = summaryData, thisLinetype = tmpLine) +
    guide_remove_linetype(isGroups)
  # add count of missing responses to plot?

  return(modelPlot)
}

#' @title guide_remove_groups
#'
#' @description helper functions for ggplot - conditionally creates call to
#'   `ggplot2::guides` to remove legends that involve groups when there is 
#'   no groups in the call to `conquestr::plotCCC()`
#'
#' @param groups A bool indicating if there is groups in the call to `conquestr::plotCCC()`.
#' @return a list with a function call to `ggplot2::guides`.
#' @keywords internal
#' @examples
#' myExample <- guide_remove_groups(TRUE)
guide_remove_groups <- function(groups = TRUE) {
  # now this function is outside of `conquestr::plotCCC()` consider
  # passing in ggplot2 object to then be returned.
  return(
    list(
      if (groups) guides(shape = guide_legend("Group")),
      if (!groups) guides(shape = "none")
    )
  )
}

#' @title guide_remove_linetype
#'
#' @description helper functions for ggplot - conditionally creates call to
#'   `ggplot2::guides` to remove legends that involve groups when there is 
#'   no groups in the call to `conquestr::plotCCC()`
#'
#' @param groups A bool indicating if there is groups in the call to `conquestr::plotCCC()`.
#' @return a list with a call to `ggplot2::guides`.
#' @keywords internal
#' @examples
#' myExample <- guide_remove_linetype(TRUE)
guide_remove_linetype <- function(groups = TRUE) {
  # now this function is outside of `conquestr::plotCCC()` consider
  # passing in ggplot2 object to then be returned.
  return(
    list(
      if (groups) guides(linetype = guide_legend("Group")),
      if (!groups) guides(linetype = "none")
    )
  )
}

#' @title geom_line_or_smooth
#'
#' @description helper functions for ggplot - conditionally creates call to
#'   `ggplot2::geom_smooth` depending on the option linetype in the call to `conquestr::plotCCC()`
#' 
#' @param thisData the data frame being worked on in the call to `conquestr::plotCCC()`
#' @param thisLinetype a string, either "bins" or "regression" set in the call to `conquestr::plotCCC()`
#' @return a list with a function call to `ggplot2::geom_line`.
#' @keywords internal
#' @importFrom ggplot2 geom_smooth
#' @examples
#' myExample <- geom_line_or_smooth()
geom_line_or_smooth <- function(thisData, thisLinetype) {
  if(missing(thisLinetype)) {
    # this is to handle R CMD check NOTE: no visible binding for global variable...
    summaryData <- avgAbility <- avgObsProp <- group <- NULL
    return(list())
  } 
  return(
    list(
      if (thisLinetype == "bins") {
        geom_line(
          data = thisData,
          aes(
            x = avgAbility, y = avgObsProp,
            group = interaction(cat, group), linetype = group
          ),
          colour = thisData$lineCols, alpha = 0.4
        )
      },
      if (thisLinetype == "regression") {
        geom_smooth(
          data = thisData,
          method = loess, span = 10,
          se = FALSE,
          aes(
            x = avgAbility, y = avgObsProp,
            group = interaction(cat, group), linetype = group
          ),
          alpha = 0.4,
          colour = thisData$lineCols, alpha = 0.4
        )
      }
    )
  )
}

#' @title plotModelCCC
#'
#' @description Creates a plot of a model implied category characteristic curve.
#'     Note this is not for use with `rout` files. See the generic function `plotRout` for plotting rout files.
#'
#' @param item Item parameters for a single item.
#' @param range Lower and upper bounds to plot over (defaults to c(-6, 6).
#' @param by Increment to the sequence along `range``.
#' @param plotZero Should the zero category be plotted?
#'   Defaults to `FALSE` when item is dichotomous and `TRUE` otherwise.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_line theme_bw
#' @examples
#' myItem <- matrix(
#'   c(
#'     0, 0, 0, 1,
#'     1, 1, 0, 1
#'   ),
#'   ncol = 4, byrow=TRUE
#' )
#' myPlot <- plotModelCCC(myItem)
plotModelCCC <- function(item, range = c(-6, 6), by = 0.1, plotZero) {
  # check inputs are okay
  if (missing(plotZero)) {
    if (length(item[, 1]) == 2) {
      plotZero <- FALSE
    } else {
      plotZero <- TRUE
    }
  }
  if (!is.logical(plotZero)) stop("`plotZero` should be bool")
  if (!is.matrix(item) && ncol(item) != 4) stop("`item` should be a matrix with 4 cols")
  if (length(range) != 2) stop("`range` should be a lower and upper limit of theta")
  if (!(range[2] > range[1])) stop("upper `range` should grater than lower `range`")

  # create data for model lines
  myProbsList <- list()
  myThetaRange <- seq(range[1], range[2], by = by)

  for (i in seq(myThetaRange)) {
    myProbsList[[i]] <- simplep(myThetaRange[i], item)
  }

  myProbs <- matrix(
    unlist(myProbsList),
    ncol = length(item[, 1]),
    byrow = TRUE
  )
  myProbsDf <- as.data.frame(myProbs)
  names(myProbsDf) <- paste0("cat_", seq(length(item[, 1])))
  myProbsDf$Theta <- myThetaRange

  myProbsDfL <- myProbsDf |> pivot_longer(
    cols = matches("^cat"),
    names_to = "Category",
    values_to = "Probability"
  )

  myMap <- data.frame(
    level = as.numeric(
      gsub("^(\\w+_)(\\d{1,})$", "\\2", unique(myProbsDfL$Category))
    ),
    label = unique(myProbsDfL$Category)
  )

  myProbsDfL$Category <- factor(
    myProbsDfL$Category,
    levels = myMap$label[order(myMap$level)],
    labels = c(item[, 1])
  )

  if (!plotZero) {
    myProbsDfL <- myProbsDfL[myProbsDfL$Category != "0", ]
  }

  # could name or post-process categories here (to flag "key" etc)
  # consider optional pass-in of names of categories, and keys?

  myPlot <- ggplot(
    myProbsDfL,
    aes(x = .data$Theta, y = .data$Probability)
  ) +
  geom_line(aes(colour = .data$Category)) +
  theme_bw()

  return(myPlot)

}

#' @title plotExpected
#'
#' @description Creates a plot of an item- or test- expected score curve.
#'     If ability estimates are provided, both empirical and model curves are produced.
#'     Can optionally handle weights and groups as required.
#'     Note this is not for use with `rout` files. See the generic function `plotRout` for plotting rout files.
#'
#' @param items a _list_ of one or more matrices of item parameters. Used in producing model-implied curves.
#' @param range Lower and upper bounds to plot over (defaults to c(-6, 6).
#'   Used in producing model-implied curves. For empirical curves a range is chosen given the
#'   min and max values in abilities.
#' @param by Increment to calculate expectation along `range`. Used in producing model-implied curves.
#' @param bins A double. Optional. How many equally sized bins should abilities be broken up into?
#'   Used in producing empirical curves. If not provided and abilities are provided, a suitable value is chosen
#'   given the length of abilities.
#' @param abilities A vector of doubles. Optional.
#' @param weights A vector of doubles. Optional.
#' @param group A vector of type factor. Optional.
#' @param scale A Boolean. Whether plot should be scaled such that the Y-axis ranges from 0 to 1.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_line theme_bw
#' @examples
#' myItem <- matrix(
#'   c(
#'     0, 0, 0, 1,
#'     1, 1, 0, 1
#'   ),
#'   ncol = 4, byrow=TRUE
#' )
#' myPlot <- plotModelExp(list(myItem))
plotModelExp <- function(
  items, range = c(-6, 6), by = 0.1,
  bins = NULL, abilities = NULL, weights = NULL, group = NULL, scale = FALSE
) {
  # check inputs are okay
  if (!is.list(items) && ncol(items[[1]]) != 4) {
    stop(
      "`items` should be a list and each element of the list
      a matrix with 4 cols"
    )
  }
  if (length(range) != 2) stop("`range` should be a vector with lower and upper limit of theta")
  if (!(range[2] > range[1])) stop("upper `range` should grater than lower `range`")

  # create data for model line
  expectedList <- list()
  myThetaRange <- seq(range[1], range[2], by = by)

  for (i in seq(myThetaRange)) {
    tmpExp <- 0
    for (j in seq(items)) {
      tmpE <- simplef(myThetaRange[i], items[[j]])
      tmpExp <- tmpExp + tmpE
    }
  expectedList[[i]] <- tmpExp
  }

  myExpectedDf <- data.frame(
    Theta = myThetaRange,
    Expected = unlist(expectedList)
  )

  names(myExpectedDf) <- c("Theta", "Expected Score")

  myPlot <- ggplot(
    myExpectedDf,
    aes(x = .data$Theta, y = .data$`Expected Score`)
  ) +
  geom_line() +
  theme_bw()

  return(myPlot)

}
