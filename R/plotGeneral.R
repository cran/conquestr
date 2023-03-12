#
# these are plotting functions that do not use the Rout object. That is, these are plots in addition to the methods specified for the plotRout generic function
#

#' @title plotCqHist
#'
#' @description generates a plot from a history object. Use `getCqHist` to create a history object from an 'ACER ConQuest' system file.
#'
#' @param myHist an R object created by the `getCqHist` function.
#' @param centre a Boolean representing whether the iteration history should be mean centred (within parameter).
#'     This is helpful for plots that include all parameters to ensure the Y axis is sensible. Consider a plot with raw values of the Likelihood _and_ item parameters on it.
#' @param params A vector of which params to plot. Must be one or more of "all", "Likelihood", "Beta", Variance", "Xsi", "Tau".
#' @param legend Should a legend be plotted?.
#' @return A ggplot2 object.
#' @examples
#' \dontrun{
#' myHistPlot <- plotCqHist(getCqHist(ConQuestSys()))
#' }
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate group_by matches
plotCqHist <- function(myHist, centre = TRUE, params = c("all"), legend = FALSE) {

  if ("all" %in% params) {
    myParams <- "Likelihood|Beta|Variance|Xsi|Tau"
  } else {
    myParams = params
  }

  if (!"Iter" %in% names(myHist)) myHist$Iter <- 1:length(myHist[ , 1]) # there is no iter column in object returned from getCqChain

  myHist_L <- myHist %>%
    pivot_longer(
      cols = matches(myParams),
      names_to = "param",
      values_to = "estimate"
    )

  yAxisLabel <- "Estimate"
  if (centre) {
    myHist_L <- myHist_L %>%
      group_by(.data$param)  %>%
      mutate(
        estimate = scale(.data$estimate)
    )

    yAxisLabel <- "Mean-centred estimate"
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
    ggplot2::theme(legend.position=LegendPos)

  return(myHistPlot)
}

#' @title plotDif
#'
#' @description Creates a plot (ggplot2 object) of item parameter estimates common to two system files (e.g., a DIF analysis).
#'
#' @param mySysToItemDifDf An R object of class data frame returned from conquestr::sysToItemDifDf
#' @param myScale A string specifying if the item parameter estimates displayed should be "centred" (default), "scaled" (z scores), or "none" (raw).
#' @param mySuffixes a vector of strings specifying the names for the two groups being analysed, e.g., if the two system files are an analysis of boys and girls, the vector may be `c(_male", "_female")`.
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
  myPlot <- ggplot2::ggplot(mySysToItemDifDf, ggplot2::aes(x = eval(parse(text = paste0("xsi", myScaleVal, mySuffixes[1]))), y = eval(parse(text = paste0("xsi", myScaleVal, mySuffixes[2]))))) +
    ggplot2::geom_point(ggplot2::aes(size = .data$myZedTest)) +
    ggplot2::theme_bw() +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggrepel::geom_text_repel(ggplot2::aes(label=ifelse(.data$myZedTest>1.96,as.character(.data$label),'')), box.padding = 0.5) +
    ggplot2::coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) +
    ggplot2::labs(x = paste0("xsi", myScaleVal, mySuffixes[1]), y = paste0("xsi", myScaleVal, mySuffixes[2]))
  return(myPlot)
}

#' @title sysToItemDifDf
#'
#' @description Creates a data frame that includes the common item parameter estimates from two (or more) system files (e.g., a DIF analysis).
#'
#' @param listOfSysFiles A list of system files returned from conquestr::ConQuestSys
#' @param mySuffixes a vector of strings specifying the names for the two groups being analysed, e.g., if the two system files are an analysis of boys and girls, the vector may be `c(_male", "_female")`.
#' @param myDims A string specifying if all or specific dimensions should be included. The default is "all", Specific dimensions are specified by the label "D1" for dimensions 1 etc.
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



#' @title plotMCC
#'
#' @description Creates a plot of an item characteristic curve (by response category). For a dichotomous item, this will
#'     yield a single curve, for polytomous items this will produce a curve for each response category.
#'     Note this is not for use with `rout` files. See the generic function `plotRout` for plotting rout files.
#'
#' @param item Item parameters for a single item.
#' @param data Two vectors of data in an _n_ by 2 matrix or data frame, where _n_ are the cases in your analysis.
#'     The first vector should be item responses. the second vector should be estimated person abilities.
#' @param range Lower and upper bounds to plot over (defaults to c(-6, 6) OR the 
#'   minimum and maximum estimated ability, whichever is larger).
#' @param e_linetype A string. Should the empirical lines be based on "bins", or "regression". Defaults to "bins"
#' @param bins If _e\_linetype_ is "bins", how many bins should be used to chunk the empirical lines?
#'     defaults to 6. Ignored otherwise.
#' @return A ggplot2 object.
#' @examples
#' myRout <- ConQuestRout()
#' myPlot <- plotRout(myRout)
#' \dontrun{
#' # if you run the above example you will have an ICC plot in the object `myPlot`.
#' plot(myPlot)
#' }
plotMCC <- function(item, data, range = c(-6, 6), e_linetype = "bins", bins = 6) {
  # check inputs are okay

  # update range if needed

  # create data for model lines
  myProbsL <- list()
  range_s <- seq(range, by = 0.1)
  for (i in seq(range_s)) {
    myProbsL[[i]] <- simplep(range_s[i], item)
  }
  myProbs <- (matrix(unlist(myProbsL), ncol = length(item[ , 1]), byrow = TRUE))

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
    labels = c(seq(item[, 1]))
  )

  if (!plotZero) {
    myProbsDfL <- myProbsDfL[myProbsDfL$Category != "1", ]
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

#' @title plotModelExp
#'
#' @description Creates a plot of a model-implied expected score curve.
#'     Note this is not for use with `rout` files. See the generic function `plotRout` for plotting rout files.
#'
#' @param items List of one or more matricies of item parameters.
#' @param range Lower and upper bounds to plot over (defaults to c(-6, 6).
#' @param by Increment to the sequence along `range``.
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
plotModelExp <- function(items, range = c(-6, 6), by = 0.1) {
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