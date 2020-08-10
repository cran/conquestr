#
# these are plotting functions that do not use the Rout object. That is, these are plots in addition to the methods specified for the plotRout generic function
#

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
#' mySys1<- ConQuestSys()
#' mySys2<- ConQuestSys()
#' mySysList<- list(mySys1, mySys2)
#' myDifDf<- sysToItemDifDf(mySysList, mySuffixes = c("_male", "_female"), myDims = "all")
#' myDifPlot<- plotDif(myDifDf,myScale = "centred", mySuffixes = c("_male", "_female"))
#' \dontrun{
#' # if you run the above example you will have the plot in the object `myDifPlot`.
#' plot(myDifPlot)
#' }
plotDif<- function(mySysToItemDifDf, myScale = "centred", mySuffixes){
  myScaleVal<- ifelse(myScale == "none", "", ifelse(myScale == "centred", "C", "Z"))
  myPlot<- ggplot2::ggplot(mySysToItemDifDf, ggplot2::aes(x = eval(parse(text = paste0("xsi", myScaleVal, mySuffixes[1]))), y = eval(parse(text = paste0("xsi", myScaleVal, mySuffixes[2]))))) +
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
#' @seealso conquestr::plotDif()
sysToItemDifDf<- function(listOfSysFiles, mySuffixes, myDims = "all"){
  # TODO - error handling
  outList<- list()
  for(i in seq_along(1:length(listOfSysFiles))){
    outList[[i]]<- ItemDfStdZ(sysToItems(listOfSysFiles[[i]], myDims))
  }
  outDf<- ItemDfStdZMerge(outList[[1]], outList[[2]], mySuffixes)
  return(outDf)
}

#' @title sysToItems
#'
#' @description Read an R object of class ConQuestSys and create a labelled representation of the B matrix (scoring matrix). This maps item response catagories to items and dimensions.
#'
#' @param mySys An R object of class ConQuestSys, returned by the function conquestr::ConQuestSys
#' @param myDims A string specifying if all or specific dimensions should be included. The default is "all", Specific dimensions are specified by the label "D1" for dimensions 1 etc.
#' @return A data frame containing R the labelled B matrix.
#' @keywords internal
#' @seealso conquestr::sysToItemDifDf()
sysToItems<- function(mySys, myDims){
  if(is.null(mySys$gLabels[1][[1]]$Label)) stop("System files must contain items that are labelled")
  myGins<- mySys$gNGins
  myDf<- data.frame(
    item = unlist(mySys$gLabels[1][[1]]$Code),
    label = unlist(mySys$gLabels[1][[1]]$Label),
    xsi = mySys$gXsi[seq_along(1:myGins) , 1],
    se = mySys$gQuickErrorsXsi[seq_along(1:myGins), 1]
  )
  if(!(myDims == "all")){
    myBMatrix<- sysToBMatrixDf(mySys)
    # which item codes in this DIM?
    myBMatrixThisDim<- myBMatrix[ , grep(paste0(myDims, "|ItemCode"), names(myBMatrix))] # get 2 cols, "myDims... and ItemCode"
    myItemsThisDim<- myBMatrixThisDim[myBMatrixThisDim[1] > 0 , 2]
    myDf<- subset(myDf, myDf$item %in% myItemsThisDim)
  }
  return(myDf)
}


#' @title ItemDfStdZ
#'
#' @description Calculates centred and scaled item parameter estimates. Also calculates standardised standard errors of item parameter estimates to complement scaled item parameter estimates.
#'
#' @param myDf a data frame.
#' @return a data frame.
#' @keywords internal
#' @seealso conquestr::sysToItemDifDf()
ItemDfStdZ<- function(myDf){
  tmpDf<- myDf
  if(length(grep("item", names(tmpDf))) > 0){
    tmpDf<- tmpDf[ , -c(grep("item", names(tmpDf)))]
  }
  tmpDf$xsiC<- scale(tmpDf$xsi, scale = FALSE)
  tmpDf$xsiZ<- scale(tmpDf$xsi)
  tmpDf$seZ<- tmpDf$se * 1/stats::sd(tmpDf$xsi, na.rm= TRUE)
  return(tmpDf)

}

#' @title ItemDfStdZMerge
#'
#' @description Calculates Z test on the common items from 2 data frames returned from `conquestr::ItemDfStdZ`.
#'
#' @param myDf1 a data frame.
#' @param myDf2 a data frame.
#' @param mySuffixes a vector of strings specifying the names for the two groups being analysed, e.g., if the two system files are an analysis of boys and girls, the vector may be `c(_male", "_female")`.
#' @return a data frame.
#' @keywords internal
#' @seealso conquestr::sysToItemDifDf()
ItemDfStdZMerge<- function(myDf1, myDf2, mySuffixes){
  tmpDfMerge<- merge(myDf1, myDf2, by = "label", suffixes = c(mySuffixes))
  # ive not squared these SEs, my thinking is that SE is the SD of the sampling dist.
  # So the z test is (mu1 - mu2 - expected diff (e.g., 0))/sqrt(sd1 + sd2)
  tmpDfMerge$myZedTest<- abs(
    # (tmpDfMerge$xsiZ - tmpDfMerge$xsiZ)/sqrt(tmpDfMerge$seZ + tmpDfMerge$seZ)
    eval(parse(text = paste0("(tmpDfMerge$xsiZ", mySuffixes[1], "- tmpDfMerge$xsiZ", mySuffixes[2], ")/sqrt(tmpDfMerge$seZ", mySuffixes[1], "+ tmpDfMerge$seZ", mySuffixes[2], ")")))
  )
  return(tmpDfMerge)
}


#' @title sysToBMatrixDf
#'
#' @description Read an R object of class ConQuestSys and create a labelled representation of the B matrix (scoring matrix). This maps item response catagories to items and dimensions. Returns long data frame, where items are duplicated if they are in many dimnsions.
#'
#' @param mySys An R object of class ConQuestSys, returned by the function conquestr::ConQuestSys
#' @param applyLabels A bool indicating whether labels (e.g., dimension labels) should be appended.
#' @return A data frame containing R the labelled B matrix.
#' @examples
#' myBMatrix<- sysToBMatrixDf(ConQuestSys())
#' \dontrun{
#' # if you run the above example you will have the B Matrix from the example system file.
#' str(myBMatrix)
#' }
# accepts CQ sys file, returns B matrix in a DF
sysToBMatrixDf<- function(mySys, applyLabels = TRUE){
  myTempMat<- matrix(unlist(mySys$gBMatrices), ncol = mySys$gNDim, byrow = TRUE)
  myTempDf<- as.data.frame(myTempMat)
  names(myTempDf)<- paste0(rep("D", mySys$gNDim), c(1:mySys$gNDim))
  if(isTRUE(applyLabels)){
    if(length(mySys$gLabels) == 0){
      return(myTempDf)
    } else {
      for(i in seq_along(length(mySys$gLabels))){
        if(mySys$gLabels[[i]]$VarNum == 0 & mySys$gLabels[[i]]$VarType == 0){ # these are item labels
          # do something
          if(length(mySys$gLabels[[i]]$code) > length(unlist(mySys$gItemListByD))){ # too many labels!
            return(myTempDf)
          }
        } else {
          return(myTempDf)
        }
      }
    }
    tmpList<- list() # list to put steps into
    myDimLabs<- sysToDimLabels(mySys, myWarn = FALSE)
    names(myTempDf)<- paste0(names(myTempDf), "_", myDimLabs$Label)
    myItemLabs<- sysToItemLabels(mySys, myWarn = FALSE)
    myItemLabsExp<- myItemLabs[rep(seq_len(nrow(myItemLabs)), unlist(mySys$gItemSteps)), ] # this expands the DF of item labels by each element in gItemSteps
    myTempDf$ItemCode<- as.numeric(as.character(myItemLabsExp$Code))
    for(i in seq(unlist(mySys$gItemSteps))){
      tmpList[[i]]<- seq(unlist(mySys$gItemSteps)[i])
    }
    myTempDf$ItemStep<- unlist(tmpList)
    myTempDf$ItemLabel<- myItemLabsExp$Label
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
sysToDimLabels<- function(mySys, myWarn = TRUE){
  tmpFlag<- FALSE
  # cycle through labels - if there are DIM labs, set flag to true
  for(i in seq_len(length(mySys$gLabels))){
    if(mySys$gLabels[[i]]$VarType == 2){
      tmpFlag<- TRUE
    }
  }
  # if there are labels, go find them and make DF
  if(isTRUE(tmpFlag)){
    for(i in seq_len(length(mySys$gLabels))){
      if(mySys$gLabels[[i]]$VarType == 2){
        myTmpMat<- matrix(unlist(mySys$gLabels[[i]][c("Code", "Label")]), ncol = 2)
        myTmpDf<- as.data.frame(myTmpMat)
        names(myTmpDf)<- c("Code", "Label")
        return(myTmpDf)
      }
    }
  } else {
    if(isTRUE(myWarn)) warning("This system file does not contain labels for dimensions, labels set to NA")
    myTmpDf<- data.frame(
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
sysToItemLabels<- function(mySys, myWarn = TRUE){
  tmpFlag<- FALSE
  # cycle through labels - if there are ITEM labs, set flag to true
  for(i in seq_len(length(mySys$gLabels))){
    if(mySys$gLabels[[i]]$VarType == 0){
      tmpFlag<- TRUE
    }
  }
  # if there are labels, go find them and make DF
  if(isTRUE(tmpFlag)){
    for(i in seq_len(length(mySys$gLabels))){
      if(mySys$gLabels[[i]]$VarType == 0){
        myTmpMat<- matrix(unlist(mySys$gLabels[[i]][c("Code", "Label")]), ncol = 2)
        myTmpDf<- as.data.frame(myTmpMat)
        names(myTmpDf)<- c("Code", "Label")
        return(myTmpDf)
      }
    }
  } else {
    if(isTRUE(myWarn)) warning("This system file does not contain labels for items, labels set to NA")
    myTmpDf<- data.frame(
      Code = seq_len(mySys$gNDim),
      Label = NA
    )
    return(myTmpDf)
  }
}






#
# wright maps from SYS files
#

#' @title plotItemMap
#'
#' @description Creates a plot (ggplot2 object) of item parameter estimates and abilities on latent trait.
#'     Note this is not for use with `rout` files. See the method method plotRout.itemMap to the generic function `plotRout`
#'
#' @param mySys An 'ACER ConQuest' system file object created using the conquestr::ConQuestSys function.
#' @param myDims A string specifying which specific dimensions should be included. The default is "D1", Specific dimensions are specified by the label "D1" for dimensions 1 etc.
#'     Alternatively, you can specify myDims = "all", though what this produces is not currenlty supported.
#' @param ... Optional arguments, mostly for debugging, e.g., `setDebug = TRUE` will print temporary data frames.
#' @return A ggplot2 object.
#' @examples
#' mySys1<- ConQuestSys()
#' myItemMap<- plotItemMap(mySys1, myDims = "all")
#' \dontrun{
#' # if you run the above example you will have the plot in the object `myItemMap`.
#' plot(myItemMap)
#' }
plotItemMap<- function(mySys, myDims = "D1", ...){
  # for debug
  myDebug<- FALSE
  setDebug<- FALSE
  if(hasArg(setDebug)){
    myArgs<- c(...) # have to get the optional arguments first!
    myDebug<- myArgs["setDebug"]
  }

  if(!mySys$gPlausibleExist) stop("You must have generated PVs in ConQuest to plot, try, `estimate ! ...abilities = yes ...;`")
  # get B matrix
  myTmpB<- sysToBMatrixDf(mySys)
  if(!(myDims == "all")){
    # which item codes in this DIM?
    myBMatrixThisDim<- myTmpB[ , grep(paste0(myDims, "|ItemCode"), names(myTmpB)), drop = FALSE] # get 2 cols, "myDims... and ItemCode"
    myItemsThisDim<- myBMatrixThisDim[myBMatrixThisDim[1] > 0 , 2]
    myTmpB<- subset(myTmpB, myTmpB$ItemCode %in% myItemsThisDim)
  }
  # get xsi
  myTmpXsi<- mySys$gXsi
  if(mySys$gLConstraint == 1){
    # only keep difficulty parms
    myTmpXsi<- matrix(myTmpXsi[seq_along(1:(mySys$gNGins-1))])
    # handle item constraint
    myTmpXsi[length(myTmpXsi)+1]<- (-1)*sum(myTmpXsi)
  } else {
    myTmpXsi<- matrix(myTmpXsi[seq_along(1:mySys$gNGins)])
  }
  myTmpXsiDf<- data.frame(
    itemCode = seq_along(myTmpXsi),
    xsi = myTmpXsi
  )
  if(myDebug) print (myTmpXsiDf)
  # merge B and xsi (because these are generalised items, each item can only have one xsi)
  myTmpXsiDf<- merge(myTmpB, myTmpXsiDf, by.x = "ItemCode", by.y = "itemCode", all.x = TRUE)
  # retain one row per Gin
  myTmpXsiDf<- myTmpXsiDf[!duplicated(myTmpXsiDf$ItemCode) ,]
  if(myDebug) print (myTmpXsiDf)
  # get abilities (prefer PV, then WLE, then none)
  myTmpAbil<- createDfFromSys(mySys)

  myPlot<- ggplot2::ggplot(myTmpAbil, ggplot2::aes(x = eval(parse(text = paste0(".data$PV1_", myDims))))) +
    ggplot2::geom_density() +
    ggplot2::theme_bw() +
    ggplot2::geom_point(data = myTmpXsiDf, ggplot2::aes(x = .data$xsi, y = 0), fill = "red", shape = 21, alpha = 0.5, size = 3) +
    ggrepel::geom_text_repel(data = myTmpXsiDf, ggplot2::aes(x = .data$xsi, y = 0, label = .data$ItemLabel), # TODO, test for no labels and use gin instead. consider above and below dodge for many items
                             nudge_y = -0.1,
                             direction = "y",
                             # angle = 90, # angle of text
                             vjust = 0,
                             segment.size = 0.2,
                             ylim = c(NA, -0.1)
    ) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::coord_flip(ylim = c(-0.2, 0.6)) + # use this instead of coord_cartesian - this is a wrapper for coord_cartesian and hence would overwrite the call to coord_cartesian
    #ggplot2::theme(axis.line.y = element_line(arrow = arrow(angle = 15, length = unit(0.25, "cm"), ends = "both", type = "closed"), size = 0.4, colour = "grey20")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")

  return(myPlot)
}




# testy123<- plotItemMap(ConQuestSys(), "all", setDebug = TRUE)
