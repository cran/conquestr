utils::globalVariables("where") # "where" is not exported by tidyselect

#' @title getCqItanal
#'
#' @description helper function to return list of lists, each list relates to one
#'   generalised item from an ACER ConQuest `itanal` output.
#'   Each list contains: (1) item-total and item-rest correlations ....
#'
#' @param sysFile An ACER ConQuest system file.
#' @param matrixPrefix to define which itanal to process
#' @param isDebug report debug output
#' @return A list.
#' @examples
#' myItanal <- getCqItanal()
#' print(myItanal[[1]])
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom methods is
#' @importFrom tidyselect everything
getCqItanal <- function(sysFile, matrixPrefix = "", isDebug = FALSE) {

  #
  # TODO - option to select suffixes (group labels)
  # TODO - handle multidim models to ensure tables have correct rows
  #

  if (!missing(isDebug))
  {
    if (!is.logical(isDebug)) stop("isDebug must be a boolean")
  }
  myDebug <- isDebug
  # test if system file is passed in
  if (missing(sysFile))
  {
    sysFile <- conquestr::ConQuestSys()
    warning(
      "No argument to `sysFile` provided. Using default
      (inbuilt) system file instead"
    )
  }
  if (!is(sysFile, "conQuestSysFile"))
  {
    stop(
      "sysFile is not a valid ACER ConQuest system
      file read-in to R using `conquestr::ConQuestSys`"
    )
  }
  if (!any(grepl("_ptbis$", names(sysFile$gMatrixList))))
  {
    stop(
      "The sysFile you have provided does not have `itanal`
       matrix objects in it. You must use the `matrixout`
      option to the command itanal in ACER ConQuest"
    )
  }
  if (length(grep("_counts$", names(sysFile$gMatrixList))) > 1)
  {
    manyItan <- TRUE
  } else {
    manyItan <- FALSE
  }
  if (manyItan & matrixPrefix == "")
  {
    stop(
      "The sysFile you have provided has more than one itanal
      in it. Please specify which itanal to use with the argument
      matrixPrefix"
    )
  }
  if (!missing(matrixPrefix))
  {
    if (!is.character(matrixPrefix)) stop("matrixPrefix must be a string")
    if (
      !any(
        grep(
          paste0("^", matrixPrefix, "_"),
          names(sysFile$gMatrixList)
        )
      )
    )
    {
      stop("matrixPrefix must be a prefix used in matrixout option to itanal")
    }
  }

  # data to work on
  tmp_ptbis <- sysFile$gMatrixList[[grep(paste0(matrixPrefix, "_ptbis$"), names(sysFile$gMatrixList))]]
  tmp_abilitymeansd <- sysFile$gMatrixList[[grep(paste0(matrixPrefix, "_abilitymeansd$"), names(sysFile$gMatrixList))]]
  tmp_counts <- sysFile$gMatrixList[[grep(paste0(matrixPrefix, "_counts$"), names(sysFile$gMatrixList))]]
  tmp_itemRest <- sysFile$gMatrixList[[grep(paste0(matrixPrefix, "_itemstats$"), names(sysFile$gMatrixList))]]
  tmp_summary <- sysFile$gMatrixList[[grep(paste0(matrixPrefix, "_summarystats$"), names(sysFile$gMatrixList))]]
  if (myDebug) print("read in itanal matrix objects to work on")

  # keys, scores
  tmpKeys <- getCqKeys(sysFile)
  tmpScores <- getCqScores(sysFile)
  if (myDebug)
  {
    print("got keys: ")
    print(tmpKeys)
    print("got scores: ")
    print(tmpScores)
  }

  # gin labels
  tmpGinLabs <- unlist(sysFile$gGinLongLabels)
  if (myDebug) {
    print("got gin labels: ")
    print(tmpGinLabs)
  }

  # response category names - used as first column in tables
  tmpRespCatNames <- matrix(gsub("(\\w+_)", "", names(as.data.frame(tmp_counts))), ncol = 1)
  # note if there are recodes, need to remove them from the possible cat names
  toDrop <- unlist(sysFile$gRecodes)[names(unlist(sysFile$gRecodes)) == "Before"]
  if (length(toDrop) > 0) {
    tmpRespCatNames <- tmpRespCatNames[!(tmpRespCatNames %in% toDrop)]
  }
  if (myDebug)
  {
    print("got resp cat names")
    print(tmpRespCatNames)
    print(tmp_counts)
  }

  # results
  tmpRes <- list()

  # work through itan_ptbis, itan_abilitymeansd, itan_counts
  for (i in seq(nrow(tmp_counts))) {
    tmp_one_abilitymeansd <- as.data.frame(t(tmp_abilitymeansd[i, ]))
    tmp_one_ptbis <- as.data.frame(t(tmp_ptbis[i, ]))
    tmp_one_counts <- as.data.frame(t(tmp_counts[i, ]))
    if (myDebug) print(paste0("read in transpose of pvmean, ptbis, and counts for item: ", i))
    # to wider
    tmp_one_abilitymeansd <- tmp_one_abilitymeansd %>% pivot_longer(
      cols = everything(),
      names_pattern = "\\w+_(\\w+_\\w+)",
      names_to = c(".value")
    )
    #if (myDebug) print(paste0("pivot_longer pvmean item: ", i))
    #if (myDebug) print(tmp_one_abilitymeansd)

    tmp_one_ptbis <- tmp_one_ptbis %>% pivot_longer(
      cols = everything(),
      names_pattern = "(\\w+)_\\s*\\w+", # there can be leading spaces in the name, e.g., "ptbis_ B"
      names_to = c(".value")
    )
    if (myDebug) print(paste0("pivot_longer ptbis item: ", i))
    if (myDebug) print(tmp_one_ptbis)

    tmp_one_counts <- tmp_one_counts %>% pivot_longer(
      cols = everything(),
      names_pattern = "(\\w+)_\\s*\\w+", # there can be leading spaces in the name, e.g., "count_ 1"
      names_to = c(".value")
    )

    #if (myDebug) print(paste0("pivot_longer counts item: ", i))
    #if (myDebug) print(tmp_one_counts)

    if (myDebug)
    {
      print("tmpRespCatNames")
      print(tmpRespCatNames)
      print("tmp_one_counts")
      print(tmp_one_counts)
      print("tmp_one_ptbis")
      print(tmp_one_ptbis)
      print("tmp_one_abilitymeansd")
      print(tmp_one_abilitymeansd)
    }

    oneResult <- cbind(tmpRespCatNames, tmp_one_counts, tmp_one_ptbis, tmp_one_abilitymeansd)
    oneResult <- replaceInDataFrame(oneResult, -1.797693e+308, NA)
    if (any(oneResult$count == 0)) oneResult$ptbis[oneResult$count == 0] <- NA

    #if (myDebug) print(oneResult)

    # get map of gins to dim
    for (j in seq(length(sysFile$gGeneraliseditemList_D))) {
      tmpGinList <- unlist(sysFile$gGeneraliseditemList_D[[j]]) + 1
      tmpDf <- data.frame(
        dim = rep(j, length(tmpGinList)),
        gin = tmpGinList
      )
      if (j == 1) {
        dimToGinMap <- tmpDf
      } else {
         dimToGinMap <- rbind(dimToGinMap, tmpDf)
      }
    }

    # if there is a key, it is applied after score statements - so no need to do both.
    if (length(tmpKeys) > 0)
    {
      oneResult <- merge(
        oneResult,
        tmpKeys[tmpKeys$gin == i, grep("cat|score", names(tmpKeys))],
        by.x = "tmpRespCatNames", by.y = "cat", all.x = TRUE
      )
      oneResult$score[is.na(oneResult$score)] <- sysFile$gKeyDefault
      oneResult$score[is.na(oneResult$ptbis)] <- NA
    } else if (length(tmpScores) > 0)
    {
      if (i %in% tmpScores$gin)
      {
        # get the rows in tmpScores that relate to that gin AND the right dim (dimToGinMap)
        tmpDimMap <- dimToGinMap[dimToGinMap$gin == i, ]
        if (length(tmpDimMap[, 1]) != 1) {
          dimToGinMap <<- dimToGinMap
          print("`dimToGinMap` now available in global scope")
          stop("possible within item design (not supported yet)?\n
          Or this item could not be found on a dimension")
        }
        thisGinDim <- as.numeric(tmpDimMap$dim)

        if (myDebug)
        {
            print("mapping of gin to dim: ")
            print(tmpDimMap)
            print("this dim (thisGinDim): ")
            print(thisGinDim)
        }

        oneResult <- merge(
          oneResult,
          tmpScores[tmpScores$gin == i & tmpScores$dim == thisGinDim, grep("before|after", names(tmpScores))],
          by.x = "tmpRespCatNames", by.y = "before", all.x = TRUE
        )
        names(oneResult)[grep("after", names(oneResult))] <- "score"
      } else
      {
        # there was no score for this gin, use default cat labels as scores.
        oneResult$score <- oneResult$tmpRespCatNames
        oneResult$score[is.na(oneResult$ptbis)] <- NA
      }

    # check for 2PL and multiply score by tau.
    # Need to check how Bock model works - is gTau a list of lists, one per gin?

    } else
    {
      oneResult$score <- as.numeric(oneResult$tmpRespCatNames) # no keys or scores, just use tmpRespCatNames
    }

    oneResult$prop <- oneResult$count / sum(oneResult$count) * 100

    # order and rename
    oneResult <- orderItanalCols(oneResult)

    # is the key included blank/dummy cats ("x"), Score will be NA
    oneResult <- oneResult[!is.na(oneResult$Score), ]

    if (myDebug) {
      print("finished with this gin: ")
      print(oneResult)
    }

    # place table and item-rest in final object
    tmpRes[[i]] <- list()
    tmpRes[[i]]["name"] <- tmpGinLabs[i]
    if (myDebug) {
      print(paste0("adding gin, ", i, " to result list"))
    }
    tmpRes[[i]][["table"]] <- oneResult
    tmpRes[[i]][["item_rest_total"]] <- tmp_itemRest[i, ]
  }

  # add on summarystats
  tmp_summary <- as.data.frame(tmp_summary)
  tmp_summary <- replaceInDataFrame(tmp_summary,  -1.797693e+308, NA)
  tmpRes[["summary_stats"]] <- tmp_summary

  class(tmpRes) <- append(class(tmpRes), "cqItanal")
  return(tmpRes)

}

#' @title fmtCqItanal
#'
#' @description helper function to produce nicely formatted summary tables from
#' a ConQuest Itanal.
#'
#' @param cqItanal An ACER ConQuest itanal list object returned by function `getCqItanal`.
#' @param itemNumber a vector of generalised item numbers to format.
#' @param ptBisFlag Something.
#' @param textColHighlight Something.
#' @param valueDecPlace Something.
#' @return A list
#' @examples
#' myEx1Sys <- ConQuestSys()
#' myEx1Sys_itanal <- getCqItanal(myEx1Sys)
#' myItanalSummary <- fmtCqItanal(myEx1Sys_itanal)
#' print(myItanalSummary[[1]])
#' @importFrom magrittr %>%
#' @importFrom methods is
fmtCqItanal <- function(cqItanal, itemNumber = "all", ptBisFlag = 0, textColHighlight = "red", valueDecPlace = 2) {
  # test if List passed in is ConQuest Itanal object
  if (!is(cqItanal, "cqItanal"))
  {
    stop(
      paste0("itanal object was not created using
      `conquestr::getCqItanal`"
      )
    )
  }

  # get items to work on
  if (itemNumber == "all") itemNumber <- 1:(length(cqItanal)-1)
  # test if itemNumber within range of list length - 1
  gins <- seq(length(cqItanal) - 1)
  if (any(!itemNumber %in% gins))
  {
    stop("One of the specified item numbers is out of range")
  }

  #
  #
  # TODO...test for valid ptBisFlag, textColHighlight, and valueDecPlace

  # we will work on this and return it at the end
  tmpCqItanal <- cqItanal

  # call fmtCqItanalSmry
  # check multidim - do we we get many alphas etc?
  tmpCqItanalSmry <- tmpCqItanal[[length(tmpCqItanal)]]
  thisCqItanalSmry <- fmtCqItanalSmry(tmpCqItanalSmry, valueDecPlace = valueDecPlace)
  tmpCqItanal[[length(tmpCqItanal)]] <- thisCqItanalSmry

  #loop through items in itemNumber and replace in tmpCqItanal
  for (item in itemNumber)
  {
    # loop through itemNumber
    # call fmtCqItanalTbl
    tmpItemTbl <- tmpCqItanal[[item]]$table
    tmpItemName <- tmpCqItanal[[item]]$name
    tmpItemTblFmt <- fmtCqItanalTbl(tmpItemTbl, ptBisFlag, textColHighlight, valueDecPlace, tmpItemName)
    tmpCqItanal[[item]]$table <- tmpItemTblFmt
  }

  return(tmpCqItanal)
}


# function to make pretty itanal category table...

#' @title fmtCqItanalTbl
#'
#' @description internal function to produce formatted itanal category statistics tables for a generalised item.
#' Note that this function is called by `fmtCqItanal` and would not usually be called by the user.
#' The returned table uses the library kable and will be formatted
#' based on the users settings.
#'
#' @param cqItanalTbl A single table in a cqItanal object (an element of type data.frame in a list of class "cqItanal".
#' @return A data frame.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom kableExtra cell_spec kable_styling
#' @importFrom dplyr across
fmtCqItanalTbl <- function(cqItanalTbl, ptBisFlag, textColHighlight, valueDecPlace, itemName) {

  # deals with "no visible binding for global variable" warning
  `Pt Bis` <- Score <- NULL

  # remove item categories that do not have an associated score
  tmpTable <- cqItanalTbl[!is.na(cqItanalTbl$`Pt Bis`), ]

  # based on Vernon's mutate and kableExtra approach so far - want to be less dependent on these packages?
  thisItemFormatted <- tmpTable %>%
    dplyr::mutate(
       across(where(is.double), ~ round(., valueDecPlace)),
       `Pt Bis` = kableExtra::cell_spec(
         `Pt Bis`,
         color = ifelse(
          !is.na(`Pt Bis`) & (`Pt Bis` > ptBisFlag) & (Score == 0),
          textColHighlight,
          "black"
          )
        )
      ) %>%
     knitr::kable(escape = FALSE, row.names = FALSE, align = "c",
                  caption = paste0("Item category statistics for: ", itemName)) %>%
     kableExtra::kable_styling()

    return(thisItemFormatted)
}


#' @title fmtCqItanalSmry
#'
#' @description internal function to produce formatted itanal summary statistics seen at the end of a ConQuest itanal.
#' Note that this function is called by `fmtCqItanal` and would not usually be called by the user.
#' The returned table uses the library kable and will be formatted
#' based on the users settings.
#'
#' @param cqItanalSmry A single table in a cqItanal object (an element of type data.frame in a list of class "cqItanal".
#' @return A data frame.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom kableExtra cell_spec
fmtCqItanalSmry <- function(cqItanalSmry, valueDecPlace) {

  tmpItanalSmry <- as.data.frame(t(cqItanalSmry))

  # the row names make a useful column
  itanalSummary <- cbind(rownames(tmpItanalSmry), data.frame(tmpItanalSmry, row.names=NULL))

  # choose generic colnames (what does the summary look like when nDim > 1)
  colnames(itanalSummary) <- c('Statistic', 'Value')

  # with minimal formatting
  itanalSummaryFormatted <- itanalSummary %>%
    dplyr::mutate(across(where(is.double), ~ round(., valueDecPlace))) %>%
    knitr::kable(escape = FALSE, row.names = FALSE,
                 caption = "Item Analysis Summary Statistics", label = "testSummary")

  # needs a return
  return(itanalSummaryFormatted)
}


#' @title getCqKeys
#'
#' @description internal function to return keys associated with item categories in itanal tables.
#'
#' @param sysFile An ACER ConQuest system file.
#' @return A data frame.
#' @keywords internal
#' @importFrom methods is
getCqKeys <- function(sysFile) {
  if (!is(sysFile, "conQuestSysFile"))
  {
    stop(
      "sysFile is not a valid ACER ConQuest system
      file read-in to R using `conquestr::ConQuestSys`"
    )
  }
  gKeyDfs <- list()
  keyL <- length(sysFile$gKeys)
  if (keyL > 0)
  {
    for (i in seq(keyL))
    {
      # for each key produce an item(gin) * key df
      tmpCat <- unlist(sysFile$gKeys[[i]][[1]]) # which cat gets this key?
      tmpKey <- unlist(sysFile$gKeys[[i]][[2]]) # what is the key score?
      gKeyDfs[[i]] <- data.frame(
        gin = 1:sysFile$gNGins,
        cat = tmpCat, # length = nGins
        score = tmpKey # recycled nGin times
      )
    }
    for (i in seq(length(gKeyDfs)))
    {
      if (i == 1)
      {
        gKeyDf <- gKeyDfs[[i]]
      } else
      {
        gKeyDf <- rbind(gKeyDf, gKeyDfs[[i]])
      }
    }
  } else
  {
    gKeyDf <- data.frame()
  }
  #gKeyDf <- gKeyDf[-grep("\\s*x$", gKeyDf$cat) , ] # drop any cat-key pairs where the category is an "x" (can have leading spaces) - this doesnt work, and is not needed
  return(gKeyDf)
}


#' @title getCqScores
#'
#' @description internal function to return scores associated with item categories in itanal tables.
#'
#' @param sysFile An ACER ConQuest system file.
#' @return A data frame.
#' @keywords internal
#' @importFrom methods is
getCqScores <- function(sysFile) {
  if (!is(sysFile, "conQuestSysFile"))
  {
    stop(
      "sysFile is not a valid ACER ConQuest system
      file read-in to R using `conquestr::ConQuestSys`"
    )
  }

  scoresL <- length(sysFile$gScores)
  if (scoresL > 0) # are there any score statements? # note usually, but not always equal to gNDim
  {
    for (i in seq(scoresL)) # loop through scores
    {
      tmpBefore <- unlist(sysFile$gScores[[i]][[1]])
      tmpAfter <- unlist(sysFile$gScores[[i]][[2]])
      if (is.null(sysFile$gScores[[i]][[3]])) # if cList is NULL then applies to all gins
      {
        #print("cList is NULL")
        tmpGins <- 1:sysFile$gNGins
      } else
      {
        tmpGins <- as.numeric(unlist(sysFile$gScores[[i]][[3]][[1]]["S"]))
      }
      for (j in seq(tmpGins))
      {
        tmpScores <- data.frame(
          before = tmpBefore,
          after = tmpAfter,
          gin = tmpGins[j],
          dim = rep(seq(sysFile$gNDim), each = length(tmpBefore))
        )
        if (i == 1 & j == 1) # first time through, create a df
        {
          scoredDf <- tmpScores
        } else # after the first iteration, append to scoredDf
        {
          scoredDf <- rbind(scoredDf, tmpScores)
        }
      }
    }
  } else
  {
    scoredDf <- data.frame() # no scores, return empty df
  }
  return(scoredDf)
}




#' @title orderItanalCols
#'
#' @description internal function to order and rename itanal tables.
#' Called by conquestr::fmtCqItanalTbl
#' Safely orders and renames tables when ndim > 1.
#'
#' @param itanalTbl An itanal table being worked on by fmtCqItanalTbl.
#' @return A data frame.
#' @keywords internal
orderItanalCols <- function(itanalTbl) {

  # ndims is implied by length of names
  ndims <- (length(names(itanalTbl))-5)/2

  # order
  myOrder <- c(
    grep("^tmpRespCatNames", names(itanalTbl)), #'Category'
    grep("^score", names(itanalTbl)), # Score'
    grep("^count", names(itanalTbl)), # Count'
    grep("^prop", names(itanalTbl)), # 'Percent'
    grep("^ptbis", names(itanalTbl)), # 'Pt Bis'
    grep("^t$", names(itanalTbl)), # 'Pt Bis t'
    grep("^p$", names(itanalTbl)) # 'Pt Bis p'
  )

  for (i in seq(ndims))
  {
    myOrder <- c(myOrder, grep(paste0("^mean_", i), names(itanalTbl)))
    myOrder <- c(myOrder, grep(paste0("^sd_", i), names(itanalTbl)))
  }

  itanalTbl <- itanalTbl[ , myOrder]

  # rename cols
  names(itanalTbl) <- gsub("^tmpRespCatNames", "Category", names(itanalTbl))
  names(itanalTbl) <- gsub("^score", "Score", names(itanalTbl))
  names(itanalTbl) <- gsub("^count", "Count", names(itanalTbl))
  names(itanalTbl) <- gsub("^prop", "Percent", names(itanalTbl))
  names(itanalTbl) <- gsub("^ptbis", "Pt Bis", names(itanalTbl))
  names(itanalTbl) <- gsub("^t$", "Pt Bis t stat.", names(itanalTbl))
  names(itanalTbl) <- gsub("^p$", "Pt Bis sig.", names(itanalTbl))
  names(itanalTbl) <- gsub("^mean_(\\d+)", "Ability mean (D\\1)", names(itanalTbl))
  names(itanalTbl) <- gsub("^sd_(\\d+)", "Ability SD (D\\1)", names(itanalTbl))


  return(itanalTbl)

}


#' @title getCqItanalFacility
#'
#' @description returns an item facility for each item in itanal object created
#'   by ACER ConQuest.
#'   For a dichotomously scored Rasch-like item, facility is the percent correct.
#'   For a polytomously scored item, or with estimated scores, facility is given by:
#'   the sum of the number of cases in each response category, multiplied by the score for that category
#'   divided by the sum of all cases responding to the items times the maximum score for the item.
#'
#' @param itan A list of class "cqItanal" created by `conquestr::getCqItanal()`
#' @return A list.
#'
#' @examples
#' mySys <- ConQuestSys()
#' myItan <- getCqItanal(mySys)
#' getCqItanalFacility(myItan)
getCqItanalFacility <- function(itan) {

  if (!is(itan, "cqItanal")) stop("`itan` must be an object returned by `conquestr::getCqItanal()`")

  facilityList <- list()

  myNItems <- length(itan) - 1 #itan has an extra element at the end with descriptive stats

  for (i in seq_len(myNItems)) {
    tName <- itan[[i]]$name
    tScore <- as.numeric(itan[[i]]$table$Score)
    tCount <- as.numeric(itan[[i]]$table$Count)

    nN <- sum(tCount)
    tFac <- (tScore * tCount) / (nN * max(tScore))
    tFac <- sum(tFac) * 100

    facilityList[[i]] <- tFac
    names(facilityList)[i] <- tName
  }

return(facilityList)

}


#' @title getCqItanalSummary
#'
#' @description returns an itanal as a data frame in summary format:
#'   one row per generalised item with:
#'
#'   - item label
#'   - valid N
#'   - facility (see `conquestr::getCqItanalFacility`)
#'   - item-rest correlation
#'   - item-total correlation
#'   - fit (infit/weighted MNSQ) if available
#'   - item locations (deltas)
#'
#' @param itan A list of class "cqItanal" created by `conquestr::getCqItanal()`
#' @return A data frame.
#'
#' @examples
#' mySys <- ConQuestSys()
#' myItan <- getCqItanal(mySys)
#' getCqItanalSummary(myItan)
getCqItanalSummary <- function(itan) {

  if (!is(itan, "cqItanal")) stop("`itan` must be an object returned by `conquestr::getCqItanal()`")

  # tmp list for results
  sumList <- list()

  # get facilities
  tmpFacil <- getCqItanalFacility(itan)

  myNItems <- length(itan) - 1 #itan has an extra element at the end with descriptive stats

  for (i in seq_len(myNItems)) {
    sumList[[i]] <- list()

    itemLab <- as.character(itan[[i]]$name)
    itemN <- as.numeric(sum(itan[[i]]$table$Count, na.rm = TRUE))
    itemFacil <- as.numeric(tmpFacil[[i]])
    itemRestCor <- as.numeric(itan[[i]]$item_rest_total[names(itan[[i]]$item_rest_total) == "item-rest"])
    itemTotalCor <- as.numeric(itan[[i]]$item_rest_total[names(itan[[i]]$item_rest_total) == "item-total"])
    itemObsMean <- as.numeric(itan[[i]]$item_rest_total[names(itan[[i]]$item_rest_total) == "obs_mean"])
    itemExpMean <- as.numeric(itan[[i]]$item_rest_total[names(itan[[i]]$item_rest_total) == "exp_mean"])
    itemAdjMean <- as.numeric(itan[[i]]$item_rest_total[names(itan[[i]]$item_rest_total) == "adj_mean"])
    # note that since ConQuest 5.40.0 the item delta dot has been added to this data structure
    if (length(itan[[i]]$item_rest_total) > 5) {
      itemDeltaDot <- as.numeric(itan[[i]]$item_rest_total[names(itan[[i]]$item_rest_total) == "delta_dot"])
    }
    

    sumList[[i]]["item_label"] <- itemLab
    sumList[[i]]["N"] <- itemN
    sumList[[i]]["facility"] <- itemFacil
    sumList[[i]]["item_rest"] <- itemRestCor
    sumList[[i]]["item_total"] <- itemTotalCor
    sumList[[i]]["obs_mean"] <- itemObsMean
    sumList[[i]]["exp_mean"] <- itemExpMean
    sumList[[i]]["adj_mean"] <- itemAdjMean
    if (exists("itemDeltaDot")) sumList[[i]]["delta_dot"] <- itemDeltaDot

    tmpResult <- matrix(unlist(sumList[[i]]), ncol = length(sumList[[i]]))
    if (i == 1) {
      tmpMat <- tmpResult
    } else {
      tmpMat <- rbind(tmpMat, tmpResult)
    }
  }

  summaryDf <- as.data.frame(tmpMat)
  names(summaryDf) <- names(sumList[[1]])

return(summaryDf)

}