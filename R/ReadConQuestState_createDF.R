#' @title createDfFromSys
#'
#' @description Read an R object of class ConQuestSys and create neat R data frame object.
#'
#' @param mySys An R object of class ConQuestSys, returned by the function conquestr::ConQuestSys
#' @return A data frame containing R data frames based on the list objects in the ConQuest system file that has been read in.
#' @seealso conquestr::ConQuestSys()
#' @importFrom stats reshape
createDfFromSys<-function(mySys){

  # insert code to check that this list is also of class ConQuestSys
  # if(!(#pseudocde#ConQuestSys))
  #   stop("you must pass this funtion a valid ConQuestSys object")

  # cast PID lookup table to df
  # NOTE IF PID in not used in CQ format statemement then gPIDLookUp is empty
  if(!length(mySys$gPIDLookUp) == 0){
    gPIDLookUpDf<- data.frame(matrix(unlist(mySys$gPIDLookUp), ncol = 1), stringsAsFactors = FALSE)
    names(gPIDLookUpDf)<- c("pid")
    gPIDLookUpDf$seqNum<- c(1:length(mySys$gPIDLookUp))
    # gPIDLookUpDfGlobal<<- gPIDLookUpDf ## debug
  } else {
    # PID was not used in CQ datafile/format statement
    gPIDLookUpDf<-data.frame(pid = c(1:mySys$gNCases), seqNum = c(1:mySys$gNCases))
  }

  # cast response data to df
  ncolgResponseData<- length(names(unlist(mySys$gResponseData[1])))
  tempNames_gResponseData<- names(unlist(mySys$gResponseData[1]))
  gResponseDataDf<- data.frame(matrix(unlist(mySys$gResponseData), ncol = ncolgResponseData, byrow = TRUE), stringsAsFactors = FALSE)
  names(gResponseDataDf)<- tempNames_gResponseData
  # sort gResponseDataDf to get items in order when we cast to wide
  gResponseDataDf<- gResponseDataDf[order(gResponseDataDf$Item), ]
  # ensure where response flag is 0, response is NA
  gResponseDataDf$Rsp[gResponseDataDf$RspFlag == 0]<- NA
  # cast gResponseDataDf from long to wide
  # this is my clumsy way of spitting a meaningful warning when there are dupe PIDs and there are not unqiue combos of PID and GIN in the long data
  gResponseDataDf<- tryCatch(
    #try this
    reshape(gResponseDataDf, timevar = "Item", idvar = "Pid", direction = "wide"),
    # if there's a wanring, handle it like this
    warning = function(w) {
      print("converting gResponseData from long to wide format has thrown a warning. This is usually caused by duplicate PIDs in the response data.")
      (reshape(gResponseDataDf, timevar = "Item", idvar = "Pid", direction = "wide"))
    },
    # finally, do this
    finally = { } # dont need anything here as reshape will always return the gResponseDataDf object in the earlier steps
  )
  # cast gAllCaseEstimates to df
  # can use gNDim and gNPlausibles to make the naming work (e.g., there will be gNDim*gNPlausibles PV columns to add)
  ncolgAllCaseEstimates<- length(names(unlist(mySys$gAllCaseEstimates[1])))
  tempNames_gAllCaseEstimatesDf<- names(unlist(mySys$gAllCaseEstimates[1]))
  # rename PVs
  tempNames_gAllCaseEstimatesDf[grep("pv", tempNames_gAllCaseEstimatesDf)]<- paste0(rep("PV", mySys$gNDim*mySys$gNPlausibles), rep(1:mySys$gNPlausibles, each = mySys$gNDim), "_D", rep(1:mySys$gNDim, mySys$gNPlausibles))
  # rename other vars
  #TODO, read in names for mle wle etc var covar
  gAllCaseEstimatesDf<- data.frame(matrix(unlist(mySys$gAllCaseEstimates), ncol = ncolgAllCaseEstimates, byrow = TRUE), stringsAsFactors = FALSE)
  names(gAllCaseEstimatesDf)<- tempNames_gAllCaseEstimatesDf

  myConQuestData<- gPIDLookUpDf
  myConQuestData<- merge(myConQuestData, gResponseDataDf, by.x = "seqNum", by.y ="Pid", all.x = TRUE) # merge response data on PID lookup, this gives us the right link between seqNum and PID
  # myConQuestData<- merge(myConQuestData, gGroupDataDf, by.x = "seqNum", by.y ="CaseNum", all.x = TRUE) # merge group data on response data, there will always be at least 1 vector of group vars (can be all NA)
  # cant currently gYDataDf - see Issue 3 - gYData does not contain either pid or seqnum
  myConQuestData<- merge(myConQuestData, gAllCaseEstimatesDf, by.x = "seqNum", by.y ="pid", all.x = TRUE) # merge estimates on response data (note some cases could be missing from gAllCaseEstimatesDf IF they are missing all repsonse data and are missing regressor data - e.g., missing regressors result in deletion)
  myConQuestData<- replaceInDataFrame(myConQuestData, -1.797693e+308, NA)


  # # put all the stuff into a list
  # systemFileDf<-list(
  #   #...
  #   # check 9
  #   # gYDataDf = gYDataDf
  # )

  # return the list with all the stuff in it
  return(myConQuestData)

}

# TODO: function to return matrix sampler
#
#
#   # make nice DF out of matrix sampler  matricies iF they exist
#   if(any(grep("_raw|_fit", names(ReadSysList$gMatrixList))))
#   {
#     # get user defined prefix
#     myMatrixoutPrefix<- strsplit(grep("_raw|_fit", names(ReadSysList$gMatrixList), value = TRUE)[1], split = "_")[[1]][1]
#
#     matrixSampler_fit<- as.data.frame(eval(parse(text=paste0("ReadSysList$gMatrixList$", myMatrixoutPrefix, "_fit"))))
#     matrixSampler_raw<- as.data.frame(eval(parse(text=paste0("ReadSysList$gMatrixList$", myMatrixoutPrefix, "_raw"))))
#
#     # add to system file
#     systemFile[["matrixSampler_fit"]]<- matrixSampler_fit
#     systemFile[["matrixSampler_raw"]]<- matrixSampler_raw
#   }
#
#   # make nice DF out of item fit to use with matrix sampler matricies iF they exist
#   if(any(grep("_userfit", names(ReadSysList$gMatrixList))))
#   {
#     # get user defined prefix
#     myMatrixoutPrefix<- strsplit(grep("_userfit", names(ReadSysList$gMatrixList), value = TRUE)[1], split = "_")[[1]][1]
#
#     matrix_userfit<- as.data.frame(eval(parse(text=paste0("ReadSysList$gMatrixList$", myMatrixoutPrefix, "_userfit"))))
#     matrix_userfit$gin<- c(1:ReadSysList$gNGins)
#     # add to system file
#     systemFile[["matrix_userfit"]]<- matrix_userfit
#   }
#
#   return(systemFile)
#
# }
