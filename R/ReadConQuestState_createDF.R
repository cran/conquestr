#' @importFrom stats reshape


#' @title createDfFromSys
#'
#' @description Internal function to read an R object of class ConQuestSys (which returns a list) and create neat R data frame objects. Called by conquestr::ConQuestSysDf.
#'
#' @param mySys An R object of class ConQuestSys, returned by the function conquestr::ConQuestSys
#' @return A list containing R data frames based on the list objects in the ConQuest system file that has been read in.
#' @seealso conquestr::ConQuestSys()
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
  gAllCaseEstimatesDf<- data.frame(matrix(unlist(mySys$gAllCaseEstimates), ncol = ncolgAllCaseEstimates, byrow = TRUE), stringsAsFactors = FALSE)
  names(gAllCaseEstimatesDf)<- tempNames_gAllCaseEstimatesDf

  myConQuestData<- gPIDLookUpDf
  myConQuestData<- merge(myConQuestData, gResponseDataDf, by.x = "seqNum", by.y ="Pid", all.x = TRUE) # merge response data on PID lookup, this gives us the right link between seqNum and PID
  # myConQuestData<- merge(myConQuestData, gGroupDataDf, by.x = "seqNum", by.y ="CaseNum", all.x = TRUE) # merge group data on response data, there will always be at least 1 vector of group vars (can be all NA)
  # cant currently gYDataDf - see Issue 3 - gYData does not contain either pid or seqnum
  myConQuestData<- merge(myConQuestData, gAllCaseEstimatesDf, by.x = "seqNum", by.y ="pid", all.x = TRUE) # merge estimates on response data (note some cases could be missing from gAllCaseEstimatesDf IF they are missing all repsonse data and are missing regressor data - e.g., missing regressors result in deletion)
  # myConQuestData<- conquestr::zapSystemMissing(myConQuestData) # this takes 25 times longer in native R - belo uses Rcpp
  myConQuestData<- replaceInDataFrame(myConQuestData, -1.797693e+308, NA)


  # # put all the stuff into a list
  # systemFileDf<-list(
  #   #...
  #   # check 9
  #   # gYDataDf = gYDataDf
  # )

  # return the list with all the stuff in it
  return(myConQuestData)
  # put all the stuff into a list
  systemFileDf<-list(
    #...
    # check 9
    # gYDataDf = gYDataDf
  )

  # return the list with all the stuff in it
  return(systemFileDf)

}

# todo: create a way of casting each object in a ConQuestSys file to a neat DF, then return them in a list.
# todo: create a way to call this function in EITHER a call to ConQuestSys() (e.g., readDf = TRUE), or a stand alone function, e.g., ConQuestSysDf(mySys)

#
# castReadSysToDf<-function(ReadSysList){
#
#
#   # cast PID lookup table to df
#   gPIDLookUpDf<-data.frame(matrix(unlist(ReadSysList$gPIDLookUp), ncol = 1))
#   names(gPIDLookUpDf)<- c("pid")
#   gPIDLookUpDf$seqNum<- c(1:length(ReadSysList$gPIDLookUp))
#
#   # cast response data to df
#   ncolgResponseData<- length(names(unlist(ReadSysList$gResponseData[1])))
#   tempNames_gResponseData<- names(unlist(ReadSysList$gResponseData[1]))
#   gResponseDataDf<- data.frame(matrix(unlist(ReadSysList$gResponseData), ncol = ncolgResponseData, byrow = TRUE))
#   names(gResponseDataDf)<- tempNames_gResponseData
#   # sort gResponseDataDf to get items in order when we cast to wide
#   gResponseDataDf<- gResponseDataDf[order(gResponseDataDf$Item), ]
#   # cast gResponseDataDf from long to wide
#     # this is my clumsy way of spitting a meaningful warning when there are dupe PIDs and there are not unqiue combos of PID and GIN in the long data
#   gResponseDataDf<- tryCatch(
#                               #try this
#                               reshape(gResponseDataDf, timevar = "Item", idvar = "Pid", direction = "wide"),
#                               # if there's a wanring, handle it like this
#                               warning = function(w) {
#                                 print("converting gResponseData from long to wide format has thrown a wanring. This is usually caused by duplicate PIDs in the response data.")
#                                 (reshape(gResponseDataDf, timevar = "Item", idvar = "Pid", direction = "wide"))
#                               },
#                               # finally, do this
#                               finally = { } # dont need anything here as reshape will always return the gResponseDataDf object in the earlier steps
#                              )
#
#
#   # cast gAllCaseEstimates to df
#   # can use gNDim and gNPlausibles to make the naming work work (e.g., there will be gNDim*gNPlausibles PV columns to add)
#   ncolgAllCaseEstimates<- length(names(unlist(ReadSysList$gAllCaseEstimates[1])))
#   tempNames_gAllCaseEstimatesDf<- names(unlist(ReadSysList$gAllCaseEstimates[1]))
#   gAllCaseEstimatesDf<- data.frame(matrix(unlist(ReadSysList$gAllCaseEstimates), ncol = ncolgAllCaseEstimates, byrow = TRUE))
#   names(gAllCaseEstimatesDf)<- tempNames_gAllCaseEstimatesDf
#
#   # cast gGroupData to df
#   # hmm this NULL when no group is in the model - need to wrap this in a function to skip/or insert as NA if NULL
#   if(length(ReadSysList$gGroupData[[1]]) == 0)
#   {
#     gGroupDataDf<- data.frame(CaseNum = gAllCaseEstimatesDf$pid, GData = NA)
#     #gGroupDataDf$CaseNum<- gAllCaseEstimatesDf$pid
#   }
#   else
#   {
#     ncolgGroupData<- length(names(unlist(ReadSysList$gGroupData[1])))
#     tempNames_gGroupDataDf<- names(unlist(ReadSysList$gGroupData[1]))
#     gGroupDataDf<- data.frame(matrix(unlist(ReadSysList$gGroupData), ncol = ncolgGroupData, byrow = TRUE))
#     names(gGroupDataDf)<- tempNames_gGroupDataDf
#   }
#
#   # cast gYData to df
#   # gRegressorLabels has text labels of each regressor
#   ncolgYData<- length(names(unlist(ReadSysList$gYData[1]))) # min shoudl be 2 (weight + intercept)
#   tempNames_gYDataDf<- c("Weight", unlist(ReadSysList$gRegressorLabels))  # previously names(unlist(ReadSysList$gYData[1])), which min should be  "Weight" "Y"
#   gYDataDf<- data.frame(matrix(unlist(ReadSysList$gYData), ncol = ncolgYData, byrow = TRUE))
#   names(gYDataDf)<- tempNames_gYDataDf
#
#   # todo: merge these together, add repsonse data? remove eap, score, PVs, fit where
#   # join gAllCaseEstimatesDf gGroupDataDf gYDataDf and delete these objects below (not needed)
#
#   myConQuestData<- gPIDLookUpDf
#   myConQuestData<- merge(myConQuestData, gResponseDataDf, by.x = "seqNum", by.y ="Pid", all.x = TRUE) # merge response data on PID lookup, this gives us the right link between seqNum and PID
#   myConQuestData<- merge(myConQuestData, gGroupDataDf, by.x = "seqNum", by.y ="CaseNum", all.x = TRUE) # merge group data on response data, there will always be at least 1 vector of group vars (can be all NA)
#   # cant currently gYDataDf - see Issue 3 - gYData does not contain either pid or seqnum
#   myConQuestData<- merge(myConQuestData, gAllCaseEstimatesDf, by.x = "seqNum", by.y ="pid", all.x = TRUE) # merge estimates on response data (note some cases could be missing from gAllCaseEstimatesDf IF they are missing all repsonse data and are missing regressor data - e.g., missing regressors result in deletion)
#   myConQuestData<- zapSystemMissing(myConQuestData)
#
#   # add objects to system file
#   systemFile[["gPIDLookUpDf"]]<-        gPIDLookUpDf
#   systemFile[["gResponseDataDf"]]<-     gResponseDataDf
#   systemFile[["gAllCaseEstimatesDf"]]<- gAllCaseEstimatesDf
#   systemFile[["gGroupDataDf"]]<-        gGroupDataDf
#   systemFile[["gYDataDf"]]<-            gYDataDf
#   systemFile[["myConQuestData"]]<-      myConQuestData
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
