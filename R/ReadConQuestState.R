#' @title DecompressSys
#'
#' @description Internal function to decompress an 'ACER ConQuest' system file
#'   that has been compressed using zlib.
#' @param myFile An connection to a compressed  'ACER ConQuest' system file
#'   created by the `put` command in 'ACER ConQuest'.`
#' @return A connection to an uncompressed system file in the users temp dir.
#' @seealso conquestr::ConQuestSys()
#' @importFrom zlib zlib decompressobj decompress
DecompressSys <- function(myFile) {
  chunkSize <- 16384
  myOut <- raw(0)
  # chunkN <- 0 # debug
  while (TRUE) {
    thisChunk <- readBin(myFile, what=raw(), n=chunkSize)
    if (length(thisChunk)==0) {
      break
    } else {
      # chunkN <- chunkN + 1 # debug
      myOut <- c(myOut, thisChunk)
    }
  }
  close(myFile) # we don't need this file any more

  decompressor <- zlib$decompressobj(zlib$MAX_WBITS)
  
  myStarts <- seq(1, length(myOut), by = chunkSize)
  
  for (i in myStarts) {
    tmpEnd <- i + chunkSize - 1
    if (tmpEnd > length(myOut)) tmpEnd <- length(myOut)
    chunk <- myOut[i:tmpEnd]
    decompressed_chunk <- decompressor$decompress(chunk)
    if (i == 1) {
      tmpHeader <- c(as.raw(c(0x0C, 0x00, 0x00, 0x00)), charToRaw("uncompressed"))
      decompressed_data <- tmpHeader
      decompressed_data <- c(decompressed_data, decompressed_chunk)
    } else {
      decompressed_data <- c(decompressed_data, decompressed_chunk)
    }
  }
  # Flush the decompressor buffer
  decompressed_data <- c(decompressed_data, decompressor$flush())

  myFile_name <- tempfile()
  myFile <- file(myFile_name, open = "wb")
  writeBin(decompressed_data, myFile)
  close(myFile)
  myFile <- file(myFile_name, open = "rb")
  
  return(myFile)
}



#' @title ReadSys
#'
#' @description Internal function to read an 'ACER ConQuest' system file.
#'   Called by conquestr::ConQuestSys.
#' @param myFile An connection to an 'ACER ConQuest' system file created by the
#'  `put` command in 'ACER ConQuest'.
#'   If the file is compressed, and uncompressed temporary file is created.
#' @param isMini A boolean, set to TRUE if the system file is a _mini_ system file created by 'ACER ConQuest'
#'   command put with option "mini = yes".
#' @return A list containing the data objects created by 'ACER ConQuest'.
#' @seealso conquestr::ConQuestSys()
#' @importFrom utils str
ReadSys <- function(myFile, isMini) {
  myDebug <- FALSE
  compressedString <- ReadString(myFile)
  if (myDebug) print(paste0("Compressed: ", compressedString))

  builddate <- ReadString(myFile)           # conquest build date
  writedate <- ReadString(myFile)           # file write date
  cqs_version <- ReadInteger(myFile)        # system file version
  if(myDebug) {
    print("print length of cqs_version\n")
    print(length(cqs_version))
    print("print str of cqs_version\n")
    print(str(cqs_version))
    print("print cqs_version\n")
    print(paste0("version: ", cqs_version))
  }
  if (!(cqs_version >= 25)) {
    stop(
      "This system file is from an old version of ACER ConQuest
      and cannot be read. Either recreate your system file in a newer release of
      ACER ConQuest or install a legacy version of conquestr"
    )
  }

  gNCases <- ReadDoubleList(myFile)
  if (myDebug) {
    print(paste0("gNCases: "))
    print(gNCases)
  }
  gNDim <- ReadInteger(myFile)
  gNGins <- ReadInteger(myFile)
  gNPlausiblesEstimate <- ReadInteger(myFile)
  # ----- here is first entry into  cmd_GetPutMini------------------------------
  if (isMini) {
    if (myDebug) print("into isMini conditional")
    tmpSystemFile <- ReadSysMini(
      myFile = myFile, 
      Dimensions = gNDim, 
      N = gNCases[[1]], 
      NPlausibles = gNPlausiblesEstimate,
      isDebug = myDebug
    )

  systemFile <- list(
    compressedString = compressedString,
    builddate = builddate,
    writedate = writedate,
    cqs_version = cqs_version,
    gNCases = gNCases,
    gNDim = gNDim,
    gNGins = gNGins,
    gNPlausiblesEstimate = gNPlausiblesEstimate
  )
  
  systemFile <- append(systemFile, tmpSystemFile)
  
  # return the list with all the stuff in it
  class(systemFile) <- append(class(systemFile), "conQuestSysFileMini")
  return(systemFile)
  }

  gMLEExist <- ReadBoolean(myFile)
  gWLEExist <- ReadBoolean(myFile)
  gEAPExist <- ReadBoolean(myFile)
  gPlausibleExist <- ReadBoolean(myFile)
  gSystemMissing <- ReadDouble(myFile)
  gApplyFilter <- ReadBoolean(myFile)
  if (myDebug) print(paste0("gApplyFilter: ", gApplyFilter))

    # debugging block - creates objects in global env in case this function fails before it creates the system file object at the end
    # gNCasesTemp<<- gNCases; print("gNCasesTemp is available for debugging") # debug
    # gNDimTemp<<- gNDim; print("gNDim is available for debugging") # debug
    # gNGinsTemp<<- gNGins; print("gNGinsTemp is available for debugging") # debug
    # gNPlausiblesEstimateTemp<<- gNPlausiblesEstimate; print("gNPlausiblesEstimateTemp is available for debugging") # debug
    # gMLEExistTemp<<- gMLEExist; print("gMLEExistTemp is available for debugging") # debug
    # gWLEExistTemp<<- gWLEExist; print("gWLEExistTemp is available for debugging") # debug
    # gEAPExistTemp<<- gEAPExist; print("gEAPExistTemp is available for debugging") # debug
    # gPlausibleExistTemp<<- gPlausibleExist; print("gPlausibleExistTemp is available for debugging") # debug
    # gSystemMissingTemp<<- gSystemMissing; print("gSystemMissingTemp is available for debugging") # debug
    # gApplyFilterTemp<<- gApplyFilter; print("gApplyFilterTemp is available for debugging") # debug

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 1

  gFilter <- ReadBitSet(myFile)
  if (myDebug) print(paste0("gFilter: ", names(gFilter)))
  if (myDebug) print(paste0("gFilter: ", gFilter))
  gBeta <- ReadMatrix(myFile)
  gOldBeta <- ReadMatrix(myFile)
  gBestBeta <- ReadMatrix(myFile)
  gXsi <- ReadMatrix(myFile)
  gOldXsi <- ReadMatrix(myFile)
  gBestXsi <- ReadMatrix(myFile)
  gTau <- ReadMatrix(myFile)
  gOldTau <- ReadMatrix(myFile)
  gBestTau <- ReadMatrix(myFile)
  gQuickErrorsXsi <- ReadMatrix(myFile)
  gQuickErrorsTau <- ReadMatrix(myFile)
  gQuickErrorsSigma <- ReadMatrix(myFile)
  gQuickErrorsBeta <- ReadMatrix(myFile)
  gMasterTheta <- ReadMatrix(myFile)
  gTheta <- ReadMatrix(myFile)
  gVariance <- ReadMatrix(myFile)
  gOldVariance <- ReadMatrix(myFile)
  gBestVariance <- ReadMatrix(myFile)
  gHistoryWeights <- ReadMatrix(myFile)
  gOldHistory <- ReadMatrix(myFile)
  gBestHistory <- ReadMatrix(myFile)
  gYBetaAll <- ReadMatrix(myFile)

    # debugging block - creates objects in global env in case this function fails before it creates the system file object at the end
    # gFilterTemp<<- gFilter; print("gFilterTemp is available for debugging") # debug
    # gBetaTemp<<- gBeta; print("gBetaTemp is available for debugging") # debug
    # gOldBetaTemp<<- gOldBeta; print("gOldBetaTemp is available for debugging") # debug
    # gBestBetaTemp<<- gBestBeta; print("gBestBetaTemp is available for debugging") # debug
    # gXsiTemp<<- gXsi; print("gXsiTemp is available for debugging") # debug
    # gOldXsiTemp<<- gOldXsi; print("gOldXsiTemp is available for debugging") # debug
    # gBestXsiTemp<<- gBestXsi; print("gBestXsiTemp is available for debugging") # debug
    # gTauTemp<<- gTau; print("gTauTemp is available for debugging") # debug
    # gOldTauTemp<<- gOldTau; print("gOldTauTemp is available for debugging") # debug
    # gBestTauTemp<<- gBestTau; print("gBestTauTemp is available for debugging") # debug
    # gQuickErrorsXsiTemp<<- gQuickErrorsXsi; print("gQuickErrorsXsiTemp is available for debugging") # debug
    # gQuickErrorsTauTemp<<- gQuickErrorsTau; print("gQuickErrorsTauTemp is available for debugging") # debug
    # gQuickErrorsSigmaTemp<<- gQuickErrorsSigma; print("gQuickErrorsSigmaTemp is available for debugging") # debug
    # gQuickErrorsBetaTemp<<- gQuickErrorsBeta; print("gQuickErrorsBetaTemp is available for debugging") # debug
    # gMasterThetaTemp<<- gMasterTheta; print("gMasterThetaTemp is available for debugging") # debug
    # gThetaTemp<<- gTheta; print("gThetaTemp is available for debugging") # debug
    # gVarianceTemp<<- gVariance; print("gVarianceTemp is available for debugging") # debug
    # gOldVarianceTemp<<- gOldVariance; print("gOldVarianceTemp is available for debugging") # debug
    # gBestVarianceTemp<<- gBestVariance; print("gBestVarianceTemp is available for debugging") # debug
    # gHistoryWeightsTemp<<- gHistoryWeights; print("gHistoryWeightsTemp is available for debugging") # debug
    # gOldHistoryTemp<<- gOldHistory; print("gOldHistoryTemp is available for debugging") # debug
    # gBestHistoryTemp<<- gBestHistory; print("gBestHistoryTemp is available for debugging") # debug
    # AllTemp<<- gYBetaAll; print("gYBetaAllTemp is available for debugging") # debug

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 2

  gWtFactor <- ReadDoubleList(myFile)
  gSuffXsi <- ReadMatrix(myFile)
  gSuffTau <- ReadMatrix(myFile)
  gModelText <- ReadString(myFile)
  gFormatText <- ReadString(myFile)
  gRegressionText <- ReadString(myFile)
  gGroupText <- ReadString(myFile)
  gOSSCP <- ReadMatrix(myFile)
  gLOSSCP <- ReadMatrix(myFile)
  gLSSCP <- ReadMatrix(myFile)
  gFullSSCP <- ReadMatrix(myFile)
  gFullSums <- ReadMatrix(myFile)
  gMinAlpha <- ReadDouble(myFile)
  gModelEstimated <- ReadBoolean(myFile)
  gIntegrationMethod <- ReadInteger(myFile)
  if (!(is.null(gIntegrationMethod))) {
    gIntegrationMethodLookUp <- data.frame(
      gIntegrationMethod = c(1:7),
      gIntegrationMethodText = c(
        "Bock Aitkin", "Monte Carlo", "Gauss-Hermite Quadrature",
        "Joint Maximum Likelihood", "estimation method has not been requested",
        "sparse Gauss-Hermite Quadrature (KPN)", "Markov Chain Montecarlo"
      )
    )
  }
  gIntegrationMethod <- gIntegrationMethodLookUp
  gPopulation <- ReadInteger(myFile)
  gSeeds <- ReadInteger(myFile)
  gMaxSinceBests <- ReadInteger(myFile)
  gInnerLoops <- ReadInteger(myFile)
  gWarnings <- ReadBoolean(myFile)
  gEstsToLog <- ReadBoolean(myFile)
  gKeepLast <- ReadBoolean(myFile)
  gAddExtension <- ReadBoolean(myFile)
  gMLEMax <- ReadDouble(myFile)
  gPlotWinMax <- ReadInteger(myFile)
  gZero <- ReadDouble(myFile)
  gRespMiss <- ReadInteger(myFile)

    # debugging block - creates objects in global env in case this function fails before it creates the system file object at the end
    # gWeightFactorTemp<<- gWeightFactor; print("gWeightFactorTemp is available for debugging") # debug
    # gSuffXsiTemp<<- gSuffXsi; print("gSuffXsiTemp is available for debugging") # debug
    # gSuffTauTemp<<- gSuffTau; print("gSuffTauTemp is available for debugging") # debug
    # gModelTextTemp<<- gModelText; print("gModelTextTemp is available for debugging") # debug
    # gFormatTextTemp<<- gFormatText; print("gFormatTextTemp is available for debugging") # debug
    # gRegressionTextTemp<<- gRegressionText; print("gRegressionTextTemp is available for debugging") # debug
    # gGroupTextTemp<<- gGroupText; print("gGroupTextTemp is available for debugging") # debug
    # gOSSCPTemp<<- gOSSCP; print("gOSSCPTemp is available for debugging") # debug
    # gLOSSCPTemp<<- gLOSSCP; print("gLOSSCPTemp is available for debugging") # debug
    # gLSSCPTemp<<- gLSSCP; print("gLSSCPTemp is available for debugging") # debug
    # gFullSSCPTemp<<- gFullSSCP; print("gFullSSCPTemp is available for debugging") # debug
    # gFullSumsTemp<<- gFullSums; print("gFullSumsTemp is available for debugging") # debug
    # gMinAlphaTemp<<- gMinAlpha; print("gMinAlphaTemp is available for debugging") # debug
    # gModelEstimatedTemp<<- gModelEstimated; print("gModelEstimatedTemp is available for debugging") # debug
    # gIntegrationMethodTemp<<- gIntegrationMethod; print("gIntegrationMethodTemp is available for debugging") # debug
    # gIntegrationMethodTextTemp<<- gIntegrationMethodText; print("gIntegrationMethodTextTemp is available for debugging") # debug
    # gPopulationTemp<<- gPopulation; print("gPopulationTemp is available for debugging") # debug
    # gSeedsTemp<<- gSeeds; print("gSeedsTemp is available for debugging") # debug
    # gMaxSinceBestsTemp<<- gMaxSinceBests; print("gMaxSinceBestsTemp is available for debugging") # debug
    # gInnerLoopsTemp<<- gInnerLoops; print("gInnerLoopsTemp is available for debugging") # debug
    # gWarningsTemp<<- gWarnings; print("gWarningsTemp is available for debugging") # debug
    # gEstsToLogTemp<<- gEstsToLog; print("gEstsToLogTemp is available for debugging") # debug
    # gKeepLastTemp<<- gKeepLast; print("gKeepLastTemp is available for debugging") # debug
    # gAddExtensionTemp<<- gAddExtension; print("gAddExtensionTemp is available for debugging") # debug
    # gMLEMaxTemp<<- gMLEMax; print("gMLEMaxTemp is available for debugging") # debug
    # gPlotWinMaxTemp<<- gPlotWinMax; print("gPlotWinMaxTemp is available for debugging") # debug
    # gZeroTemp<<- gZero; print("gZeroTemp is available for debugging") # debug
    # gRespMissTemp<<- gRespMiss; print("gRespMissTemp is available for debugging") # debug

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 3

  gDatafileName <- ReadString(myFile)
  gDatafileFormats <- ReadInteger(myFile)
  gDatafileNameDisplay <- ReadString(myFile)
  gStopReason <- ReadInteger(myFile)
  gImplicit <- ReadImplicitVar(myFile)
  gNImpValue <- ReadInteger(myFile)
  gPIDVar <- ReadInteger(myFile)
  gModelVariables <- ReadVarList(myFile)
  gNRec <- ReadInteger(myFile)
  gResponseLookUp <- ReadLookUp(myFile)
  gPreKeyLookUp <- ReadLookUp(myFile)
  gNDataRecords <- ReadInteger(myFile)
  gFacetVariables <- ReadVarList(myFile)
  tmpVarList <- ReadVarList(myFile) # this is a tmp VarList (previously this was gRegressionVariables)
  gGroupVariables <- ReadVarList(myFile)
  gWeightVariable <- ReadVarList(myFile)
  gTDFileV <- ReadVarList(myFile)
  gValidC <- ReadStringList(myFile)
  gFileRebuildNeeded <- ReadBoolean(myFile)
  gAMatrixImportFileName <- ReadString(myFile)
  gCMatrixImportFileName <- ReadString(myFile)
  gHistoryFileName <- ReadString(myFile) # no longer in use? Now read a temp string?
  gTitle <- ReadString(myFile)
  gStoreInRAM <- ReadBoolean(myFile) # no longer in use? Now read a temp boolean?

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 4

  gSubmitMode <- ReadBoolean(myFile)
  gMaxCats <- ReadInteger(myFile)
  gConvergenceOK <- ReadBoolean(myFile)
  gParameterConvCriterion <- ReadDouble(myFile)
  gDevianceConvCriterion <- ReadDouble(myFile)
  gFitDraws <- ReadInteger(myFile)
  gMaxIterations <- ReadInteger(myFile)
  gAccuracy <- ReadInteger(myFile)
  gPVNodes <- ReadInteger(myFile)
  gFitNodes <- ReadInteger(myFile)
  gIteration <- ReadInteger(myFile)
  gBestIter <- ReadInteger(myFile)
  gStdError <- ReadInteger(myFile) # 0 = QUICK; 1 = FULL (deprecated); 2 = EMPIRICAL; 3 = NONE;
  gIFit <- ReadBoolean(myFile)
  gPFit <- ReadBoolean(myFile)
  gScore <- ReadBoolean(myFile)
  gSLM <- ReadBoolean(myFile)
  gTwoPL <- ReadBoolean(myFile)
  gNominalResponse <- ReadBoolean(myFile)
  gPairWise <- ReadBoolean(myFile) # no longer in use - i think we DO need this, however - otherwise we need to know about gEstimationAllMethods before it is read! TODO: revert to set in CQ, ensure is in envir
  gRegC <- ReadBoolean(myFile) # no longer in use
  gNPlausibles <- ReadInteger(myFile)
  gLConstraint <- ReadInteger(myFile)
  gNRegressors <- ReadInteger(myFile) # no longer in use
  gThreePL <- ReadBoolean(myFile)
  gUniquePID <- ReadBoolean(myFile)
  gNGroup <- ReadInteger(myFile)
  gNReg <- ReadInteger(myFile) # no longer in use, but needed to read YFile later

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 5

  gDeriv2nd <- ReadMatrix(myFile)
  gReliability <- ReadMatrixList(myFile)
  #gMLEReliability <- ReadMatrix(myFile)
  #gEAPReliability <- ReadMatrix(myFile)
  #gWLEReliability <- ReadMatrix(myFile)
  gLogLike <- ReadDouble(myFile)
  gOldLogLike <- ReadDouble(myFile)
  gBestLogLike <- ReadDouble(myFile)
  gRegParamConverged <- ReadBoolean(myFile)
  gCovParamConverged <- ReadBoolean(myFile)
  gCovarianceAnchors <- ReadMatrix(myFile)
  gBetaAnchors <- ReadMatrix(myFile)
  gVarianceInverse <- ReadMatrix(myFile)
  gOriginalNParameters <- ReadInteger(myFile)
  gNParameters <- ReadInteger(myFile) # count of _item_ params
  gNParameters_C <- ReadInteger(myFile) # count of constrained _item_ params
  gNTau <- ReadInteger(myFile)
  gImportParameters <- ReadAnchorList(myFile)
  gKeyDefault <- ReadString(myFile)
  gMLECriterion <- ReadDouble(myFile)
  gDist <- ReadInteger(myFile)
  gMinBin <- ReadDouble(myFile)
  gMaxBin <- ReadDouble(myFile)
  gUnconstrainedYY <- ReadMatrix(myFile)
  gNXsiAnchors <- ReadInteger(myFile)
  gVarList <- ReadStringList(myFile)
  gNTauAnchors <- ReadInteger(myFile)
  gVarNoDim <- ReadInteger(myFile)
  gXsiAnchor <- ReadBooleanList(myFile)
  gTauAnchors <- ReadBooleanList(myFile)
  gYYinv <- ReadMatrixList(myFile)
  gVar <- ReadVariableList(myFile)
  gResponseBlock <- ReadResponseList(myFile)
  gKeys <- ReadKeyList(myFile)
  gLabels <- ReadLabelList(myFile)
  gImpValue <- ReadIntegerListList(myFile)
  gTerms <- ReadTermsList(myFile, cqs_version)
  gExplicit <- ReadLookUpList(myFile)
  gRecodes <- ReadIRecodeList(myFile)
  gScores <- ReadIRecodeList(myFile)
  gDeletes <- ReadIRecodeList(myFile)
  gLevel <- ReadIntegerList(myFile) # // No. of levels for each variable in model statement
    # gLevelTemp<<- gLevel; print("gLevelTemp is available for debugging") # debug
  gItemSteps <- ReadIntegerList(myFile)
    # gItemStepsTemp<<- gItemSteps; print("gItemStepsTemp is available for debugging") # debug
  gStartSteps <- ReadIntegerList(myFile)
  gParam <- ReadParametersList(myFile)
  gParamConstrained <- ReadParametersList(myFile)
  if (cqs_version < 29)
  {
    gNRegC <- ReadIntegerList(myFile) # number of regressors by dim
    gRegConstraints <- ReadMatrixList(myFile)
    gRegLookUp <- ReadMatrixList(myFile)
  }
  
  gPIndex <- ReadIntegerList(myFile)
  gProblemGins <- ReadIntegerList(myFile)

    # debugging block - creates objects in global env in case this function fails before it creates the system file object at the end
    # gDeriv2ndTemp<<- gDeriv2nd; print("gDeriv2ndTemp is available for debugging") # debug
    # gMLEReliabilityTemp<<- gMLEReliability; print("gMLEReliabilityTemp is available for debugging") # debug
    # gEAPReliabilityTemp<<- gEAPReliability; print("gEAPReliabilityTemp is available for debugging") # debug
    # gWLEReliabilityTemp<<- gWLEReliability; print("gWLEReliabilityTemp is available for debugging") # debug
    # gLogLikeTemp<<- gLogLike; print("gLogLikeTemp is available for debugging") # debug
    # gOldLogLikeTemp<<- gOldLogLike; print("gOldLogLikeTemp is available for debugging") # debug
    # gBestLogLikeTemp<<- gBestLogLike; print("gBestLogLikeTemp is available for debugging") # debug
    # gRegParamConvergedTemp<<- gRegParamConverged; print("gRegParamConvergedTemp is available for debugging") # debug
    # gCovParamConvergedTemp<<- gCovParamConverged; print("gCovParamConvergedTemp is available for debugging") # debug
    # gCovarianceAnchorsTemp<<- gCovarianceAnchors; print("gCovarianceAnchorsTemp is available for debugging") # debug
    # gBetaAnchorsTemp<<- gBetaAnchors; print("gBetaAnchorsTemp is available for debugging") # debug
    # gVarianceInverseTemp<<- gVarianceInverse; print("gVarianceInverseTemp is available for debugging") # debug
    # gOriginalNParametersTemp<<- gOriginalNParameters; print("gOriginalNParametersTemp is available for debugging") # debug
    # gNParametersTemp<<- gNParameters; print("gNParametersTemp is available for debugging") # debug
    # gNParameters_CTemp<<- gNParameters_C; print("gNParameters_CTemp is available for debugging") # debug
    # gNTauTemp<<- gNTau; print("gNTauTemp is available for debugging") # debug
    # gImportParametersTemp<<- gImportParameters; print("gImportParametersTemp is available for debugging") # debug
    # gKeyDefaultTemp<<- gKeyDefault; print("gKeyDefaultTemp is available for debugging") # debug
    # gMLECriterionTemp<<- gMLECriterion; print("gMLECriterionTemp is available for debugging") # debug
    # gDistTemp<<- gDist; print("gDistTemp is available for debugging") # debug
    # gMinBinTemp<<- gMinBin; print("gMinBinTemp is available for debugging") # debug
    # gMaxBinTemp<<- gMaxBin; print("gMaxBinTemp is available for debugging") # debug
    # gUnconstrainedYYTemp<<- gUnconstrainedYY; print("gUnconstrainedYYTemp is available for debugging") # debug
    # gNXsiAnchorsTemp<<- gNXsiAnchors; print("gNXsiAnchorsTemp is available for debugging") # debug
    # gVarListTemp<<- gVarList; print("gVarListTemp is available for debugging") # debug
    # gNTauAnchorsTemp<<- gNTauAnchors; print("gNTauAnchorsTemp is available for debugging") # debug
    # gVarNoDimTemp<<- gVarNoDim; print("gVarNoDimTemp is available for debugging") # debug
    # gXsiAnchorTemp<<- gXsiAnchor; print("gXsiAnchorTemp is available for debugging") # debug
    # gTauAnchorsTemp<<- gTauAnchors; print("gTauAnchorsTemp is available for debugging") # debug
    # gYYinvTemp<<- gYYinv; print("gYYinvTemp is available for debugging") # debug
    # gVarTemp<<- gVar; print("gVarTemp is available for debugging") # debug
    # gResponseBlockTemp<<- gResponseBlock; print("gResponseBlockTemp is available for debugging") # debug
    # gKeysTemp<<- gKeys; print("gKeysTemp is available for debugging") # debug
    # gLabelsTemp<<- gLabels; print("gLabelsTemp is available for debugging") # debug
    # gImpValueTemp<<- gImpValue; print("gImpValueTemp is available for debugging") # debug
    # gTermsTemp<<- gTerms; print("gTermsTemp is available for debugging") # debug
    # gExplicitTemp<<- gExplicit; print("gExplicitTemp is available for debugging") # debug
    # gRecodesTemp<<- gRecodes; print("gRecodesTemp is available for debugging") # debug
    # gScoresTemp<<- gScores; print("gScoresTemp is available for debugging") # debug
    # gDeletesTemp<<- gDeletes; print("gDeletesTemp is available for debugging") # debug
    # gLevelTemp<<- gLevel; print("gLevelTemp is available for debugging") # debug
    # gItemStepsTemp<<- gItemSteps; print("gItemStepsTemp is available for debugging") # debug
    # gStartStepsTemp<<- gStartSteps; print("gStartStepsTemp is available for debugging") # debug
    # gParamTemp<<- gParam; print("gParamTemp is available for debugging") # debug
    # gParamConstrainedTemp<<- gParamConstrained; print("gParamConstrainedTemp is available for debugging") # debug
    # gNRegCTemp<<- gNRegC; print("gNRegCTemp is available for debugging") # debug
    # gRegConstraintsTemp<<- gRegConstraints; print("gRegConstraintsTemp is available for debugging") # debug
    # gRegLookUpTemp<<- gRegLookUp; print("gRegLookUpTemp is available for debugging") # debug
    # gPIndexTemp<<- gPIndex; print("gPIndexTemp is available for debugging") # debug
    # gProblemGinsTemp<<- gProblemGins; print("gProblemGinsTemp is available for debugging") # debug

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 6

  gItemListByD <- ReadIntegerListList(myFile)
  gGeneraliseditemList_D <- ReadIntegerListList(myFile)
  if (cqs_version < 29) gRegToCategorise <- ReadCategoriseListLegacy(myFile)
  gFitStatistics <- ReadFitList(myFile)
  if (cqs_version < 29)
  {
    # this is the legacy regression object - not retained and replaced by read later (ReadRegressionList)
    gRegressors <- ReadRegressionListLeg(myFile) 
    gDummies <- ReadMatrixList(myFile)
    gHasDummies <- ReadBooleanList(myFile)
  }
  
  gItemGroups <- ReadItemSetList(myFile)
  if (cqs_version < 29) 
  {
    if (myDebug) print(paste0("gHasDummies: ", gHasDummies)) 
  }
  gHistory <- ReadHistory(myFile)
    # print(str(gHistory)); # debug
    # print(names(gHistory)); # debug
    # gHistoryTemp<<- gHistory; print("object `gHistoryTemp` is available for debugging"); # debug
  gNModelVariables <- ReadInteger(myFile)
  gModelVariables <- ReadVarList(myFile)
  gMinNode <- ReadDouble(myFile)
  gMaxNode <- ReadDouble(myFile)
  gTotalNodes <- ReadInteger(myFile)    #need to raise to power gNdim

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 7
  
  if (!gPairWise)
  {
    gAllCaseEstimates <- ReadAllCaseEstimates(
      myFile = myFile,
      Dimensions = gNDim,
      N = gNCases[[1]],
      NPlausibles = gNPlausiblesEstimate,
      cqs_version = cqs_version
    )
    # TODO: close this?

    check <- ReadInteger(myFile)
    if (myDebug) print(paste0("check: ", check)) # check 8

    gAMatrices <- ReadADesignMatrices(
      myFile = myFile,
      Columns = gNParameters,
      Items = gNGins,
      ItemSteps = gItemSteps
    )

      # print(str(gAMatrices)); # debug
      # print(names(gAMatrices)); # debug
      # gAMatricesTemp<<- gAMatrices; print("object `gAMatricesTemp` is available for debugging"); # debug

    check <- ReadInteger(myFile)
    if (myDebug) print(paste0("check: ", check)) # check 100

    gACMatrices <- ReadADesignMatrices(
      myFile = myFile,
      Columns = gNParameters_C,
      Items = gNGins,
      ItemSteps = gItemSteps
    )


    check <- ReadInteger(myFile)
    if (myDebug) print(paste0("check: ", check)) # check 200

    gBMatrices <- ReadBDesignMatrices(myFile = myFile,
                                    ItemSteps = gItemSteps,
                                    Items = gNGins)

      # print(str(gBMatrices)); # debug
      # print(names(gBMatrices)); # debug
      # gBmatricesTemp<<- gBMatrices; print("object `gBmatricesTemp` is available for debugging"); # debug

    check <- ReadInteger(myFile)
    if (myDebug) print(paste0("check: ", check)) # check 300

    if (gScore)
    {
      gCmatrices <- ReadCDesignMatrices(myFile,
                                      Dimensions = gNDim,
                                      ItemSteps = gItemSteps,
                                      Items = gNGins)
    }
    else
    {
      gCmatrices <- list()
    }

    # print("printing str(gCmatrices)"); print(str(gCmatrices)); # debug
    # print("printing names(gCmatrices)"); print(names(gCmatrices)); # debug
    # gCmatricesTemp<<- gCmatrices; print("object `gCmatricesTemp` is available for debugging"); # debug
  } else {
    gAllCaseEstimates <- list()
    gAMatrices <- list()
    gACMatrices <- list()
    gBMatrices <- list()
    gCmatrices <- list()
  }
  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 9

  gYData <- ReadAllY(myFile = myFile, N = gNCases[[1]], NReg = gNReg)
  # gYDataTemp<<- gYData; print("object `gYDataTemp` is available for debugging"); # debug


  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 10

  gGroupData <- ReadAllGroupsData(myFile = myFile,
                                N = gNCases[[1]],
                                GroupVariables = gGroupVariables,
                                AllVariables = gVar)

  # gGroupDataTemp<<- gGroupData; print("object `gGroupDataTemp` is available for debugging"); # debug

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 11

  gResponseData <- ReadAllResponseData(myFile,N = gNDataRecords)

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 12

  if (cqs_version >=27) {
    gTokenList <- ReadNamedStringList(myFile)
    if (myDebug) print(str(gTokenList))
  } else {
    gTokenList <- NULL
  }
  gMatrixList <- ReadMatrixVars(myFile)

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 13

  gXsiParameterLabels <- ReadStringList(myFile)
  gTauParameterLabels <- ReadStringList(myFile)

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 14

  if (cqs_version < 29) 
  {
    gRegressionLabels <- ReadStringList(myFile)
  }
    
  gGinLongLabels <- ReadStringList(myFile)
  gGinShortLabels <- ReadStringList(myFile)
  gPIDLookUp <- ReadStringList(myFile)

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 15

  gCommandHistory <- ReadStringList(myFile)
  gBandDefines <- ReadBandDefinesList(myFile)
  gDIC <- ReadDouble(myFile)
  gPositiveScores <- ReadBoolean(myFile)
  gScoresMax <- ReadDouble(myFile)
  gRandomStructure <- ReadRandomStructure(myFile)
  gSConstraint <- ReadInteger(myFile)
  gBurn <- ReadInteger(myFile)
  gSkip <- ReadInteger(myFile)
  gXsiProposalVariance <- ReadDouble(myFile)
  gTauProposalVariance <- ReadDouble(myFile)
  gThetaProposalVariance <- ReadDouble(myFile)
  gXsiIncMax <- ReadDouble(myFile)
  gFacOldXsi <- ReadDouble(myFile)
  gBlockBeta <- ReadInteger(myFile)

  check <- ReadInteger(myFile)
  if (myDebug) print(paste0("check: ", check)) # check 16

  if (cqs_version < 29) gRegressors <- ReadRegressionList(myFile) # overwrites old legacy gRegression object - note different from "new" gRegression class

  gSpeed <- ReadInteger(myFile)
  gEstimationAllMethods <- ReadIntegerList(myFile)
  gExportOptions <- ReadGExportOptionsList(myFile)

  if (cqs_version >= 29) 
  {
    check <- ReadInteger(myFile)
    if (myDebug) print(paste0("check: ", check)) # check 17
  }

  if (cqs_version >= 28) 
  {
    # gStdErrors
    gStdErrors <- ReadErrors(myFile)
  }

  if (cqs_version >= 29) 
  {
    check <- ReadInteger(myFile)
    if (myDebug) print(paste0("check: ", check)) # check 18
    gRegressors <- ReadRegressors(myFile)
    gMaxPIDLen  <- ReadInteger(myFile)
    check <- ReadInteger(myFile)
    if (myDebug) print(paste0("check: ", check)) # check 19
  }

    # debug
    # gXsiParameterLabelsTemp<<- gXsiParameterLabels; print("gXsiParameterLabelsTemp is available for debugging") # debug
    # gGinLongLabelsTemp<<- gGinLongLabels; print("gGinLongLabelsTemp is available for debugging") # debug
    # gGinShortLabelsTemp<<- gGinShortLabels; print("gGinShortLabelsTemp is available for debugging") # debug
    # gPIDLookUpTemp<<- gPIDLookUp; print("gPIDLookUpTemp is available for debugging") # debug


  # put all the stuff into a list
  systemFile <- list(
    compressedString = compressedString,
    builddate = builddate,
    writedate = writedate,
    cqs_version = cqs_version,
    gNCases = gNCases,
    gNDim = gNDim,
    gNGins = gNGins,
    gNPlausiblesEstimate = gNPlausiblesEstimate,
    # then if mini...
    # {
    #  
    #}
    gMLEExist = gMLEExist,
    gWLEExist = gWLEExist,
    gEAPExist = gEAPExist,
    gPlausibleExist = gPlausibleExist,
    gSystemMissing = gSystemMissing,
    gApplyFilter = gApplyFilter,
    # check 1
    gFilter = gFilter,
    gBeta = gBeta,
    gOldBeta = gOldBeta,
    gBestBeta = gBestBeta,
    gXsi = gXsi,
    gOldXsi = gOldXsi,
    gBestXsi = gBestXsi,
    gTau = gTau,
    gOldTau = gOldTau,
    gBestTau = gBestTau,
    gQuickErrorsXsi = gQuickErrorsXsi,
    gQuickErrorsTau = gQuickErrorsTau,
    gQuickErrorsSigma = gQuickErrorsSigma,
    gQuickErrorsBeta = gQuickErrorsBeta,
    gMasterTheta = gMasterTheta,
    gTheta = gTheta,
    gVariance = gVariance,
    gOldVariance = gOldVariance,
    gBestVariance = gBestVariance,
    gHistoryWeights = gHistoryWeights,
    gOldHistory = gOldHistory,
    gBestHistory = gBestHistory,
    gYBetaAll = gYBetaAll,
    # check 2
    gWtFactor = gWtFactor,
    gSuffXsi = gSuffXsi,
    gSuffTau = gSuffTau,
    gModelText = gModelText,
    gFormatText = gFormatText,
    gRegressionText = gRegressionText,
    gGroupText = gGroupText,
    gOSSCP = gOSSCP,
    gLOSSCP = gLOSSCP,
    gLSSCP = gLSSCP,
    gFullSSCP = gFullSSCP,
    gFullSums = gFullSums,
    gMinAlpha = gMinAlpha,
    gModelEstimated = gModelEstimated,
    gIntegrationMethod = gIntegrationMethod,
    gPopulation = gPopulation,
    gSeeds = gSeeds,
    gMaxSinceBests = gMaxSinceBests,
    gInnerLoops = gInnerLoops,
    gWarnings = gWarnings,
    gEstsToLog = gEstsToLog,
    gKeepLast = gKeepLast,
    gAddExtension = gAddExtension,
    gMLEMax = gMLEMax,
    gPlotWinMax = gPlotWinMax,
    gZero = gZero,
    gRespMiss = gRespMiss,
    # check 3
    gDatafileName = gDatafileName,
    gDatafileFormats = gDatafileFormats,
    gDatafileNameDisplay = gDatafileNameDisplay,
    gStopReason = gStopReason,
    gImplicit = gImplicit,
    gNImpValue = gNImpValue,
    gPIDVar = gPIDVar,
    gModelVariables = gModelVariables,
    gNRec = gNRec,
    gResponseLookUp = gResponseLookUp,
    gPreKeyLookUp = gPreKeyLookUp,
    gNDataRecords = gNDataRecords,
    gFacetVariables = gFacetVariables,
    # tmpVarList = tmpVarList, # this is a tmp VarList (previously this was gRegressionVariables)
    gGroupVariables = gGroupVariables,
    gWeightVariable = gWeightVariable,
    gTDFileV = gTDFileV,
    gValidC = gValidC,
    gFileRebuildNeeded = gFileRebuildNeeded,
    gAMatrixImportFileName = gAMatrixImportFileName,
    gCMatrixImportFileName = gCMatrixImportFileName,
    # gHistoryFileName = gHistoryFileName, # no longer used
    gTitle = gTitle,
    # gStoreInRAM = gStoreInRAM, # no longer used
    # check 4
    gSubmitMode = gSubmitMode,
    gMaxCats = gMaxCats,
    gConvergenceOK = gConvergenceOK,
    gParameterConvCriterion = gParameterConvCriterion,
    gDevianceConvCriterion = gDevianceConvCriterion,
    gFitDraws = gFitDraws,
    gMaxIterations = gMaxIterations,
    gAccuracy = gAccuracy,
    gPVNodes = gPVNodes,
    gFitNodes = gFitNodes,
    gIteration = gIteration,
    gBestIter = gBestIter,
    gStdError = gStdError,
    gIFit = gIFit,
    gPFit = gPFit,
    gScore = gScore,
    gSLM = gSLM,
    gTwoPL = gTwoPL,
    gNominalResponse = gNominalResponse,
    # gPairWise = gPairWise,
    # gRegC = gRegC,
    gNPlausibles = gNPlausibles,
    gLConstraint = gLConstraint,
    # gNRegressors = gNRegressors,
    gThreePL = gThreePL,
    gUniquePID = gUniquePID,
    gNGroup = gNGroup,
    gNReg = gNReg,
    # check 5
    gDeriv2nd = gDeriv2nd,
    gReliability = gReliability,
    #gMLEReliability = gMLEReliability,
    #gEAPReliability = gEAPReliability,
    #gWLEReliability = gWLEReliability,
    gLogLike = gLogLike,
    gOldLogLike = gOldLogLike,
    gBestLogLike = gBestLogLike,
    gRegParamConverged = gRegParamConverged,
    gCovParamConverged = gCovParamConverged,
    gCovarianceAnchors = gCovarianceAnchors,
    gBetaAnchors = gBetaAnchors,
    gVarianceInverse = gVarianceInverse,
    gOriginalNParameters = gOriginalNParameters,
    gNParameters = gNParameters,
    gNParameters_C = gNParameters_C,
    gNTau = gNTau,
    gImportParameters = gImportParameters,
    gKeyDefault = gKeyDefault,
    gMLECriterion = gMLECriterion,
    gDist = gDist,
    gMinBin = gMinBin,
    gMaxBin = gMaxBin,
    gUnconstrainedYY = gUnconstrainedYY,
    gNXsiAnchors = gNXsiAnchors,
    gVarList = gVarList,
    gNTauAnchors = gNTauAnchors,
    gVarNoDim = gVarNoDim,
    gXsiAnchor = gXsiAnchor,
    gTauAnchors = gTauAnchors,
    gYYinv = gYYinv,
    gVar = gVar,
    gResponseBlock = gResponseBlock,
    gKeys = gKeys,
    gLabels = gLabels,
    gImpValue = gImpValue,
    gTerms = gTerms,
    gExplicit = gExplicit,
    gRecodes = gRecodes,
    gScores = gScores,
    gDeletes = gDeletes,
    gLevel = gLevel,
    gItemSteps = gItemSteps,
    gStartSteps = gStartSteps,
    gParam = gParam,
    gParamConstrained = gParamConstrained,
    gPIndex = gPIndex,
    gProblemGins = gProblemGins,
    # check 6
    gItemListByD = gItemListByD,
    gGeneraliseditemList_D = gGeneraliseditemList_D,
    gFitStatistics = gFitStatistics,
    gItemGroups = gItemGroups,
    gHistory = gHistory,
    gNModelVariables = gNModelVariables,
    gModelVariables = gModelVariables,
    gMinNode = gMinNode,
    gMaxNode = gMaxNode,
    gTotalNodes = gTotalNodes,
    # check 7
    gAllCaseEstimates = gAllCaseEstimates,
    # check 8
    gAMatrices = gAMatrices,
    # check 100
    gACMatrices = gACMatrices,
    # check 200
    gBMatrices = gBMatrices,
    # check 300
    gCmatrices = gCmatrices,
    # check 9
    gYData = gYData,
    # check 10
    gGroupData = gGroupData,
    # check 11
    gResponseData = gResponseData,
    # check 12
    gTokenList = gTokenList,
    gMatrixList = gMatrixList,
    gXsiParameterLabels = gXsiParameterLabels,
    gTauParameterLabels = gTauParameterLabels,
    # check 14
    # gRegressorLabels = gRegressorLabels,
    gGinLongLabels = gGinLongLabels,
    gGinShortLabels = gGinShortLabels,
    gPIDLookUp = gPIDLookUp,
    # check 15
    gCommandHistory = gCommandHistory,
    gBandDefines = gBandDefines,
    gDIC = gDIC,
    gPositiveScores = gPositiveScores,
    gScoresMax = gScoresMax,
    gRandomStructure = gRandomStructure,
    gSConstraint = gSConstraint,
    gBurn = gBurn,
    gSkip = gSkip,
    gXsiProposalVariance = gXsiProposalVariance,
    gTauProposalVariance = gTauProposalVariance,
    gThetaProposalVariance = gThetaProposalVariance,
    gXsiIncMax = gXsiIncMax,
    gFacOldXsi = gFacOldXsi,
    gBlockBeta = gBlockBeta,
    # check 16
    gRegressors = gRegressors,
    gSpeed = gSpeed,
    gEstimationAllMethods = gEstimationAllMethods,
    gExportOptions = gExportOptions,
    gStdErrors = if (cqs_version >= 28)
    {
      gStdErrors
    } else {
      NULL
    },
    gRegressors = if (cqs_version >= 29)
    {
      gRegressors
    } else {
       NULL
    },
    gMaxPIDLen = if (cqs_version >= 29)
    {
      gMaxPIDLen
    } else {
      NULL
    }
    #TODO: when cqs_version < 29 populate gRegressors with legacy stuff from above

  )

  # return the list with all the stuff in it
  class(systemFile) <- append(class(systemFile), "conQuestSysFile")
  return(systemFile)

}


#' @title ReadSysMini
#'
#' @description Internal function to read an 'ACER ConQuest' system file.
#'   Called by conquestr::ConQuestSys.
#' @param myFile An 'ACER ConQuest' _mini_ system file created by the `put` 
#'   command in 'ACER ConQuest' with the option "mini = yes".
#' @param Dimensions gNDim object passed in to this call to `ReadSysMini` from
#'   the higher-level original call to ReadSys. This value is returned in the
#'   list returned by this function call.
#' @param N gNCases object passed in to this call to `ReadSysMini` from
#'   the higher-level original call to ReadSys. This value is returned in the
#'   list returned by this function call.
#' @param NPlausibles gNPlausibles object passed in to this call to `ReadSysMini` from
#'   the higher-level original call to ReadSys. This value is returned in the
#'   list returned by this function call.
#' @param isDebug Bool. Passed in to this call to `ReadSysMini` from
#'   the higher-level original call to ReadSys. This value is used to trigger
#'   output of debug information to standard out.
#' @return A list containing the data objects created by 'ACER ConQuest'.
#' @seealso conquestr::ConQuestSys()
#' @importFrom utils str
ReadSysMini <- function(myFile, Dimensions, N, NPlausibles, isDebug) {

  miniList <- list()
  if (isDebug) print("before ReadAllCaseEstimates")
  miniList[["gAllCaseEstimates"]] <- ReadAllCaseEstimates(
      myFile = myFile,
      Dimensions = Dimensions,
      N = N,
      NPlausibles = NPlausibles
  )
  if (isDebug) print("after ReadAllCaseEstimates")
  miniList[["gParam"]] <- ReadParametersList(myFile)
  miniList[["gBeta"]] <- ReadMatrix(myFile)
  miniList[["gVariance"]] <- ReadMatrix(myFile)
  miniList[["gXsi"]] <- ReadMatrix(myFile)
  miniList[["gTau"]] <- ReadMatrix(myFile)
  miniList[["gLConstraint"]] <- ReadInteger(myFile)
  miniList[["gSConstraint"]] <- ReadInteger(myFile)
  miniList[["gScore"]] <- ReadBoolean(myFile)

  return(miniList)
}