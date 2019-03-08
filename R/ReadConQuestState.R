#' @title ReadSys
#'
#' @description Internal function to read an 'ACER ConQuest' system file. Called by conquestr::ConQuestSys.
#'
#' @param myFile An 'ACER ConQuest' system file created by the `put` command in 'ACER ConQuest'. The put command must use the option `compressed = no`.
#' @return A list containing the data objects created by 'ACER ConQuest'.
#' @seealso conquestr::ConQuestSys()
ReadSys<-function(myFile){

  # requires funtions in R/ReadConQuestLibrary.R

  Compressed<-ReadString(myFile); Compressed
  # insert code to check that Compressed="uncompressed" if not we can't proceed
  if(!(Compressed == "uncompressed"))
    stop("This system file is compressed and i dont know how to handle it, use option ! compress = no in conquest")

  builddate<-ReadString(myFile)           # conquest build date
  writedate<-ReadString(myFile)           # file write date
  version<-ReadInteger(myFile)            # system file version
  # insert code to check that version=>19 if not we can't proceed


  gNCases<-ReadInteger(myFile)
  gNDim<-ReadInteger(myFile)
  gNGins<-ReadInteger(myFile)
  gNPlausiblesEstimate<-ReadInteger(myFile)
  gMLEExist<-ReadBoolean(myFile)
  gWLEExist<-ReadBoolean(myFile)
  gEAPExist<-ReadBoolean(myFile)
  gPlausibleExist<-ReadBoolean(myFile)
  gSystemMissing<-ReadDouble(myFile)
  gApplyFilter<-ReadBoolean(myFile)

    # debugging block - creates objects in global env in case this funtion fails before it creates the system file object at the end
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

  check<-ReadInteger(myFile)
  #print(check) # check 1

  gFilter<-ReadBitSet(myFile)
  gBeta<-ReadMatrix(myFile)
  gOldBeta<-ReadMatrix(myFile)
  gBestBeta<-ReadMatrix(myFile)
  gXsi<-ReadMatrix(myFile)
  gOldXsi<-ReadMatrix(myFile)
  gBestXsi<-ReadMatrix(myFile)
  gTau<-ReadMatrix(myFile)
  gOldTau<-ReadMatrix(myFile)
  gBestTau<-ReadMatrix(myFile)
  gQuickErrorsXsi<-ReadMatrix(myFile)
  gQuickErrorsTau<-ReadMatrix(myFile)
  gQuickErrorsSigma<-ReadMatrix(myFile)
  gQuickErrorsBeta<-ReadMatrix(myFile)
  gMasterTheta<-ReadMatrix(myFile)
  gTheta<-ReadMatrix(myFile)
  gVariance<-ReadMatrix(myFile)
  gOldVariance<-ReadMatrix(myFile)
  gBestVariance<-ReadMatrix(myFile)
  gHistoWeights<-ReadMatrix(myFile)
  gOldHisto<-ReadMatrix(myFile)
  gBestHisto<-ReadMatrix(myFile)
  gYBeta<-ReadMatrix(myFile)

    # debugging block - creates objects in global env in case this funtion fails before it creates the system file object at the end
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
    # gHistoWeightsTemp<<- gHistoWeights; print("gHistoWeightsTemp is available for debugging") # debug
    # gOldHistoTemp<<- gOldHisto; print("gOldHistoTemp is available for debugging") # debug
    # gBestHistoTemp<<- gBestHisto; print("gBestHistoTemp is available for debugging") # debug
    # gYBetaTemp<<- gYBeta; print("gYBetaTemp is available for debugging") # debug

  check<-ReadInteger(myFile)
  #print(check) # check 2

  gWeightFactor<-ReadDouble(myFile)
  gSuffXsi<-ReadMatrix(myFile)
  gSuffTau<-ReadMatrix(myFile)
  gModelText<-ReadString(myFile)
  gFormatText<-ReadString(myFile)
  gRegressionText<-ReadString(myFile)
  gGroupText<-ReadString(myFile)
  gOSSCP<-ReadMatrix(myFile)
  gLOSSCP<-ReadMatrix(myFile)
  gLSSCP<-ReadMatrix(myFile)
  gFullSSCP<-ReadMatrix(myFile)
  gFullSums<-ReadMatrix(myFile)
  gMinAlpha<-ReadDouble(myFile)
  gModelEstimated<-ReadBoolean(myFile)
  gIntegrationMethod<-ReadInteger(myFile) # 1 Bock Aitkin, 2 Monte Carlo, 3 gauss-hermite quadrature, 4 joint maximum likelihood, 5 estimation method has not been requested, 6 sparse gauss-hermite quadrature (KPN), 7 markov chain montecarlo
    # create text version of gIntegrationMethod
    if(!(is.null(gIntegrationMethod))) {

      gIntegrationMethodLookUp<- data.frame(gIntegrationMethod = c(1:7), gIntegrationMethodText = c("Bock Aitkin", "Monte Carlo", "Gauss-Hermite Quadrature", "Joint Maximum Likelihood", "estimation method has not been requested", "sparse Gauss-Hermite Quadrature (KPN)", "Markov Chain Montecarlo"))
      gIntegrationMethodText<- as.character(gIntegrationMethodLookUp$gIntegrationMethodText[gIntegrationMethod])

    }
  gPopulation<-ReadInteger(myFile)
  gSeeds<-ReadInteger(myFile)
  gMaxSinceBests<-ReadInteger(myFile)
  gInnerLoopss<-ReadInteger(myFile)
  gWarningss<-ReadBoolean(myFile)
  gEstsToLog<-ReadBoolean(myFile)
  gKeepLast<-ReadBoolean(myFile)
  gAddExtension<-ReadBoolean(myFile)
  gMLEMax<-ReadDouble(myFile)
  gPlotWinMax<-ReadInteger(myFile)
  gZero<-ReadDouble(myFile)
  gRespMiss<-ReadInteger(myFile)

    # debugging block - creates objects in global env in case this funtion fails before it creates the system file object at the end
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
    # gInnerLoopssTemp<<- gInnerLoopss; print("gInnerLoopssTemp is available for debugging") # debug
    # gWarningssTemp<<- gWarningss; print("gWarningssTemp is available for debugging") # debug
    # gEstsToLogTemp<<- gEstsToLog; print("gEstsToLogTemp is available for debugging") # debug
    # gKeepLastTemp<<- gKeepLast; print("gKeepLastTemp is available for debugging") # debug
    # gAddExtensionTemp<<- gAddExtension; print("gAddExtensionTemp is available for debugging") # debug
    # gMLEMaxTemp<<- gMLEMax; print("gMLEMaxTemp is available for debugging") # debug
    # gPlotWinMaxTemp<<- gPlotWinMax; print("gPlotWinMaxTemp is available for debugging") # debug
    # gZeroTemp<<- gZero; print("gZeroTemp is available for debugging") # debug
    # gRespMissTemp<<- gRespMiss; print("gRespMissTemp is available for debugging") # debug

  check<-ReadInteger(myFile)
  #print(check) # check 3

  gDatafileName<-ReadString(myFile)
  gDatafileFormats<-ReadInteger(myFile)
  gDatafileNameDisplay<-ReadString(myFile)
  gStopReason<-ReadInteger(myFile)
  gImplicit<-ReadImplicitVar(myFile)
  gNImpValue<-ReadInteger(myFile)
  gPIDVar<-ReadInteger(myFile)
  gModelVariables<-ReadVarList(myFile)
  gNRec<-ReadInteger(myFile)
  gResponseLookUp<-ReadLookUp(myFile)
  gPreKeyLookUp<-ReadLookUp(myFile)
  gNDataRecords<-ReadInteger(myFile)
  gFacetVariables<-ReadVarList(myFile)
  gRegressionVariables<-ReadVarList(myFile)
  gGroupVariables<-ReadVarList(myFile)
  gWeightVariable<-ReadVarList(myFile)
  gTDFileV<-ReadVarList(myFile)
  gValidC<-ReadStringList(myFile)
  gFileRebuildNeeded<-ReadBoolean(myFile)
  gAMatrixImportFileName<-ReadString(myFile)
  gCMatrixImportFileName<-ReadString(myFile)
  gExportXsiFile<-ReadString(myFile)
  gExportTauFile<-ReadString(myFile)
  gExportScoredDataFile<-ReadString(myFile)
  gAMatrixExportFileName<-ReadString(myFile)
  gCMatrixExportFileName<-ReadString(myFile)
  gExportBetaFile<-ReadString(myFile)
  gExportSigmaFile<-ReadString(myFile)
  gExportScoredDataFile<-ReadString(myFile)
  gHistoryFileName<-ReadString(myFile)
  gTitle<-ReadString(myFile)
  gStoreInRAM<-ReadBoolean(myFile)

  check<-ReadInteger(myFile)
  #print(check) # check 4

  gSubmitMode<-ReadBoolean(myFile)
  gMaxCats<-ReadInteger(myFile)
  gConvergenceOK<-ReadBoolean(myFile)
  gParameterConvCriterion<-ReadDouble(myFile)
  gDevianceConvCriterion<-ReadDouble(myFile)
  gFitDraws<-ReadInteger(myFile)
  gMaxIterations<-ReadInteger(myFile)
  gAccuracy<-ReadInteger(myFile)
  gPVNodes<-ReadInteger(myFile)
  gFitNodes<-ReadInteger(myFile)
  gIteration<-ReadInteger(myFile)
  gBestIter<-ReadInteger(myFile)
  gStdError<-ReadInteger(myFile)
  gIFit<-ReadBoolean(myFile)
  gPFit<-ReadBoolean(myFile)
  gScore<-ReadBoolean(myFile)
  gSLM<-ReadBoolean(myFile)
  gTwoPL<-ReadBoolean(myFile)
  gNominalResponse<-ReadBoolean(myFile)
  gPairWise<-ReadBoolean(myFile)
  gRegC<-ReadBoolean(myFile)
  gNPlausibles<-ReadInteger(myFile)
  gLConstraint<-ReadInteger(myFile)
  gNRegressors<-ReadInteger(myFile)
  gThreePL<-ReadBoolean(myFile)
  gUniquePID<-ReadBoolean(myFile)
  gNGroup<-ReadInteger(myFile)
  gNReg<-ReadInteger(myFile)

  check<-ReadInteger(myFile)
  #print(check) # check 5

  gDeriv2nd<-ReadMatrix(myFile)
  gMLEReliability<-ReadMatrix(myFile)
  gEAPReliability<-ReadMatrix(myFile)
  gWLEReliability<-ReadMatrix(myFile)
  gLogLike<-ReadDouble(myFile)
  gOldLogLike<-ReadDouble(myFile)
  gBestLogLike<-ReadDouble(myFile)
  gRegParamConverged<-ReadBoolean(myFile)
  gCovParamConverged<-ReadBoolean(myFile)
  gCovarianceAnchors<-ReadMatrix(myFile)
  gBetaAnchors<-ReadMatrix(myFile)
  gVarianceInverse<-ReadMatrix(myFile)
  gOriginalNParameters<-ReadInteger(myFile)
  gNParameters<-ReadInteger(myFile)
  gNParameters_C<-ReadInteger(myFile)
  gNTau<-ReadInteger(myFile)
  gImportParameters<-ReadAnchorList(myFile)
  gKeyDefault<-ReadString(myFile)
  gMLECriterion<-ReadDouble(myFile)
  gDist<-ReadInteger(myFile)
  gMinBin<-ReadDouble(myFile)
  gMaxBin<-ReadDouble(myFile)
  gUnconstrainedYY<-ReadMatrix(myFile)
  gNXsiAnchors<-ReadInteger(myFile)
  gVarList<-ReadStringList(myFile)
  gNTauAnchors<-ReadInteger(myFile)
  gVarNoDim<-ReadInteger(myFile)
  gAnchor<-ReadBooleanList(myFile)
  gTauAnchors<-ReadBooleanList(myFile)
  gYYinv<-ReadMatrixList(myFile)
  gVar<-ReadVariableList(myFile)
  gResponseBlock<-ReadResponseList(myFile)
  gKeys<-ReadKeyList(myFile)
  gLabels<-ReadLabelList(myFile)
  gImpValue<-ReadIntegerListList(myFile)
  gTerms<-ReadTermsList(myFile)
  gExplicit<-ReadLookUpList(myFile)
  gRecodes<-ReadIRecodeList(myFile)
  gScores<-ReadIRecodeList(myFile)
  gDeletes<-ReadIRecodeList(myFile)
  gLevel<-ReadIntegerList(myFile) # // No. of levels for each variable in model statement
    # gLevelTemp<<- gLevel; print("gLevelTemp is available for debugging") # debug
  gItemSteps<-ReadIntegerList(myFile)
    # gItemStepsTemp<<- gItemSteps; print("gItemStepsTemp is available for debugging") # debug
  gStartSteps<-ReadIntegerList(myFile)
  gParam<-ReadParametersList(myFile)
  gParamConstrained<-ReadParametersList(myFile)
  gNRegC<-ReadIntegerList(myFile) # number of regressors by dim
  gRegConstraints<-ReadMatrixList(myFile)
  gRegLookUp<-ReadMatrixList(myFile)
  gPIndex<-ReadIntegerList(myFile)
  gProblemGins<-ReadIntegerList(myFile)

    # debugging block - creates objects in global env in case this funtion fails before it creates the system file object at the end
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
    # gAnchorTemp<<- gAnchor; print("gAnchorTemp is available for debugging") # debug
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

  check<-ReadInteger(myFile)
  #print(check) # check 6

  gItemListByD<-ReadIntegerListList(myFile)
  gGeneraliseditemList_D<-ReadIntegerListList(myFile)
  gRegToCategorise<-ReadCategoriseList(myFile)
  gFitStatistics<-ReadFitList(myFile)
  gRegressors<-ReadRegressionList(myFile)
  gDummies<-ReadMatrixList(myFile)
  gHasDummies<-ReadBooleanList(myFile)
  gItemGroups<-ReadItemSetList(myFile)
  gHistory<-ReadHistory(myFile)
    # print(str(gHistory)); # debug
    # print(names(gHistory)); # debug
    # gHistoryTemp<<- gHistory; print("object `gHistoryTemp` is available for debugging"); # debug
  gNModelVariables<-ReadInteger(myFile)
  gModelVariables<-ReadVarList(myFile)
  gMinNode<-ReadDouble(myFile)
  gMaxNode<-ReadDouble(myFile)
  gTotalNodes<-ReadInteger(myFile)    #need to raise to power gNdim

  check<-ReadInteger(myFile)
  #print(check) # check 7

  gAllCaseEstimates<-ReadAllCaseEstimates(myFile=myFile,
                                          Dimensions=gNDim,
                                          N=gNCases,
                                          NPlausibles=gNPlausiblesEstimate)

  check<-ReadInteger(myFile)
  #print(check) # check 8

  gAMatrices<-ReadADesignMatrices(myFile=myFile,
                                  Columns=gNParameters,
                                  Items=gNGins,
                                  ItemSteps=gItemSteps)

    # print(str(gAMatrices)); # debug
    # print(names(gAMatrices)); # debug
    # gAMatricesTemp<<- gAMatrices; print("object `gAMatricesTemp` is available for debugging"); # debug

  check<-ReadInteger(myFile)
  #print(check) # check 100

  gACMatrices<-ReadADesignMatrices(myFile=myFile,
                                   Columns=gNParameters_C,
                                   Items=gNGins,
                                   ItemSteps=gItemSteps)


  check<-ReadInteger(myFile)
  #print(check) # check 200

  gBMatrices<-ReadBDesignMatrices(myFile=myFile,
                                  ItemSteps=gItemSteps,
                                  Items=gNGins)

    # print(str(gBMatrices)); # debug
    # print(names(gBMatrices)); # debug
    # gBmatricesTemp<<- gBMatrices; print("object `gBmatricesTemp` is available for debugging"); # debug

  check<-ReadInteger(myFile)
  #print(check) # check 300

  if(gScore)
  {
    gCmatrices<-ReadCDesignMatrices(myFile,
                                    Dimensions=gNDim,
                                    ItemSteps=gItemSteps,
                                    Items=gNGins)
  }
  else
  {
    gCmatrices<- list()
  }

  # print("printing str(gCmatrices)"); print(str(gCmatrices)); # debug
  # print("printing names(gCmatrices)"); print(names(gCmatrices)); # debug
  # gCmatricesTemp<<- gCmatrices; print("object `gCmatricesTemp` is available for debugging"); # debug

  check<-ReadInteger(myFile)
  # print(paste(check, " : check #9, gCmatrices")) # check 9

  gYData<-ReadAllY(myFile=myFile,N=gNCases,NReg=gNReg)
  # gYDataTemp<<- gYData; print("object `gYDataTemp` is available for debugging"); # debug


  check<-ReadInteger(myFile)
  # print(paste(check, " : check #10, gYData")) # check 10

  gGroupData<-ReadAllGroupsData(myFile=myFile,
                                N=gNCases,
                                GroupVariables=gGroupVariables,
                                AllVariables=gVar)

    # gGroupDataTemp<<- gGroupData; print("object `gGroupDataTemp` is available for debugging"); # debug

  check<-ReadInteger(myFile)
  #print(check) # check 11

  gResponseData<-ReadAllResponseData(myFile,N=gNDataRecords)

  check<-ReadInteger(myFile)
  #print(check) # check 12

  gMatrixList<- ReadMatrixVars(myFile)

  check<-ReadInteger(myFile)
  #print(check) # check 13

  gXsiParameterLabels<-ReadStringList(myFile)
	gTauParameterLabels<-ReadStringList(myFile)

	check<-ReadInteger(myFile)
	#print(check) # check 14

	gRegressorLabels<-ReadStringList(myFile)
	gGinLongLabels<-ReadStringList(myFile)
	gGinShortLabels<-ReadStringList(myFile)
	gPIDLookUp<-ReadStringList(myFile)

	check<-ReadInteger(myFile)
	#print(check) # check 15

	gCommandHistory<- ReadStringList(myFile)
	gBandDefines<- ReadBandDefinesList(myFile)

	  # debug
	  # gXsiParameterLabelsTemp<<- gXsiParameterLabels; print("gXsiParameterLabelsTemp is available for debugging") # debug
	  # gGinLongLabelsTemp<<- gGinLongLabels; print("gGinLongLabelsTemp is available for debugging") # debug
	  # gGinShortLabelsTemp<<- gGinShortLabels; print("gGinShortLabelsTemp is available for debugging") # debug
	  # gPIDLookUpTemp<<- gPIDLookUp; print("gPIDLookUpTemp is available for debugging") # debug


  # put all the stuff into a list
  systemFile<-list(
    Compressed = Compressed,
    builddate = builddate,
    writedate = writedate,
    version = version,
    gNCases = gNCases,
    gNDim = gNDim,
    gNGins = gNGins,
    gNPlausiblesEstimate = gNPlausiblesEstimate,
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
    gHistoWeights = gHistoWeights,
    gOldHisto = gOldHisto,
    gBestHisto = gBestHisto,
    gYBeta = gYBeta,
    # check 2
    gWeightFactor = gWeightFactor,
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
    gInnerLoopss = gInnerLoopss,
    gWarningss = gWarningss,
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
    gRegressionVariables = gRegressionVariables,
    gGroupVariables = gGroupVariables,
    gWeightVariable = gWeightVariable,
    gTDFileV = gTDFileV,
    gValidC = gValidC,
    gFileRebuildNeeded = gFileRebuildNeeded,
    gAMatrixImportFileName = gAMatrixImportFileName,
    gCMatrixImportFileName = gCMatrixImportFileName,
    gExportXsiFile = gExportXsiFile,
    gExportTauFile = gExportTauFile,
    gExportScoredDataFile = gExportScoredDataFile,
    gAMatrixExportFileName = gAMatrixExportFileName,
    gCMatrixExportFileName = gCMatrixExportFileName,
    gExportBetaFile = gExportBetaFile,
    gExportSigmaFile = gExportSigmaFile,
    gExportScoredDataFile = gExportScoredDataFile,
    gHistoryFileName = gHistoryFileName,
    gTitle = gTitle,
    gStoreInRAM = gStoreInRAM,
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
    gPairWise = gPairWise,
    gRegC = gRegC,
    gNPlausibles = gNPlausibles,
    gLConstraint = gLConstraint,
    gNRegressors = gNRegressors,
    gThreePL = gThreePL,
    gUniquePID = gUniquePID,
    gNGroup = gNGroup,
    gNReg = gNReg,
    # check 5
    gDeriv2nd = gDeriv2nd,
    gMLEReliability = gMLEReliability,
    gEAPReliability = gEAPReliability,
    gWLEReliability = gWLEReliability,
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
    gAnchor = gAnchor,
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
    gNRegC = gNRegC,
    gRegConstraints = gRegConstraints,
    gRegLookUp = gRegLookUp,
    gPIndex = gPIndex,
    gProblemGins = gProblemGins,
    # check 6
    gItemListByD = gItemListByD,
    gGeneraliseditemList_D = gGeneraliseditemList_D,
    gRegToCategorise = gRegToCategorise,
    gFitStatistics = gFitStatistics,
    gRegressors = gRegressors,
    gDummies = gDummies,
    gHasDummies = gHasDummies,
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
    gMatrixList = gMatrixList,
	  gXsiParameterLabels=gXsiParameterLabels,
		gTauParameterLabels=gTauParameterLabels,
		# check 14
		gRegressorLabels=gRegressorLabels,
		gGinLongLabels=gGinLongLabels,
		gGinShortLabels=gGinShortLabels,
		gPIDLookUp=gPIDLookUp,
		# check 15
	  gCommandHistory=gCommandHistory,
		gBandDefines=gBandDefines
  )

  # return the list with all the stuff in it
  return(systemFile)

}

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
