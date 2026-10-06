# Readers for the elements of 'ACER ConQuest' binary files (system and rout files).
# These are thin wrappers around the C++ readers in src/readCq.cpp.
# `myFile` is a 'cqReader': the file held in memory with a read cursor, created
# with `.cqReaderNew(<raw vector>)` (see `ConQuestSys()` and `ConQuestRout()`).

#' @title ReadDouble
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadDouble <- function(myFile) .cqReadDouble(myFile)

#' @title ReadDoubleList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadDoubleList <- function(myFile) .cqReadDoubleList(myFile)

#' @title ReadInteger
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadInteger <- function(myFile) .cqReadInteger(myFile)

#' @title ReadBoolean
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadBoolean <- function(myFile) .cqReadBoolean(myFile)

#' @title ReadString
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadString <- function(myFile) .cqReadString(myFile)

#' @title ReadIntegerList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadIntegerList <- function(myFile) .cqReadIntegerList(myFile)

#' @title ReadIntegerListList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadIntegerListList <- function(myFile) .cqReadIntegerListList(myFile)

#' @title ReadBooleanList
#' @param myFile An uncompress 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadBooleanList <- function(myFile) .cqReadBooleanList(myFile)

#' @title ReadStringList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadStringList <- function(myFile) .cqReadStringList(myFile)

#' @title ReadNamedStringList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A data frame
#' @keywords internal
ReadNamedStringList <- function(myFile)
{
  x <- .cqReadNamedStringList(myFile)
  if (length(x$names) == 0) return(data.frame(names = NULL, data = NULL))
  data.frame(names = x$names, data = x$data)
}

#' @title ReadIntArray2
#' @param myFile An 'ACER ConQuest' system file.
#' @return A matrix
#' @keywords internal
ReadIntArray2 <- function(myFile) .cqReadIntArray2(myFile)

#' @title ReadTerm
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadTerm <- function(myFile) .cqReadTerm(myFile)

#' @title ReadBitSet
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadBitSet <- function(myFile) .cqReadBitSet(myFile)

#' @title ReadMatrix
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadMatrix <- function(myFile) .cqReadMatrix(myFile)

#' @title ReadMatrixList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadMatrixList <- function(myFile) .cqReadMatrixList(myFile)

#' @title ReadErrors
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadErrors <- function(myFile) .cqReadErrors(myFile)

#' @title ReadRegressors
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadRegressors <- function(myFile) .cqReadRegressors(myFile)

#' @title ReadImplicitVar
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadImplicitVar <- function(myFile) .cqReadImplicitVar(myFile)

#' @title ReadVarList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadVarList <- function(myFile) .cqReadVarList(myFile)

#' @title ReadLookUp
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadLookUp <- function(myFile) .cqReadLookUp(myFile)

#' @title ReadLookUpList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadLookUpList <- function(myFile) .cqReadLookUpList(myFile)

#' @title ReadBandDefine
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadBandDefine <- function(myFile) .cqReadBandDefine(myFile)

#' @title ReadBandDefinesList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadBandDefinesList <- function(myFile) .cqReadBandDefinesList(myFile)


#' @title ReadAnchor
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadAnchor <- function(myFile) .cqReadAnchor(myFile)

#' @title ReadAnchorList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadAnchorList <- function(myFile) .cqReadAnchorList(myFile)

#' @title ReadVariable
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadVariable <- function(myFile) .cqReadVariable(myFile)

#' @title ReadVariableList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadVariableList <- function(myFile) .cqReadVariableList(myFile)

#' @title ReadResponse
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadResponse <- function(myFile) .cqReadResponse(myFile)

#' @title ReadResponseList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadResponseList <- function(myFile) .cqReadResponseList(myFile)

#' @title ReadKey
#' @keywords internal
ReadKey <- function(myFile) .cqReadKey(myFile)

#' @title ReadKeyList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadKeyList <- function(myFile) .cqReadKeyList(myFile)

#' @title ReadLabel
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadLabel <- function(myFile) .cqReadLabel(myFile)

#' @title ReadLabelList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadLabelList <- function(myFile) .cqReadLabelList(myFile)

#' @title ReadTerms
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadTerms <- function(myFile, cqs_version) .cqReadTerms(myFile, cqs_version)

#' @title ReadTermsList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadTermsList <- function(myFile, cqs_version) .cqReadTermsList(myFile, cqs_version)

#' @title ReadVarInfo
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadVarInfo <- function(myFile) .cqReadVarInfo(myFile)

#' @title ReadCodeList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadCodeList <- function(myFile) .cqReadCodeList(myFile)

#' @title ReadIRecode
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadIRecode <- function(myFile) .cqReadIRecode(myFile)

#' @title ReadIRecodeList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadIRecodeList <- function(myFile) .cqReadIRecodeList(myFile)

#' @title ReadParameters
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadParameters <- function(myFile) .cqReadParameters(myFile)

#' @title ReadParametersList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadParametersList <- function(myFile) .cqReadParametersList(myFile)

#' @title ReadCategoriseLegacy
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadCategoriseLegacy <- function(myFile) .cqReadCategoriseLegacy(myFile)

#' @title ReadCategorise
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadCategorise <- function(myFile) .cqReadCategorise(myFile)

#' @title ReadCategoriseListLegacy
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadCategoriseListLegacy <- function(myFile) .cqReadCategoriseListLegacy(myFile)

#' @title ReadCategoriseList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadCategoriseList <- function(myFile) .cqReadCategoriseList(myFile)

#' @title ReadFit
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadFit <- function(myFile) .cqReadFit(myFile)

#' @title ReadFitList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadFitList <- function(myFile) .cqReadFitList(myFile)

#' @title ReadRegression
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadRegression <- function(myFile) .cqReadRegression(myFile)

#' @title ReadRegressionListLeg
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadRegressionListLeg <- function(myFile) .cqReadRegressionListLeg(myFile)

#' @title ReadRegressionList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadRegressionList <- function(myFile) .cqReadRegressionList(myFile)

#' @title ReadItemSet
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadItemSet <- function(myFile) .cqReadItemSet(myFile)

#' @title ReadItemSetList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadItemSetList <- function(myFile) .cqReadItemSetList(myFile)

#' @title ReadHistory
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadHistory <- function(myFile) .cqReadHistory(myFile)

#' @title ReadEstimatesRecord
#' @param myFile An 'ACER ConQuest' system file.
#' @param Dimensions An integer representation of 'ACER ConQuest' object gNDim.
#' @param NPlausibles An integer representation of 'ACER ConQuest' object gNPlausibles.
#' @param n An integer representation of 'ACER ConQuest' object gNCases.
#' @param cqs_version An integer - the version of the CQS system file.
#' @return A list
#' @keywords internal
ReadEstimatesRecord <- function(myFile, Dimensions, NPlausibles, n, cqs_version) {
  .cqReadEstimatesRecord(myFile, Dimensions, NPlausibles, n, cqs_version)
}

#' @title ReadAllCaseEstimates
#' @param myFile An 'ACER ConQuest' system file.
#' @param Dimensions An integer representation of 'ACER ConQuest' object gNDim.
#' @param N An integer representation of 'ACER ConQuest' object gNCases
#' @param NPlausibles An integer representation of 'ACER ConQuest' object gNPlausibles.
#' @return A list
#' @keywords internal
ReadAllCaseEstimates <- function(myFile,Dimensions,N,NPlausibles,cqs_version)
{
  # the number of PVs (or chain length when patz) is read from the file:
  # gNPlausibles is _not_ always the number of PVs in the last run, so
  # `NPlausibles` is not used
  .cqReadAllCaseEstimates(myFile, Dimensions, N, cqs_version)
}

#' @title ReadDataRecord
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadDataRecord <- function(myFile) .cqReadDataRecord(myFile)

#' @title ReadAllResponseData
#' @param myFile An 'ACER ConQuest' system file.
#' @param N An integer representation of 'ACER ConQuest' object gNCases.
#' @return A list
#' @keywords internal
ReadAllResponseData <- function(myFile,N) .cqReadAllResponseData(myFile, N)

#' @title ReadADesignMatrices
#' @param myFile An 'ACER ConQuest' system file.
#' @param Columns An integer representation of 'ACER ConQuest' object gNParameters.
#' @param Items An integer representation of 'ACER ConQuest' object gNGins.
#' @param ItemSteps An integer representation of 'ACER ConQuest' object gItemSteps.
#' @return A list
#' @keywords internal
#' @description ReadSys Read the A design matrix (A list of length gNGins of matrices.  For each matrix the number of rows is gItemSteps (for that item) and the number of columns is gNParameters_C).
ReadADesignMatrices <- function(myFile,Columns,Items,ItemSteps) {
  .cqReadADesignMatrices(myFile, Columns, Items, ItemSteps)
}

#' @title ReadBDesignMatrices
#' @param myFile An 'ACER ConQuest' system file.
#' @param Items An integer representation of 'ACER ConQuest' object gNGins.
#' @param ItemSteps An integer representation of 'ACER ConQuest' object gItemSteps.
#' @return A list
#' @keywords internal
#' @description  ReadSys Read the B design matrix (A list of length gNGins of lists, one per item. For each item a list of length gItemSteps of matrices).
ReadBDesignMatrices <- function(myFile,ItemSteps,Items) .cqReadBDesignMatrices(myFile, ItemSteps, Items)

#' @title ReadCDesignMatrices
#' @param myFile An 'ACER ConQuest' system file.
#' @param Dimensions An integer representation of 'ACER ConQuest' object gNDim.
#' @param Items An integer representation of 'ACER ConQuest' object gNGins.
#' @param ItemSteps An integer representation of 'ACER ConQuest' object gItemSteps.
#' @return A list
#' @keywords internal
#' @description  ReadSys Read the C design matrix (A list of length gNGins of lists, one per item. For each item a list of length gItemSteps of matrices).
ReadCDesignMatrices <- function(myFile,Dimensions,ItemSteps,Items) {
  .cqReadCDesignMatrices(myFile, Dimensions, ItemSteps, Items)
}

#' @title ReadYOneCase
#' @param myFile An 'ACER ConQuest' system file.
#' @param NReg An integer representing the number of regressors in the model, a representation of 'ACER ConQuest' object gNReg.
#' @return A list
#' @keywords internal
ReadYOneCase <- function(myFile,NReg) .cqReadYOneCase(myFile, NReg)

#' @title ReadAllY
#' @param myFile An 'ACER ConQuest' system file.
#' @param N An integer representation of 'ACER ConQuest' object gNCases
#' @param NReg An integer representing the number of regressors in the model, a representation of 'ACER ConQuest' object gNReg.
#' @keywords internal
ReadAllY <- function(myFile,N,NReg) .cqReadAllY(myFile, N, NReg)

#' @title ReadGroupsOneCase
#' @param myFile An 'ACER ConQuest' system file.
#' @param GroupVariables A list of group variables for this case.
#' @param AllVariables A list of variables for this case.
#' @param CaseNum An integer representing the case number used in the lookup tables.
#' @return A list
#' @keywords internal
ReadGroupsOneCase <- function(myFile, GroupVariables, AllVariables, CaseNum) {
  # when missing values (" ", ".") are in group data the sys file encodes
  # embedded nuls; these are read as blanks (see readCharSafe)
  .cqReadGroupsOneCase(myFile, GroupVariables, AllVariables, CaseNum)
}

#' @title ReadAllGroupsData
#' @param myFile An 'ACER ConQuest' system file.
#' @param N An integer representation of 'ACER ConQuest' object gNCases.
#' @param GroupVariables A list of group variables.
#' @param AllVariables A list of variables.
#' @return A list
#' @keywords internal
ReadAllGroupsData <- function(myFile,N,GroupVariables,AllVariables) {
  .cqReadAllGroupsData(myFile, N, GroupVariables, AllVariables)
}

#' @title ReadMatrixVars
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadMatrixVars <- function(myFile) .cqReadMatrixVars(myFile)

#' @title ReadPoint
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadPoint <- function(myFile) .cqReadPoint(myFile)

#' @title ReadSeries
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadSeries <- function(myFile) .cqReadSeries(myFile)

#' @title ReadGraph
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadGraph <- function(myFile) .cqReadGraph(myFile)

#' @title ReadRandomStructure
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list
#' @keywords internal
ReadRandomStructure <- function(myFile) .cqReadRandomStructure(myFile)

#' @title ReadGExportOptions
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list of a single ExportOption (e.g., used in ExportXsi())
#' @keywords internal
ReadGExportOptions <- function(myFile) .cqReadGExportOptions(myFile)

#' @title ReadGExportOptionsList
#' @param myFile An 'ACER ConQuest' system file.
#' @return A list of ExportOptions
#' @keywords internal
ReadGExportOptionsList <- function(myFile) .cqReadGExportOptionsList(myFile)
