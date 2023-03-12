#' @title ReadDouble
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadDouble <- function(myFile)
{
    readBin(myFile, double(), endian = "little", size = 8, n = 1)
}

#' @title ReadDoubleList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadDoubleList <- function(myFile)
{
    N <- ReadInteger(myFile)
    Value <- list()
    for (i in seq_len(N))
    {
      Value[[i]] <- ReadDouble(myFile)
    }
    return(Value)
}

#' @title ReadInteger
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadInteger <- function(myFile)
{
    readBin(myFile, integer(), endian = "little", size = 4, n = 1)
}

#' @title ReadBoolean
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadBoolean <- function(myFile)
{
    readBin(myFile, logical(), endian = "little", size = 1, n = 1)
}

#' @title ReadString
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadString <- function(myFile)
{
    String <- ""
    L <- readBin(myFile, integer(), endian = "little", size = 4, n = 1)
     if (L == 0)
     {
       return(String)
     } else {
       String <- readCharSafe(myFile, L)
       return(String)
     }

}

#' @title ReadIntegerList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadIntegerList <- function(myFile)
{
    N <- ReadInteger(myFile)
    Value <- list()
    for (i in seq_len(N))
    {
      Value[[i]] <- ReadInteger(myFile)
    }
    return(Value)
}

#' @title ReadIntegerListList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadIntegerListList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadIntegerList(myFile)
    }
    return(Value)
}

#' @title ReadBooleanList
#' @param myFile An uncompress 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadBooleanList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <- ReadBoolean(myFile)
    }
    return(Value)
}

#' @title ReadStringList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadStringList <- function(myFile)
{
  N <- ReadInteger(myFile)
  Value <- list()
  for (i in seq_len(N))
  {
    Value[[i]] <- ReadString(myFile)
  }
  return(Value)
}

#' @title ReadBitSet
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadBitSet <- function(myFile)
{
  String <- matrix()
  Items <- ReadInteger(myFile)
  Columns <- ReadInteger(myFile)
  Size <- ReadInteger(myFile)
  if (Size>0) {
    for (i in 1:Size) {
      tmpString <- readBin(myFile, what = "raw")
      tmpLogical <- as.logical(rawToBits(tmpString))
      if (i == 1) {
          String <- tmpLogical
        } else {
          String <- c(String, tmpLogical)
        }
      }
      # subset String as it is likely too big!
      String <- matrix(String[1:(Items*Columns)], nrow = Items, ncol = Columns, byrow = TRUE)
    }
    V <- list(String, Items, Columns, Size)
    names(V)<-c("String", "Items", "Columns", "Size")
    return(V)
}

#' @title ReadMatrix
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadMatrix <- function(myFile)
{
  A <- matrix()
  Rows <- ReadInteger(myFile)
  Columns <- ReadInteger(myFile)
  Empty <- ReadBoolean(myFile)
  Title <- ReadString(myFile)
  cLabels <- ReadStringList(myFile)
  rLabels <- ReadStringList(myFile)
  if (Rows == 0 || Columns == 0) return(A)
  # resize A
  A <- matrix(1:Rows * Columns, nrow = Rows, ncol = Columns)
  for (r in seq_len(Rows)) {
    for (c in seq_len(Columns)) {
      A[r, c] <- ReadDouble(myFile)
    }
  }
  colnames(A) <- unlist(cLabels)
  rownames(A) <- unlist(rLabels)
  return(A)
}

#' @title ReadMatrixList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadMatrixList <- function(myFile)
{
  N <- ReadInteger(myFile)
  Value <- list()
  for (i in seq_len(N)) {
    Value[[i]] <-ReadMatrix(myFile)
  }
  return (Value)
}

#' @title ReadImplicitVar
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadImplicitVar <- function(myFile)
{
    L <- ReadInteger(myFile)
  Name <- list()
    for (i in seq_len(L))
    {
            Name[[i]] <-ReadString(myFile)
    }
  Levels <- list()
    for (i in seq_len(L))
    {
            Levels[[i]] <-ReadInteger(myFile)
    }
  V <- list(Name,Levels)
  names(V)<-c("Name","Levels")
  return (V)
}

#' @title ReadVarList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadVarList <- function(myFile)
{
    LX <- ReadInteger(myFile)
  XV <- list()
    for (i in seq_len(LX))
    {
            XV[[i]] <-ReadInteger(myFile)
    }
    LI <- ReadInteger(myFile)
  IV <- list()
  for (i in seq_len(LI))
  {
        IV[[i]] <-ReadInteger(myFile)
  }
  V <- list(XV,IV)
  names(V) <- c("XV","IV")
  return(V)
}

#' @title ReadLookUp
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadLookUp <- function(myFile)
{
    VarNumber <- ReadInteger(myFile)
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadString(myFile)
    }
  V <- list(VarNumber,Value)
  names(V)<-c("VarNumber","Value")
  return(V)
}

#' @title ReadLookUpList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadLookUpList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadLookUp(myFile)
    }
    return(Value)
}

#' @title ReadBandDefine
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadBandDefine <- function(myFile)
{
  Dimension <- ReadInteger(myFile)
  UpperBound <- ReadDouble(myFile)
  LowerBound <- ReadDouble(myFile)
  BandLabel <- ReadString(myFile)
  V <- list(Dimension, UpperBound, LowerBound, BandLabel)
  names(V)<-c("Dimension", "Upper Bound", "Lower Bound", "Band Label")
  return(V)
}

#' @title ReadBandDefinesList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadBandDefinesList <- function(myFile)
{
  N <- ReadInteger(myFile)
  Value <- list()
  for (i in seq_len(N))
  {
    Value[[i]] <-ReadBandDefine(myFile)
  }
  return(Value)
}


#' @title ReadAnchor
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadAnchor <- function(myFile)
{
    Type <- ReadInteger(myFile)
    I1 <-ReadInteger(myFile)
    I2 <-ReadInteger(myFile)
    Value <- ReadDouble(myFile)
  V <- list(Type,I1,I2,Value)
  names(V)<-c("Type","I1","I2","Value")
  return (V)
}

#' @title ReadAnchorList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadAnchorList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadAnchor(myFile)
    }
    return(Value)
}

#' @title ReadVariable
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadVariable <- function(myFile)
{
    Name <- ReadString(myFile)
    N <- ReadInteger(myFile)
  Begin <- list()
  End <- list()
  Record <- list()
    for (i in seq_len(N))
    {
        Begin[[i]] <-ReadInteger(myFile)
    }
    for (i in seq_len(N))
    {
        End[[i]] <-ReadInteger(myFile)
    }
    for (i in seq_len(N))
    {
        Record[[i]] <-ReadInteger(myFile)
    }
    Level <- ReadInteger(myFile)
    NMissing <- ReadInteger(myFile)
  MissingList <- list()
    for (i in seq_len(NMissing))
    {
        MissingList[[i]] <-ReadString(myFile)
    }
    NMissingMatchMethod <- ReadInteger(myFile)
    MissingMatchMethod <- list()
    for (i in seq_len(NMissingMatchMethod))
    {
        MissingMatchMethod[[i]] <-ReadInteger(myFile)
    }
    NDropKeepList <- ReadInteger(myFile)
    DropKeepList <- list()
    for (i in seq_len(NDropKeepList))
    {
        DropKeepList[[i]] <-ReadString(myFile)
    }
    NDropKeepMatchMethod <- ReadInteger(myFile)
    DropKeepMatchMethod <- list()
  for (i in seq_len(NDropKeepMatchMethod))
  {
        DropKeepMatchMethod[[i]] <-ReadInteger(myFile)
  }
    DropKeepType <- ReadInteger(myFile)
    Categorical <- ReadBoolean(myFile)
    C <- ReadStringList(myFile)
  V <- list(Name,Begin,End,Level,MissingList,MissingMatchMethod,DropKeepList,
          DropKeepMatchMethod,DropKeepType,Categorical,C)
  names(V)<-c("Name","Begin","End","Level","MissingList","MissingMatchMethod","DropKeepList",
               "DropKeepMatchMethod","DropKeepType","Categorical","c")
  return (V)
}

#' @title ReadVariableList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadVariableList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadVariable(myFile)
    }
    return(Value)
}

#' @title ReadResponse
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadResponse <- function(myFile)
{
    N <- ReadInteger(myFile)
  Col <- list()
  Rec <- list()
    for (i in seq_len(N))
    {
            Col[[i]] <-ReadInteger(myFile)
    }
    for (i in seq_len(N))
    {
            Rec[[i]] <-ReadInteger(myFile)
    }
    Width <- ReadInteger(myFile)
  V <- list(Col,Rec,Width)
  names(V)<-c("Col","Rec","Width")
  return (V)
}

#' @title ReadResponseList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadResponseList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadResponse(myFile)
    }
    return(Value)
}

#' @title ReadKey
#' @keywords internal
ReadKey <- function(myFile)
{
    N <- ReadInteger(myFile)
  Key <- list()
    for (i in seq_len(N))
    {
            Key[[i]] <-ReadString(myFile)
    }
  Score <- ReadString(myFile)
  V <- list(Key,Score)
  names(V)<-c("Key","Score")
  return (V)
}

#' @title ReadKeyList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadKeyList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadKey(myFile)
    }
    return(Value)
}

#' @title ReadLabel
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadLabel <- function(myFile)
{
    VarNum <- ReadInteger(myFile)
    VarType <- ReadInteger(myFile) #IMPLICIT=0, EXPLICIT=1, DIMENSION=2, PARAMETER=3, FIT=4
    N <- ReadInteger(myFile)
  Code <- list()
    for (i in seq_len(N))
    {
            Code[[i]] <-ReadString(myFile)
    }
  Label <- list()
    for (i in seq_len(N))
    {
            Label[[i]] <-ReadString(myFile)
    }
  V <- list(VarNum,VarType,Code,Label)
  names(V)<-c("VarNum","VarType","Code","Label")
  return (V)
}

#' @title ReadLabelList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadLabelList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadLabel(myFile)
    }
    return(Value)
}

#' @title ReadTerms
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadTerms <- function(myFile) {
    N <- ReadInteger(myFile)
    NP <- ReadInteger(myFile)
  VariableNumber <- list()
  VariableType <- list()
  ParamNumber <- list()
  ParamType <- list()
    for (i in seq_len(N))
    {
            VariableNumber[[i]] <- ReadInteger(myFile) # MAYBE: sequential count of terms in model
    }
    for (i in seq_len(N))
    {
            VariableType[[i]] <- ReadInteger(myFile) # 0 if implicit, 1 if explicit
    }
    for (i in seq_len(NP))
    {
            ParamNumber[[i]] <- ReadInteger(myFile) # param number in the model (0 offset)
    }
    for (i in seq_len(NP))
    {
            ParamType[[i]] <- ReadInteger(myFile) # 0 if estimated, 1 if constrained
    }
    Sign <- readChar(myFile, 1)
    Label <- ReadString(myFile)
  V <- list(VariableNumber,VariableType,ParamNumber,ParamType,Sign,Label)
  names(V)<-c("VariableNumber","VariableType","ParamNumber","ParamType","Sign","Label")
  return (V)
}

#' @title ReadTermsList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadTermsList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadTerms(myFile)
    }
    return(Value)
}

#' @title ReadVarInfo
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadVarInfo <- function(myFile)
{
    VarNumber <- ReadInteger(myFile)
    VarType <- ReadInteger(myFile)
  V <- list(VarNumber,VarType)
  names(V)<-c("VarNumber","VarType")
  return (V)
}

#' @title ReadCodeList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadCodeList <- function(myFile)
{
    VarInfo <- ReadVarInfo(myFile)
    S <- ReadStringList(myFile)
  V <- list(VarInfo,S)
  names(V)<-c("VarInfo","S")
  return (V)
}

#' @title ReadIRecode
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadIRecode <- function(myFile)
{
    Before <- ReadStringList(myFile)
    NAfter <- ReadInteger(myFile)
  AfterList <- list()
    for (i in seq_len(NAfter))
    {
            AfterList[[i]] <-ReadStringList(myFile)
    }
    NList <- ReadInteger(myFile)
  CList <- list()
    for (i in seq_len(NList))
    {
            CList[[i]] <-ReadCodeList(myFile)
    }
  V <- list(Before,AfterList,CList)
  names(V)<-c("Before","AfterList","CList")
  return (V)
}

#' @title ReadIRecodeList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadIRecodeList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadIRecode(myFile)
    }
    return(Value)
}

#' @title ReadParameters
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadParameters <- function(myFile)
{
  Levels <- ReadIntegerList(myFile)
    Step <- ReadInteger(myFile)
  Sign <- readChar(myFile, 1)
  V <- list(Levels,Step,Sign)
  names(V)<-c("Levels","Step","Sign")
  return (V)
}

#' @title ReadParametersList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadParametersList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadParameters(myFile)
    }
    return(Value)
}

#' @title ReadCategorise
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadCategorise <- function(myFile)
{
  Type <- ReadInteger(myFile)
  Varnum <- ReadInteger(myFile)
  ValueList <- ReadStringList(myFile)
  MatchType <- ReadIntegerList(myFile)
  V <- list(Type, Varnum, ValueList, MatchType)
  names(V) <- c("Type", "Varnum", "ValueList", "MatchType")
  return(V)
}

#' @title ReadCategoriseList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadCategoriseList <- function(myFile)
{
  N <- ReadInteger(myFile)
  Value <- list()
  for (i in seq_len(N))
  {
    Value[[i]] <- ReadCategorise(myFile)
  }
  return(Value)
}

#' @title ReadFit
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadFit <- function(myFile)
{
    UnWeightedMNSQ <- ReadDouble(myFile)
    UnWeightedtfit <- ReadDouble(myFile)
  WeightedCW2 <-ReadDouble(myFile)
    WeightedMNSQ <- ReadDouble(myFile)
    Weightedtfit <- ReadDouble(myFile)
    WeightedNumerator <- ReadDouble(myFile)
    WeightedDenominator <- ReadDouble(myFile)
    UnWeightedSE <- ReadDouble(myFile)
    WeightedSE <- ReadDouble(myFile)
    Failed <- ReadBoolean(myFile)
  V <- list(UnWeightedMNSQ,UnWeightedtfit,WeightedCW2,WeightedMNSQ,Weightedtfit,
          WeightedNumerator,WeightedDenominator,UnWeightedSE,WeightedSE,Failed)
  names(V)<-c("UnWeightedMNSQ","UnWeightedtfit","WeightedCW2","WeightedMNSQ","Weightedtfit",
              "WeightedNumerator","WeightedDenominator","UnWeightedSE","WeightedSE","Failed")
  return (V)
}

#' @title ReadFitList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadFitList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Names <- list()
  Value <- list()
  V <- list(Names,Value)
  names(V)<-c("Names","Value")
    for (i in seq_len(N))
    {
           Names[[i]] <-ReadString(myFile)
            Value[[i]] <-ReadFit(myFile)
    }
  V <- list(Names,Value)
  names(V)<-c("Names","Value")
  return (V)
}

#' @title ReadRegression
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadRegression <- function(myFile)
{
    Varnum <- ReadInteger(myFile)
    ValueList <- ReadStringList(myFile)
  V <- list(Varnum,ValueList)
  names(V)<-c("Varnum","ValueList")
  return (V)
}

#' @title ReadRegressionListLeg
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadRegressionListLeg <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadRegression(myFile)
    }
    return(Value)
}

#' @title ReadRegressionList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadRegressionList <- function(myFile)
{
  N <- ReadInteger(myFile)
  Value <- list()
  for (i in seq_len(N))
  {
    Value[[i]] <-ReadRegression(myFile)
  }
  tmpRandGroupName <- ReadString(myFile)
  tmpRandGroupExists <- ReadBoolean(myFile)
  Result <- list(Value, tmpRandGroupName, tmpRandGroupExists)
  names(Result)<- c("RegVars", "RandGroupName", "RandGroupExists")
  return(Result)
}

#' @title ReadItemSet
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadItemSet <- function(myFile)
{
    Name <- ReadString(myFile)
    ValueList <- ReadIntegerList(myFile)
  V <- list(Name,ValueList)
  names(V)<-c("Name","ValueList")
  return (V)
}

#' @title ReadItemSetList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadItemSetList <- function(myFile)
{
    N <- ReadInteger(myFile)
  Value <- list()
    for (i in seq_len(N))
    {
            Value[[i]] <-ReadItemSet(myFile)
    }
    return(Value)
}

#' @title ReadHistory
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadHistory <- function(myFile)
{
    N <- ReadInteger(myFile)
    RunNo <- list()
    Iter <- list()
  Likelihood <- list()
  Beta <- list()
  WithinVariance <- list()
  Xsi <- list()
  Tau <- list()
  RanTermVariance <- list()
  BetweenVariance <- list()
  for (i in seq_len(N))
    {
    myTmpInt <- ReadInteger(myFile)
    RunNo[[i]] <- myTmpInt
    Iter[[i]] <- ReadInteger(myFile)
        Likelihood[[i]] <-ReadDouble(myFile)
       Beta[[i]] <-ReadMatrix(myFile)
       WithinVariance[[i]] <-ReadMatrix(myFile)
    Xsi[[i]] <-ReadMatrix(myFile)
    Tau[[i]] <-ReadMatrix(myFile)
    RanTermVariance[[i]] <-ReadMatrix(myFile)
    BetweenVariance[[i]] <-ReadMatrix(myFile)
  }
  V <- list(RunNo, Iter,Likelihood,Beta,WithinVariance,Xsi,Tau,RanTermVariance,BetweenVariance)
  names(V)<-c("RunNo", "Iter","Likelihood","Beta","Variance","Xsi","Tau","RanTermVariance","BetweenVariance")
  return (V)
}

#' @title ReadEstimatesRecord
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @param Dimensions An integer representation of 'ACER ConQuest' object gNDim.
#' @param NPlausibles An integer representation of 'ACER ConQuest' object gNPlausibles.
#' @param n An integer representation of 'ACER ConQuest' object gNCases.
#' @return A list
#' @keywords internal
ReadEstimatesRecord <- function(myFile, Dimensions, NPlausibles, n)
{
  if (Dimensions > 0)
  {
    alla_eap <- vector(mode = "double", Dimensions)
    alla_eaperr <- vector(mode = "double", Dimensions)
    eap <- vector(mode = "double", Dimensions)
    eaperr <- matrix(1:Dimensions * Dimensions, nrow = Dimensions, ncol = Dimensions)
    wle <- vector(mode="double",Dimensions)
    wleerr <- matrix(1:Dimensions*Dimensions,nrow = Dimensions,ncol = Dimensions)
    jml <- vector(mode = "double", Dimensions)
    jmlerr <- matrix(1:Dimensions*Dimensions,nrow = Dimensions,ncol = Dimensions)
    scores <- vector(mode = "double",Dimensions)
    maxscores <- vector(mode = "double", Dimensions)
    if (NPlausibles > 0)
    {
        pvs <- matrix(1:Dimensions * NPlausibles, nrow = Dimensions, ncol = NPlausibles)
    }
  }
     # read data into objects
  for (i in seq_len(Dimensions))
  {
    alla_eap[i] <- ReadDouble(myFile)
  }
  for (i in seq_len(Dimensions))
  {
    alla_eaperr[i] <- ReadDouble(myFile)
  }
  for (i in seq_len(Dimensions))
  {
    eap[i] <- ReadDouble(myFile)
  }
  for (r in seq_len(Dimensions))
  {
    for (c in seq_len(Dimensions))
    {
      eaperr[r,c] <- ReadDouble(myFile)
    }
  }
  for (i in seq_len(Dimensions))
  {
    wle[i] <-ReadDouble(myFile)
  }
  for (r in seq_len(Dimensions))
  {
    for (c in seq_len(Dimensions))
    {
      wleerr[r,c] <-ReadDouble(myFile)
    }
  }
  for (r in seq_len(Dimensions))
  {
    for (c in seq_len(NPlausibles))
    {
      pvs[r,c] <- ReadDouble(myFile)
    }
  }
  for (i in seq_len(Dimensions))
  {
    jml[i] <- ReadDouble(myFile)
  }
  for (r in seq_len(Dimensions))
  {
    for (c in seq_len(Dimensions))
    {
      jmlerr[r,c] <- ReadDouble(myFile)
    }
  }
  for (i in seq_len(Dimensions))
  {
    scores[i] <-ReadDouble(myFile)
  }
  for (i in seq_len(Dimensions))
  {
    maxscores[i] <-ReadDouble(myFile)
  }
  fit <- ReadDouble(myFile)
  weight <- ReadDouble(myFile)
  pid <- n
  V <- list(
      pid, alla_eap, alla_eaperr, eap, eaperr, wle, 
      wleerr, pvs, jml, jmlerr, scores, maxscores, fit, weight
  )
  names(V) <- c(
      "pid", "alla_eap", "alla_eaperr", "eap", "eaperr", "wle",
      "wleerr", "pvs", "jml", "jmlerr", "scores","maxscores", "fit", "weight"
  )
  return(V)
}

#' @title ReadAllCaseEstimates
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @param Dimensions An integer representation of 'ACER ConQuest' object gNDim.
#' @param N An integer representation of 'ACER ConQuest' object gNCases
#' @param NPlausibles An integer representation of 'ACER ConQuest' object gNPlausibles.
#' @return A list
#' @keywords internal
ReadAllCaseEstimates <- function(myFile,Dimensions,N,NPlausibles)
{
    V <- list()
    chainLen <- ReadInteger(myFile) # the chain length can be updated by subsequent calls to estimate, gNPlausibles is not the number of PVs in the last run
    for (i in seq_len(N))
    {
        V[[i]] <-ReadEstimatesRecord(
            myFile = myFile,
            Dimensions = Dimensions,
            NPlausibles = chainLen,
            n = i
        )
    }
    return(V)
}

#' @title ReadDataRecord
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadDataRecord <- function(myFile)
{
    Pid <- ReadInteger(myFile)+1 # note in CQ this is a seq from 0:n-1 cases, this ensures the PID is the same as the seqnum in gAllCaseEstimates
    Rsp <- ReadInteger(myFile)
    Item <- ReadInteger(myFile)
    PreKeyRsp <- ReadInteger(myFile)
    RspFlag <- ReadInteger(myFile)
  V <- list(Pid,Rsp,Item,PreKeyRsp,RspFlag)
  names(V)<-c("Pid","Rsp","Item","PreKeyRsp","RspFlag")
  return (V)
}

#' @title ReadAllResponseData
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @param N An integer representation of 'ACER ConQuest' object gNCases.
#' @return A list
#' @keywords internal
ReadAllResponseData <- function(myFile,N)
{
    V <- list()
    for (i in seq_len(N))
    {
        V[[i]]=ReadDataRecord(myFile)
    }
    return (V)
}

#' @title ReadADesignMatrices
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @param Columns An integer representation of 'ACER ConQuest' object gNParameters.
#' @param Items An integer representation of 'ACER ConQuest' object gNGins.
#' @param ItemSteps An integer representation of 'ACER ConQuest' object gItemSteps.
#' @return A list
#' @keywords internal
#' @description ReadSys Read the A design matrix (A list of length gNGins of matrices.  For each matrix the number of rows is gItemSteps (for that item) and the number of columns is gNParameters_C).
ReadADesignMatrices <- function(myFile,Columns,Items,ItemSteps)
{
    V <- list()      # this is a list of matrices, for review
    if (Columns == 0) return(V)
    for (i in seq_len(Items))
    {
      if (ItemSteps[[i]]>0)
      {
            V[[i]] <-matrix(1:ItemSteps[[i]]*Columns,nrow = ItemSteps[[i]],ncol = Columns)
            for (r in seq_len(ItemSteps[[i]]))
            {
                for (c in seq_len(Columns))
                {
                   V[[i]][r,c] <-ReadDouble(myFile);
                }
            }
        }
        else
        {
            V[[i]]=matrix()
        }
    }
    return(V)
}

#' @title ReadBDesignMatrices
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @param Items An integer representation of 'ACER ConQuest' object gNGins.
#' @param ItemSteps An integer representation of 'ACER ConQuest' object gItemSteps.
#' @return A list
#' @keywords internal
#' @description  ReadSys Read the B design matrix (A list of length gNGins of lists, one per item. For each item a list of length gItemSteps of matrices).
ReadBDesignMatrices <- function(myFile,ItemSteps,Items)
{
    V <- list()      # this will be a 2D list of matrices, for review
    for (i in seq_len(Items))
    {
        V[[i]] <-list()
        TempIndex <- ItemSteps[[i]]
        if (TempIndex==0)TempIndex <- 1
        for (j in seq_len(TempIndex))
        {
                 V[[i]][[j]] <-ReadMatrix(myFile);
        }
    }
    return(V)
}

#' @title ReadCDesignMatrices
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @param Dimensions An integer representation of 'ACER ConQuest' object gNDim.
#' @param Items An integer representation of 'ACER ConQuest' object gNGins.
#' @param ItemSteps An integer representation of 'ACER ConQuest' object gItemSteps.
#' @return A list
#' @keywords internal
#' @description  ReadSys Read the C design matrix (A list of length gNGins of lists, one per item. For each item a list of length gItemSteps of matrices).
ReadCDesignMatrices <- function(myFile,Dimensions,ItemSteps,Items)
{
    #print(paste(Dimensions, " : printing Dimensions - gNDim")); # debug
  #print(paste(ItemSteps, " : printing ItemSteps - gItemSteps")); # debug
  #print(paste(Items, " : printing Items - gNGins")); # debug

  V <- list()
    for (d in seq_len(Dimensions))
    {
      # print(d); print("d in seq_len(Dimensions)") # debug
      V[[d]] <-list()
        for (i in seq_len(Items))
        {
          #print(paste(i, ": item. from call: (i in seq_len(Items))")) # debug
          V[[d]][[i]] <-list()
          TempIndex <- ItemSteps[[i]]
          #print(paste(ItemSteps[[i]], ": item steps for item i, from call: ItemSteps[[i]]")) # debug
      if (TempIndex==0)TempIndex <- 1
          #print(paste(TempIndex, ": TempIndex from call: TempIndex <- ItemSteps[[i]]")) # debug
      for (k in seq_len(TempIndex))
      {
        #print(paste(k, ": kth item step from call, seq_len(ItemSteps[[i]])")) # debug
              V[[d]][[i]][[k]] <-ReadMatrix(myFile);
      }
    }
    }
  return(V)
}

#' @title ReadYOneCase
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @param NReg An integer representing the number of regressors in the model, a representation of 'ACER ConQuest' object gNReg.
#' @return A list
#' @keywords internal
ReadYOneCase <- function(myFile,NReg)
{
    Y <- vector(mode="double",NReg)
    Weight <- ReadDouble(myFile)
    for (i in seq_len(NReg))
    {
        Y[i] <-ReadDouble(myFile)
    }
    V <- list(Weight,Y)
  names(V)<-c("Weight","Y")
  return (V)
}

#' @title ReadAllY
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @param N An integer representation of 'ACER ConQuest' object gNCases
#' @param NReg An integer representing the number of regressors in the model, a representation of 'ACER ConQuest' object gNReg.
#' @keywords internal
ReadAllY <- function(myFile,N,NReg)
{
    V <- list()
    for (n in seq_len(N))
    {
        V[[n]] <-ReadYOneCase(myFile,NReg = NReg)
    }
    return(V)
}

#' @title ReadGroupsOneCase
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @param GroupVariables A list of group variables for this case.
#' @param AllVariables A list of variables for this case.
#' @param CaseNum An integer representing the case number used in the lookup tables.
#' @return A list
#' @keywords internal
ReadGroupsOneCase <- function(myFile, GroupVariables, AllVariables, CaseNum)
{
  Dummy <- readChar(myFile,1)
    NGvars <- length(GroupVariables$XV)
    GData <- vector()
    V <- list()
    if (NGvars==0)return (V)
    GData <- vector(mode="character",NGvars)
    for (k in seq_len(NGvars))
    {
      WhichVarNum <- GroupVariables$XV[[k]]
      WhichVar <- AllVariables[[WhichVarNum+1]]
        L <- WhichVar$End[[1]]-WhichVar$Begin[[1]]+1
        # when missing values (" ", ".") in group data
        # sys file encodes embedded nuls
        # this is a safe work around
        # tS <- readBin(myFile, "raw", L)
        # if (any(tS == as.raw(0))) {
        #   tS <- charToRaw(paste0(rep(" ", L), collapse = ""))
        # }
        GData[k] <- readCharSafe(myFile, L)
        #if (nchar(GData[k]) != L) {
        #  warning("Group data contained missing value - file may not be read correctly")
        #  print(paste0("CaseNum: ", CaseNum))
        #  seek(con = myFile, where = (-1*L)) # rewind file
        #  readBin(myFile, "raw", L) # re-read raw - safe if some embedded nulls
        #  GData[k] <- paste0(rep(" ", L), collapse = "") # empty value
        #}
    }
    V <- list(CaseNum, GData)
    names(V) <- c("CaseNum", "GData")

    return(V)
}

#' @title ReadAllGroupsData
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @param N An integer representation of 'ACER ConQuest' object gNCases.
#' @param GroupVariables A list of group variables.
#' @param AllVariables A list of variables.
#' @return A list
#' @keywords internal
ReadAllGroupsData <- function(myFile,N,GroupVariables,AllVariables)
{
    V <- list()
    for (n in seq_len(N))
    {
        V[[n]] <- ReadGroupsOneCase(
          myFile = myFile,
          GroupVariables = GroupVariables,
          AllVariables = AllVariables,
          CaseNum = n
        )
    }
    #print(paste0("exited on casenum = ", n)) # debug
    #print(paste0("N was passed in as  = ", N)) # debug
    return(V)
}

#' @title ReadMatrixVars
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadMatrixVars <- function(myFile)
{
  # intitate lists
  m <- list()
  # read length of list of matrix objects
  nMatricies <- ReadInteger(myFile)
  for (n in seq_len(nMatricies))
    {
      myTempName <- ReadString(myFile)
      m[[myTempName]] <- ReadMatrix(myFile)
    }
  return(m)
}

#' @title ReadPoint
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadPoint <- function(myFile)
{
    x <- ReadDouble(myFile)
    y <- ReadDouble(myFile)
    z <- ReadDouble(myFile)
  Label <- ReadString(myFile)
  Point <- list(x,y,z,Label)
  names(Point)<-c("x","y","z","Label")
  return(Point)
}

#' @title ReadSeries
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadSeries <- function(myFile)
{
    SType <- ReadInteger(myFile)
    # print(paste0("SType = ", SType))
    PointCount <- ReadInteger(myFile)
    # print(PointCount)
    Points <- list()
    for (i in seq_len(PointCount))
    {
            # print(p("point number ",i))
            Points[[i]]=ReadPoint(myFile)
    }
    MinX <- ReadDouble(myFile)
    MaxX <- ReadDouble(myFile)
    MinY <- ReadDouble(myFile)
    MaxY <- ReadDouble(myFile)
    Name <- ReadString(myFile)
    DrawSeries <- ReadBoolean(myFile)
    DrawPoints <- ReadBoolean(myFile)
    JoinPoints <- ReadBoolean(myFile)
    LabelPoints <- ReadBoolean(myFile)
    LineWidth <- ReadInteger(myFile)
    PointColour <- ReadInteger(myFile)
    LineColour <- ReadInteger(myFile)
    PointStyle <- ReadInteger(myFile)
    LabelStyle <- ReadInteger(myFile)
    LineStyle <- ReadInteger(myFile)
    Series = list(
                SType = SType,
                PointCount = PointCount,
                Points = Points,
                MinX = MinX,
                MaxX = MaxX,
                MinY = MinY,
                MaxY = MaxY,
                Name = Name,
                DrawSeries = DrawSeries,
                DrawPoints = DrawPoints,
                JoinPoints = JoinPoints,
                LabelPoints = LabelPoints,
                LineWidth = LineWidth,
                PointColour = PointColour,
                LineColour = LineColour,
                PointStyle = PointStyle,
                LabelStyle = LabelStyle,
                LineStyle = LineStyle
                )
    # print(Series)
    return(Series)
}

#' @title ReadGraph
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadGraph <- function(myFile)
{
    GType <- ReadInteger(myFile)
    # print(GType)
    NSeries <- ReadInteger(myFile)
    #print(NSeries)
    Series <- list()
    for (i in seq_len(NSeries))
    {
        #print("Series ",i)
        Series[[i]] <- ReadSeries(myFile)
    }
    MinX <- ReadDouble(myFile)
    MaxX <- ReadDouble(myFile)
    MinY <- ReadDouble(myFile)
    MaxY <- ReadDouble(myFile)
    PointColourIndex <- ReadInteger(myFile)
    LineColourIndex <- ReadInteger(myFile)
    PointStyleIndex <- ReadInteger(myFile)
    FreeTextCount <- ReadInteger(myFile)
    L <- ReadInteger(myFile)
    GraphTitleText <- ReadString(myFile)
    GraphSubTitleText <- ReadString(myFile)
    xAxisLabelText <- ReadString(myFile)
    yAxisLabelText <- ReadString(myFile)
    DifficultyLabelText <- ReadString(myFile)
    FitLabelText <- ReadString(myFile)
    NStrings <- L-6;
    Strings <- list()
    # print(p("other strings ",NStrings))
    for (i in seq_len(NStrings))
    {
        Strings[[i]]=ReadPoint(myFile)
    }
    Graph <- list(
                GType = GType,
                NSeries = NSeries,
                Series = Series,
                MinX = MinX,
                MaxX = MaxX,
                MinY = MinY,
                MaxY = MaxY,
                PointColourIndex = PointColourIndex,
                LineColourIndex = LineColourIndex,
                PointStyleIndex = PointStyleIndex,
                GraphTitleText = GraphTitleText,
                GraphSubTitleText = GraphSubTitleText,
                xAxisLabelText = xAxisLabelText,
                yAxisLabelText = yAxisLabelText,
                DifficultyLabelText = DifficultyLabelText,
                FitLabelText = FitLabelText,
                NStrings = NStrings,
                Strings = Strings
                )
      return(Graph)
    }

#' @title ReadRandomStructure
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list
#' @keywords internal
ReadRandomStructure <- function(myFile)
{
  randomStructureList <- list()
  termName <- ReadString(myFile)
  termNumber <- ReadInteger(myFile)
  randomL <- ReadBooleanList(myFile)
  randomV <- ReadMatrix(myFile)
  randomStructureList[["termName"]] <- termName
  randomStructureList[["termNumber"]] <- termNumber
  randomStructureList[["randomL"]] <- randomL
  randomStructureList[["randomV"]] <- randomV
  return(randomStructureList)
}

#' @title ReadGExportOptions
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list of a single ExportOption (e.g., used in ExportXsi())
#' @keywords internal
ReadGExportOptions <- function(myFile)
{
  FileFormat <- ReadInteger(myFile)
  Sort <- ReadInteger(myFile)
  DataType <- ReadInteger(myFile)
  FileName <- ReadString(myFile)
  V <- list(FileFormat, Sort, DataType, FileName)
  names(V) <- c("File Format", "Sort", "Data Type", "File Name")
  return(V)
}

#' @title ReadGExportOptionsList
#' @param myFile An uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest'.
#' @return A list of ExportOptions
#' @keywords internal
ReadGExportOptionsList <- function(myFile)
{
  N <- ReadInteger(myFile)
  Value <- list()
  for (i in seq_len(N))
  {
    Value[[i]] <- ReadGExportOptions(myFile)
  }
  return(Value)
}