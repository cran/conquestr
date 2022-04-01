#' @title q3ExpCorrect
#'
#' @description Helper function to apply correction to correlation matrix. When working with standardised residuals,
#' the expectation of the correlations is -1/(L-1) rather than 0 See DOI: 10.1177/0013164410379322
#'
#' @param myCorMat A correlation matrix.
#' @return A correlation matrix with the Q3 statistic correction applied.
q3ExpCorrect<- function(myCorMat){
  # takes in a correlation matrix, returns a matrix corrected for expecation that off diaganals = −1/(I − 1)
  if(!all(diag(myCorMat) == 1)) stop("you must provide a correlation matrix")
  myCorrection<- -1/(nrow(myCorMat)-1)
  myResult<- myCorMat
  myResult[lower.tri(myResult)]<- myResult[lower.tri(myResult)] - myCorrection
  myResult[upper.tri(myResult)]<- myResult[upper.tri(myResult)] - myCorrection
  myResult[myResult > 1]<- 1
  myResult[myResult < -1]<- -1
  return(myResult)
}

#' @title fisherTrnsfrm
#'
#' @description Helper function to apply Fisher's transformation to a correlation matrix.
#'
#' @param myCorMat A correlation matrix.
#' @return A correlation matrix with Fisher's transform applied to values -1 > x > 1.
fisherTrnsfrm<- function(myCorMat){
  # takes in a correlation matrix, returns a matrix where correaltions are fisher tranformed (r_ij becomes z_ij)
  if(!all(diag(myCorMat) == 1)) stop("you must provide a correlation matrix")
  myResult<- myCorMat
  myResult[abs(myCorMat) < 1]<- atanh(myCorMat[abs(myCorMat) < 1])
  return(myResult)
}

#' @title steigerStat
#'
#' @description Function to cacluate the Steiger statistic. The Steiger statistic is a test of independance of
#' the standardised residuals ((O-E)/sqrt(Var(E))), where Var(E) = p(x)/(1-p(x)).
#'
#' @param myDat A data frame or matrix containing standardised residuals.
#' @param q3Adj A bool indicating whether the Q3 correction should be applied.
#' @param fisher A bool indicating whether the Fisher Transform should be applied.
#' @param dfAdj A bool indicating whether the df should be adjusted for sample size, L, and targeting.
#' If dfAdj is TRUE, then you must pass in the optional argument `tmp` (test-person match)
#' @param tpm A number indicating the test-person match, where 0 indicates that
#' mean item difficulty is equal to mean person ability, and -1 indcates that mean item difficulty is 1 logit below mean person ability.
#' @return A list of class "steigerStat" with the Steiger Statistic, correlation matrix, and chi square test.
#' @importFrom stats cor na.omit pchisq qchisq
steigerStat<- function(myDat, q3Adj = TRUE, fisher = TRUE, dfAdj = FALSE, tpm){
  if(missing(tpm)) tpm<- 0
  myResultList<- list()
  if(!"matrix" %in% class(myDat)){
    # should test if data frame, and then cast to numeric
    # shoudl try this and handle exceptions
    myDatT<- myDat
    for(i in seq_len(ncol(myDatT))){
      myDatT[ , i]<- as.numeric(myDatT[ , i])
    }
    myDatT<- as.matrix(myDatT)
  } else {
    myDatT<- myDat
  }
  myN<- nrow(myDatT)
  myL<- ncol(myDatT)
  myDf<- myL*(myL-1)/2
  myCorMat<- cor(na.omit(myDatT))
  if(q3Adj) myCorMat<- q3ExpCorrect(myCorMat)
  if(fisher) myCorMat<- fisherTrnsfrm(myCorMat)
  if(dfAdj){
    myDf<- myDf + 1/70*myN - 1/4*myL - 12*tpm - 1/40*myN*tpm + 2/3*myL*tpm
  }
  myResultList[["Cor"]]<- myCorMat
  myResult<- (myN - 3) * sum(myCorMat[lower.tri(myCorMat)]^2)
  myResultList[["steigerStat"]]<- myResult
  myChiSq<- pchisq(myResult, myDf, lower.tail = FALSE)
  myResultList[["Chisq_p"]]<- myChiSq
  myResultList[["Chisq_crit"]]<- qchisq(0.95, myDf)
  myResultList[["N"]]<- myN
  myResultList[["L"]]<- myL
  myResultList[["DF"]]<- myDf
  return(myResultList)
}
