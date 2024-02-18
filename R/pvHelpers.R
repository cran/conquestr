#' @title pvMeanVar
#'
#' @description Applies the law of total variance (EVEs law)
#'     to calculate the mean and variance of a set of PVs for one dimension.
#'
#' @param myData A matrix of PVs for one dimension: m PVs by n cases.
#' @return A list containing the mean and variance of the PVs.
pvMeanVar <- function(myData)
{
  nPvs <- ncol(myData)
  # mean and var of each vector of PVs
  # row 1 = means
  # row 2 = vars
  tmpMeanVarPvs <- matrix(NA, ncol = nPvs, nrow = 2)
  for (i in seq(ncol(myData)))
  {
    tmpMeanVarPvs[1, i] <- mean(myData[ , i])
    tmpMeanVarPvs[2, i] <- var(myData[ , i])
  }

  pvM <-  mean(tmpMeanVarPvs[1, ]) # mean of PV means
  pvVw <- mean(tmpMeanVarPvs[2, ]) # mean of PV variances

  # variance
  pvVb <- (1/(nPvs-1)) * sum((tmpMeanVarPvs[1, ] - pvM)^2) # between variance
  pvV <- (1+(1/nPvs)) * pvVb + pvVw
  # if (!covar == "") myResults[[i]]["level"] <- paste0(covar, ": ", myLevels[i])
  myResults <- list()
  myResults[["mean"]] <- pvM
  myResults[["var"]] <- pvV

  return(myResults)
}