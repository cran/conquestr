#' @include conquestr.R

packageStartupMessage("\nConQuestR requires a copy of ACER ConQuest version 4.")

#
# required functions
#

#' @title zapNulls
#'
#' @description Zaps NULL values embedded in ConQuest data objects.
#'
#' @param x a data frame.
#' @return x.
#' @keywords internal
zapNulls <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#' @title zapSystemMissing
#'
#' @description Coerce ConQuest system missing values to NA.
#'
#' @param x a data frame.
#' @return x
#' @keywords internal
zapSystemMissing<- function(x){
  if(!(class(x) == "data.frame")){
    stop("x must be a data.frame")
  } else {
    for(i in seq_along(x)){

      # if column is not numeric
      if(!(class(x[[i]]) == "numeric")){
        next
      } else {
        # print(names(myDemo$myConQuestData[i]))
        for(j in seq_along(x[[i]])){
          if(all.equal(x[[i]][j], -1.7976931348e+308) == TRUE){ # this is the required level of precision to get this to return true, -1.797693e+308 will retunr FALSE
            x[[i]][j]<- NA
          } else {
            next
          }
        }
      }
    }
  }
  return(x)
}
