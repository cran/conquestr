#' @title postProcessSys
#'
#' @description Post processing of ConQuest system file created by ConQuestSys.
#'   Adds names and attributes (e.g., descriptions) to objects in list.
#' @param sysFile A list of class conQuestSysFile
#' @return A list containing the data objects created by 'ACER ConQuest'.
#'  Object is of class conQuestSysFile.
#' @seealso conquestr::ConQuestSys()
#' @keywords internal
postProcessSys <- function(sysFile) {
  myDebug <- TRUE
  if (!any(class(sysFile) %in% "conQuestSysFile")) stop("must be a system file object")

  sysFileT <- sysFile

  for (i in seq_len(length(sysFileT))) {
    if (myDebug) print(paste0("names(sysFileT)[i]: ", names(sysFileT)[i]))
    if ((names(sysFileT)[i]) == "gNCases") { # better way to do this than name-by-name?
      sysFileT[[i]] <- postProcessGncases(sysFileT[[i]], myDebug)
    } else {

    }
  }
  return(sysFileT)
}

#' @title postProcessGncases
#'
#' @description internal function to processing gNCases
#' @param this_gNCases An object of type gNCases from a list of class conQuestSysFile
#' @param debug is this a debug run?
#' @return A list containing a documented gNCases object
#' @seealso conquestr::postProcessSys()
#' @keywords internal
postProcessGncases <- function(this_gNCases, debug) {
  tmpNames <- c(
    "RETAINED_CASES", "ALL_CASES", "NOZEROWGHTKEPT_CASES", "COMPLETEY_CASES",
    "COMPLETEYSOMER_CASES", "ANY_PZ"
  )
  for (j in seq_len(length(this_gNCases))) {
    if (j <= length(this_gNCases)) {
      names(this_gNCases)[j] <- tmpNames[j]
        if (debug) print(
          paste0(
            "j: ", j,
            " names(this_gNCases[j]): ", names(this_gNCases[j])
          )
        )
      # TODO: add attributes with comments about what each element is
      # 0    // cases not "dropped" ie retained after drops and keeps
      # 1    // all cases in file
      # 2    // retained in file with non-zero weight
      # 3    // complete Y variables
      # 4    // complete Y and some responses
      # 5    // cases that have a zero or perfect on any dim)
    }
  }
  return(this_gNCases)
}