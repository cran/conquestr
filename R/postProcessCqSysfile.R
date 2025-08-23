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
  myDebug <- FALSE
  if (!any(class(sysFile) %in% "conQuestSysFile")) stop("must be a system file object")

  sysFileT <- sysFile

  for (i in seq_len(length(sysFileT))) {
    if (myDebug) print(paste0("names(sysFileT)[i]: ", names(sysFileT)[i]))
    if ((names(sysFileT)[i]) == "gNCases") { # better way to do this than name-by-name?
      sysFileT[[i]] <- postProcess_gNCases(sysFileT[[i]], myDebug)
    } 
    if ((names(sysFileT)[i]) == "gLConstraint") { # better way to do this than name-by-name?
      sysFileT[[i]] <- postProcess_gLConstraint(sysFileT[[i]], myDebug)
    } 
  }
  return(sysFileT)
}

#' @title postProcess_gNCases
#'
#' @description internal function to processing gNCases
#' @param this_gNCases An object of type gNCases from a list of class conQuestSysFile
#' @param debug is this a debug run?
#' @return A list containing a documented gNCases object
#' @seealso conquestr::postProcessSys()
#' @keywords internal
postProcess_gNCases <- function(this_gNCases, debug) {
  tmpNames <- c(
    "RETAINED_CASES",                 # 0    // cases not "dropped" ie retained after drops and keeps
    "ALL_RECORDS",                    # 1    // all "records" in file
    "NOZEROWGHTKEPT_CASES",           # 2    // retained in file with non-zero weight
    "COMPLETEY_CASES",                # 3    // complete Y variables
    "COMPLETEYSOMER_CASES",           # 4    // complete Y and some responses (MML & MCMC estimable)
    "ANY_PZ",                         # 5    // cases that have a zero or perfect on any dim (regardless of missing data)
    "JMLESTIMABLE",                   # 6    // cases that are JML estimable
    "WEIGHTED_COMPLETEY_CASES",       # 7    // weighted (PVWT) complete Y variables (i.e. zero weights are omitted)
    "WEIGHTED_COMPLETEYSOMER_CASES",  # 8    // weighted (MMLWT) complete Y and some responses (MML & MCMC estimable)
    "WEIGHTED_JMLESTIMABLE",          # 9    // weighted (MLEWT) cases that are JML estimable (excludes perfects and zeros)
    "WEIGHTED_MLES"                   # 10   // weighted (MLEWT) cases with MLEs
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
    }
  }
  return(this_gNCases)
}


#' @title postProcess_gLConstraint
#'
#' @description internal function to process gLConstraint
#' @param this_gLConstraint An object of type gLConstraint from a list of class conQuestSysFile
#' @param debug is this a debug run?
#' @return A list containing a documented gLConstraint object
#' @seealso conquestr::postProcessSys()
#' @keywords internal
postProcess_gLConstraint <- function(this_gLConstraint, debug) {
  tmpNames <- c(
    "SETLCONSTRAINTDEFAULT",              # 0    // default location constraints (set implicitly or by "set lconstraints = smart")
    "SETLCONSTRAINTITEMS",                # 1    // item location constraints via "set lconstraints = items"
    "SETLCONSTRAINTCASES",                # 2    // case location constraints via "set lconstraints = cases"
    "SETLCONSTRAINTNONE"                  # 3    // no location constraints via "set lconstraints = none"
  )
  
  if (debug) print(paste0("this_gLConstraint[1] prior: ", this_gLConstraint[1]))
  this_gLConstraint[1] <- tmpNames[this_gLConstraint[1]+1]
  if (debug) print(paste0("this_gLConstraint[1] post: ", this_gLConstraint[1]))
  return(this_gLConstraint)
}