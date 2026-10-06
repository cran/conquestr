#' @useDynLib conquestr
#' @importFrom Rcpp sourceCpp
NULL

# this exports all functions so that every function in the package is visible
#    (otherwise need to manually add exports to NAMESPACE)
#' @rawNamespace exportPattern("^[[:alpha:]]+")
#' @rawNamespace if (.Platform$OS.type=="windows") importFrom(utils,shortPathName)

packageStartupMessage("\n`conquestr` requires a copy of ACER ConQuest version <= 5.40.0")

# for vignette or default we can access files like this: system.file("extdata", "ex1.cqc", package = "conquestr")
# consider using this in the future https://www.tidyverse.org/blog/2018/09/processx-3.2.0/

#' @title ConQuestCall
#'
#' @description Call an instance of 'ACER ConQuest' at the command line and run a control file (syntax).
#'
#' @param cqc The location of the control file (syntax) to be run.
#' @param cqExe The path to the 'ACER ConQuest' executable. Note, if this argument is missing, conquestr
#'    will find a local installation of ACER ConQuest by first searching the default installation locations
#'    (Program Files on Windows and Applications on Mac) then searching other local directories (Appdata
#'    and the HOME path).
#' @param stdout On Mac only, can be toggled to NULL (or a connection) to suppress output to R console.
#' @return prints 'ACER ConQuest' output to stdout.
#' @examples
#' \dontrun{
#' ConQuestCall()
#' }
ConQuestCall <- function(cqc, cqExe, stdout = "") {

  if (missing(cqExe)) {
    cqExe <- findConQuestExe()
  }

  if (missing(cqc)) {
    cqc <- system.file("extdata", "conquestabout.cqc", package = "conquestr")
  }

  ##_______________________ CALL CONQUEST!

  if (Sys.info()["sysname"] == "Windows")
  {
    system2(
      "cmd.exe",
      input = paste0(
        shQuote(cqExe, type = "cmd"),
        paste0(" "),
        paste0("\"", cqc, "\""),  # just in case the file.path has spaces in it, this wraps it in quotes
        " true"),
      # this should also work but i'm a bit scared to change it given the above has been working on win for a while
      # command = cqExe,
      # args = c(paste0('"', cqc, '"'), "true"),
      stdout = stdout
    )
  } else if (Sys.info()["sysname"] == "Darwin") {

    system2(
        command = cqExe,
        args = c(paste0('"', cqc, '"'), "true"),
        stdout = stdout,
        stderr = ""
    )

  } else {
    stop("your operating system is not currently supported. ConQuest is available on Windows and Mac OS")
  }

}


#' @title ConQuestSys
#'
#' @description Read an ''ACER ConQuest'' system file created by a `put` command in 'ACER ConQuest'.
#' The system file must not be compressed. Use the option `compressed=no`` in the put command within 'ACER ConQuest'.
#'
#' @param myCqs The location of an uncompressed 'ACER ConQuest' system file created by 'ACER ConQuest' > 4.35.
#' @param isMini A boolean, set to TRUE if the system file is a _mini_ system file created by 'ACER ConQuest'
#'   command put with option "mini = yes".
#' @return A list containing the data objects created by 'ACER ConQuest'.
#' @examples
#' mySysData <- ConQuestSys()
#' myEx1SysData <- ConQuestSys(myCqs = system.file("extdata", "mysysfile.cqs", package = "conquestr"))
#' \dontrun{
#' # if you run the above example this will return your original 'ACER ConQuest' syntax.
#' cat(unlist(myEx1SysData$gCommandHistory))
#' }
ConQuestSys <- function(myCqs, isMini = FALSE) {
  isDebug <- FALSE
  if (isDebug) message("Begin call to `ConQuestSys`")
  if (isDebug) message("  Check if `myCqs` is provided")
  if (missing(myCqs))
  {
    message("no system file provided, loading the example system file instead")
    # anon function to get cqs - this is due to weird behaviour on Fedora
    myCqsGet <- function() system.file("extdata", "mysysfile.cqs", package = "conquestr")
    myCqs <- tryCatch(
      {
        tmp <- myCqsGet()
        if (tmp == "") warning("failed to read from extdata folder")
        myCqsGet()
      },
      error = function(e) {
        print(tmp)
        e$message <- paste0(e$message, " (in ", myCqs, ")")
        stop(e)
      }
    )
  }

  if (!file.exists(myCqs)) stop("system file not found: ", myCqs)
  sysBytes <- readBin(myCqs, what = "raw", n = file.size(myCqs))

  if (isDebug) message("    Check if  `myCqs` is compressed")
  # uncompressed files begin with the string "uncompressed"; compressed files
  # are a zlib stream (CMF byte with deflate method 8, and (CMF*256 + FLG) %% 31 == 0)
  compressedString <- tryCatch(
    ReadString(.cqReaderNew(sysBytes)),
    # silent: an error here must not break knitr or shiny workflows
    error = function(e) NULL
  )
  hdr <- as.integer(sysBytes[1:2])
  isZlib <- length(sysBytes) >= 2 && bitwAnd(hdr[1], 15L) == 8L &&
    (hdr[1] * 256L + hdr[2]) %% 31L == 0L
  if (isDebug) message("    End check if  `myCqs` is compressed")

  if (!identical(compressedString, "uncompressed") && isZlib) {
    message("System file is compressed. Decompressing ... ")
    sysBytes <- DecompressSys(sysBytes)
    message(
      "complete"
    )
  }

  r <- tryCatch(
    {
      conquestr::ReadSys(.cqReaderNew(sysBytes), isMini)
    },
    error = function(e) {
      e$message <- paste0(e$message, " (in ", myCqs, ")")
      stop(e)
    }
  )
  return(r)

}



#' @title ConQuestRout
#'
#' @description Read an 'ACER ConQuest' rout file (a binary file) 
#'   created by a [`plot`](https://conquestmanual.acer.org/s4-00.html#commandPlot) 
#'   command in 'ACER ConQuest'.
#'
#' @param myRout The path to the binary rout file.
#' @return A list containing the data objects to recreate 'ACER ConQuest' plots.
#' @examples
#' myPlot <- ConQuestRout()
#' \dontrun{
#' # the default example is an ICC plot from Example 1 
#' # <https://conquestmanual.acer.org/s2-00.html#s2-02>.
#' str(myPlot)
#' }
ConQuestRout <- function(myRout) {

  if (missing(myRout)) {
    message("no rout file provided, loading the example rout file instead")
    myRout <- system.file("extdata", "myicc.rout", package = "conquestr")
  }
  if (!file.exists(myRout)) stop("rout file not found: ", myRout)
  r <- ReadGraph(.cqReaderNew(readBin(myRout, what = "raw", n = file.size(myRout))))

  # append class to r (used later for dispatching)
  myRoutType <- routType(r)
  # append class so we can do dispatching
  class(r)<- append(class(r), myRoutType)
  return(r)

}
