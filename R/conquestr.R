#' @useDynLib conquestr
#' @importFrom Rcpp sourceCpp
NULL

# this exports all functions so that every function in the package is visible
#    (otherwise need to manually add exports to NAMESPACE)
#' @rawNamespace exportPattern("^[[:alpha:]]+")
#' @rawNamespace if (.Platform$OS.type=="windows") importFrom(utils,shortPathName)

packageStartupMessage("\nConQuestR requires a copy of ACER ConQuest version <= 5.40.0")

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

  if (missing(myCqs))
  {
    message("no system file provided, loading the example system file instead")
    # anon function to get cqs - this is due to wierd behaviour on Fedora
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

  myMode <- "rb"
  #if (Sys.info()["sysname"] == "Linux") myMode <- "w+b"
  myFile <- file(myCqs, myMode)

  # is this a compressed file?
  compressedString <- tryCatch(
    {
       ReadString(myFile)
    },
    error = function(e) {
      e$message <- paste0("System file is compressed. Decompressing ... ")
      message(e)
    },
    finally = {
      # close and reopen file - rewind is discouraged for Win in man
      close(myFile)
      myFile <- file(myCqs, myMode)
    }
  )

  if (is.null(compressedString)) {
    compressedString <- "compressed"
    myFile <- DecompressSys(myFile)
    message(
      "decompression complete"
    )
  }

  r <- tryCatch(
    {
      invisible(conquestr::ReadSys(myFile, isMini))
    },
    error = function(e) {
      close(myFile)
      e$message <- paste0(e$message, " (in ", myCqs, ")")
      stop(e)
    }
  )
  # tidy up by closing the connection to sysfile on exit
  on.exit(close(myFile), add = TRUE)
  return(r)

}



#' @title ConQuestRout
#'
#' @description Read an ''ACER ConQuest'' rout file created by a `plot` command in 'ACER ConQuest'.
#'
#' @param myRout The location of an 'ACER ConQuest' rout file created by 'ACER ConQuest'
#' @return A list containing the data objects created by 'ACER ConQuest' plot command.
#' @examples
#' myPlot <- ConQuestRout()
#' \dontrun{
#' # if you run the above example you will have the points from a plot ICC command.
#' str(myPlot)
#' }
ConQuestRout <- function(myRout) {

  if (missing(myRout)) {

    message("no rout file provided, loading the example rout file instead")
    routFile <- list()
    myFile <- file(system.file("extdata", "myicc.rout", package = "conquestr"), "rb")
    r <- invisible(ReadGraph(myFile))
    on.exit(
      close(myFile)
    )

  } else {

    # create required lists
    routFile <- list()
    myFile <- file(myRout, "rb")
    r <-invisible(ReadGraph(myFile))
    on.exit(
      close(myFile)
    )


  }

  # append class to r (used later for dispatching)
  myRoutType <- routType(r)
  # append class so we can do dispatching
  class(r)<- append(class(r), myRoutType)
  return(r)

}
