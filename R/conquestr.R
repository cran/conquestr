#' @useDynLib conquestr
#' @importFrom Rcpp sourceCpp
NULL

#' @rawNamespace exportPattern("^[[:alpha:]]+") # this exports all functions that start with an alphanumeric charachter so that every function in the package is visible (otherwise need to manually add exports to NAMESPACE)
#' @rawNamespace if (.Platform$OS.type=="windows") importFrom(utils,shortPathName)

#' @include conquestrFunc.R


# for vignette or default we can access files like this: system.file("extdata", "ConQuestTest.cqc", package = "conquestr")

# defaults for cqc is ConQuestTest.cqc, default for cqInstallLocation is defaultCqLoc
# consider using this in the future https://www.tidyverse.org/blog/2018/09/processx-3.2.0/

#' @title ConQuestCall
#'
#' @description Call 'ACER ConQuest' and run a control file.
#'
#' @param cqInstallLocation The location of the 'ACER ConQuest' executable.
#' @param cqc The locaiton of the control file to be run.
#' @param stdout On Mac only, can be toggled to NULL (or a connection) to supress output to R console.
#' @return prints 'ACER ConQuest' output to stdout.
#' @examples
#' \dontrun{
#' ConQuestCall(cqInstallLocation = file.path("/Applications", "ConQuest BETA", "ConQuest"))
#' }
ConQuestCall<- function(cqInstallLocation, cqc, stdout = ""){ # note cqc must not use illegal ConQuest chars - e.g., ~ in relative paths

  if(missing(cqInstallLocation)){

    stop("you must specify where the ConQuest executable is")

  }

  if(missing(cqc)){

    cqc<- system.file("extdata", "ConQuestAbout.cqc", package = "conquestr")

  }

  ##_______________________ CALL CONQUEST!

  # WIN: when installed by the MSI the exe is "ConQuest4Console.exe", when built from source it is ConQuestx64console.exe

  if(Sys.info()["sysname"] == "Windows")
  {
    #cqInstallLocationPath<- strsplit(cqInstallLocation, "[/|\\]*\\w+\\.+exe")
    #cqInstallLocationExe<- regmatches(cqInstallLocation, regexpr("(\\w+\\.+exe)", cqInstallLocation))
    shell(
      paste(
        shortPathName(cqInstallLocation),
        paste('"', cqc, '"', sep = ""),  # just in case the file.path has spaces in it, this wraps it in quotes
        "true"
      )
    )
  } else if(Sys.info()["sysname"] == "Darwin") {

    system2(
        cqInstallLocation,
        paste0('"', cqc, '"', ' true'),
        stdout = stdout, stderr = ""
    )

  } else {
    stop("your operating system is not currenbtly supported. ConQuest is available on Windows and Mac OS")
  }

}


#' @title ConQuestSys
#'
#' @description Read an ''ACER ConQuest'' system file created by a `put` command in 'ACER ConQuest'. The system file must not be compressed. Use the option `compressed=no`` in the put command within 'ACER ConQuest'.
#'
#' @param myCqs The location of an uncompresed 'ACER ConQuest' system file created by 'ACER ConQuest' > 4.30.2.
#' @return A list containing the data objects created by 'ACER ConQuest'.
#' @examples
#' mySysData<- ConQuestSys()
#' myEx1SysData<- ConQuestSys(myCqs = system.file("extdata", "Ex1.cqs", package = "conquestr"))
#' \dontrun{
#' # if you run the above example this will return your original 'ACER ConQuest' syntax.
#' cat(unlist(myEx1SysData$gCommandHistory))
#' }
ConQuestSys<- function(myCqs){

  if(missing(myCqs)){

    message("no system file provide, loading the example system file instead")
    systemFile<- list()
    myFile<- file(system.file("extdata", "mySysFile.cqs", package = "conquestr"), "rb")
    r<-invisible(ReadSys(myFile))
    on.exit(
      close(myFile)
    )

    } else {

    # create required lists
    systemFile<- list()

    myFile<- file(myCqs, "rb")
    r<-invisible(ReadSys(myFile))
    on.exit(
      close(myFile)
    )


    }

  return(r)

}



#' @title ConQuestRout
#'
#' @description Read an ''ACER ConQuest'' rout file created by a `plot` command in 'ACER ConQuest'.
#'
#' @param myRout The location of an 'ACER ConQuest' rout file created by 'ACER ConQuest' > 5.1.4.
#' @return A list containing the data objects created by 'ACER ConQuest' plot command.
#' @examples
#' myPlot<- ConQuestRout()
#' \dontrun{
#' # if you run the above example you will have the points from a plot ICC command.
#' str(myPlot)
#' }
ConQuestRout<- function(myRout){

  if(missing(myRout)){

    message("no rout file provide, loading the example rout file instead")
    routFile<- list()
    myFile<- file(system.file("extdata", "myIcc.rout", package = "conquestr"), "rb")
    r<-invisible(ReadGraph(myFile))
    on.exit(
      close(myFile)
    )

  } else {

    # create required lists
    routFile<- list()
    myFile<- file(myRout, "rb")
    r<-invisible(ReadGraph(myFile))
    on.exit(
      close(myFile)
    )


  }

  # append class to r (used later for dispaching)
  myRoutType<- routType(r)
  # append class so we can do dispaching
  class(r)<- append(class(r), myRoutType)
  return(r)

}
