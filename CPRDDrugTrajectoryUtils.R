#===================================================================================================================
#' Retrieves patids from a dataframe
#'
#' @description
#' Retrieves unique patid entries from a CPRD dataset stored in an R dataframe.
#'
#' @details
#' The dataframe must contains a 'patid' column. If the patid column contains any NAs, the code executes but the user is warned.
#' Typically, indication of NAs suggests something went wrong with the creation of the dataframe and not the ability to retrieve patids.
#'
#' @param df Dataframe that contains a patid column.
#'
#' @return List or vector of patid numbers. Returns NULL if the data frame is either NULL or empty.
#' @export
#'
#' @examples
#' idList <- getUniquePatidList(testClinicalDF)
#' medHistory <- constructMedicalHistory(testClinicalDF, NULL, testTherapyDF)
#' idList <- getUniquePatidList(medHistory)
#' print(paste("There are", length(idList), "patients."))
getUniquePatidList <- function(df) {

  dfCheck <- errorChecker(df)
  if(isFALSE(dfCheck)) {
    print("Warning: problem with df argument in getUniquePatidList. Returning NULL.")
    return(NULL)
  }
  uniquePatIDList <- as.list(unique(df$patid))
  checkForNA <- length(which(is.na(uniquePatIDList)))
  if(checkForNA > 0) {
    print(which(is.na(uniquePatIDList)))
    print("There are NA values in your patient ID list. Go back and check your dataframe.")
    print(paste("Number of NA entries:", checkForNA))
  } else {
    names(uniquePatIDList) <- unique(df$patid)
  }

  return(uniquePatIDList)
}


#' Not for external call. Checks whether a function argument is NULL or empty
#'
#' @description
#' This is for developer use only.
#'
#' @param x Dataframe, List, Vector, or S3 objects FirstDrugObject and MedHistory.
#'
#' @return Logical. TRUE if the argument has dimension or FALSE if the argument is NULL or empty.
#' A data type that does not match those expected in the function call will return FALSE.
#' @export
#'
errorChecker <- function(x) {
  argumentName <- deparse(substitute(x))
  returnValue <- FALSE
  if(is.null(x)) {
    print(paste("Error: Passing object", argumentName, "evaluating as NULL."))
  } else {
      #if the argument is not NULL then check its size.
      if(is.data.frame(x)) {
        n <- nrow(x)
      } else if(class(x) == "FirstDrugObject") {
        n <- length(x)
      } else if(is.list(x) | is.vector(x)) {
        n <- length(x)
      } else if(class(x) == "Date") {
        n <- length(x)
      } else {
        print("Unrecognised objects!")
        return(returnValue)
      }

      #if the variable has a zero size
      if(n == 0) {
        print(paste("Warning: The R object", argumentName, "is empty."))
      } else {
        returnValue <- TRUE
      }
  }
  return(returnValue)
}

#===================================================================================================================
#' Combines clinical, referral and therapy records in event date order
#'
#' @description
#' Takes at least one argument and returns a dataframe with the columns
#' patid, eventdate, code, codetype. The column codetype
#' clinical ('c'), referral ('r'), or therapy ('t'). The input argument data frames can contain any number of columns,
#' however, the columns patid, eventdate and medcode/prodcode must be in that order.
#'
#' @details
#' Constructing a medical history using only one dataframe, for example, therapy data, returns
#' a modified therapy dataframe, preserving the event order. Each dataframe input has a very deliberate and
#' strict column order: patid, eventdate and prodcode/medcode. Additional columns can be added, yet they
#' are dropped during the construction of the returned medical dataframe.
#'
#' @param orderedClinicalDF Dataframe containing clinical records with at least patid, eventdate and medcode (in that order).
#' @param orderedReferralDF Dataframe containing referral records with at least patid, eventdate and medcode (in that order).
#' @param orderedTherapyDF Dataframe containing therapy records with at least patid, eventdate and prodcode (in that order).
#'
#' @return Dataframe with columns patid, eventdate, code, and codetype.
#' @export
#'
#' @examples
#' clinicalTherapyDF <- constructMedicalHistory(testClinicalDF,
#'                                              NULL,
#'                                              testTherapyDF)
constructMedicalHistory <- function(orderedClinicalDF=NULL, orderedReferralDF=NULL, orderedTherapyDF=NULL) {
  clinicalCheckDF <- FALSE
  referralCheckDF <- FALSE
  therapyCheckDF <- FALSE
  newColumns <- c("patid","eventdate","code")

  if(!is.null(orderedClinicalDF)) {
    clinicalCheckDF <- errorChecker(orderedClinicalDF)
    if(isFALSE(clinicalCheckDF)) {
      return(NULL)
    } else {
      clinicalReferralColNames <- c("patid","eventdate","medcode")
      if(sum(colnames(orderedClinicalDF) %in% clinicalReferralColNames) >= 3) {
        if(length(colnames(orderedClinicalDF)) != 3) {
          orderedClinicalDF <- orderedClinicalDF[,clinicalReferralColNames]
        }
        checkNames <- colnames(orderedClinicalDF)
        if(isTRUE(checkNames[1] == "patid") & isTRUE(checkNames[2] == "eventdate") & isTRUE(checkNames[3] == "medcode")) {
          colnames(orderedClinicalDF) <- newColumns
        } else {
          print("Warning: Please order orderClinicalDF columns as: patid, eventdate, medcode. Returning NULL.")
          return(NULL)
        }
      } else {
        print("Warning: orderedClinicalDF does not contain the column names patid, eventdate, medcode. Returning NULL.")
        return(NULL)
      }
    }
  }
  if(!is.null(orderedReferralDF)) {
    referralCheckDF <- errorChecker(orderedReferralDF)
    if(isFALSE(referralCheckDF)) {
      return(NULL)
    } else {
      clinicalReferralColNames <- c("patid","eventdate","medcode")
      if(sum(colnames(orderedReferralDF) %in% clinicalReferralColNames) >= 3) {
        if(length(colnames(orderedReferralDF)) != 3) {
          orderedReferralDF <- orderedReferralDF[,clinicalReferralColNames]
        }
        checkNames <- colnames(orderedReferralDF)
        if(isTRUE(checkNames[1] == "patid") & isTRUE(checkNames[2] == "eventdate") & isTRUE(checkNames[3] == "medcode")) {
          colnames(orderedReferralDF) <- newColumns
        } else {
          print("Warning: Please order orderReferralDF columns as: patid, eventdate, medcode. Returning NULL.")
          return(NULL)
        }
      } else {
        print("Warning: orderedReferrallDF does not contain the column names patid, eventdate, medcode. Returning NULL.")
        return(NULL)
      }
    }
  }
  if(!is.null(orderedTherapyDF)) {
    therapyCheckDF <- errorChecker(orderedTherapyDF)
    if(isFALSE(therapyCheckDF)) {
      return(NULL)
    } else {
      therapyColNames <- c("patid","eventdate","prodcode")
      if(sum(colnames(orderedTherapyDF) %in% therapyColNames) >= 3) {
        if(length(colnames(orderedTherapyDF)) != 3) {
          orderedTherapyDF <- orderedTherapyDF[,therapyColNames]
        }
        checkNames <- colnames(orderedTherapyDF)
        if(isTRUE(checkNames[1] == "patid") & isTRUE(checkNames[2] == "eventdate") & isTRUE(checkNames[3] == "prodcode")) {
          colnames(orderedTherapyDF) <- newColumns
        } else {
          print("Warning: Please order orderTherapyDF columns as: patid, eventdate, prodcode Returning NULL.")
          return(NULL)
        }
      } else {
        print("Warning: orderedTherapyDF does not contain the column names patid, eventdate, prodcode Returning NULL.")
        return(NULL)
      }
    }
  }

  if(isFALSE(clinicalCheckDF) & isFALSE(referralCheckDF) & isFALSE(therapyCheckDF)) {
    print("Warning: require at least one valid argument to constructMedicalHistory. Returning NULL.")
    return(NULL)
  }

  orderedClinicalDFSize <- 0
  orderedReferralDFSize <- 0
  orderedTherapyDFSize <- 0

  bitOperator <- 0

  if(!is.null(orderedClinicalDF)) {
    print("Using clinical data.")
    orderedClinicalDFSize <- nrow(orderedClinicalDF)
    orderedClinicalDF <- cbind(orderedClinicalDF, codetype="c")
    bitOperator <- bitOperator + 1
  }
  if(!is.null(orderedReferralDF)) {
    print("Using referral data.")
    orderedReferralDFSize <- nrow(orderedReferralDF)
    orderedReferralDF <- cbind(orderedReferralDF, codetype="r")
    bitOperator <- bitOperator + 2
  }
  if(!is.null(orderedTherapyDF)) {
    print("Using therapy data.")
    orderedTherapyDFSize <- nrow(orderedTherapyDF)
    orderedTherapyDF <- cbind(orderedTherapyDF, codetype="t")
    bitOperator <- bitOperator + 4
  }

  totalSize <- orderedClinicalDFSize + orderedReferralDFSize + orderedTherapyDFSize
  if(totalSize == 0) {
    print("Warning: check that the dataframe arguments contained values. Size of medical history is 0.")
    return(NULL)
  }

  if(bitOperator==1) {
    print("Building with clinical data.")
    medicalHistoryDF <- orderedClinicalDF
  } else if(bitOperator==2) {
    print("Building with referral data.")
    medicalHistoryDF <- orderedReferralDF
  } else if(bitOperator==3) {
    print("Building with clinical and referral data.")
    if(ncol(orderedClinicalDF) != ncol(orderedReferralDF)) {
      print("Warning: The number of clinical and referral columns are not the same. Returning NULL.")
      return(NULL)
    } else {
      medicalHistoryDF <- rbind(orderedClinicalDF, orderedReferralDF)
    }
  } else if(bitOperator==4) {
    print("Building with therapy data.")
    medicalHistoryDF <- orderedTherapyDF
  } else if(bitOperator==5) {
    print("Building with clinical and therapy data.")
    if(ncol(orderedClinicalDF) != ncol(orderedTherapyDF)) {
      print("Warning: The number of clinical and therapy columns are not the same. Returning NULL.")
      return(NULL)
    } else {
      medicalHistoryDF <- rbind(orderedClinicalDF, orderedTherapyDF)
    }
  } else if(bitOperator==7) {
    a <- ncol(orderedClinicalDF)
    b <- ncol(orderedReferralDF)
    c <- ncol(orderedTherapyDF)
    if((a != b) | (a != c) | (c != b)) {
      print("Warning: The number of clinical, referral and therapy columns are not the same. Returning NULL.")
      return(NULL)
    } else {
      medicalHistoryDF <- rbind(orderedClinicalDF, orderedReferralDF, orderedTherapyDF)
    }
  }

  medicalHistoryDF <- dplyr::mutate(medicalHistoryDF, eventdate=as.Date(medicalHistoryDF$eventdate,format="%d/%m/%Y"))
  medicalHistoryDF <- dplyr::arrange(medicalHistoryDF, medicalHistoryDF$patid,medicalHistoryDF$eventdate)

  #class(medicalHistoryDF) <- "MedHistory"
  return(medicalHistoryDF)
}


#' For external call. Breaks a dataframe into a chunk per core.
#'
#' @description
#' This is for multiple-processing. If a dataframe contains 20,000 patient records (thus at least 20,000 rows)
#' and four cores are requested, a list of four dataframes, each containing 5000 patient records is returned.
#'
#' @details
#' Unfortunately, package development in line with CRAN requirements limits the number of cores available in
#' a package to 2! For now, I have hard coded a switch to stop R from taking
#' any more than 2 cores. This will change.
#'
#' @param df Dataframe to divide into chunks.
#' @param numCores Number of cores to use.
#'
#' @return List of dataframes. Each dataframe contains Sum(number_of_patients)/core and the last entry is rounded to ensure all
#' patients are included.
#' @export
#'
#' @examples
#' numCores <- 4
#' dfList <- getParallelDF(testTherapyDF, numCores)
getParallelDF <- function(df, numCores) {
  idList <- getUniquePatidList(df)
  nLength <- length(idList)
  print(paste("Dividing", nLength, "records over", numCores,"groups."))
  patidList <- list()

  resDiv <- nLength/numCores
  if(isTRUE(resDiv%%1==0)) {  #did it divide well? Yes, then this is straight forward to divide
    print("The groups were divided evenly.")
    runningValue <- 1
    for(i in 1:numCores) {
      if(i == 1) {
        patidList[[1]] <- idList[1:resDiv]
        print(paste("Entry", 1, "to entry", resDiv))
        runningValue <- resDiv + 1
      } else if(i==numCores) {
        patidList[[i]] <- idList[runningValue:length(idList)]
        print(paste("Entry", runningValue, "to entry", length(idList)))
      } else {
        patidList[[i]] <- idList[runningValue:(runningValue+resDiv)]
        print(paste("Entry", runningValue, "to entry", runningValue+resDiv))
        runningValue <- runningValue+resDiv+1
      }
    }

  } else {
    print("The groups were not divided evenly.")
    resDiv <- floor(resDiv)
    resDiv_offset <- resDiv+1
    #loop numCores-1, then the final one is the resDiv_offset

    runningValue <- 1
    for(i in 1:numCores) {
      if(i == 1) {
        patidList[[1]] <- idList[1:resDiv]
        print(paste("Entry", 1, "to entry", resDiv))
        runningValue <- resDiv+1
      } else {
        if(i==numCores) {
          patidList[[i]] <- idList[runningValue:nLength]
          print(paste("Entry", runningValue, "to entry", nLength))
        } else {
          patidList[[i]] <- idList[runningValue:(runningValue+resDiv_offset)]
          print(paste("Entry", runningValue, "to entry", runningValue+resDiv_offset))
          runningValue <- runningValue+resDiv_offset+1
        }
      }
    }
  }

  dfList <- list()
  dfCount <- 1
  #go through and get the data from the DF
  for(i in 1:length(patidList)) {
    indDF <- subset(df, df$patid %in% patidList[[i]])
    if(nrow(indDF) > 0) {
      dfList[[dfCount]] <- indDF
      dfCount <- dfCount + 1
    }
  }

  return(dfList)
}

#===================================================================================================================
#type is either "MED" or "PROD", MED is NULL by default and it's checked to ensure there is no clash
# Not to be used by package user.
#
# @param codeVector
# @param descriptionFile
# @param type
#
# @return
# @export
#
# @examples
convertCodesToDescriptions <- function(codeVector, descriptionFile, type=NULL) {
  if(is.null(type)) {
    stop("You must specify either MED or  PROD.")
  }
  descriptionTable <- loadCPRDTabFile(descriptionFile)
  if(type=="MED") {
    names(descriptionTable) <- c("code","B","C","D","E","F","description","H")
  } else {
    names(descriptionTable) <- c("code","B","C","description","E","F","G","H","I","type","date","L")
  }

  descriptionVector <- as.character()
  codeVectorToDisplay <- as.character()

  for(i in 1:length(codeVector)) {
    indCode <- codeVector[i]
    rowEntry<-subset(descriptionTable, descriptionTable$code==indCode)
    if(nrow(rowEntry)==0) {
      description <- "Unknown"
    } else {
      description <- rowEntry$description
    }

    descriptionVector[i] <- description
    codeVectorToDisplay[i] <- codeVector[i]
  }

  resultDF <- data.frame(code=codeVectorToDisplay, description=descriptionVector)

  return(resultDF)
}


#====================================================================================================================
#' Save CPRD dataframe
#'
#' @description
#' Use to save any data frame including all those generated by this package. Call \code{\link{loadCPRDDataframe}}
#' to load the data frame into the R environment. Save from the working directory set in R.
#'
#' @details
#' If a file path has not been provided the current working directory is used.
#'
#' @param dataFrame Dataframe to save.
#' @param fileNameString Output file path with file name and extension.
#'
#' @export
#'
#' @examples
#' clinicalTherapyDF <- constructMedicalHistory(testClinicalDF,
#'                                              NULL,
#'                                              testTherapyDF)
#' saveCPRDDataframe(clinicalTherapyDF, "my_medHistory.Rda")
saveCPRDDataframe <- function(dataFrame, fileNameString) {
  stopifnot(!is.null(dataFrame))
  stopifnot(!is.null(fileNameString))

  print(paste("Saving data to:", fileNameString, sep=""))
  saveRDS(dataFrame,file=fileNameString)
}

#===============================================================
#' Load CPRD dataframe
#'
#' @description
#' Use this function to load any data frame including all those generated by this package. Call \code{\link{saveCPRDDataframe}}
#' to save a dataframe.
#'
#' @details
#' Unless a full fie path is provided the working directory will be used.
#'
#' @param fileNameString File path with file name and extension.
#'
#' @return dataframe. Returns NULL if the file is not found.
#' @export
#'
#' @examples
#' clinicalTherapyDF <- loadCPRDDataframe("my_medHistory.Rda")
loadCPRDDataframe <- function(fileNameString) {
  stopifnot(!is.null(fileNameString))

  if(!file.exists(fileNameString)) {
    print(paste(fileNameString,"does not exist."))
    return(NULL)
  } else {
    return(readRDS(file=fileNameString))
  }
}

#===============================================================
#' Save CPRD List
#'
#' @description
#' Use to save any R List object including all those generated by this package.
#'
#' @details
#' There is no fixed file extension of a list, therefore, we stick to the convention 'lst'. If a file path
#' is not provided, the current working directory is used.
#' Call \code{\link{loadCPRDList}} to load the List object into the R environment.
#'
#' @param listStr List variable.
#' @param fileNameString Output file name and extension.
#'
#' @export
#'
#' @examples
#' clinicalTherapyDF <- constructMedicalHistory(testClinicalDF,
#'                                              NULL,
#'                                              testTherapyDF)
#' patidList <- getUniquePatidList(clinicalTherapyDF)
#' saveCPRDList(patidList, "patientIDs.lst")
saveCPRDList <- function(listStr, fileNameString) {
  stopifnot(!is.null(listStr))
  stopifnot(!is.null(fileNameString))

  print(paste("Saving data to:", fileNameString, sep=""))
  saveRDS(listStr,file=fileNameString)
}

#===============================================================
#' Save CPRD dataframe as text to file
#'
#' @description
#' Very useful for storing text data. Saves to the working directory set in R.
#'
#' @details
#' Executes the R write.table(). If a file path is not supplied, the current working directory is used.
#'
#' @param df Dataframe to save to text file.
#' @param fileNameString Output file name and extension.
#' @param sepChar Separation between data frame columns. Single white space is default.
#'
#' @export
#'
#' @examples
#' clinicalTherapyDF <- constructMedicalHistory(testClinicalDF,
#'                                              NULL,
#'                                              testTherapyDF)
#' saveCPRDDataframeAsText(clinicalTherapyDF, "clinical_therapy_data.txt", " ")
saveCPRDDataframeAsText <- function(df, fileNameString, sepChar=" ") {
  stopifnot(!is.null(df))
  stopifnot(!is.null(fileNameString))

  print(paste("Saving data to:", fileNameString, sep=""))
  utils::write.table(df,fileNameString,sep=sepChar,row.names=FALSE)
}

#===============================================================
#' Load CPRD List
#'
#' Use this function to load any base R List including all those generated by this package. Call \code{\link{saveCPRDList}}
#' to save a List.
#'
#' @details
#' Unless a full fie path is provided the working directory will be used.
#'
#' @param fileNameString file path with file name and extension.
#'
#' @return List. Returns NULL if the file is not found.
#' @export
#'
#' @examples
#' patidList <- getUniquePatidList(testTherapyDF)
#' saveCPRDList(patidList, "patientIDs.lst")
#' patidList <- loadCPRDList("patientIDs.lst")
loadCPRDList <- function(fileNameString) {
  stopifnot(!is.null(fileNameString))

  if(!file.exists(fileNameString)) {
    print(paste(fileNameString,"does not exist."))
    return(NULL)
  } else {
    print(paste("Loading:",getwd(),"/",fileNameString,sep=""))
    return(readRDS(file=fileNameString))
  }

}

#===============================================================
# For development only.
#
# @param fileNameString
#
# @return
# @export
#
# @examples
loadCPRDCSVFile <- function(fileNameString) {
  stopifnot(!is.null(fileNameString))

  if(!file.exists(fileNameString)) {
    print(paste(fileNameString),"does not exist.")
    return(NULL)
  } else {
    #print(paste("Loading:",getwd(),"/",fileNameString,sep=""))
    asDF <- utils::read.csv(fileNameString)
    asList <- lapply(as.matrix(asDF), function(x)x)
    return(asList)
  }
}

#===============================================================
# For development only.
#
# @param fileNameString
# @param fileEncodingString
#
# @return
# @export
#
# @examples
loadCPRDTabFile <- function(fileNameString, fileEncodingString = NULL) {
  stopifnot(!is.null(fileNameString))
  if(!file.exists(fileNameString)) {
    print(paste("Your current working directory is", getwd()))
    print(paste("Cannot load", fileNameString," as it does not exist!"))
    return(NULL)
  } else {
    asDF <- NULL
    if(is.null(fileEncodingString)) {
      asDF <- utils::read.csv(fileNameString,sep="\t",stringsAsFactors=FALSE)
    } else {
      asDF <- utils::read.csv(fileNameString,sep="\t",stringsAsFactors=FALSE, fileEncoding=fileEncodingString)
    }
    return(asDF)
  }
}

#====================================================================================================================
#Not to be called by the package user.
is.date <- function(x) inherits(x, 'Date')

#====================================================================================================================
# Not to be called by the package user.
#
# @param medHistoryDF
# @param dateDF
#
# @return
# @export
#
# @examples
trimMedHistoryDates <- function(medHistoryDF, dateDF) {

  if(is.null(medHistoryDF)) {
    return(NULL)
  }
  if(nrow(medHistoryDF)==0) {
    return(NULL)
  }
  if(is.null(dateDF)) {
    return(NULL)
  }

  patidList <- getUniquePatidList(medHistoryDF)
  dfList <- list()
  listCount <- 1
  nPatid <- length(patidList)
  for(i in 1:nPatid) {
    id <- patidList[i]
    indDF <- medHistoryDF[medHistoryDF$patid == id,]
    indDateDF <- dateDF[dateDF$patid==id,]
    if(nrow(indDateDF)==0) {
      next()
    }

    startDate <- indDateDF$start
    stopDate <- indDateDF$stop

    trimLowDF <- indDF[indDF$eventdate >= startDate,]
    trimLowHighDF <- trimLowDF[trimLowDF$eventdate <= stopDate,]

    if(nrow(trimLowHighDF) > 0) {
      dfList[[listCount]] <- trimLowHighDF
      listCount <- listCount + 1
    }
  }

  if(length(dfList) == 0) {
    return(NULL)
  }

  resultDF <- do.call(rbind, dfList)
  return(resultDF)
}

#===============================================================
#' Not for external call. Checks whether a CPRD data frame is formatted well
#'
#' @description
#' Every CPRD dataframe must have at least the following columns: patid, eventdate, medcode/prodcode. The
#' user can include additional data columns.
#'
#' @param df Fataframe to check.
#' @param dataType A character vector of either "clinical", "referral" or "therapy." NULL as default, which throws a warning.
#'
#' @return Logical. TRUE, indicates a well formatted dataframe, FALSE, otherwise.
#' @export
#'
#' @examples
#' checkBool <- checkCPRDRecord(testTherapyDF, "therapy")
checkCPRDRecord <- function(df, dataType=NULL) {
  if(missing(df)) {
    stop("The value to the argument df is missing.")
  }
  if(missing(dataType)) {
    stop("The value to the argument dataType is missing.")
  }
  dfCheck <- errorChecker(df)
  dataTypeCheck <- errorChecker(dataType)
  if(isFALSE(dfCheck) || isFALSE(dataTypeCheck)) {
    print("There is an problem with one of the checkCPRDRecord function arguments. Returning NULL.")
    return(NULL)
  }
  dataType <- toupper(dataType)
  acceptedTypes <- c("CLINICAL","THERAPY","REFERAL")
  if(length(dataType) > 1) {
    print("Please provide only one dataType at a time. Returning NULL.")
    return(FALSE)
  }
  if(is.na(match(dataType, acceptedTypes))) {
    print("The checkCPRDRecord function didn't recognised the value of dataType.")
    return(FALSE)
  }

  #check a nrow >= 1
  if(nrow(df)==0) {
    print("All data.frames need rows. Returning NULL.")
    return(FALSE)
  }

  #check for a patid column
  if(sum(names(df) %in% "patid")==0) {
    print("Couldn't find a patid column. All CPRD data.frames require a patid column.")
    return(FALSE)
  }

  #check for an eventdate column
  if(sum(names(df) %in% "eventdate")==0) {
    print("Couldn't find an eventdate column. All CPRD data.frames require an eventdate column.")
    return(FALSE)
  }

  #check whether the eventdate column in a characgter string
  if(isFALSE(is.date(df$eventdate))) {
    print("The eventdate column must be a character vector.")
    return(FALSE)
  }

  #for a clinical check for a medcode
  if(dataType == "CLINICAL") {
    if(sum(names(df) %in% "medcode")==0) {
      print("Couldn't find a medcode column in the clinical data.frame.")
      return(FALSE)
    }
  }

  #for a referral check for a medcode
  if(dataType == "REFERRAL") {
    if(sum(names(df) %in% "medcode")==0) {
      print("Couldn't find a medcode column in the referral data.frame.")
      return(FALSE)
    }
  }

  #for a therapy check of a prodcode
  if(dataType == "THERAPY") {
    if(sum(names(df) %in% "prodcode")==0) {
      print("Couldn't find a prodcode column in the therapy data.frame.")
      return(FALSE)
    }
  }

  print("The data.frame is appropriately formatted. Returning TRUE.")
  return(TRUE)
}

#===============================================================
#' Sums frequencies of grouped events across age groups
#'
#' @description
#' Sums the values by matching each age group with the age group lists generated using \code{\link{getAgeGroupByEvents}}.
#'
#' @details
#' For example, summing across the matching age groups from the two list elements:
#'
#' \preformatted{
#' [[1]]
#' 18-24 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60+
#'  104    99    108   134   172   195   175   206 295
#'
#'  [[2]]
#'  18-24 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60+
#'  45    41    35    23    44    35    34    20  29
#' }
#'
#' Would yield the named numeric vector result:
#'
#' \preformatted{
#' 18-24 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60+
#'  149   140   143   157   216   230   209   226  324
#' }
#'
#' @param ageGroupList List generated using getAgeGroupByEvents.
#'
#' @return Numeric vector of each summed age group. Entries are named.
#' @export
#'
#' @examples
#' fileLocation <- NULL
#' drugList <- unique(testTherapyDF$prodcode)
#' requiredProds <- drugList[1:10]
#' names(ageGenderDF) <- c("patid","age","gender")
#' ageGroupVector <- c(18,25,30,35,40,45,50,55,60)
#' ageAtYear <- "2017"
#'
#' #get the first drug object
#' fdoList <- getFirstDrugPrescription(testTherapyDF,
#'                                     idList=NULL,
#'                                     prodCodesVector=requiredProds,
#'                                     descriptionFile=fileLocation)
#' idList <- as.list(fdoList[[1]][1:2])
#' eventdateList <- as.list(fdoList[[2]][1:2])
#'
#' #organise eventdates by age groups
#' ageListResult1 <- getAgeGroupByEvents(idList,
#'                                       eventdateList,
#'                                       ageGenderDF,
#'                                       ageGroupVector,
#'                                       ageAtYear)
#' #sum across the age groups
#' summedAgeGroupsVector <- sumAcrossAgeGroups(ageListResult1)
sumAcrossAgeGroups <- function(ageGroupList) {
  checkAgeGroupList <- errorChecker(ageGroupList)
  if(isFALSE(checkAgeGroupList)) {
    print("Error in the ageGroupList argument to the function sumAcrossAgeGroups. Returning NULL.")
    return(NULL)
  }

  #ensure that the length of the list entries are identical
  lengthVector <- sapply(ageGroupList, function(x) length(x))

  if(isFALSE(all.equal(lengthVector, rep(lengthVector[1], length(lengthVector))))) {
    print("Error in ageGroupList argument to the function sumAcrossAgeGroups. Please check that the length (number
          of age groups) are identical across all list elements. Returning NULL.")
    return(NULL)
  }

  #ensure that the age group names are the same - first get a matrix of names eg,
  #     [,1]    [,2]    [,3]
  #[1,] "0-17"  "0-17"  "0-17"
  #[2,] "18-24" "18-24" "18-24"
  #[3,] "25-29" "25-29" "25-29"
  #........
  nameMatrix <- sapply(ageGroupList, function(x) names(x))
  #go through each row and check the names are the same by returning a boolean
  nameMatrixEquality <- sapply(nameMatrix, function(x) all.equal(x, rep(x[1], length(x))))
  if(isFALSE(all(nameMatrixEquality))) {
    print("Error in ageGroupList argument to the function sumAcrossAgeGroups. Please check that the age group names match between list elements.")
    return(NULL)
  }

  #sum across the groups and return as a named vector
  tempDF <- do.call("rbind", ageGroupList)
  returnVector <- colSums(tempDF)

  return(returnVector)
}

#===================================================================================================================
#' Calculates the age of a patient given their age at a particular year and the date at an event.
#'
#' CPRD provides patient age as an adjusted integer that can be decoded given a fixed integer and the year of the CPRD database e.g., 2017. Before
#' using this function ensure that the ageDF argument is a data.frame of unique patid values with the age of the patient calculated from the database release year. Provide
#' that same database release year as the value to the ageAtYear argument.
#'
#' The patids are provided in a list of patid vectors and the eventdates are also provided in a list of eventdate vector. The two lists are identical in List structure and size.
#' The Nth patid vector corresponds with the Nth eventdate vector. The Mth patid in the Nth patid List element vector corresponds to the Mth eventdate in the Nth eventdate List element vector.
#' The format of idList is identical to the first element in the FirstDrugObject from getFirstDrugPrescription. Patids must be unique across the whole list, i.e., a patid can be found once across
#' all List patid Vectors. Duplicate patids will return a NULL.
#'
#' @param idList List of patid vectors.
#' @param eventdateList List of eventdates. The format of eventdateList is identical to the second element in the FirstDrugObject from getFirstDrugPrescription. The Nth eventdate vector corresponds with the Nth patid vector.
#' @param ageDF data frame with at least columns "patid" and "age." The age is precalculated for a particular year and that year is passed in via ageAtYear.
#' @param ageGroupVector vector of age in years used to denote the lower inclusive bounds of an age group. For example c(18,25,30,...) denotes: 18-24, 25-29, 30... The last age cannot be greater than 199.
#' A patient age during an event less than min(ageGroupVector) is ignored.
#' @param ageAtYear character string or integer denoting a year #### e.g., "2017" or 2017. This will always be converted to a number. If any of the eventdates in eventdateList happens after this number the patient age will be incorrect.
#'
#' @return List of group data.frames. Each element of the list is a data.frame that corresponds to the same list index position in idList and eventdateList. Each column in an age group with the count of events that occured at that age interval.
#' @export
#'
#' @examples
#' drugList <- unique(testTherapyDF$prodcode)
#' requiredProds <- drugList[1:5]
#' #ensure column names are as follows:
#' names(ageGenderDF) <- c("patid","age","gender")
#' ageDF <- ageGenderDF
#' ageGroupVector <- c(18,25,30,35,40,45,50,55,60)
#' ageAtYear <- "2017"
#'
#' resultList <- getFirstDrugPrescription(testTherapyDF,
#'                                        idList=NULL,
#'                                        prodCodesVector=requiredProds,
#'                                        descriptionFile=NULL)
#' #get the first two vectors of patids (each associated with the drug that can
#' #be deciphered from the drug code name of each named element in this list).
#' idList <- as.list(resultList[[1]][1:2])
#' #get the first two vectors of events dates (corresponds to the first two
#' #patid vectors above)
#' eventdateList <- as.list(resultList[[2]][1:2])
#'
#' ageListResult <- getAgeGroupByEvents(idList,
#'                                      eventdateList,
#'                                      ageDF,
#'                                      ageGroupVector,
#'                                      ageAtYear)
#'
#' firstPrescriptionByAgeGroupDF <- ageListResult[[1]]
#' secondPrescriptionByAgeGroupDF <- ageListResult[[2]]
getAgeGroupByEvents <- function(idList, eventdateList, ageDF, ageGroupVector, ageAtYear) {
  idListCheck <- errorChecker(idList)
  eventdateListCheck <- errorChecker(eventdateList)
  ageDFCheck <- errorChecker(ageDF)
  ageGroupVectorCheck <- errorChecker(ageGroupVector)
  ageAtYearCheck <- errorChecker(ageAtYear)

  if(isFALSE(idListCheck) | isFALSE(eventdateListCheck) | isFALSE(ageDFCheck) | isFALSE(ageGroupVectorCheck) | isFALSE(ageAtYearCheck)) {
    print("Warning: There is an error with one of the arguments to getAgeGroupByEvents. Returning NULL.")
    return(NULL)
  }
  if(length(idList) != length(eventdateList)) {
    print("Warnings: unequal number of list elements between idList and eventdateList. Returning NULL.")
    return(NULL)
  }
  if(length(unlist(idList)) != length(unlist(eventdateList))) {
    print("Warnings: unequal number of patid and eventdates between idList and eventdateList. Returning NULL.")
    return(NULL)
  }

  if(isTRUE(is.character(ageAtYear))) {
    if(isFALSE(splus2R::is.number(as.integer(ageAtYear)))) {
      print(paste("Warning: the value", ageAtYear, "for ageAtYear isn't recognised.You passed in type",typeof(ageAtYear),"Returning NULL."))
      return(NULL)
    }
    ageAtYear <- as.integer(ageAtYear)
  } else if(isFALSE(splus2R::is.number(ageAtYear))) {
    print(paste("Warning: the value", ageAtYear, "for ageAtYear isn't recognised. You passed in type",typeof(ageAtYear),"Returning NULL."))
    return(NULL)
  } else {
    print(paste("Warning: the value", ageAtYear, "for ageAtYear isn't recognised. You passed in type",typeof(ageAtYear),"Returning NULL."))
    return(NULL)
  }

  #check that it's four integers
  if(nchar(ageAtYear) != 4) {
    print(paste("The date", ageAtYear, "in ageAtYear is not recognised. Returning NULL."))
    return(NULL)
  }

  #make sure that patid include is unique between idList elements
  if( length(unique(unlist(idList))) != length(unlist(idList)) ) {
    print("Warning: Please ensure that patids are unique between idList elements. A patid cannot be included in more than one element group. Returning NULL.")
    return(NULL)
  }

  #makesure ageGroupvector is numeric
  if(sum(splus2R::is.number(ageGroupVector)) != length(ageGroupVector)) {
    print("Warning: Please check the ageGroupVector values. At least one entry is not a number. Returning NULL.")
    return(NULL)
  }

  #make sure that all the patids are available in the ageDF
  if(sum(unlist(idList) %in% unique(ageDF$patid)) != length(unlist(idList))) {
    print("Warning: The ageDF data.frame doesn't contain all patids to match for an age. Returning NULL.")
    return(NULL)
  }

  #ensure everything is being treated as a string to speed access via a matrix rather than data.frame
  `%>%` <- dplyr::`%>%`
  ageDF <- ageDF %>% dplyr::mutate_all(as.character)
  ageDFMatrix <- as.matrix(ageDF)
  #give the matrix row names as patids (for referral) and column names from the data.frame.
  row.names(ageDFMatrix) <- ageDF$patid
  colnames(ageDFMatrix) <- names(ageDF)

  #reorder the ageGroupVector and check whether the last age isn't greater than 199.
  ageGroupVector<- ageGroupVector[order(ageGroupVector)]
  if(isTRUE(ageGroupVector[length(ageGroupVector)] > 199)) {
    print("Warning You cannot provide an age greater than 199. Returning NULL.")
    return(NULL)
  }

  ageGroupList <- list()
  ageGroupCounter <- 1
  ageTempMatrix <- matrix(0, nrow=1, ncol=(length(ageGroupVector))) #this was -2
  ageTempDF <- as.data.frame(ageTempMatrix)
  ageGroupNames <- NA
  ageGroupNumberList <- list()
  for(i in 1:length(ageTempDF)) {
    if(i==length(ageTempDF)) {
      ageGroupNames[i] <- paste0(as.character(ageGroupVector[i]),"+")
      ageGroupNumberList[[i]] <- c(as.integer(ageGroupVector[i]), as.integer(200))
    } else {
      ageGroupNames[i] <- paste0(as.character(ageGroupVector[i]), "-",as.character((ageGroupVector[i+1]-1)))
      ageGroupNumberList[[i]] <- c(as.integer(ageGroupVector[i]), as.integer(ageGroupVector[i+1]-1))
    }
  }
  colnames(ageTempMatrix) <- ageGroupNames

  #set up the list of ageGroups
  #if more than one element in eventdateList is provided e.g., a list of all eventdates for amitriptyline ([[1]])
  #and a list of all eventdates for triptan ([[2]]) a separate set of age groups are generated and stored.
  #The return object is a List of data.frames. Each list element data.frame corresponds to the eventdates & patids at the
  #same element position in idList and eventdateList.
  #The columns correspond to the age groups.

  #missingAgeCount <- 0

  #work out the age at every event (going through the list of events)
  for(i in 1:length(eventdateList)) {
    #make a fresh set of agegroups to add to

    newAgeGroupMatrix <- ageTempMatrix

    patidVector <- as.character(idList[[i]])
    eventdateVector <- eventdateList[[i]]
    if(length(patidVector) != length(eventdateVector)) {
      print(paste("There was a discrepancy in vector size between List element", i, "between idList and eventdateList. Returning NULL."))
      return(NULL)
    }
    #tempAgeDT <- data.table::as.data.table(subset(ageDF, ageDFpatid %in% patidVector))

    for(j in 1:length(patidVector)) {
      #age <- tempAgeDT[tempAgeDT$patid==patidVector[j],]
      age <- as.integer(ageDFMatrix[patidVector[j],"age"])
      eventdate <- eventdateVector[j]
      eventYear <- as.numeric(substr(eventdate,1,4))
      ageAtEvent <- age + (eventYear - ageAtYear)

      #check which group the ageAtEvent belong in
      for(n in 1:length(ageGroupNumberList)) {
        minAge <- ageGroupNumberList[[n]][1]
        maxAge <- ageGroupNumberList[[n]][2]
        if(maxAge == "+") {
          newAgeGroupMatrix[1,n] <- newAgeGroupMatrix[1,n] + 1
          break()
        } else if(ageAtEvent >= minAge & ageAtEvent <= maxAge) {
          newAgeGroupMatrix[1,n] <- newAgeGroupMatrix[1,n] + 1
          break()
        } #else {
        #missingAgeCount <- missingAgeCount + 1
        #break()
        #}
      }
    }

    ageGroupList[[ageGroupCounter]] <- as.data.frame(newAgeGroupMatrix)
    ageGroupCounter <- ageGroupCounter + 1
  }

  #print(paste(missingAgeCount,"feel before the first age group so were not counted."))
  return(ageGroupList)
}
