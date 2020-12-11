utils::globalVariables(c("i"))

#====================================================================================================================
#' Combine multiple prescription frequencies under a common name
#'
#' @description
#' Use to combine the drug prescription frequencies from a dataframe of descriptions and codes and to give those combined
#' frequencies a new name, for example, "All Amitriptyline". A type of drug may have several drug codes, each with a unique name/description. See examples.
#'
#' @param df Dataframe with at least the columns "description" and "Frequency", such as drugDistributionDF from \code{\link{getFirstDrugPrescription}}.
#' @param structureList A named list of numeric vectors. Each vector holds the drug prodcode or the row indices of the df row that
#' you want to sum Frequencies and return as a single entry.
#' @param rowIndices A logical value. When TRUE it treats the structureList numeric vectors as data.frame row indices. If FALSE (default)
#' it treats the numeric vectors as prodcodes.
#' @return Dataframe with the same structure as the input df (with columns: description | code | Frequency), vut using the new product description
#' from the named structureList and the combined Frequencies. The code entry is NA. A NULL is returned if anything went wrong.
#' @export
#' @examples
#' fileLocation <- NULL
#' firstDrugObj <- getFirstDrugPrescription(testTherapyDF,
#' idList=NULL,
#' prodCodesVector=NULL,
#' descriptionFile=fileLocation)
#'
#' structureList <- list(
#'   Amitriptyline = c(139,209,31123,1395,2234,1425,1146,1147),
#'   Propranolol = c(145,5,41,8776,1166,3945,4493,543)
#' )
#' combinedDF <- combinePrescriptionFrequencies(firstDrugObj$drugDistributionDF,
#'                                              structureList)
#'
#' #This totals the frequencies of those grouped product codes into the one
#' #entry and uses the list element name to create a new dataframe entry.
#'
#' #As row indices:
#'structureList <- list(
#'  Amitriptyline = c(1,2,3,13,22,45,46,47),
#'  Propranolol = c(4,5,6,7,11,41,42,43)
#')
#' combinedDF <- combinePrescriptionFrequencies(firstDrugObj$drugDistributionDF,
#'                                              structureList,
#'                                              rowIndices=TRUE)
#'
#' #This combines the entries at index 1,2,3,13,22,45,46,47 into the one entry
#' #and be named "Amitriptyline". If any two indices are out of scope they are ignored with a warning.
combinePrescriptionFrequencies <- function(df, structureList, rowIndices=FALSE) {
  dfCheck <- errorChecker(df)
  structureListCheck <- errorChecker(structureList)

  if(isFALSE(dfCheck) | isFALSE(structureListCheck)) {
    print("Please check your code. An argument of combinePrescriptionFrequencies() is either missing or empty. Returning NULL.")
    return(NULL)
  }

  #get the new number of descriptions to generate
  numDescriptions <- length(structureList)
  newDescriptionNames <- names(structureList)

  #this is temporary storage for the new description and Frequencies
  newDescriptionVector <- character()
  newFrequencyVector <- integer()
  newCounter <- 1

  for(i in 1:numDescriptions) {
    newDescription <- newDescriptionNames[i]
    entriesToFindVector <- structureList[[i]]
    if(length(entriesToFindVector) == 0) {
      print("There were no indices or prodcodes! Double check your code. Returning NULL to prevent further problems.")
      return(NULL)
    }

    #find if they are prodcodes
    if(isFALSE(rowIndices)) {
      #subset those
      indDF <- subset(df, df$code %in% entriesToFindVector)
      if(nrow(indDF) > 0) {
        newFrequencyVector[newCounter] <- sum(indDF$Frequency)
        newDescriptionVector[newCounter] <- newDescription
        newCounter <- newCounter + 1
      } else {
        print(paste("I could't find some of the prodcodes:", entriesToFindVector, "from", newDescription))
      }
    }
    #find if they are row indices then get out the rows.
    else {
      minRow <- min(entriesToFindVector)
      maxRow <- max(entriesToFindVector)
      if(minRow < 1) {
        print("Check your code. There appears to be either a 0 or negative row index. Returning NULL to avoid further problems.")
        return(NULL)
      }
      if(maxRow > nrow(df)) {
        print(paste("Check your code. There were", nrow(df), "row in the df but one of the maximum row index requested was", maxRow, "Returning NULL to avoid further problems."))
        return(NULL)
      }

      #only get those rows which match the indices for that descriptionn
      indDF <- df[entriesToFindVector,]
      if(nrow(indDF) > 0) {
        newFrequencyVector[newCounter] <- sum(indDF$Frequency)
        newDescriptionVector[newCounter] <- newDescription
        newCounter <- newCounter + 1
      } else {
        print(paste("I could't find some row indicdes:", entriesToFindVector, "from", newDescription, "Returning NULL."))
        return(NULL)
      }
    }
  }

  #construct the returning data.frame using the temporary storage vectors.
  if(length(newDescriptionVector)==0) {
    print("No descriptions could be found. Returning NULL.")
    return(NULL)
  } else {
    returnDF <- data.frame(description=newDescriptionVector, code=NA, Frequency=newFrequencyVector)
    return(returnDF)
  }
}

#====================================================================================================================
#' Searches for those patients with sufficient record presence before an event of interest
#'
#' @description
#' Returns those patients with sufficient record presence over a given duration before an event of interest.
#'
#' @details
#' For example, consider the start of the burn as I and the event of interest as E (denotes the end of the burn in duration), all other events are x,
#' a therapeutic time line could look like:
#' start_of_record|>-x--x--Ix--x---x--(365 days from I to E)--x-----x--E--x->|end_of_record. Clearly there are several events prior to the start of the
#' burn in period.
#'
#' On the other hand, consider 365 days of time had elapsed between an event x (where x is a placeholder
#' for any event in the provided data frame with the exception of those code in the startCodesVector) and the event of interest (E). Consider
#' the event of interest being amitriptyline (amitriptyline prodcodes specified in the startCodesVector).
#' This function returns all patients who have therapeutic events occurring at least 365 days before their amitriptyline prescription.
#'
#' On the other hand, an example of a patient record without sufficient burn in time:
#'
#' start_of_record|>-------I-------x--(365 days from I to E)--x-----x--E--x->|end_of_record
#'
#' This patient had no events before the start of the burn in observation window (I to E).
#'
#' This is very useful, if for example, one wishes to build a cohort of CPRD patients for interrupted time series analysis. The
#' input therapeutic dataframe only contains triptans (headache analgesics) and all prescriptions for
#' propranolol (an off label migraine prophylactic). The first prescription of propranolol is the event of interest. Patient ID are
#' collected if there are records of triptans more than 365 days before the first propranolol prescription. One can then use the
#' triptan first-prescription as the point of intervention. Those patients with records that do not reach back beyond the 365 observation window
#' are seen as having insufficient record length for pre-intervention regression analysis.
#'
#' Note: This does not support medcode and prodcode at the same time. For example, searching for all patients with
#' triptans at least 365 days before a CVD medcode will not work. The user must provide either a clinical dataframe or
#' a therapeutic dataframe. The code will parse for columns medcode and prodcode and act accordingly. Supporting both
#' is a planned feature.
#'
#' @param df Dataframe with patid column and either medcode (clinical) or prodcode (therapeutic) column.
#' @param startCodesVector Vector of codes for the event of interest.
#' @param periodDaysBefore Integer of days of burn in time: the duration in days immediately before the event of interest specified in the
#' startCodesVector.
#'
#' @return List of patids of those patients that satisfy the burn in period. Will return NULL on error.
#' @export
#'
#' @examples
#' drugOfInterestVector <- c(83,49,297,1888,940,5)
#' burnInTime <- 172
#' #testTherapyDF contains all therapy codes for these patients. Look for those
#' #patients with at least 172 days of event records.
#' patientList <- getBurnInPatients(testTherapyDF,
#'                                  drugOfInterestVector,
#'                                  burnInTime)
#'
getBurnInPatients <- function(df, startCodesVector, periodDaysBefore=365) {
  #check everything being sent into the function is ok
  dfCheck <- errorChecker(df)
  codesCheck <- errorChecker(startCodesVector)
  periodDaysCheck <- errorChecker(periodDaysBefore)

  if(isFALSE(dfCheck) | isFALSE(codesCheck) | isFALSE(periodDaysCheck)) {
    print("Please check your code. An argument of getBurnInPatients() is either missing or empty.")
    return(NULL)
  }

  if(periodDaysBefore < 1) {
    print("periodDaysBefore must be >= 1. Returning NULL.")
    return(NULL)
  }

  #===========================
  #main body of the function

  #determine whether this is prodcode or medcode
  prodcodeName <- "prodcode"
  medcodeName <- "medcode"
  colnames <- names(df)
  if( isTRUE(sum(colnames %in% prodcodeName) ==1) ) {
    names(df)[names(df) ==  "prodcode"] <- "code"
  } else if(isTRUE(sum(colnames %in% medcodeName) ==1)) {
    names(df)[names(df) ==  "medcode"] <- "code"
  } else {
    print("Cannot recognise the df data.frame column names. Looking for either prodcode or medcode. Returning NULL.")
    return(NULL)
  }

  #get  out all the patients with a startCode
  print(paste("There are", length(getUniquePatidList(df)), "patients to consider."))
  n <- length(getUniquePatidList(df[df$code %in% startCodesVector,]))

  if(n==0) {
    print("There no patients with the start codes of interest. Returning NULL.")
    return(NULL)
  }

  print(paste("There are", n, "patients with a start code"))
  tempDF <- subset(df, df$code %in% startCodesVector)
  idList <- getUniquePatidList(tempDF)
  df <- subset(df, df$patid %in% idList)
  print(paste("Trimmed the medical record down to", length(getUniquePatidList(df)), "to only contain patients with code of interest."))

  #make this a parallel process if possible
  numCores <- 1
  idList <- getUniquePatidList(df)
  if(length(idList) > 400) {
    numCores <- parallel::detectCores()
    if(numCores > 1) {
      #numCores <- numCores-1
      #if(numCores > 1) {
      #  print(paste("Found", numCores, "cores available (total minus one)."))
      #}
      #This is for CRAN!!
      if(numCores > 2) {
        numCores <- 2
      }
    }
  }

  if(numCores > 1) {
    #sadly CRAN restricts this to 2 cores only.
    if(numCores > 2) {
      numCores <- 2
    }

    print("Running in parallel.")
    parallelList <- getParallelDF(df, numCores)
    stopifnot(length(parallelList) == numCores)

    validPatientList <- list()

    `%dopar%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(numCores, outfile="")
    doParallel::registerDoParallel(cl)
    parallel::clusterExport(cl, varlist = c("getUniquePatidList","startCodesVector","periodDaysBefore","errorChecker"), envir=environment())
    validPatientList <- foreach::foreach(i = 1:numCores, .combine = 'c') %dopar% {
      dt <- data.table::as.data.table(parallelList[[i]])
      innerValidPatientList <- list()
      idList <- getUniquePatidList(dt)
      for(id in idList) {
        indDF <- data.table::as.data.table(dt[dt$patid==id,])
        #get out the first code on record
        firstCode <- indDF$code[1]

        #ignore the patient if the start code is the first on record
        if(isTRUE(sum(firstCode %in% startCodesVector)==0)) {
          firstDate <- indDF$eventdate[1]
          #get out the first startcode
          firstStartDate <- indDF[indDF$code %in% startCodesVector,]$eventdate[1]

          #time difference between the first drug on record and the first drug of interest
          diffTime <- as.numeric(firstStartDate - firstDate)

          #if the difference in time is greater than the required burn in period keep the individual
          #else ignore the individual
          if(diffTime > periodDaysBefore) {
            innerValidPatientList <- append(innerValidPatientList, id)
          }
        }
      }
      return(innerValidPatientList)
    }
    print("Stopping cluster.")
    parallel::stopCluster(cl)
  } else {
    #in serial
    #go through each patient looking for an inclusive conndition i.e., some other drug N days before the start drug
    print("Running in serial.")
    validPatientList <- list()
    idList <- getUniquePatidList(df)
    dt <- data.table::as.data.table(df)
    for(id in idList) {
      indDF <- data.table::as.data.table(dt[dt$patid==id,])
      #get out the first code on record
      firstCode <- indDF$code[1]

      #ignore the patient if the start code is the first on record
      if(isTRUE(sum(firstCode %in% startCodesVector)==0)) {
        firstDate <- indDF$eventdate[1]
        #get out the first startcode
        firstStartDate <- indDF[indDF$code %in% startCodesVector,]$eventdate[1]

        #time difference between the first drug on record and the first drug of interest
        diffTime <- as.numeric(firstStartDate - firstDate)

        #if the difference in time is greater than the required burn in period keep the individual
        #else ignore the individual
        if(diffTime > periodDaysBefore) {
          validPatientList <- append(validPatientList, id)
        }
      }
    }
  }

  if(length(validPatientList) == 0) {
    print("There were no valid patients for a burn-in period.")
    return(NULL)
  }
  print(paste("There are", length(validPatientList), "valid patients for a burn-in period"))

  return(validPatientList)
}


#====================================================================================================================
#' Gets all patients prescribed a prodcode of interest
#'
#' @description
#' Returns the therapy records of patients with a therapy record of interest.
#'
#' @details
#' If a dataframe is returned the size can be reduced further by
#' requesting that any therapy event not of interest i.e., not within the prodCodesVector, are removed
#' from the dataframe. The default behaviour is to keep all therapy events for those patients with at least one
#' drug prescription matching one of the codes in prodCodesVector.
#'
#' This function support medical history dataframes by observing the names of the incoming df columns. If "code" is present
#' then it expects a medical history history dataframe, on the other hand, if "prodcode" is present then it expects a therapy dataframe.
#'
#' @param df Data frame with patid column present and either a "prodcode" or a "code" column (therapy dataframe or medical history dataframe, respectively).
#' @param prodCodesVector A vector of prodcodes.
#' @param removeExcessDrugs Logical. If FALSE (default) all prodcodes are retained in a patients record. If TRUE
#' all prodcodes in that patient that are not part of the prodCodesVector are removed before returning.
#' @param returnIDList Logical. If FALSE (default) a reduced dataframe is returned. If TRUE
#' a list of patid values are returned.
#'
#' @return Dataframe of those patients with a prescription for any of the prodcodes in prodCodesVector. Alternatively,
#' a list of patient IDS.
#' @export
#'
#' @examples
#' prodCodesVector <- unique(testTherapyDF$prodcode)
#' reducedProdCodesVector <- prodCodesVector[1:10]
#' therapyOfInterestDF <- getPatientsWithProdCode(testTherapyDF,
#'                                                reducedProdCodesVector)
#' reducedTherapyOfInterestDF <- getPatientsWithProdCode(testTherapyDF,
#'                                                       reducedProdCodesVector,
#'                                                       removeExcessDrugs=TRUE)
#' #the same number of patients are returned, however, fewer drug therapy records are
#' #returned in reducedTherapyOfInterestDF.
getPatientsWithProdCode <- function(df, prodCodesVector, removeExcessDrugs=FALSE, returnIDList=FALSE) {
  #check everything being sent into the function is ok
  dfCheck <- errorChecker(df)
  prodCodesCheck <- errorChecker(prodCodesVector)

  if(isFALSE(dfCheck) | isFALSE(prodCodesCheck)) {
    print("Please check your code. An argument to getPatientsWithProdCode() is either missing or empty.")
    return(NULL)
  }

  #===========================
  #main body of the function

  #check whether this is a therapy dataframe or a medical history dataframe.
  columnNameVector <- colnames(df)
  if(sum("code" %in% columnNameVector)==1) {
    #this is for a medical history
    thoseWithADrugDF <- df[df$codetype == "t",]
    dfReduced <- thoseWithADrugDF[thoseWithADrugDF$code %in% prodCodesVector,]
  } else {
    dfReduced <- df[df$prodcode %in% prodCodesVector,]
  }

  print(paste("There are", length(getUniquePatidList(dfReduced)), "patients with the prodcodes of interest. All other patients were removed."))
  if(isTRUE(returnIDList)) {
    return(getUniquePatidList(dfReduced))
  } else {
    #we want to keep all drugs on these patients, we just use the drugs of interests to identify the patients
    if(isFALSE(removeExcessDrugs)) {
      dfReduced <- df[df$patid %in% getUniquePatidList(dfReduced),]
    } else {
      print("Removing prodcode events that are not of interest.")
      print(paste("Number of events before removing:",  nrow(df)))
      print(paste("Number of events after eventdates  werer removed:", nrow(dfReduced)))
    }
    return(dfReduced)
  }

}


#===================================================================================================================
#' Gets all patient IDs by gender
#'
#' @description
#' The function links patient IDs to gender values.
#'
#' @details
#' Any patients specified in the idList but
#' absent from the genderDF are ignored. In CPRD, male is 1 and female is 2. By default both
#' genders are parsed, although that can be controlled by used genderCodeVector. Saves you the time with
#' defining an idList for each gender.
#'
#' @param idList List of patid values.
#' @param genderDF Dataframe with at least "patid" and "gender" columns.
#' @param genderCodeVector An integer vector specifying either 1, 2 or c(1,2) for male, female or male and female.
#'
#' @return Dataframe with "patid" and "gender".
#' @export
#'
#' @examples
#' idList <- getUniquePatidList(testTherapyDF)
#' idList <- idList[1:(length(idList)/2)]
#' malePatientsDF <- getGenderOfPatients(idList, ageGenderDF, 1)
#' femalePatientsDF <- getGenderOfPatients(idList, ageGenderDF, 2)
#' allPatientsDF <- getGenderOfPatients(getUniquePatidList(testTherapyDF),
#'                                      ageGenderDF)
#'
getGenderOfPatients <- function(idList, genderDF, genderCodeVector=c(1,2)) {
  #check everything being sent into the function is ok
  dfCheck <- errorChecker(genderDF)
  idCheck <- TRUE
  if(!is.null(idList)) {
    idCheck <- errorChecker(idList)
  }
  genderCodeVectorCheck <- TRUE
  if(!is.null(genderCodeVector)) {
    genderCodeVectorCheck <- errorChecker(genderCodeVector)
  }

  if(isFALSE(idCheck) | isFALSE(dfCheck) | isFALSE(genderCodeVectorCheck)) {
    print("Please check your code. An argument of getGenderOfPatients() is either missing or empty.")
    return(NULL)
  }

  requiredColumns <- c("patid","gender")
  columnNames <- colnames(genderDF)
  if(sum(columnNames %in% requiredColumns) != 2) {
    print(paste("The column names in genderDF did not contain all of the required columns."))
    return(NULL)
  }

  #===========================
  #main body of the function
  if(sum(unique(genderDF$gender) %in% genderCodeVector)==0) {
    print(paste("The gender codes", genderCodeVector, "were not recognised in the genderDF. Returning NULL."))
    return(NULL)
  }


  #pick specific patients
  genderSpecifiedDF <- genderDF[genderDF$gender %in% genderCodeVector,] #gets out all those by gender code
  if(nrow(genderSpecifiedDF) > 0) {
    genderSpecifiedDF <- subset(genderSpecifiedDF, genderSpecifiedDF$patid %in% idList)
  } else {
    print(paste("No patients with gender code", genderCodeVector, ". Returning null."))
    return(NULL)
  }


  if(nrow(genderSpecifiedDF) == 0) {
    print(paste("No patients in the specified idList found. Returning NULL."))
    return(NULL)
  }


  if(length(idList) != nrow(genderSpecifiedDF)) {
    diffPatients <- length(idList)-nrow(genderSpecifiedDF)
    print(paste("Not every patient was matched with a gender. Missing", diffPatients, "patients."))
    print("This is quite normal if you only specified one gender.")
  }

  returnDF <- data.frame(patid = genderSpecifiedDF$patid, gender=genderSpecifiedDF$gender)
  return(returnDF)
}


#===================================================================================================================
#' Get all patient IDs by IMD scores.
#'
#' @description
#' Will return a dataframe of patient records linked to an IMD score.
#'
#' @details
#' Approximately 1/4 of patients in the CPRD Gold
#' database have been linked with an IMD score. This function only returns those patients with a linked record.
#'
#' @param idList List of patid values. Those patients without a corresponding IMD score are ignored.
#' @param imdDF Dataframe with at least "patid" and "score" columns.
#' @param imdScoreVector Vector of IMD score to search against. Default are all scores between 1:5.
#'
#' @return Data frame with "patid" and "score". NULL otherwise.
#' @export
#'
#' @examples
#' idList <- getUniquePatidList(testTherapyDF)
#' idList <- idList[1:(length(idList)/2)]
#' onePatientsDF <- getIMDOfPatients(idList, imdDF, 1)
#' twoPatientsDF <- getIMDOfPatients(idList, imdDF, 2)
#' allPatientsDF <- getIMDOfPatients(getUniquePatidList(testTherapyDF), imdDF)
#'
getIMDOfPatients <- function(idList, imdDF, imdScoreVector=c(1,2,3,4,5)) {
  #check everything being sent into the function is ok
  imdCheck <- errorChecker(imdDF)
  idCheck <- TRUE
  if(!is.null(idList)) {
    idCheck <- errorChecker(idList)
  }
  imdCodeVectorCheck <- TRUE
  if(!is.null(imdScoreVector)) {
    imdCodeVectorCheck <- errorChecker(imdScoreVector)
  }

  if(isFALSE(idCheck) | isFALSE(imdCheck) | isFALSE(imdCodeVectorCheck)) {
    print("Please check your code. An argument of getIMDOfPatients() is either missing or empty.")
    return(NULL)
  }

  requiredColumns <- c("patid","score")
  columnNames <- colnames(imdDF)
  if(sum(columnNames %in% requiredColumns) != 2) {
    print(paste("The column names in imdDF did not contain all of the required columns."))
    return(NULL)
  }

  #===========================
  #main body of the function
  if(sum(unique(imdDF$score) %in% imdScoreVector)==0) {
    print(paste("The IMD score codes", imdScoreVector, "were not recognised in the imdDF. Returning NULL."))
    return(NULL)
  }

  #pick specific patients
  imdDF <- subset(imdDF, imdDF$patid %in% idList)
  #pick a specific gender
  imdDF <- subset(imdDF, imdDF$score %in% imdScoreVector)

  if(nrow(imdDF) == 0) {
    print(paste("No patients with requested IMD scodes codes in the specified idList. Return NULL."))
    return(NULL)
  }

  if(length(idList) != nrow(imdDF)) {
    diffPatients <- length(idList)-nrow(imdDF)
    print(paste("Not every patient was matched with an IMD score. Missing", diffPatients, "patients."))
  }

  returnDF <- data.frame(patid = imdDF$patid, score=imdDF$score)
  return(returnDF)
}




#===================================================================================================================
#' Returns only those patients that do not have multiple prodcodes on the same day
#'
#' @param df Dataframe with a "patid" column. This can cover clinical, referral, and therapy dataframes or a combination of all three
#' via a medical history dataframe.
#' @param prodCodesVector If NULL (default) the function looks at any drug code to determine whether a patient should be excluded for having
#' multiple prescription drugs on the same day. A NULL (default) value is useful if the data has already been cleaned of all but the essential prodcodes.
#' @param returnDF If TRUE return a dataframe, else return a list of patient patid values.
#' @param removePatientsWithoutDrugs If TRUE will remove all those patients who didn't have those drugs specified in the prodCodesVector.
#'
#' @return Dataframe of patient records or a list of patient patid values.
#' @export
#'
#' @examples
#' df <- getMultiPrescriptionSameDayPatients(testTherapyDF)
#' prodcodesVector <- unique(testTherapyDF$prodcode[1:20])
#' medHistoryDF <- constructMedicalHistory(NULL,
#'                                         NULL,
#'                                         testTherapyDF)
#' df <- getMultiPrescriptionSameDayPatients(medHistoryDF,
#'                                          prodcodesVector,
#'                                          removePatientsWithoutDrugs=TRUE)
getMultiPrescriptionSameDayPatients <- function(df, prodCodesVector=NULL, returnDF=TRUE, removePatientsWithoutDrugs=FALSE) {
  #check everything being sent into the function is ok
  dfCheck <- errorChecker(df)
  prodCheck <- TRUE
  if(!is.null(prodCodesVector)) {
    prodCheck <- errorChecker(prodCodesVector)
  }
  returnDFCheck <- TRUE
  if(is.null(returnDF)) {
    returnDFCheck <- FALSE
  }
  removePatientsWithoutDrugsCheck <- TRUE
  if(is.null(removePatientsWithoutDrugs)) {
    removePatientsWithoutDrugsCheck <- FALSE
  }

  if(isFALSE(prodCheck) | isFALSE(dfCheck) | isFALSE(returnDFCheck | isFALSE(removePatientsWithoutDrugsCheck))) {
    print("Please check your code. An argument of getMultiPrescriptionSameDayPatients() is either missing or empty.")
    return(NULL)
  }

  if(is.null(prodCodesVector) &  isTRUE(removePatientsWithoutDrugs)) {
    print("You cannot remove patients without particular drugs if the prodCodesVecor is NULL. Returning NULL.")
    return(NULL)
  }

  if((length(prodCodesVector) == 0) &  isTRUE(removePatientsWithoutDrugs)) {
    print("You cannot remove patients without particular drugs if the prodCodesVecor is empty. Returning NULL.")
    return(NULL)
  }

  #===========================
  #main body of the function
  print(paste("There are ", length(getUniquePatidList(df)), "patients with therapy data"))

  #get all those individuals with these drugs
  if(isTRUE(removePatientsWithoutDrugs)) {
    print("Checking to see whether any events related to unspecified drugs.")
    df <- getPatientsWithProdCode(df, prodCodesVector, removeExcessDrugs=TRUE)

    #if there aren't any patients on the specified drugs then notify and return NULL
    if(nrow(df)==0) {
      print("There were no patients on the specified drugs. NULL returned.")
      return(NULL)
    }
  }
  idList <- getUniquePatidList(df)
  n <- length(idList)
  print(paste("There were", n, "patients on supplied drugs"))


  #****get all rows which are not unique by their eventdate*****
  #if two ore more event dates are the same that means those drugs were given out on the same day.
  #If you put in all the amitriptiptyline and propranolol codes, those patients might be getting either multiple amitriptlyine on
  #one date or an amitriptyline and a propranolol on one date.
  newIndDFEntriesList <- list()
  count <- 1
  #removing those patients and return a data.frame (returns anything that is not a duplicate)
  for(i in 1:length(idList)) {
    #Get the patient's medical record.
    indDF <- df[df$patid == idList[[i]],]
    #Get the events which are not duplicates
    tempDF <- indDF[!duplicated(indDF$eventdate),]
    #if those none duplicate event dates are the same length as the medical record then there is no multiple prescriptions
    if(nrow(indDF)==nrow(tempDF)) {
      newIndDFEntriesList[[count]] <- indDF
      count <- count + 1
    }
  }

  if(length(newIndDFEntriesList) == 0) {
    print("All patients had duplicate-eventdate drug prescriptions. There are no patients left. Returning NULL")
    return(NULL)
  }

  df <- do.call(rbind, newIndDFEntriesList)
  print(paste("There are", length(getUniquePatidList(df)), "patients having removed any with multiple prescriptions on the same date."))

  idList <- getUniquePatidList(df)
  names(idList) <- unlist(idList)
  thoseOnMultiple <- n - length(idList)

  print(paste("There were", thoseOnMultiple, "patients with 2+ drugs on a single event date. Those have been removed."))

  #return a modified data.frame as default or the corrresponding patids.
  if(isTRUE(returnDF)) {
    return(df)
  } else {
    return(idList)
  }
}


#===================================================================================================================
#' Returns data on first drug prescriptions
#'
#' @description
#' Returns an S3 List object called FirstDrugObject, which contains all the information necessary to characterise the
#' cohort based on first drug prescriptions. By providing a therapy dataframe, which may have already
#' gone through a series of preprocessing steps, for example, having used getPatientsWithFirstDrugWithDisease first,
#' the algorithm will return a linked patid and eventdate for the first drug prescribed on record.
#'
#' @details
#' The first
#' drug is determined either by (a) what the user provides via a list of prodcodes (any drugs available
#' in the dataframe which are not specified by the user are reported in the fourth element of FirstDrugObject as a data frame)
#' or (b) if no drug list is provided by the user the algorithm will use all drugs in the data frame as the search criteria. For each first drug event
#' identified, the patient patid and the event date are added to the prodcode named list entry in patidList (FirstDrugObject[[1]]) and eventdateList (FirstDrugObject[[2]]).
#'
#' For example, for the product Amitriptyline 10mg, the prodcode 83 is used to name the element in each of the two Lists as described.
#' Thus, for each patient with prodcode 83 coming first (before all drugs or those requested), the patient patid and eventdate of drug
#' prescription is added to the vector inside FirstDrugObject[[1]]`83` and FirstDrugObject[[2]]`83`.
#'
#' The number of first prescriptions for each
#' prodcode is stored in a dataframe that can be accessed in FirstDrugObject[[3]]. The columns are named
#' description, code and Frequency. The Frequency value equals the length of the vector inside the corresponding prodcode named vector
#' of the patidList of FirstDrugObject[[1]] and eventdateList of FirstDrugObject[[2]].
#'
#' There are several subtle behaviours of this function one should be aware of. Firstly, if a list of prodcodes are provided, the
#' fourth element of FirstDrugObject will contain a description and prodcode for all those drugs identified in the therapy dataframe
#' (at any stage in a patient's record) but not part of the user's drug list. This is useful for a very rough description of what types of
#' drugs the cohort are being exposed to. Secondly, it's best to run this function only once and to ensure it's executed over all drugs
#' of interest; take into account any future drugs your study may be considering. If you compare two  separate FirstDrugObjects,
#' the first with drug A and the second with drugs A and B, the number of patients taking drug A as a first line of defence may be reduced
#' after introducing drug B. This happens when a patient took drug B before A, but the function only considers drug B when it's included
#' in the drug list search criteria.
#'
#' @param df Therapy dataframe.
#' @param idList List or vector of patient patids to consider. If this is not included all records in the therapy dataframe will be used.
#' @param prodCodesVector List or vector of prodcodes to consider. If this is not included all available prodcodes in the therapy dataframe will be used.
#' @param descriptionFile A character vector with the file location of the CPRD data dictionary product.txt file. If NULL prodcodes aren't given a description.
#'
#' @return An S3 List object. There are four elements to the list. The first, a list of patid vectors. Each patid vector element is named using the prodcode.
#' The second element is a List of Date vectors. Each Date vector element is named using the prodcode. The third element is a data frame containing the
#' prodcode, description and number of patients with that prodcode as a first drug prescription. The length of each patid vector and Date vector is equal to
#' the corresponding value in the Freq column. The fourth element is a dataframe containing the prodcode and description of any
#' drugs not specified by the user and found in the therapy data frame. If the prodCodesVector is left NULL all drugs become part of the search criteria and therefore
#' the fourth element of the S3 List is NULL.
#' @export
#'
#' @examples
#' fileLocation <- NULL
#' drugList <- unique(testTherapyDF$prodcode)
#' requiredProds <- drugList[1:10]
#' df <- getFirstDrugPrescription(testTherapyDF,
#'                                idList=NULL,
#'                                prodCodesVector=requiredProds,
#'                                descriptionFile=fileLocation)
#'
getFirstDrugPrescription <- function(df, idList=NULL, prodCodesVector=NULL, descriptionFile=NULL) {
  #check everything being sent into the function is ok
  #dfMissing <- is_missing(df)
  #descriptionFileMissing <- is_missing(descriptionFile)

  #if(isTRUE(dfMissing) | isTRUE(descriptionFileMissing)) {
  #  stop("Error: at least one non-default argument in getFirstDrugPrescription is missing")
  #}

  #now check for strange values e.g., NULL, 0 rows, 0 length.
  dfCheck <- errorChecker(df)
  idCheck <- TRUE
  prodCheck <- TRUE
  #descriptionCheck <- TRUE
  if(!is.null(idList)) {
    idCheck <- errorChecker(idList)
  }
  if(!is.null(prodCodesVector)) {
    prodCheck <- errorChecker(prodCodesVector)
  }
  if(!is.null(descriptionFile)) {
    descriptionCheck <- errorChecker(descriptionFile)
  }

  if(isFALSE(idCheck) | isFALSE(prodCheck) |  isFALSE(dfCheck)) {
    print("Please check your code. An argument of getFirstDrugPrescription() is either missing or empty.")
    return(NULL)
  }

  #check if the drugCodes in the prodCodesVector are unique i.e., one copy each.
  n <- length(unique(prodCodesVector))
  if(n != length(prodCodesVector)) {
    print("There are duplicate codes in prodCodesVector. Check your design. Returning NULL. ")
    return(NULL)
  }

  #check if the IDs in idList are unique
  n <- length(unique(idList))
  if(n != length(idList)) {
    print("There are duplicate codes in idList. Check your design. Returning NULL.")
    return(NULL)
  }

  #===========================
  #main body of the function

  #if a specified list of drug codes are supplied then the DF is adjusted
  missingDrugsDF <- NULL
  missingCodes <- NULL
  if(!is.null(prodCodesVector)) {
    print("Using a specified drug list.")
    missingDrugsDF <- subset(df, !(df$prodcode %in% prodCodesVector))
    missingCodes <- unique(missingDrugsDF$prodcode)
    df <- subset(df, df$prodcode %in% prodCodesVector)

    #check description structure for both prodcode and missingcodes
    if(is.list(descriptionFile)) {
      descriptionVector <- unlist(descriptionFile)
      if(length(missingCodes) > 0) {
        #make sure that missingCodes fits inside descriptionVector
        if(isFALSE(sum(missingCodes %in% descriptionVector) == length(missingCodes))) {
          print("The specified list of prodcodes with descriptions does not encapsulate all missing codes within the input frame. Returning NULL.")
          return(NULL)
        }
      }
      #make sure the requested prodcodes match the prodcode-description structure
      if(isFALSE(sum(unique(df$prodcode) %in% descriptionVector) == length(unique(df$prodcode)))) {
        print("The specified list of prodcodes with descriptions does not encapsulate all codes within the input frame. Returning NULL.")
        return(NULL)
      }
    } #end of is.list
  } else {
    print("Using the prodcodes within the df itself.")
    prodCodesVector <- unique(df$prodcode)

    #Using the codes within the df so only need to check description structure for prodcode as no missing codes will appear
    if(is.list(descriptionFile)) {
      descriptionVector <- unlist(descriptionFile)

      if(isFALSE(sum(prodCodesVector %in% descriptionVector) == length(prodCodesVector))) {
        print("The specified list of prodcodes with descriptions does not encapsulate all codes within the input frame. Returning NULL.")
        return(NULL)
      }
    }
  }

  #a list of specific patients is required if idList is not NULL
  if(!is.null(idList)) {
    print("Using a patient ID list.")
    df <- subset(df, df$patid %in% idList)

    #I now need to adjust the idList as some patients will have disappeared if a specified list of drug was provided
    #i.e., they were patients without those drugs
    idList <- getUniquePatidList(df)
  } else {
    #the idList is null - you take everyone!
    print("Using all patients.")
    idList <- getUniquePatidList(df)
  }

  #get the drug names from the product description file or the description structure to build the drugDescriptionDF return object
  if(!is.null(descriptionFile)) {
    if(is.character(descriptionFile)) {
      print("Code descriptions provided by file.")
      descriptionDF <- convertCodesToDescriptions(prodCodesVector, descriptionFile, "PROD")
    } else if(is.list(descriptionFile)) {
      print("Code description provided by list.")
      descriptions <- names(descriptionFile)

      #make the DF but without description
      descriptionDF <- data.frame(code=prodCodesVector, description=NA, Frequency=0)
      for(n in 1:nrow(descriptionDF)) {
        #assign a description given the prodcode
        requestedDescription <- NA
        for(m in 1:length(descriptionFile)) {
          if(descriptionFile[[m]]==descriptionDF$code[n]) {
            requestedDescription <- descriptions[m]
            break()
          }
        }
        descriptionDF$description[n] <- requestedDescription
      }

    } else {
      #descriptionDF <- data.frame(code=prodCodesVector, description=NA, Frequency=0)
      print("Warning: unrecognised code description. Provide either a named list or a character string to a file. Returning NULL.")
      return(NULL)
    }

    #if(!is.null(missingCodes)) {
    if(length(missingCodes) > 0) {
      if(is.character(descriptionFile)) {
        print("Missing code descriptions provided by file.")
        missingDrugsDF <- convertCodesToDescriptions(missingCodes, descriptionFile, "PROD")
      } else if(is.list(descriptionFile)) {
        #this needs testing
        #missingDescriptions <- names(missingCodes %in% descriptionFile)
        missingDrugsDF <- data.frame(code=missingCodes, description=NA)
        for(n in 1:nrow(missingDrugsDF)) {
          requestedDescription <- NA
          for(m in 1:length(descriptionFile)) {
            if(descriptionFile[[m]]==missingDrugsDF$code[n]) {
              requestedDescription <- descriptions[m]
              break()
            }
          }
          missingDrugsDF$description[n] <- requestedDescription
        }
      } else {
        print("Warning: unrecognised code description. Provide either a named list or a character string to a file. Returning NULL.")
        return(NULL)
      }
    }
  } else {
    descriptionDF <- data.frame(code=prodCodesVector, description=NA)
    #if(is.null(missingCodes)) {
    if(length(missingCodes) == 0) {
      missingDrugsDF <- data.frame(code=NA, description=NA)
    } else {
      missingDrugsDF <- data.frame(code=missingCodes, description=NA)
    }
  }

  #drugDescriptionDF has a "code", "description" & "Frequency" column
  drugDistributionDF <- cbind(descriptionDF, Frequency=0)

  #just get the first entry for each patid (so this will be the first drug!)
  #df <- df %>% distinct(patid, .keep_all = TRUE)
  df <- dplyr::distinct(df, df$patid, .keep_all = TRUE)

  #before going on make sure that the desired patients (idList) and the refined drug dataframe (df) are the same size
  stopifnot(nrow(df) == length(idList))

  patientIDVector <- df$patid
  prodcodeVector <- df$prodcode
  eventdateVector <- df$eventdate

  #make a list (empty) length and names defined by the unique prodcodes (drugs)
  #we'll add a vector of patid into the corresponding list elementt
  patidList <- vector("list", length(unique(prodcodeVector)))
  names(patidList) <- unique(prodcodeVector)

  #problem with the data type not being recognised as a Date
  #eventdateList <- vector("list", length(unique(prodcodeVector)))
  startDate <- as.Date("2000-01-01")
  eventdateListSetup <- as.Date(rep(startDate,length(unique(prodcodeVector))))
  eventdateList <- as.list(eventdateListSetup)
  names(eventdateList) <- unique(prodcodeVector)


  #go through each patient and take their first drug, check the right position in the DF and up the frequency by one
  intVecCheck <- integer(0)
  for(i in 1:length(patientIDVector)) {
    prodcode <- prodcodeVector[i]
    drugDistributionDF[drugDistributionDF$code==prodcode,]$Frequency <- drugDistributionDF[drugDistributionDF$code==prodcode,]$Frequency + 1
    id <- patientIDVector[i]
    eventdate <- eventdateVector[i]

    #add the eventdate of when the first drug occured for that patient
    #if(i==1) {
    #if(eventdateList[[as.character(prodcode)]] == 1) {

    #}
    #  eventdateList[[as.character(prodcode)]] <- as.Date(eventdate)
    #} else {
    #eventdateList[[as.character(prodcode)]] <- c(eventdateList[[as.character(prodcode)]], eventdate)
    #}

    #we've seen this drug
    if(sum(prodcode %in% intVecCheck) > 0) {
      eventdateList[[as.character(prodcode)]] <- c(eventdateList[[as.character(prodcode)]], eventdate)
      patidList[[as.character(prodcode)]] <- c( patidList[[as.character(prodcode)]], id)
    } else {
      #new drug
      intVecCheck <- rlist::list.append(intVecCheck, prodcode)
      eventdateList[[as.character(prodcode)]] <- as.Date(eventdate)
      patidList[[as.character(prodcode)]] <-id
    }

    #list.append(eventdateList[[as.character(prodcode)]], eventdate)
    #adding the patient ID to the relevant list element denoted by the prodcode
    #patidList[[as.character(prodcode)]] <- c( patidList[[as.character(prodcode)]], id)
  }

  #remove drug frequencies which are zero
  drugDistributionDF <- subset(drugDistributionDF, drugDistributionDF$Frequency > 0)

  #create an S3 object to hold a list of patids denoted by which drug they took first and a dataframe of drugIDs, description
  #and the number of times they were ued as first drug.
  returnS3List <- list(patidList = patidList, eventdateList = eventdateList, drugDistributionDF = drugDistributionDF, missingDrugsDF=missingDrugsDF)
  class(returnS3List) <- "FirstDrugObject"

  return(returnS3List)

}


#===================================================================================================================
#' Return drug summary data for a single patient
#'
#' @description
#' Calculates basic summary  statistics of a single patient's therapy record.
#'
#' @details
#' The following information is returned in a List of two elements The first element is a vector of the time in days between each
#' drug prescription and the second element is a data frame with the following information  (see the example): patid of patient, number of event dates, median days between events,
#' minimum time between events, maximum time between events, longest duration between events, and record duration (from first to last record or 0 if only one event).
#'
#' @param df Dataframe of a patient's therapeutic records.
#' @param prodCodesVector vector of product codes to use. All other codes in the df are ignored.
#'
#' @return S3 EventdateSummaryObj object of patient prescription information.
#' @export
#'
#' @examples
#' idList <- getUniquePatidList(testTherapyDF)
#' resultList <- getEventdateSummaryByPatient(
#'    testTherapyDF[testTherapyDF$patid==idList[[1]],]
#' )
#' str(resultList)
#' #List of 2
#' #$ TimeSeriesList: num [1:6] 336 652 2540 34 42 44
#' #$ SummaryDF     :'data.frame':	1 obs. of  7 variables:
#' #..$ patid          : int 3101001
#' #..$ numberOfEvents : int 7
#' #..$ medianTime     : num 190
#' #..$ minTime        : num 34
#' #..$ maxTime        : num 2540
#' #..$ longestDuration: num 2540
#' #..$ recordDuration : int 3648
#' #- attr(*, "class")= chr "EventdateSummaryObj"
#'
getEventdateSummaryByPatient <- function(df, prodCodesVector=NULL) {
  dfCheck <- errorChecker(df)
  prodCheck <- TRUE
  if(!is.null(prodCodesVector)) {
    prodCheck <- errorChecker(prodCodesVector)
  }
  if(isFALSE(prodCheck) | isFALSE(dfCheck)) {
    print("Please check your code. An argument of getEventdateSummaryByPatient() is either missing or empty.")
    return(NULL)
  }
  n <- length(getUniquePatidList(df))
  if(n != 1) {
    print("Multiple patients have been supplied to getEventdateSummaryByPatient. This function must only have a single patient. Returning NULL.")
    return(NULL)
  }

  #get all the eventdates from the patient and check whether specific codes have been supplied
  if(!is.null(prodCodesVector)) {
    df <- df[df$prodcode %in% prodCodesVector,]
    if(nrow(df) == 0) {
      print("The patient had no event dates for those codes. Returning NULL.")
      return(NULL)
    }
    eventdateVector <- df$eventdate
  } else {
    eventdateVector <- df$eventdate
  }

  longestDuration <- 0
  #determine the logic if there is only one event
  if(length(eventdateVector)==1) {
    timeDiffVector <- c(0)
  } else {
    timeDiffVector <- as.integer(rep(0,length(eventdateVector)-1))
    firstEvent <- eventdateVector[1]
    #go through all the events (take the first and last to work out the total length of the record)
    for(i in 1:length(eventdateVector)) {
      if(i==length(eventdateVector)) {
        lastEvent <- eventdateVector[i]
        break()
      }
      timeDiff <- as.numeric(eventdateVector[i+1] - eventdateVector[i])
      #for a broken record
      if(timeDiff < 0) {
        print(paste("Setting durations to 0 as dates are not ordered for patient",df$patid[1]))
        timeDiffVector[i] <- NA
        longestDuration <- NA
        firstEvent <- NA
        lastEvent <- NA
        eventdateVector <- NA
        break()
      }
      timeDiffVector[i] <- timeDiff
      if(timeDiff > longestDuration) {
        longestDuration <- timeDiff
      }
    }
  }

  patid <- df$patid[1]

  numberEvents <- length(eventdateVector)
  medianTime <- stats::median(timeDiffVector)
  minTime <- min(timeDiffVector)
  maxTime <- max(timeDiffVector)
  recordDuration <- 0
  if(numberEvents > 1) {
    recordDuration <- as.integer(lastEvent - firstEvent)
  }

  summaryDF <- data.frame(patid=patid, numberOfEvents=numberEvents, medianTime=medianTime, minTime=minTime, maxTime=maxTime, longestDuration=longestDuration, recordDuration=recordDuration)
  resultList <- list(TimeSeriesList=timeDiffVector, SummaryDF=summaryDF)
  class(resultList) <- "EventdateSummaryObj"

  return(resultList)
}


#===================================================================================================================
#' Returns population level drug summary
#'
#' @description
#' Returns population level drug summary by calling \code{\link{getEventdateSummaryByPatient}} for each patient.
#'
#' @details
#' See the example for the structure of the returned S3 object.
#'
#' @param df Dataframe of therapeutic records. Requires columns "patid" and "prodcode".
#' @param prodCodesVector Vector of prodcodes. If NULL (default), then all prodcode in the therapy dataframe are considered.
#'
#' @return S3 PopulationEventdateSummary object (a List) containing patient level EventdateSummaryObj objects extracted into
#' a data frame in the first element and a list of duration in days between drug prescriptions on a patient level basis.
#' @export
#'
#' @examples
#' resultList <- getPopulationDrugSummary(testTherapyDF, prodCodesVector=NULL)
#' str(resultList)
#' #List of 2
#' #$ SummaryDF     :'data.frame':	3838 obs. of  7 variables:
#' #  ..$ patid          : int [1:3838] 3101001 3235001 3333001 3389001 379002
#' #..$ numberOfEvents : int [1:3838] 7 21 1 1 13 2 15 2 23 79 ...
#' #..$ medianTime     : num [1:3838] 190 60 0 0 28.5 ...
#' #..$ minTime        : num [1:3838] 34 34 0 0 11 ...
#' #..$ maxTime        : num [1:3838] 2540 1623 0 0 322 ...
#' #..$ longestDuration: num [1:3838] 2540 1623 0 0 322 ...
#' #..$ recordDuration : num [1:3838] 3648 5329 0 0 630 ...
#' #$ TimeSeriesList:List of 3838
#' #..$ 3101001: num [1:6] 336 652 2540 34 42 44
#' #..$ 3235001: num [1:20] 890 222 182 301 539 ...
#' #..$ 3333001: num 0
#' #..$ 3389001: num 0
#' #..$ 379002 : num [1:12] 26 23 24 24 32 322 31 29 11 51 ...
#' #..$ 487002 : num 1254
#' #..$ 637002 : num [1:14] 29 587 9 34 2 0 24 0 22 0 ...
#' #.. [list output truncated]
#' #- attr(*, "class")= chr "PopulationEventdateSummary"
#'
getPopulationDrugSummary <- function(df, prodCodesVector=NULL) {
  dfCheck <- errorChecker(df)
  prodCodesCheck <- TRUE
  if(!is.null(prodCodesVector)) {
    prodCodesCheck <- errorChecker(prodCodesVector)
  }

  #check that the data passed in is not malformed in any way
  if(isFALSE(dfCheck) | isFALSE(prodCodesCheck)) {
    print("Please check your code. An argument of getPopulationDrugSummary() is either missing or empty.")
    return(NULL)
  }

  #if prodCodesVector is not null then only keep the events from those codes.
  if(!is.null(prodCodesVector)) {
    df <- subset(df, df$prodcode %in% prodCodesVector)
    if(nrow(df)==0) {
      print("There were no patients with those codes.")
      return(NULL)
    }
  }

  #send off each patient to getEventdateSummaryByPatient and keep a list of the results
  summaryList <- list()
  timeSeriesList <- list()
  idList <- getUniquePatidList(df)
  count <- 1
  for(id in idList) {
    indDF <- df[df$patid==id,]
    eventdateSummaryObj <- getEventdateSummaryByPatient(indDF, prodCodesVector)
    if(!is.null(eventdateSummaryObj)) {
      summaryList[[count]] <- eventdateSummaryObj$SummaryDF
      tempList <- list()
      tempList[[as.character(id)]] <- eventdateSummaryObj$TimeSeriesList
      timeSeriesList <- c(timeSeriesList,tempList)
      count <- count + 1
    }
  }

  if(length(summaryList) == 0) {
    print("No patients were identified. Returning NULL")
    return(NULL)
  } else {
    summaryDF <- do.call(rbind,summaryList)
    returnList <- list(SummaryDF=summaryDF, TimeSeriesList=timeSeriesList)
    class(returnList) <- "PopulationEventdateSummary"
    return(returnList)
  }
}

#===================================================================================================================
#Given a setting e.g., clinical (face to face) or over the phone, return a clinicalDF of patients where the first
#presecription of any drug (in the FirstDrugObject) has a eventdate that matches
#e.g.,
#Return a clinicalDF of all patients whose first drug prescription was on the same day as a headache consultation over the phone
#Return a clinicalDF of all patients whose first drug prescription was on the same day as a headache consultation in person
#
#This could be used to answer questions such as "what's the proportion of first prescriptions given over the phone compared to in person?"

#Execute with a degree of caution. If a patient has two events on the same day, each of which have a different consid ID, bot will be returned.
#
#clinicalDF - a data.frame of clinical data (complete of otherwise)
#FirstDrugObject - an object containing the firstevent date for drugs along with the patids of those patients
#settingCode - a vector of considIDs which are matched against the clinicalDF. Those clinicalDF entries without a matching considID are removed.
#specificPrescription - a vector of drug codes when provided are used. If NULL (default) all drugcodes in the FirstDrugObject are used.
#Return - a clinical data.frame of patients with a drug prescribed with a matching eventdate on a particular setting e.g., phone, in-person, emergency etc.
# Title
#
# @param clinicalDF
# @param FirstDrugObject
# @param settingCode
# @param specificPrescription
#
# @return
# @export
#
# @examples
#getSettingFromFirstPrescription <- function(clinicalDF, FirstDrugObject, settingCode=NULL, specificPrescription=NULL) {
#  clinicalDFCheck <- errorChecker(clinicalDF)
#  FirstDrugObjectCheck <- errorChecker(FirstDrugObject)
#  settingCodeCheck <- TRUE
#  if(!is.null(settingCodeCheck)) {
#    settingCodeCheck <- errorChecker(settingCode)
#  }
#  specificPrescriptionCheck <- TRUE
#  if(!is.null(specificPrescription)) {
#    specificPrescriptionCheck <- errorChecker(specificPrescription)
#  }
#  if(isFALSE(clinicalDFCheck) | isFALSE(settingCodeCheck) | isFALSE(FirstDrugObjectCheck) | isFALSE(specificPrescriptionCheck)) {
#    print("Please check your code. An argument of getSettingFromFirstPrescription() is either missing or empty.")
#    return(NULL)
#  }
#
#
#
#}

#===================================================================================================================
#Unlike getFirstDrugPrescriptionByYear this function just looks at drugs prescribed in the year range and reports
#how many prescriptions; this is not concerned whether it's the first prescription of a particular drug.
#getAnyDrugPrescriptionByYear <- function(therapyDF, yearVector, drugList, idList) {
#
#}

#FINISHED - NEEDS TESTING
#===================================================================================================================
#' First prescription frequencies per drug by year.
#'
#' Note: this function operates over only those patients with a first-drug prescription event. Retrusn a reclassified (S3) List object. Each
#' list element is named using the year values provided (yearVector) and holds a dataframe of drug names and frequencies.
#'
#' @param firstDrugObject will contain the drug names, number of patients, and the data on which the prescription was recorded.
#' @param yearVector vector of years e.g., c(2000,2004,2008) or c(2000:2016).
#'
#' @return S3 DrugByYearObj object (list) that holds data frames denoting drug frequency per year requested.
#' @export
#'
#' @examples
#' fileLocation <- NULL
#' drugList <- unique(testTherapyDF$prodcode)
#' requiredProds <- drugList[1:10]
#' fdo <- getFirstDrugPrescription(testTherapyDF,
#'                                 idList=NULL,
#'                                 prodCodesVector=requiredProds,
#'                                 descriptionFile=fileLocation)
#' yearList <- getFirstDrugPrescriptionByYear(fdo, "2001")
#'
getFirstDrugPrescriptionByYear <- function(firstDrugObject, yearVector) {
  if(is.null(firstDrugObject)) {
    print("Please check your code as FirstDrugObject was NULL. Returning NULL.")
    return(NULL)
  }
  if(length(firstDrugObject) != 4) {
    print("Please check your code as FirstDrugObject does not contain the usual three objects. Returning NULL.")
    return(NULL)
  }
  yearCheck <- TRUE
  if(!is.null(yearCheck)) {
    yearCheck <- errorChecker(yearVector)
  }
  if(isFALSE(yearCheck)) {
    print("Please check your code. An argument of getFirstDrugPrescriptionByYear() is either missing or empty.")
    return(NULL)
  }

  #For the FirstDrugObject which is the first input into this function.
  #-----------------------
  #The first element of FirstDrugObject is a List of patid vector e.g., the first list element is a vector of patids. The first
  #list element corresponds to the first row in the accompany data.frame (see second element of FirstDrugObject). The second list
  #element corresponds to the second row in the data.frame
  #The second element of FirstDrugObject is a List of eventdate Date with the identical structure to the patidList (above).
  #The third element of FirstDrugObject is a drug data.frame of description| code | Frequency. Each row corresponds to the element index of the patidList.
  patidList <- firstDrugObject[[1]]
  eventdateList <- firstDrugObject[[2]]
  drugFrequencyDF <- firstDrugObject[[3]]

  #this is the object which will be populated with N number of data.frames, where N is the number of entries in the yearVector argument.
  #the data.frame per year entry in the list (DrugByYearObj), should be of the design
  drugByYearList <- vector("list", length(yearVector))
  names(drugByYearList) <- yearVector
  class(drugByYearList) <- "DrugByYearObj"

  #create a list of data.frames that will be used to contain the frequency of drugs found per year, and where each year is labeled in the list entry.
  for(i in 1:length(yearVector)) {
    drugByYearList[[i]] <- data.frame(description=drugFrequencyDF$description, code=drugFrequencyDF$code, Frequency=0)
  }

  #Go through each dataframe of first drugs prescribed
  #corresponnds to the list index in eventdateList as a first prescription.
  #Go through each drug and match the year for the eventdate to the yearVector

  #i = the drug we are on
  for(i in 1:nrow(drugFrequencyDF)) {
    #drugCode <- drugFrequencyDF$code[i]
    #drugDescription <- drugFrequencyDF$description[i]

    #get out the patid and eventdate information for the drug entry determined by the index i
    eventdatesVector <- eventdateList[[i]]
    patidVector <- patidList[[i]]

    #go through each requested year
    for(n in 1:length(yearVector)) {
      #get out the relevant data frame for storing the frequency values of first prescription by year
      currentYearDF <- drugByYearList[[n]]
      frequencyCountForDrug <- 0

      #go through all the event dates looking for a matching entry
      for(j in 1:length(eventdatesVector)) {
        eventdate <- eventdatesVector[j]
        #get out the year
        eventdateYear <- format(as.Date(eventdate, format="%Y-%m-%d"),"%Y")
        #does the year belong to the one requested?
        if(eventdateYear %in% yearVector[n]) {
          #I have found a first event date that was in a require year.
          #Take the year from the event date to know where it goes....
          #Add it to some kind of data frame or list or S3 oject.
          frequencyCountForDrug <- frequencyCountForDrug + 1

        } #else go to the next event vector

      } #end of eventdate for each patients.
      currentYearDF$Frequency[i] <- frequencyCountForDrug
      drugByYearList[[n]] <- currentYearDF
    } #end of yearVector loop

  }

  return(drugByYearList)
}


#' Calculates an incidence rate for prescription in total person-time
#'
#' @description
#' Incidence rate of first prescription allows those enrolled to enter and leave the study at will. Person-time
#' assumes equal probability of incident i.e., 10 people followed for 1 year is the same as 1 person followed for 10 years.
#' Person-time is also known as person-years. Please read through the details very carefully.
#'
#' @details
#' Records/indicators/events are used interchangeably, they are synonymous with the eventdate column
#' in the medHistoryDF object (also, a clinical, referral and therapy dataframe).
#'
#' As a good number of patient records will have no information on their entering or
#' leaving the CPRD/electronic healthcare record dataset, as much of their medical
#' data as possible is required to determine whether a person will contribute to a particular year. By observing the absence of health indicators,
#' one does not have to provide death-data and one is able to include patients who are lost to followup (LTF) cases
#' when a patient may (a) have transferred to a different medical practice not enrolled on the CPRD scheme or
#' (b) the patient gave up on treatment.
#'
#' The accuracy of incidence rate is dependent on the amount of information provided by the user. The precision is
#' down to half person-years.
#'
#' The observation period is defined from the enrollmentDate through to the the studyEndDate. Please ensure these
#' are year start/end dates to ensure the half-person-year contributions make sense.
#'
#' This function encapsulates the following behaviour:
#'
#' 1) Only those years from the enrollmentDate to the studyEndDate make up the person-years of the population at risk. This is
#' only possible if there are recorded health indicators to parse. A patient must have at least one record to parse, this can be the
#' first drug prescription date if that is their only record.
#'
#' 2) Patients are not included if their drug prescription event happened before the study begins.
#'
#' 3) On entry into a study, a patient will contribute one-full person-year if the first medical indicator on record
#' happened within the first six months (half year) of that study opening year. If the medical indicator on record
#' happened after or on the 1st of July of that opening year, that patient contributes half a person-year.
#' This definition for half a year continues throughout.
#'
#' 4) If health indicators in a patient's record stop before the second-half of the last year (e.g., their last indicator is 2010-04-24
#' and the observation period ends 2010-12-31) of the observation period then that patient is LTF and they contribute half a person-year.
#' All records with a date inside the last half of the final year will contribute a full person-year. Any patient with a single event
#' can only contribute half a person-year before (a) being LTF as nothing happened after, (b) being LTF as this happened within the last half
#' of the final observation year, or (c) that single event was a first drug prescription and they contributed half a person-year.
#'
#' 5) Contribution to person-years stops when a first drug prescription event is found within the observation period. The position of that
#' drug prescription date is determined, half a person-year is contributed if the prescription date falls within the first half of that year
#' and a full person-year is contributed if the prescription date falls within the second half of that year. Before counting contributions
#' to person-years, the function removes all events after a first drug prescription (if within the observation period). If the drug
#' prescription observation happens after the end of the observation period, it won't count, the algorithm also removes all events after
#' the end of the observation period (for speed).
#'
#' 6) Patients do not need a record for every year from study entry and up to the end of the observation. The first record from entry
#' dictates the behavior of the first contribution of person-year (as described above in (3)), the last record (either LTF, end of
#' study, or first drug prescription) within the observation period dictates the behaviour of the last person-year contribution
#' (as described above in (4)). All years bound between the first valid and last valid indicators are counted as part of the observation
#' study period, and each year contributes to a person-year.
#'
#' 7) If a patient has all record data before the start of the observation period, they will not enter the study (one record must occur
#' within the study period for that patient to contribute). If the first record happens after the end of the observation period, they
#' are not included.
#'
#' 8) If there are no patients left, having first removed those records that are not appropriate for the defined
#' observation period, the algorithm will return NULL.
#'
#' The function will attempt to run in parallel using half the number of available cores. Please note, the more information
#' used the more reliable the results, but the slower the calculation. There are plans to optimise this code in later releases.
#'
#' @param firstDrugObject FirstDrugPrescription object.
#' @param medHistoryDF All clinical, referral and therapy data available for the cohort. This information
#' must also include the therapy data used to retrieve the FirstDrugPrescription object. If not, those patients
#' will be removed.
#' @param enrollmentDate Date object of when to beginn observing e.g., as.Date("2000-01-01"). The enrollment is
#' always treated as the start of the year.
#' @param studyEndDate Date object of when the observation ends e.g., as.Date("2010-12-31"). The end date is
#' always treated as the end of the year.
#'
#' @return Matrix containing patient person-years contribution.
#' @export
#'
#' @examples
#' drugList <- unique(testTherapyDF$prodcode)
#' requiredProds <- drugList[1:10]
#' #what we have here is a slimmed down data dictionary file - this has a very specific layout
#' fileLocation <- NULL
#' firstDrugObject <- getFirstDrugPrescription(testTherapyDF,
#'                                             idList=NULL,
#'                                             prodCodesVector=requiredProds,
#'                                             descriptionFile=fileLocation)
#'enrollmentDate <- as.Date("2000-01-01")
#'studyEndDate <- as.Date("2016-12-31")
#'medhistoryDF <- constructMedicalHistory(testClinicalDF, NULL, testTherapyDF)
#'patidList <- unlist(firstDrugObject$patidList)
#'resultMatrix <- getFirstDrugIncidenceRate(firstDrugObject,
#'                                          medhistoryDF,
#'                                          enrollmentDate,
#'                                          studyEndDate)
#'
getFirstDrugIncidenceRate <- function(firstDrugObject, medHistoryDF, enrollmentDate, studyEndDate) {

  #divide the set in two (1) all those who do not take a first drug prescription (2) all those who DO take
  #a first drug prescription.
  firstDrugPatidVector <- unlist(firstDrugObject[[1]])
  #firstDrugDateVector <- unlist(firstDrugObject[[2]]) unlist does not preserve Date objects
  firstDrugDateVector <- do.call("c", firstDrugObject[[2]])

  #determine whether all prescription first drug patients have a medical record.
  medHistoryWithFirstPrescriptionDF <- medHistoryDF[medHistoryDF$patid %in% firstDrugPatidVector,,drop=FALSE]
  patidsWithPrescriptionAndMedHistory <- getUniquePatidList(medHistoryWithFirstPrescriptionDF)
  if(length(firstDrugPatidVector) != length(patidsWithPrescriptionAndMedHistory)) {
    print("Error: ensure that all patients with a first drug prescription entry have a set of records in medHistoryDF. Returning NULL.")
    return(NULL)
  }

  #entryExitDateList <- List()
  #reduce the medHistoryDF in size and convert to a data.table to make retrieval quicker.
  #miniMedHistoryDT <- data.table::as.data.table(medHistoryDF[,c("patid","eventdate")])
  miniMedHistoryDT <- medHistoryDF[,c("patid","eventdate")]
  #patidList <- getUniquePatidList(miniMedHistoryDT)

  #pull out all patients and retrieve their first and last records. This is patient-entry and potential patient-LTO.
  #there must be at least two records for this to work.
  #
  #At the moment I am not sure whether this is right. Should a patient be able to contribute half a person-year
  #if they only have one record?
  #
  #patidListBoolean <- lapply(patidList, function(x){
  #  miniDF <- miniMedHistoryDT[miniMedHistoryDT$patid==x,, drop=FALSE]
  #  if(isTRUE(nrow(miniDF) >=2)) {
  #    return(TRUE)
  #  } else {
  #    return(FALSE)
  #  }
  #})
  #patidListBoolean <- patidListBoolean[patidListBoolean==TRUE]
  #only those with two dates:
  #miniMedHistoryDT <- miniMedHistoryDT[miniMedHistoryDT$patid %in% names(patidListBoolean),, drop=FALSE]


  #determine whether the first drug prescription object eventdates fall before the observation period starts
  #these patients had previously been exposed and cannot be included in the study
  firstDrugDateBoolean <- lapply(firstDrugDateVector, function(x) {
    #xYear <- substr(as.character(x),1,4)
    if(x < enrollmentDate) {
      return(FALSE)
    } else {
      #keep this records
      return(TRUE)
    }
  })


  toRemoveFirstDrugVector <- firstDrugPatidVector[firstDrugDateBoolean==FALSE]
  miniMedHistoryDT <- miniMedHistoryDT[!(miniMedHistoryDT$patid %in% toRemoveFirstDrugVector),, drop=FALSE]
  if(nrow(miniMedHistoryDT) == 0) {
    print("After removing all patients with a first drug prescription before the start of the study there were no patients left!")
    print("Warning: the cohort failed to supply valid patients. Returning NULL.")
    return(NULL)
  }

  #those left had an event AFTER the start of the observation period
  firstDrugPatidVector <- firstDrugPatidVector[firstDrugDateBoolean==TRUE]
  firstDrugDateVector <- firstDrugDateVector[firstDrugDateBoolean==TRUE]

  #remove all indications that happen before the start of the observation
  allEventdatesVector <- miniMedHistoryDT$eventdate
  recordsToRemoveBeforeStartLogic <- lapply(allEventdatesVector, function(eventdate) {
    if(eventdate < enrollmentDate) {
      return(FALSE) #do not keep these records
    } else {
      return(TRUE)
    }
  })
  miniMedHistoryDT <- miniMedHistoryDT[unlist(recordsToRemoveBeforeStartLogic),]
  if(nrow(miniMedHistoryDT) == 0) {
    print("After removing all eventdates that happened before the start of the obervation there were no patientsleft!")
    print("Warning: the cohort failed to supply valid patients. Returning NULL.")
    return(NULL)
  }

  #now: remove all of those who enter AFTER the end date.
  patidList <- getUniquePatidList(miniMedHistoryDT)
  patidListBoolean <- lapply(patidList, function(x){
    miniDF <- miniMedHistoryDT[miniMedHistoryDT$patid==x,, drop=FALSE]
    #is the first record after the end of the study?
    firstDate <- miniDF$eventdate[1]
    patientIsValid <- TRUE
    if(firstDate >= studyEndDate) {
      patientIsValid <- FALSE
    } else {
      patientIsValid <- TRUE
    }

    #if all the records begin before the start of the observation? They never enrolled.
    endDate <- miniDF$eventdate[nrow(miniDF)]
    if(endDate < enrollmentDate) {
      if(patientIsValid == TRUE) {
        patientIsValid <- FALSE
      }
    }

    return(patientIsValid)
  })

  #only keep these patients
  patidList <- patidList[patidListBoolean==TRUE]
  miniMedHistoryDT <- miniMedHistoryDT[miniMedHistoryDT$patid %in% patidList, , drop=FALSE]
  if(nrow(miniMedHistoryDT) == 0) {
    print("After removing all patients with all records before or after the observation period there were no patients left!")
    print("Warning: the cohort failed to supply valid patients. Returning NULL.")
    return(NULL)
  }


  #build a matrix to assign values to a patient-year basis.
  startDate <- substr(as.character(enrollmentDate),1,4)
  endDate <- substr(as.character(studyEndDate),1,4)
  yearDuration <- as.numeric(endDate) - as.numeric(startDate) + 1 #+1 requires as 2004-2005 must equal five years



  #calculate the years-at-risk per person for all those who do not have a first drug prescription. Use the
  #enrollment year and study period to help.


  #work on those who take a prescription
  prescriptionMedHistoryDT <- miniMedHistoryDT[miniMedHistoryDT$patid %in% firstDrugPatidVector, , drop=FALSE]
  if(nrow(prescriptionMedHistoryDT) == 0) {
    print("There are no patients with a first drug object having first trimmed the cohort for patients that can enter the study period. Returning NULL")
    return(NULL)
  }

  #go through each patient who has a first drug event
  #remove all records for that patient after the end
  adjustedPrescribedMedHistoryList <- list()
  listCounter <- 1
  for(i in 1:length(firstDrugPatidVector)) {
    id <- firstDrugPatidVector[i]
    firstDrugEventdate <- firstDrugDateVector[i]
    indDF <- prescriptionMedHistoryDT[prescriptionMedHistoryDT$patid == id, , drop=FALSE]
    if(nrow(indDF)==0) {
      next()
    }
    indDF <- indDF[indDF$eventdate <= firstDrugEventdate,]
    if(nrow(indDF)==0) {
      next()
    }
    adjustedPrescribedMedHistoryList[[listCounter]] <- indDF
    listCounter <- listCounter + 1
  }

  adjustedPrescribedMedHistoryDF <- do.call(rbind, adjustedPrescribedMedHistoryList)

  #THE ABOVE DATA FRAME NOW CONTAINS ALL MED HISTORY FOR A PATIENT WITH A FIRST DRUG PRESCRIPTION EVENT.
  #THE DATA.FRAME WAS TRIMED TO REMOVE ALL EVENTS THAT HAPPENED AFTER THE CLINICAL EVENT
  #I SHOULD TRY AND ADJUST THE LOOP BEHAVIOUR BELOW.i.e., if you get to the last record



  patidPrescriptionList <- getUniquePatidList(adjustedPrescribedMedHistoryDF)
  #work on those who do not have a prescription
  otherMedHistoryDT <- miniMedHistoryDT[!(miniMedHistoryDT$patid %in% patidPrescriptionList), , drop=FALSE]
  #patidNonPrescriptionList <- getUniquePatidList(otherMedHistoryDT)

  newFullMedHistoryDF <- rbind(otherMedHistoryDT,adjustedPrescribedMedHistoryDF)
  patidValidList <- getUniquePatidList(newFullMedHistoryDF)

  #+1 to row and column to fit in totals
  populationMatrix <- matrix("0",nrow = (length(patidValidList)+1), ncol = (yearDuration+1))
  row.names(populationMatrix) <- c(names(patidValidList), "Total")
  colnames(populationMatrix) <- c(as.character(startDate:endDate), "Years at risk")


  fullYearStr <- "F"
  halfYearStr <- "H"
  fullYearExposureStr <- "Fx"
  halfYearExposureStr <- "Hx"

  #for those without a prescription - there may be none! That's ok.
  for(i in 1:length(patidValidList)) {
    #patid <- patidNonPrescriptionList[[i]]
    patid <- patidValidList[[i]]
    indDT <- newFullMedHistoryDF[newFullMedHistoryDF$patid == patid, , drop=FALSE]

    #what happens if it's just the one entry? firstDate and lastDate become the same?
    currentDateVector <- indDT$eventdate

    #what happens if all event dates occur within the one year?
    firstDate <- currentDateVector[1]
    lastDate <- currentDateVector[nrow(indDT)]
    firstDateYear <- substr(as.character(firstDate),1,4)
    lastDateYear <- substr(as.character(lastDate),1,4)

    #the first and the last record happen in the same year
    if(firstDateYear == lastDateYear) {
      duration <- as.numeric(lastDate - firstDate)
      if(duration >= 178) {
        #contribute a full year
        populationMatrix[which(row.names(populationMatrix) == patid), which(colnames(populationMatrix) == firstDateYear)] <- fullYearStr
      } else {
        #contribute half a year
        populationMatrix[which(row.names(populationMatrix) == patid), which(colnames(populationMatrix) == firstDateYear)] <- halfYearStr
      }
    } else {

      patientLastDate <- currentDateVector[nrow(indDT)]
      PatientLastDateAsChar <- as.character(patientLastDate)
      patientLastYear <- substr(PatientLastDateAsChar,1,4)

      firstYear <- NULL
      lastYearVisited <- NULL
      #multiple years have been found - loop through the records
      for(j in 1:nrow(indDT)) {
        currentDate <- currentDateVector[j]
        currentDateAsChar <- as.character(currentDate)
        currentYear <- substr(currentDateAsChar,1,4)

        #first record, which half of the year does it occur
        if(j == 1) {
          halfYearDate <- as.Date(paste0(currentYear,"-07-01"))
          lastYearVisited <- currentYear

          firstYear <- currentYear
          #count as a full year
          if(currentDate < halfYearDate) {
            populationMatrix[which(row.names(populationMatrix) == patid), which(colnames(populationMatrix) == currentYear)] <- fullYearStr
          } else {
            populationMatrix[which(row.names(populationMatrix) == patid), which(colnames(populationMatrix) == currentYear)] <- halfYearStr
          }

        #last record, what half of the year does it occur
        } else if(j == nrow(indDT)) {
          #check whether this record is beyond the observation date
          if(currentDate >= studyEndDate) {
            break()
          }

          convertedHalfYearStr <- fullYearStr
          convertedFullYearStr <- halfYearStr
          #only the last record can have the firstDrugEvent (we trimmed all those patients with a drug event)
          if(sum(patid %in% patidPrescriptionList)==1) {
            dateIndex <- which(firstDrugPatidVector == patid)
            firstDrugDate <- firstDrugDateVector[dateIndex]
            if(firstDrugDate == currentDate) {
              convertedHalfYearStr <- halfYearExposureStr
              convertedFullYearStr <- fullYearExposureStr
            }
          }

          #there might be white space between years (including the last year and the one-before-last)
          yearDiffInt <- as.numeric(currentYear) - as.numeric(lastYearVisited)
          if(yearDiffInt > 1) {
            #greater than one year and it means we had no records to parse to fill those gap years
            populationMatrix <- populateMissingYears(populationMatrix, currentYear, lastYearVisited, patid)
          }

          #does this happen after the last observation year
            if(patientLastYear == currentYear) {
              halfYearDate <- as.Date(paste0(currentYear,"-07-01"))
              #count as half a full year
              if(currentDate < halfYearDate) {
                populationMatrix[which(row.names(populationMatrix) == patid), which(colnames(populationMatrix) == currentYear)] <- convertedHalfYearStr
              } else {
                populationMatrix[which(row.names(populationMatrix) == patid), which(colnames(populationMatrix) == currentYear)] <- convertedFullYearStr
              }
            }

        } else { #all records not the first and not the last
          #check whether this record is beyond the observation date
          if(currentDate >= studyEndDate) {
            break()
          }

          #this is very inefficient - as multiple records happening in the same year will cause multiple writes to the same
          #check whether the the currentYear is the same as the patients last Year
          if(currentYear == patientLastYear) {
            #perform a check to see whether any of the entries go beyond the half-way point
            halfYearDate <- as.Date(paste0(currentYear,"-07-01"))
            if(currentDate > halfYearDate) {
              populationMatrix[which(row.names(populationMatrix) == patid), which(colnames(populationMatrix) == currentYear)] <-fullYearStr
            } else {
              populationMatrix[which(row.names(populationMatrix) == patid), which(colnames(populationMatrix) == currentYear)] <- halfYearStr
            }
          } else if(currentYear == firstYear) { #do not try to repopulate the first year
            next()
          } else {
            #there might be white space between years (including the last year and the one-before-last)
            yearDiffInt <- as.numeric(currentYear) - as.numeric(lastYearVisited)
            if(yearDiffInt > 1) {
              #greater than one year and it means we had no records to parse to fill those gap years
              populationMatrix <- populateMissingYears(populationMatrix, currentYear, lastYearVisited, patid)
            }

            populationMatrix[which(row.names(populationMatrix) == patid), which(colnames(populationMatrix) == currentYear)] <- fullYearStr
            lastYearVisited <- currentYear
          }
        } #if not the first or last record


      } #if the occur across years then begin loop through all records

    } # if/else whether all the records occur within the same year

  } #loop - those without a prescription


  #calculate the Years at risk for each individual
  yearsAtRiskList <- lapply(unlist(patidValidList), function(x) {
    patientRow <- populationMatrix[as.character(x),]
    numFullYears <- sum(patientRow == "F") +sum(patientRow == "Fx")
    numHalfYears <- ((sum(patientRow == "H") + sum(patientRow == "Hx"))/2)
    return(numFullYears+numHalfYears)
    #populationMatrix[which(row.names(populationMatrix) == x),c("Years at risk")] <- as.character(patientPersonYears)
  })
  yearsAtRiskVector <- c(unlist(yearsAtRiskList),0) #must put an additional 0 at the end so the matrix is valid
  populationMatrix[,which(colnames(populationMatrix) == "Years at risk")] <- yearsAtRiskVector

  eventsInYearList <- lapply(colnames(populationMatrix), function(x) {
    colVector <- populationMatrix[,x]
    numberOfEvents <- sum(colVector == "Hx") + sum(colVector == "Fx") + sum(colVector == "F") + sum(colVector == "H")
    #populationMatrix[which(row.names(populationMatrix) == "Total"),x] <- as.character(numberOfEvents)
    return(numberOfEvents)
  })
  numberOfEventsVector <- unlist(eventsInYearList)
  populationMatrix[which(row.names(populationMatrix) == "Total"),] <- numberOfEventsVector
  populationMatrix[which(row.names(populationMatrix) == "Total"),dim(populationMatrix)[2]] <- sum(as.numeric(populationMatrix[nrow(populationMatrix),1:(dim(populationMatrix)[2]-1)]))

  #count the number of prescription events (Hx and Fx)
  totalEvents <- sum(populationMatrix == "Fx") + sum(populationMatrix == "Hx")

  incidentRate <- totalEvents/as.numeric(populationMatrix[which(row.names(populationMatrix) == "Total"),dim(populationMatrix)[2]])

  print(paste(incidentRate, "per", dim(populationMatrix)[2]-1, "person years."))

  return(populationMatrix)
}

# Used by getFirstDrugIncidenceRate. Do not call separately.
#
populateMissingYears <- function(populationMatrix, currentYear, lastYearVisited, patid) {

  yearDiffInt <- as.numeric(currentYear) - as.numeric(lastYearVisited) - 1

  #more than one year missing
  if(yearDiffInt > 1) {
    yearRange <- (as.numeric(lastYearVisited)+1):(as.numeric(lastYearVisited) + yearDiffInt)
  } else {
    yearRange <- as.numeric(lastYearVisited)+1
  }

  yearRangeChr <- as.character(yearRange)
  for(year in yearRangeChr) {
    populationMatrix[which(row.names(populationMatrix) == patid), which(colnames(populationMatrix) == year)] <- "F"
  }

  return(populationMatrix)
}


#===================================================================================================================
#' Searches for patients with a minimum observation period
#'
#' @description
#' Searches for events that hold to the minimum number of years for drugs to be present. A further constraint can be added
#' to ensure that a minimum number of observation years is populated by a series of events that are no more than a number of years apart.
#'
#' @param minObsYr Number of years in a drug record to select patients by.
#' @param minBreakYr Number of years allowed between drug prescriptions before a patient is discounted. If this is set to NA then the minimum number of years is ignored.
#' @param therapyDF Dataframe of clinical or therapeutic patient records.
#'
#' @return Dataframe of those patients with a record that fit the specified criteria.
#' @export
#'
#' @examples
#' df <- removePatientsByDuration(5, 2, testTherapyDF)
removePatientsByDuration <- function(minObsYr, minBreakYr, therapyDF) {

  therapyDFCheck <- errorChecker(therapyDF)
  minObsCheck <- TRUE
  minBreakCheck <- TRUE
  if(!is.null(minObsYr)) {
    minObsCheck <- errorChecker(minObsYr)
  }
  if(!is.null(minBreakYr)) {
    minBreakCheck <- errorChecker(minBreakYr)
  }
  if(isFALSE(therapyDFCheck) | isFALSE(minObsCheck) | isFALSE(minBreakCheck)) {
    print("Please check your code. An argument of removePatientsByDuration() is either missing or empty.")
    return(NULL)
  }

  idList <- getUniquePatidList(therapyDF)
  if(length(idList) == 0) {
    print("Can't find any patients in therpyDF of removePatientsByDuration. Returning NULL.")
    return(NULL)
  }

  minBreakDays <- minBreakYr*365
  minObsDays <- minObsYr*365

  patidToKeep <- integer()
  patidCount <- 1

  #go through each patient
  for(i in 1:length(idList)) {
    #get out each patient's therapy data
    indDF <- therapyDF[therapyDF$patid == idList[[i]],]
    resultList <- getEventdateSummaryByPatient(indDF)
    resultDF <- resultList$SummaryDF
    longestDuration <-  resultDF$longestDuration[1]

    #if the longest duration between two therapy events is greater than the requested minimum break in days then this patient is not suitable
    if(is.na(minBreakDays)) {
      if(longestDuration > minBreakDays) {
        next()
      }
    }

    #now check if this patient is suitable
    recordDuration <- resultDF$recordDuration[1]
    if(recordDuration < minObsDays) {
      next()
    }

    #as the minimum duration between evennts and the drug record duration are all satisfied we need to keep this individual
    patidToKeep[patidCount] <- idList[[i]]
    patidCount <- patidCount + 1
  }

  resultDF <- subset(therapyDF, therapyDF$patid %in% patidToKeep)
  return(resultDF)
}

#TO-DO
#===================================================================================================================
#clinicalDF - the data.frame of those patients you are interested in, they must have a drug record.
#therapyDF - the data.frame of the drugs of interest, they must have a clinical  record
#medcodesVector - the medical codes to look for when matching first disease with first drug.
#returnIDList - boolean, whether to return an adjusted therapyDF (FALSE by detault) or the patient IDs of the adjusted therapyDF (TRUE - not detault).
# Title
#
# @param clinicalDF
# @param therapyDF
# @param medCodesVector
# @param drugCodesVector
# @param returnIDList
#
# @return
# @export
#
# @examples
#getPatientsWithDiseaseButNoDrug <- function(clinicalDF, therapyDF, medCodesVector=NULL, drugCodesVector=NULL, returnIDList=FALSE) {
#
#}


#===================================================================================================================
#' Returns a dataframe of therapy records for all those patients who have a drug of interest free from a
#' first clinical event of interest
#'
#' @description
#' Looks through all patients clinical and therapy records and identifies all those patients where the first drug
#' prescription was recorded on a day when a particular clinical complaint was not recorded.
#'
#' @details
#' For example, if headache is the clinical event of interest and migraine preventatives are the drugs of interest, the returned dataframe
#' will only contain therapy patient records where migraine preventatives were not prescribed during the first headache event.
#'
#' There are two ways of executing this: (1) From scratch, by providing the clinicalDF and therapyDF and keeping therapyOfDrugDiseaseDF as NULL,
#'or (2) by including the result from \code{\link{getPatientsWithFirstDrugWithDisease}} in the therapyOfDrugOnDiseaseDF argument along with a patient therapy dataframe.
#' We recommend running the second scenario to limit complexity.
#'
#' @param clinicalDF data frame of patient clinical data. Every patient must have a clinical and therapy entry.
#' This parameter is not required if a therapyOfDrugOnDiseaseDF dataframe is provided.
#' @param therapyDF data frame of therapy data. This is always required.
#' @param therapyOfDrugOnDiseaseDF data frame result from \code{\link{getPatientsWithFirstDrugWithDisease}}. This must be accompanied with
#' the original input therapy dataframe used during the call to \code{\link{getPatientsWithFirstDrugWithDisease}}.
#' @param medCodesVector Vector of clinical medcodes.
#' @param drugCodesVector Vector of therapy prodcodes codes.
#' @param returnIDList If TRUE returns a List of patient ids or if FALSE (default) returns a dataframe of therapy records of those patients that
#' satisfy the search.
#'
#' @return Dataframe of therapy records that only include those patients who didn't have a drug of interest during their first clinical data.
#' The data frame will contain all therapy instances for matching patients. If returnIDList is TRUE then the patids are returned as a list.
#' @export
#'
#' @examples
#' returnTherapyDF <- getPatientsWithFirstDrugWithDisease(
#'    clinicalDF=testClinicalDF,
#'    therapyDF=testTherapyDF,
#'    medCodesVector=NULL,
#'    drugCodesVector = NULL,
#'    FALSE, c(0,0))
#' df <- getPatientsWithFirstDrugWithNoDisease(
#'    therapyDF=testTherapyDF,
#'    therapyOfDrugOnDiseaseDF=returnTherapyDF)
getPatientsWithFirstDrugWithNoDisease <- function(clinicalDF=NULL, therapyDF=NULL, therapyOfDrugOnDiseaseDF=NULL, medCodesVector=NULL, drugCodesVector=NULL, returnIDList=FALSE) {

  medCodesVectorCheck <- TRUE
  drugCodesVectorCheck <- TRUE
  therapyOfDrugOnDiseaseDFCheck <- TRUE
  therapyDFCheck <- TRUE
  clinicalDFCheck <- TRUE
  if(!is.null(therapyOfDrugOnDiseaseDF)) {
    therapyOfDrugOnDiseaseDFCheck <- errorChecker(therapyOfDrugOnDiseaseDF)

    if(!is.null(therapyDF)) {
      clinicalDFCheck <- errorChecker(therapyDF)
    } else {
      print("Warning: therapyDF is NULL. A therapyOfDrugOnDiseaseDF also requires a therapyDF. Returning NULL.")
      return(NULL)
    }
  } else {
    #for when a clinical and therapy has been provided
    therapyDFCheck <- errorChecker(therapyDF)
    if(!is.null(clinicalDF)) {
      clinicalDFCheck <- errorChecker(clinicalDF)
    }
  }

  if(!is.null(medCodesVector)) {
    medCodesVectorCheck <- errorChecker(medCodesVector)
  }

  if(!is.null(drugCodesVector)) {
    drugCodesVectorCheck <- errorChecker(drugCodesVector)
  }

  #check that the data passed in is not malformed in any way
  if(isFALSE(therapyOfDrugOnDiseaseDFCheck) | isFALSE(therapyDFCheck) | isFALSE(clinicalDFCheck) | isFALSE(medCodesVectorCheck) | isFALSE(drugCodesVectorCheck)) {
    print("Please check your code. An argument of getPatientsWithFirstDrugWithNoDisease() is either missing or empty.")
    return(NULL)
  }

  #get an idea of whether the therapyDF was the original data.frame used to create the therapyOfDrugOnDiseaseDF
  if(is.null(isFALSE(therapyOfDrugOnDiseaseDFCheck)) & is.null(isFALSE(therapyDFCheck)) ) {
    tempAIDList <- getUniquePatidList(therapyOfDrugOnDiseaseDF)
    tempBIDList <- getUniquePatidList(therapyDF)
    if(length(tempAIDList)==length(tempBIDList)) {
      print("The number of therapyDF patients and therapyOfDrugOnDiseaseDF patients is identical. Nothing to return! Returning NULL.")
      return(NULL)
    }
    if(length(tempAIDList) > length(tempBIDList)) {
      print("There are more therapyOfDrugOnDiseaseDF patients than patients in the therapyDF. Please check whether the therapyDF was used to create the therapyOfDrugOnDiseaseDF object. Returning NULL.")
      return(NULL)
    }

    tempTherapyDF <- subset(therapyDF, therapyDF$patid %in% tempAIDList)
    if(length(getUniquePatidList(tempTherapyDF)) != length(tempAIDList)) {
      print("Not every patient in therapyOfDrugOnDiseaseDF is in the therapyDF. Please check whether the therapyDF was used to create the therapyOfDrugOnDiseaseDF object. Returning NULL.")
      return(NULL)
    }
  }

  #if a therapy data.frame from getPatientsWithFirstDrugWithDisease is provided (first mechanism)
  #=====================================
  if(!is.null(therapyOfDrugOnDiseaseDF)) {
    print("Using those patients with a drug on an event date of clinical interest to get out all those patient who have a clinical event of interest but with no first drug.")
    idList <- getUniquePatidList(therapyOfDrugOnDiseaseDF)
    returnTherapyDF <- subset(therapyDF, !(therapyDF$patid %in% idList))

    #check whether returnTherapyDF contains the drugs that I'm interested in..
    if(!is.null(drugCodesVector)) {
      returnTherapyDF <- subset(returnTherapyDF, returnTherapyDF$prodcode %in% drugCodesVector)
      if(nrow(returnTherapyDF)==0) {
        print("No patient therapy records identified for the drug codes provided. Returning NULL.")
        return(NULL)
      }
    }

    if(isTRUE(returnIDList)) {
      return(getUniquePatidList(returnIDList))
    } else {
      return(returnTherapyDF)
    }
  }

  #otherwise use therapyDF and clinicalDF (second mechanism)
  #================================================================
  #just get the drugs we're interested in as supplied by the user
  therapyBackupDF <- therapyDF
  if(!is.null(drugCodesVector)) {
    therapyDF <- subset(therapyDF, therapyDF$prodcode %in% drugCodesVector)
    if(nrow(therapyDF)==0) {
      print("No patient therapy records identified for the drug codes provided. Returning NULL.")
      return(NULL)
    }
    print("Using user supplied drugCodesVector for drugs of interst. ")
  } else {
    print("Using all therapy drug codes for drugs of interest.")
    #fetch drug codes from those therapy events available
    drugCodesVector <- unique(therapyDF$prodcode)
  }
  #just get the clinical medcodes of interest - be careful using this! You must always have therapyDF as a subset of clinicalDF
  if(!is.null(medcodeVector)) {
    clinicalDF <- subset(clinicalDF, clinicalDF$medcode %in% medcodeVector)
    if(nrow(clinicalDF)==0) {
      print("No patient clinical records identified for the clinical med codes provided. Returning NULL.")
      return(NULL)
    }
    print("Using user supplied clinical medcodes for disease/condition of interest.")
  } else {
    print("Using all clinical medcodes for disease/condition of interest.")
    medcodeVector <- unique(clinicalDF$medcode)
  }

  #now ensure the clinical data.frame for clinicalDF is alligned to the therapyDF
  #if the clinicalDF has fewer patients than the therapyDF patients then warm and return null.
  if(length(getUniquePatidList(clinicalDF)) < length(getUniquePatidList(therapyDF))) {
    print("There are fewer clinical records than therapy records. Please check you input clinical and therapy data.frame inputs and/or medcodes and drugcodes inputs. Returning NULL.")
    return(NULL)
    #if there are more clinical records than therapy records trim the clinical records down
  } else if(length(getUniquePatidList(clinicalDF)) > length(getUniquePatidList(therapyDF))) {
    clinicalDF <- subset(clinicalDF, clinicalDF$patid %in% getUniquePatidList(therapyDF))
    if(nrow(clinicalDF)==0) {
      print("After subsetting the clinical records by those patients with a therapy record no clinical records exist. Returning NULL.")
      return(NULL)
    }
  } else {
    #if they are the same size
    clinicalDF <- subset(clinicalDF, clinicalDF$patid %in% getUniquePatidList(therapyDF))
    if(nrow(clinicalDF)==0) {
      print("After subsetting the clinical records by those patients with a therapy record no clinical records exist. Returning NULL.")
      return(NULL)
    }
  }

  #then reduce the therapyDF record down to the size of the clinical group
  therapyDF <- subset(clinicalDF, clinicalDF$patid %in% getUniquePatidList(therapyDF))
  if(nrow(therapyDF)==0) {
    print("No patient therapy records identified for the drug codes provided. Returning NULL.")
    return(NULL)
  }

  #clinicalDF and therapyDF must be the same size
  if(!length(getUniquePatidList(therapyDF)) == length(getUniquePatidList(clinicalDF))) {
    print(paste("Warning: failed to match therapy and clinical patient records. ClinicalDF must include at least all clinical patients records for those patients inside therapyDF. Returning NULL."))
    return(NULL)
  }

  #get out the first therapy events - we want all those events dates which we'll then compare to the clinical dates
  #we remove all those patient therapies that have a matching event to clinical event, and keep the rest
  therapyFirstDrugDF <- therapyDF[!duplicated(therapyDF$patid),]
  #assign 1:n row numbers as row names so it's an easy names index to use
  therapyPatidVector <- therapyFirstDrugDF$patid
  therapyEventdateVector <- therapyFirstDrugDF$eventdate
  storedPatidVector <- as.character(0)

  #now, look for all the clinical events and hold onto the patids to then extract
  idList <- getUniquePatidList(therapyFirstDrugDF)

  for(id in idList) {
    #get the individual's (id) clinical data
    indClinicalDF <- clinicalDF[clinicalDF$patid == id,]
    #find the vector index in the first therapy vectors and then get the therapyevent
    therapyPatidIndex <- match(id,therapyPatidVector)
    therapyEvent <- therapyEventdateVector[therapyPatidIndex]

    #get the eventdates of clinical as a vector
    eventdataVector <- indClinicalDF$eventdate
    #is there a match between the therapyevent and vector event? If yes, store the patid
    if(sum(therapyEvent %in% eventdataVector) > 0) {
      if(length(storedPatidVector)==0) {
        storedPatidVector[1] <- id
      } else {
        storedPatidVector <- append(id, storedPatidVector)
      }
    }
  }

  if(length(storedPatidVector) == 0) {
    print("There were no matches. Returning NULL.")
    return(NULL)
  }

  returnTherapyDF <- subset(therapyBackupDF, therapyBackupDF$patid %in% storedPatidVector)
  if(isTRUE(returnIDList)) {
    return(getUniquePatidList(returnTherapyDF))
  } else {
    return(returnTherapyDF)
  }

}

#FINISHED
#===================================================================================================================
#' Searches for those patients with a first-drug matched to a disease event
#'
#' This is used to look for all patients where a first-drug prescription shares the date with a disease of interest.
#' This helps ensure that there is no mistaking what the first drug was prescribed for.
#'
#' @details
#' Expect a potential big drop in patients in the returning data.
#' Some patients might consult a doctor, be told to wait a couple of days then a follow-up call consultation takes places and only
#' a drug is prescribed (rather than a repeat of the original complaint on record). Such events can be captured by modifying the search buffer (number
#' of days either side of a drug date).
#' The returning data is by default (FALSE) a modified therapyDF based only on the patients from the clinicalDF. If
#' set as TRUE, then the a List of patient IDs will be returned Keeping it set to FALSE is easier to use.
#'
#' @param clinicalDF Dataframe of those patients you are interested in. Each patient must have a drug record (entry in therapyDF).
#' @param therapyDF Dataframe of the drugs of interest. Each patient must have a clinical record (entry in clinicalDF).
#' @param medCodesVector Vector of clinical medical codes to look for when matching first disease with first drug. All other clinical events are removed.
#' @param drugCodesVector Vector of product codes to look for when matching first disease with first drug. All other therapy events are removed.
#' @param returnIDList If TRUE returns an adjusted therapyDF (default) else returns the patient IDs.
#' @param bufferVector Integer vector of two values. The first denotes the number of days before a drug event
#' and then second the number of days after the drug event. Diseases within this buffer are considered as being linked.
#' By default this is c(0,0), only an exact date match counts.
#'
#' @return Dataframe (default) subset of therapyDF or the a List of patient IDs.
#' @export
#'
#' @examples
#' returnTherapyDF <- getPatientsWithFirstDrugWithDisease(
#'    clinicalDF=testClinicalDF,
#'    therapyDF=testTherapyDF,
#'    medCodesVector=NULL,
#'    drugCodesVector = NULL,
#'    FALSE, c(0,0))
getPatientsWithFirstDrugWithDisease <- function(clinicalDF,
                                                therapyDF,
                                                medCodesVector=NULL,
                                                drugCodesVector=NULL,
                                                returnIDList=FALSE,
                                                bufferVector=c(0,0)) {
  therapyDFCheck <- errorChecker(therapyDF)
  clinicalDFCheck <- errorChecker(clinicalDF)
  bufferCheck <- errorChecker(bufferVector)
  medCodesVectorCheck <- TRUE
  drugCodesVectorCheck <- TRUE
  if(!is.null(medCodesVector)) {
    medCodesVectorCheck <- errorChecker(medCodesVector)
  }
  if(!is.null(drugCodesVector)) {
    drugCodesVectorCheck <- errorChecker(drugCodesVector)
  }

  #check that the data passed in is not malformed in any way
  if(isFALSE(therapyDFCheck) | isFALSE(clinicalDFCheck) | isFALSE(medCodesVectorCheck) | isFALSE(drugCodesVectorCheck) | isFALSE(bufferCheck)) {
    print("Please check your code. An argument of getPatientsWithFirstDrugWithDisease() is either missing or empty.")
    return(NULL)
  }

  if(!is.null(medCodesVector)) {
    print("Looking for specific med codes.")
    clinicalDF <- subset(clinicalDF, clinicalDF$medcode %in% medCodesVector)
  } else {
    print("Using all med codes in data set.")
  }

  if(!is.null(drugCodesVector)) {
    print("Looking for specific drug codes.")
    therapyDF <- subset(therapyDF, therapyDF$prodcode %in% drugCodesVector)
  } else {
    print("Using all drug codes in data set.")
  }

  #check that the buffer values make sense
  if(length(bufferVector) != 2) {
    print(paste("Error: The bufferVector requires two element. There are currently", length(bufferVector), "Returning NULL."))
    return(NULL)
  }
  intSumBuffer <- sum(bufferVector < 0)
  if(intSumBuffer > 0) {
    print("Error: There was an error with the bufferVecttor function argument. Please assign two integer elements, 0 or greater. Returning NULL.")
    print(paste("Day buffers assigned. Check", bufferVector[1], "before the drug and", bufferVector[2], "after the drug."))
    return(NULL)
  } else {
    print(paste("Day buffers assigned. Check", bufferVector[1], "before the drug and", bufferVector[2], "after the drug."))
  }

  #get the clinical patients
  clinicalIDList <- getUniquePatidList(clinicalDF)

  #ensure that only the drugs with a clinical patient ID are used.
  therapyDF <- subset(therapyDF, therapyDF$patid %in% clinicalIDList)
  if(nrow(therapyDF) == 0) {
    print("There are no therapy events for the clinical cohort. They don't match! Returning NULL.")
    return(NULL)
  }

  #ensure that every drug record has a clinical record
  therapyIDList <- getUniquePatidList(therapyDF)
  clinicalDF <- subset(clinicalDF, clinicalDF$patid %in% therapyIDList)
  clinicalIDList <- getUniquePatidList(clinicalDF)

  if(length(therapyIDList) != length(clinicalIDList)) {
    print("There was a discrepancy between the patient record sizes. Ensure that every therapy record has a clinical record and vice versa. Returning NULL.")
    return(NULL)
  }
  #print("Finished checking and data curation.")

  numCores <- parallel::detectCores()
  if(numCores > 1) {
    numCores <- ceiling(numCores/2)
    #this is for CRAN
    if(numCores > 2) {
      numCores <- 2
    }
  }

  #REMEMBER: We want 1st drug that has a complaint of interest (the complaint does not need to be the first complaint).

  #Get the first drug for each patient
  therapyFirstDrugDF <- therapyDF[!duplicated(therapyDF$patid),]
  if(length(getUniquePatidList(therapyFirstDrugDF)) != length(therapyIDList)) {
    print("Something went wrong with extracting the first drug from the therapyDF - this might be a developer code problem. Returning NULL.")
    return(NULL)
  }

  idFirstDrugWithMatchVector <- numeric()
  #Now go through each patient and see if they have a record for the disease on the same date. Apply the bufferVector.

  #use a patid and eventdate check....
  firstDrugEventdateVector <- therapyFirstDrugDF$eventdate

  #there is currently a bug for multicore processing!
  print("Current version restricted to one core.")
  tempNumCores <- numCores
  numCores <- 1
  daysBefore <- bufferVector[1]
  daysAfter <- bufferVector[2]
  if(numCores == 1) {
    print("Running on one core.")
    counter <- 1
    #go through each patient (using the therapyIDList)
    for(i in 1:length(therapyIDList)) {
      clinicalIndDF <- clinicalDF[clinicalDF$patid == therapyIDList[[i]],]
      clinicalEventdateVector <- clinicalIndDF$eventdate

      #is the first drug event in the clinical event date?
      #This looks for a matching date i.e., when no buffer was provided c(0,0) by default.
      if(daysBefore == 0 & daysAfter == 0) {

        if(sum(firstDrugEventdateVector[i] %in% clinicalEventdateVector) >= 1) {
          idFirstDrugWithMatchVector[counter] <- therapyIDList[[i]]
          counter <- counter + 1
        }
      } else {
        #this is for when a set of buffer days were requested
        #we are checking the firstDrugEventdateVector[i] against all entries in clinicalEventdateVector

        for(k in 1:length(clinicalEventdateVector)) {
          dateToConsider <- clinicalEventdateVector[k]
          dateToConsiderLower <- dateToConsider - daysBefore
          dateToConsiderUpper <- dateToConsider + daysAfter

          if(firstDrugEventdateVector[i] >= dateToConsiderLower & firstDrugEventdateVector[i] <= dateToConsiderUpper) {
            idFirstDrugWithMatchVector[counter] <- therapyIDList[[i]]
            counter <- counter + 1
            break()
          }
        }


      }
    }
  } else {
    #perform in parallel --- not yet supported.
    print(paste("Running in parallel on", numCores, "cores."))
    parallelList <- getParallelList(therapyIDList, numCores)
    stopifnot(length(parallelList) == numCores)

    sumEntries <- 0
    for(i in 1:length(parallelList)) {
      sumEntries <- sumEntries + length(parallelList[[i]])
    }
    stopifnot(sumEntries == length(therapyIDList))

    `%dopar%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(numCores, outfile="")
    doParallel::registerDoParallel(cl)
    parallel::clusterExport(cl, varlist = c("getUniquePatidList", "clinicalDF","therapyFirstDrugDF","bufferVector"), envir=environment())
    idFirstDrugWithMatchVector <- foreach::foreach(i = 1:numCores, .combine = cbind) %dopar% {
      subList <- parallelList[[i]]
      subTherapyFirstDrugDF <- subset(therapyFirstDrugDF, therapyFirstDrugDF$patid %in% subList)

      stopifnot(length(subList) == length(getUniquePatidList(subTherapyFirstDrugDF)))

      subClinicalDF <- subset(clinicalDF, clinicalDF$patid %in% subList)
      subIdFirstDrugWithMatchVector <- numeric()
      for(j in 1:length(subList)) {
        if(j %% 2500) {
          print(paste("Patient count",j))
        }
        indTherapyFirstDF <- subset(subTherapyFirstDrugDF, subTherapyFirstDrugDF$patid %in% subList)
        indFirstDrugEventdateVector <- indTherapyFirstDF$eventdate

        clinicalIndDF <- subClinicalDF[subClinicalDF$patid == subList[[j]],]
        clinicalEventdateVector <- clinicalIndDF$eventdate

        #is the first drug event in the clinical event date?
        if(sum(indFirstDrugEventdateVector[j] %in% clinicalEventdateVector) >= 1) {
          subIdFirstDrugWithMatchVector <- append(subIdFirstDrugWithMatchVector, subList[[j]])
        }
      }
      print(paste("In parallel a core returned", length(subIdFirstDrugWithMatchVector), "matching patids."))
      subIdFirstDrugWithMatchVector
    }
    parallel::stopCluster(cl)
  }

  numCores <- tempNumCores

  refinedTherapyDF <- subset(therapyDF, therapyDF$patid %in% idFirstDrugWithMatchVector)
  if(nrow(refinedTherapyDF) == 0) {
    print("There were no therapy patient records with a corresponding disease at first drug instance.")
  }

  print("Finished finding all patients where their first drug had a matching clinical event.")
  print(paste("Started with", length(therapyIDList), "patient therapy records."))
  print(paste("Ended with", length(idFirstDrugWithMatchVector), "patient therpay records."))

  #How  to return therapy records; as an altered therapyDF or the patient IDs.
  if(isTRUE(returnIDList)) {
    returnList <- getUniquePatidList(refinedTherapyDF)
    return(returnList)
  } else {
    return(refinedTherapyDF)
  }
}

#===================================================================================================================
#' Searches for drug prescription switching within a cohort
#'
#' @description
#' Constructs a record of patients moving between different drugs, starting with their first drug prescription, as a matrix.
#'
#' @details
#' The current implementation does not support multiple processes.
#' Only those patients with a clinical event of interest will be searched. Clinical events can be specified using medcodeVector.
#' Please note, if you are using clinical data then there must be a therapy record for each clinical record.
#'
#' @param df Dataframe of therapy data.
#' @param clinicalDF Dataframe of clinical data for the matching of medcodes with prodcodes.
#' @param medcodeVector A numeric vector of medcodes to apply over the clinical data.
#' @param minDepth The minimum number of prescriptions required for a patient to be accepted.
#' @param maxDepth The maximum drug switches considered (including initial i.e., 2 = initial + first switch).
#' @param groupingList A named list of drug code (see example).
#' @param removeUndefinedCode Logical. TRUE (default) removes prodcodes not specified by the user. This will speed up calculations.
#'
#' @return List with three elements, each is a dataframe for plotting. Please see the example for how to plot using the third element.
#' @export
#'
#' @examples
#' drugList <- unique(testTherapyDF$prodcode)
#' requiredProds <- drugList[1:18]
#' structureList <- list(
#'   Amitriptyline = c(83,49,1888),
#'   Propranolol = c(707,297,769),
#'   Topiramate = c(11237),
#'   Venlafaxine = c(470,301,39359),
#'   Lisinopril = c(78,65,277),
#'   Atenolol = c(5,24,26),
#'   Candesartan = c(531)
#' )
#'
#' resultList <- mapDrugTrajectory(testTherapyDF,
#'                                 NULL,
#'                                 NULL,
#'                                 5,
#'                                 5,
#'                                 groupingList=structureList,
#'                                 removeUndefinedCode=TRUE)
#' df3 <- resultList[[3]]
#' #str(df3)
#' #data.frame':	96 obs. of  6 variables:
#' #$ FirstDrug: chr  "Amitriptyline" "Propranolol" "Venlafaxine" "Atenolol" ...
#' #$ Switch1  : chr  "Propranolol" "Amitriptyline" "Propranolol" "Lisinopril" .
#' #$ Switch2  : chr  "Amitriptyline" "Propranolol" "Venlafaxine" "Atenolol" ...
#' #$ Switch3  : chr  "Propranolol" "Amitriptyline" "Propranolol" "Lisinopril" .
#' #$ Switch4  : chr  "Amitriptyline" "Propranolol" "Venlafaxine" "Atenolol" ...
#' #$ Freq     : num  18 30 5 25 34 6 7 3 1 1 ...
#'
#' #required as ggalluvial has not been loaded as a library
#' StatStratum <- ggalluvial::StatStratum
#'
#' ggalluvial::is_alluvia_form(as.data.frame(df3), axes = 1:5, silent = TRUE)
#' ggplot2::ggplot(df3, ggplot2::aes(y = Freq,
#'         axis1 = FirstDrug,
#'         axis2 = Switch1,
#'         axis3 = Switch2,
#'         axis4 = Switch3,
#'         axis5 = Switch4)) +
#'  ggalluvial::geom_alluvium(ggplot2::aes(fill = FirstDrug), width = 1/12) +
#'  ggalluvial::geom_stratum(width = 1/12, fill = "black", color = "grey") +
#'  ggplot2::scale_fill_brewer(type = "qual", palette = "Set1") +
#'  ggplot2::theme_bw() + ggplot2::theme(legend.position = "none") +
#'  ggplot2::scale_x_discrete(limits = c("First Drug",
#'                              "1st Switch",
#'                              "2nd Switch",
#'                              "3rd Switch",
#'                              "4th Switch"), expand = c(.05, .05)) +
#'  ggplot2::ggtitle("Migraine Preventative Switching")
#'  #include this line if you want labels on the bars
#'  #ggplot2::geom_label(stat = "stratum", infer.label = TRUE)
mapDrugTrajectory <- function(df, clinicalDF=NULL, medcodeVector=NULL, minDepth=2, maxDepth=5, groupingList=NULL, removeUndefinedCode=TRUE) {

  #keep only those prodcodes of interest
  if(!is.null(groupingList)) {
    groupingCodes <- unlist(groupingList)
    if(length(groupingCodes)==0) {
      print("The groupingList didn't contain any codes. Returning NULL.")
      return(NULL)
    }
    if(isTRUE(removeUndefinedCode)) {
      print("Keeping prodcodes of interest only")
      df <- subset(df, df$prodcode %in% groupingCodes)
      if(nrow(df)==0) {
        print("The df didn't contain any records for the supplied code groupingList. Returning NULL.")
        return(NULL)
      }
    }
  }

  #keep only those patients with medcodes of interests
  if(!is.null(clinicalDF)) {
    clinicalDF <- subset(clinicalDF, clinicalDF$medcode %in% medcodeVector)
    clinicalIDList <- getUniquePatidList(clinicalDF)
    #every therapy DF entery must have a clinical match
    #then ensure for every therapyDF patid you have a clinicalDF patid
    df <- subset(df, df$patid %in% clinicalIDList)
    if(length(getUniquePatidList(df)) != length(getUniquePatidList(clinicalDF))) {
      print("The number of clinical patients does not equal the number of therapy patients.")
      return(NULL)
    }
  }


  #id list of all those patients eligable for attempted switching. They will make up at least all of the returning list.
  idList <- getUniquePatidList(df)
  #build a matrix to return with
  switchMatrix <- matrix(NA,nrow=length(idList),ncol=maxDepth)
  row.names(switchMatrix) <- idList

  #If a drug list was not supported we go through all drugs present
  if(is.null(groupingList)) {
    #for each patient
    for(i in 1:length(idList)) {
      #get out the therapy DF for the patient
      indDF <- subset(df, df$patid == idList[[i]])
      #get out their prodcodes
      prodCodes <- indDF$prodcode
      depthCount <- 1
      previousCode <- NULL
      for(j in 1:length(prodCodes)) {
        if(depthCount > maxDepth) {
          break()
        }
        if(j==1) {
            switchMatrix[i,depthCount] <- prodCodes[j]
            previousCode <- prodCodes[j]
            depthCount <- depthCount + 1
        } else {
          if(isFALSE(previousCode == prodCodes[j])) {
            switchMatrix[i,depthCount] <- prodCodes[j]
            previousCode <- prodCodes[j]
            depthCount <- depthCount + 1
          }
        }
      }
    }
  } else { #if a groupingList has been provided.

    for(i in 1:length(idList)) {
      indDF <- subset(df, df$patid == idList[[i]])
      prodCodes <- indDF$prodcode
      depthCount <- 1
      previousCode <- NULL
      for(j in 1:length(prodCodes)) {
        if(depthCount > maxDepth) {
          break()
        }
        if(j==1) {
          prodCodeDescription <- findProdCode(prodCodes[j],groupingList)
          switchMatrix[i,depthCount] <- prodCodeDescription
          previousCode <- prodCodeDescription#prodCodes[j]
          depthCount <- depthCount + 1
        } else {
          prodCodeDescription <- findProdCode(prodCodes[j],groupingList)
          if(isFALSE(previousCode == prodCodeDescription)) {
            prodCodeDescription <- findProdCode(prodCodes[j],groupingList)
            switchMatrix[i,depthCount] <- prodCodeDescription
            previousCode <- prodCodeDescription #prodCodes[j]
            depthCount <- depthCount + 1
          }
        }
      }
    }
  }
  switchDF <- as.data.frame(switchMatrix,stringsAsFactors = FALSE)
  switchList <- list()
  #ensure that at least one switch occurs
  for(i in 1:nrow(switchDF)) {
    if(sum(is.na(switchDF[i,c(minDepth:maxDepth)]))  == 0 ) {
      switchList <- rlist::list.append(switchList,switchDF[i,])
    }
  }
  switchDF <- do.call("rbind",switchList)

  switchNames <- character(0)
  for(i in 1:maxDepth) {
    if(i==1) {
      switchNames[i] <- "FirstDrug"
    } else {
      switchNames[i] <- paste0("Switch",(i-1))
    }
  }

  colnames(switchDF) <- switchNames
  switchReducedDF <- dplyr::distinct(switchDF)

  # FirstDrug       Switch1       Switch2       Freq
  # Amitriptyline   NA            NA            1023
  # Amitriptyline   Amitriptyline Amitriptyline 0 <-remove these with 0
  # Amitriptyline   Amitriptyline Propranolol 0
  # Amitriptyline   Propranolol Propranolol 0
  # Amitriptyline   Toperimate Propranolol 512

  #make a data.frame switchNames cols + 1
  #alluviumDF <- data.frame(matrix(NA,nrow=nrow(switchReducedDF),ncol=length(switchNames)+1))
  alluviumDF <- cbind(switchReducedDF,Freq=0)
  colnames(alluviumDF) <- c(switchNames,"Freq")
  for(i in 1:nrow(alluviumDF)) {

    matchedIndex <- plyr::match_df(switchDF,alluviumDF[i,c(1:maxDepth)])
    alluviumDF$Freq[i] <- nrow(matchedIndex)
  }

  alluviumDF[is.na(alluviumDF)] <- "Stopped"

  resultList <- list(condensedDF=switchReducedDF, rawDF=switchDF, alluviumDF=alluviumDF)
  return(resultList)
}

# Used by mapDrugTrajectory.
findProdCode <- function(prodCode, groupingList) {
  drugNames <- names(groupingList)
  for(i in 1:length(groupingList)) {
    codes <- groupingList[[i]]
    if(sum(prodCode %in% codes)==1) {
      return(drugNames[i])
    }
  }
  return("Other")
}

#===================================================================================================================

#' Matches a drug with a clinical diagnosis
#'
#' @description
#' This is extremely useful for generating baseline characteristics of a cohort by answering questions such as,
#' 1) How many patients in the cohort were prescribed drugs x, y, & z?
#' 2) How many patients in the cohort were prescribed drugs x, y, & z at the same time as having a
#' diagnosis for a particular disease  (by medcode)?
#' 3) How many patients in the cohort were prescribed drugs x, y, & z at the same time as having an
#' exclusive diagnosis (no accompanying comorbidities on the same date) for a particular disease(by medcode)?
#'
#' @details
#' The above three questions are answered by changing the severity parameter. The function returns a list of patids.
#'
#' A bounding date interval can be specified. This is a dataframe that specifies where to look for each patient in terms of time. By supplying a
#' yearly lower (column nameand  start) and upper (column name and stop) date (YYYY-MM-DD), one can determine how many patients were prescribed a particular drug within a given time frame. The patid
#' can be used to subset the cohort e.g., "all those patients prescribed X drugs with Y condition, who have an IMD of 5 (use the patid in dateDF), between
#' the dates A - B".
#'
#' @param clinicalDF Dataframe with at least patid, eventdate, medcode columns. NULL as default.
#' @param referralDF Dataframe with at least patid, eventdate, prodcode columns. NULL as default.
#' @param therapyDF Dataframe with at least patid, eventdate, medcode columns.
#' @param patidList Vector of patient IDs, only these patients are considered. If one hasn't been provided it will be created using the clinicalDF (default).
#' @param medcodeList Vector of medcodes of interest. NULL as default.
#' @param drugcodeList Vector of prodcodes of interest. NULL as default.
#' @param severity Integer to define search strictness criteria. 1 - a drug anywhere (default), 2 - a drug with a matching clinical event (comorbidities are not important), 3 - a drug with only a clinical event of interest (if comorbidites are present on the same day, this is not a match).
#' @param dateDF Dataframe with column names patid - start - stop. It must contain all patids available in the combination of clinicalDF, referralDF and therapyDF. NULL as default.
#'
#' @return A List of patids that are identified to have used a drug of interest.
#'
#' @export
#'
#' @examples
#' prodcodes <- unique(testTherapyDF$prodcode)
#' amitriptylineCodes <- prodcodes[1:5]
#' propranololCodes <- prodcodes[6:11]
#'
#' medcodeList <- unique(testClinicalDF$medcode)
#' headacheCodes <- medcodeList[1:10]
#' amitriptylineResult1 <- matchDrugWithDisease(clinicalDF = testClinicalDF,
#'                                              therapyDF = testTherapyDF,
#'                                              medcodeList = headacheCodes,
#'                                              drugcodeList=amitriptylineCodes,
#'                                              severity = 1)
#' amitriptylineResult2 <- matchDrugWithDisease(clinicalDF = testClinicalDF,
#'                                              therapyDF = testTherapyDF,
#'                                              medcodeList = headacheCodes,
#'                                              drugcodeList=amitriptylineCodes,
#'                                              severity = 2)
#' amitriptylineResult3 <- matchDrugWithDisease(clinicalDF = testClinicalDF,
#'                                              therapyDF = testTherapyDF,
#'                                              medcodeList = headacheCodes,
#'                                              drugcodeList=amitriptylineCodes,
#'                                              severity = 3)
#'
#' propranololResult1 <- matchDrugWithDisease(clinicalDF = testClinicalDF,
#'                                            therapyDF = testTherapyDF,
#'                                            medcodeList = headacheCodes,
#'                                            drugcodeList = propranololCodes,
#'                                            severity = 1)
#' propranololResult2 <- matchDrugWithDisease(clinicalDF = testClinicalDF,
#'                                            therapyDF = testTherapyDF,
#'                                            medcodeList = headacheCodes,
#'                                            drugcodeList = propranololCodes,
#'                                            severity = 2)
#' propranololResult3 <- matchDrugWithDisease(clinicalDF = testClinicalDF,
#'                                            therapyDF = testTherapyDF,
#'                                            medcodeList = headacheCodes,
#'                                            drugcodeList = propranololCodes,
#'                                            severity = 3)
matchDrugWithDisease <- function(clinicalDF=NULL, referralDF=NULL, therapyDF, patidList=NULL, medcodeList=NULL, drugcodeList=NULL, severity=1, dateDF=NULL) {

  checkTherapyDF <- errorChecker(therapyDF)
  checkReferralDF <- TRUE
  checkClinicalDF <- TRUE
  checkPatidList <- TRUE
  checkMedcodeList <- TRUE
  checkDrugcodeList <- TRUE
  checkDateDF <- TRUE
  checkSeverity <- errorChecker(severity)

  if(!is.null(clinicalDF)) { checkClinicalDF <- errorChecker(clinicalDF) }
  if(!is.null(referralDF)) { checkReferralDF <- errorChecker(referralDF) }
  if(!is.null(patidList)) { checkPatidList <- errorChecker(patidList) }
  if(!is.null(medcodeList)) { checkMedcodeList <- errorChecker(medcodeList) }
  if(!is.null(drugcodeList)) { checkDrugcodeList <- errorChecker(drugcodeList) }
  if(!is.null(dateDF)) { checkDateDF <- errorChecker(dateDF) }

  if(isFALSE(checkClinicalDF) | isFALSE(checkReferralDF) | isFALSE(checkTherapyDF)) {
    print("Warning: one of the input data.frames is incorrect. Returning NULL.")
    return(NULL)
  }
  if(isFALSE(checkPatidList)) {
    print("Warning: patidList argument was incorrectly formatted. Returning NULL.")
    return(NULL)
  }
  if(isFALSE(checkMedcodeList)) {
    print("Warning: medcodeList argument was incorrectly formatted. Returning NULL.")
    return(NULL)
  }
  if(isFALSE(checkDrugcodeList)) {
    print("Warning: drugcodeList argument was incorrectly formatted. Returning NULL.")
    return(NULL)
  }
  if(isFALSE(checkSeverity)) {
    print("Warning: severity argument was incorrectly formatted. Returning NULL.")
    return(NULL)
  }

  if(is.null(clinicalDF) & is.null(referralDF) & is.null(therapyDF)) {
    print("Warning: all input data.frames are null. The function matchDrugWithDisease requires at least the therapyDF and clinicalDF or referralDF to be present. Returning NULL.")
    return(NULL)
  }

  #create a patid list if one hasn't been provided
  if(is.null(patidList)) {
    patidList <- getUniquePatidList(clinicalDF)
  }

  #if we don't care about comorbidities get rid of them (providing a medcodeList has been provided)
  if((!is.null(medcodeList)) & severity==2) {
    print("Severity 2 selected. Removing all disease codes apart from those of innterest.")
    numRecords <- 0
    #just strip back to the disease we are interested in i.e., those matching with a drug prescription
    print(paste(nrow(clinicalDF),"clinical events before removing those that don't match the disease of interest. You had", length(getUniquePatidList(clinicalDF)), "patients."))
    clinicalDF <- subset(clinicalDF, clinicalDF$medcode %in% medcodeList)
    print(paste(nrow(clinicalDF),"clinical events after removing those that don't match the disease of interest. You now have", length(getUniquePatidList(clinicalDF)), "patients."))
    numRecords <- nrow(clinicalDF)
    if(nrow(clinicalDF)==0) {
      print("Warning: there is no clinical data after removing comorbidities. Returning NULL.")
      return(NULL)
    }

    #Go in here if a referral has been added
    if(!is.null(referralDF)) {
      referralDF <- subset(referralDF, referralDF$medcode %in% medcodeList)
      numRecords <- numRecords + nrow(referralDF)
      if(nrow(referralDF)==0) {
        referralDF <- NULL
      }
    }

    #check whether any suitable records were found
    if(numRecords == 0) {
      print("Warning: there is no clinical data or referral after removing comorbidities. Returning NULL.")
      return(NULL)
    }

  } else if(severity == 1) {
    print("Severity 1 selected. Only using therapy data.")
    clinicalDF <- NULL
    referralDF <- NULL
  }

  #only keep those drugs of interest
  if(!is.null(drugcodeList)) {
    therapyDF <- subset(therapyDF, therapyDF$prodcode %in% drugcodeList)
    if(nrow(therapyDF)==0) {
      print("Warning: there is no therapy data having removed those patients that do not have a specified drugList element. Returning NULL.")
      return(NULL)
    }
  }

  if(severity > 1) {
    print(paste("Working on", length(getUniquePatidList(clinicalDF)), "patients."))
  }

  #construct medical history and restrict by the patient patidList (if supplied - still happens if it isn't supplied)
  medHistoryDF <- constructMedicalHistory(clinicalDF, referralDF, therapyDF)
  if(!is.null(patidList)) {
    medHistoryDF <- subset(medHistoryDF, medHistoryDF$patid %in% patidList)
  }


  #if bounding dates were supplied trim down the dateDF
  if(!is.null(dateDF)) {
    dateDF <- subset(dateDF, dateDF$patid %in% getUniquePatidList(medHistoryDF))
    medHistoryDF <- trimMedHistoryDates(medHistoryDF, dateDF)
  }

  #Whether a patient ever had a prescription from the set of drugs
  if(severity == 1) {
    #technically, this first step is not needed; it's here to be certain
    medHistoryScenarioOneDF <- subset(medHistoryDF, medHistoryDF$codetype=="t")
    #return the patidList of all those patients with a preventative drug present
    return(getUniquePatidList(medHistoryScenarioOneDF))
  }

  #We run the severity 2 and 3 in parallel as they can be very CPU intensive
  patidResultList <- list()

  numCores <- parallel::detectCores()
  if(numCores > 1) {
    numCores <- ceiling(numCores/2)

    #this is for CRAN!
    if(numCores > 2) {
      numCores <- 2
    }
  }
  print(paste("Using", numCores, "cores."))

  parallelMedHistoryList <- getParallelDF(medHistoryDF, numCores)
  `%dopar%` <- foreach::`%dopar%`
  cl <- parallel::makeCluster(numCores, outfile="")
  on.exit(parallel::stopCluster(cl)) #always put this immediately after makeCluster
  parallel::clusterExport(cl, c("getUniquePatidList", "matchDrugWithDiseaseParallel","severity", "parallelMedHistoryList","errorChecker","medcodeList"), envir = environment())
  doParallel::registerDoParallel(cl)

  patidResultList <- foreach::foreach(i = 1:numCores, .combine = 'c') %dopar% {
    subPatidList <- getUniquePatidList(parallelMedHistoryList[[i]])
    patidResultList <- matchDrugWithDiseaseParallel(severity, parallelMedHistoryList[[i]], subPatidList, medcodeList)
  }


  return(patidResultList)

}

#===================================================================================================================

# This is the parallel arm to the user facing function matchDrugWithDisease.
#
# There is no reason for a user of this package to execute this function.
#
# @param severity integer to define search strictness criteria. 1 - a drug anywhere (default), 2 - a drug with a clinical event and comorbidities are unnimportant, 3 - a drug with only a clinical event of interest.
# @param medHistoryDF data frame of any clinical, referral or therapy records that make it this far.
# @param patidList list of patient IDs.
# @param medcodeList list of clinical medcodes.
#
# @return List of data for final return to the user.
matchDrugWithDiseaseParallel <- function(severity, medHistoryDF, patidList, medcodeList) {
  patidResultList <- list()
  listCounter <- 1
  #Whether a patient ever had a prescription on a clincal date
  nMed <- length(getUniquePatidList(medHistoryDF))
  for(i in 1:nMed) {
    id <- patidList[[i]]
    indDF <- medHistoryDF[medHistoryDF$patid == id,]

    #these are the diseases of interest
    diseaseDF <- indDF[indDF$codetype=="c" | indDF$codetype=="r",]

    #this is what I compare too
    drugDF <- indDF[indDF$codetype=="t",]
    if(nrow(drugDF)==0) {
      next()
    }
    drugDatesVector <- drugDF$eventdate

    #check whether a comorbidity is to be detected
    if(severity == 3) {

      #get all diseases (comorbidities) that are not the disease of interest
      comorbidityIndDF <- diseaseDF[!(diseaseDF$code %in% medcodeList),]

      #if no comorbidities were identified
      if(nrow(comorbidityIndDF) == 0) {
        diseaseDatesVector <- diseaseDF$eventdate
        #is there an overlap between dates?
        if(sum(diseaseDatesVector %in% drugDatesVector) > 0) {
          #save the patid - update the counter
          patidResultList[[listCounter]] <- id
          listCounter <- listCounter + 1
        }

      } else {
        #if comorbidities were identified
        comorbidityDatesVector <- comorbidityIndDF$eventdate

        #remove any clinical event that have an event date that matches a comorbidity date
        diseaseDF <- diseaseDF[!(diseaseDF$eventdate %in% comorbidityDatesVector),] #this avoids an overlap between disease and comorbidity

        #get out the date vector of what remains and compare with the drug dates vector
        diseaseDatesVector <- diseaseDF$eventdate
        if(sum(diseaseDatesVector %in% drugDatesVector) > 0) {
          #save the patid and update the counter
          patidResultList[[listCounter]] <- id
          listCounter <- listCounter + 1
        }
      }

    } else if(severity==2) {
      #check whether a clinical event happens on the same date as a therapy event
      diseaseDatesVector <- diseaseDF$eventdate
      #is there an overlap between dates?
      if(sum(diseaseDatesVector %in% drugDatesVector) > 0) {
        #save the patid
        #update the counter
        patidResultList[[listCounter]] <- id
        listCounter <- listCounter + 1
      }
    } else {
      print("Warning: The status supplied was not recognised. This should never happen. Returning NULL.")
      return(NULL)
    }
  }

  return(patidResultList)
}

#===================================================================================================================
#' Retrieve concurrent patient drug prescription records
#'
#' @description
#' Searches a therapy dataset for all drugs (supplied or all drugs present in the therapy dataframe) that coincide with medical events
#' with or without the consideration of comorbidities (adjusted using the severity setting).
#'
#' @details
#' Repeat prescriptions are treated with care. The severity setting will affect how the function interprets the prodcode issueseq value.
#' Firstly, only those drugs prescribed on the date of a clinical event are considered. The accompanying issueseq value can take one of three values:
#'
#' \preformatted{
#' 0 - a one off prescription
#' 1 - the first of a repeat prescription
#' N - the Nth prodcode event of a repeat prescription
#'}
#'
#' As we can't directly link a drug to a clinical event, there are a number of scenarios to consider depending on (a) the
#' requested severity setting, and (2) the value of the prodcode issueseq:
#'
#' 0 - a one off. This is always included with a severity of 1. If there are comorbidities of the same date and the severity is 2, this particular prodcode
#' event is ignored.
#'
#' 1 - the start of a repeat prescription, the functions searches along the patient record and retains
#' the prescriptions. If a comorbidity is recorded during the first prescribing date (issueseq is 1)
#' then the series is discounted if severity was set to 2, otherwise it is kept.
#'
#' N - any number in the prescribing series. The patient has come in with a complaint
#' and there is a prescription event for a drug of interest on the same date. However, it's not the first
#' in a sequence of prescriptions, indicating that it could have been given for a different complaint earlier
#' and that previously prescribed medication is overlapping with a new condition. However, one must consider
#' the situation when a patient comes in for a routine follow-up to an existing complain and the GP recommends that
#' they continue taking the previously prescribed medication. Therefore, overlaps between condition and prescriptions
#' with a issueseq > 1 are also kept. Use this with caution.
#'
#' The argument evidenceDF is a dataframe of events medcode/prodcode which
#' indicate that any prescriptions of drugs in the drugcodeList that fall on these dates
#' (with these events) matches the disease of interest. For example, Triptans are only given
#' to patients with a headache. However, headaches can also be treated with preventatives. Unfortunately,
#' preventatives can also prescribed for other conditions. We can make a rough approximate of the use of
#' these preventatives by not only retaining them according to the list of rules above but also if they
#' are being prescribed on the same date as a Triptan. This is in the instance of when a patient with
#' a history of headache sees their GP. The GP does not record a headache on their record
#' (because they are currently undergoing treatment, it is not a new diagnosis), but the GP
#' begins a prescription for the drug of interest (a migraine preventative) along with
#' the indicator events, the prescription of Triptans.
#'
#' @param clinicalDF Dataframe with at least patid, eventdate, medcode as columns. NULL as default.
#' @param referralDF Dataframe with at least patid, eventdate, medcode as columns. NULL as default.
#' @param therapyDF Dataframe with at least patid, eventdate, prodcode as columns. NULL as default.
#' @param medcodeList List or vector of medcodes. NULL as default.
#' @param drugcodeList List or vector of prodcodes. NULL as default.
#' @param severity integer can be either 1 - search for all drugs which coincide with a medcode of interest, or 2 - search for all
#' drug which coincide with a medcode of interest and free from comorbidities on those dates. Currently, severity 2 code
#' is incomplete and will be released in a later version.
#' @param evidenceDF Dataframe of events medcode/prodcode. Similar to a clinicalDF or therapyDF.
#'
#' @return Dataframe containing only those patient prescription records that satisfy the search criteria.
#' @export
#'
#' @examples
#' requiredProds <- unique(testTherapyDF$prodcode)
#' allMedCodes <- unique(testClinicalDF$medcode)
#' headacheCodes <- allMedCodes[1:10]
#' mockComorbidityCodes <- allMedCodes[11:52]
#' fdo <- getFirstDrugPrescription(testTherapyDF,
#'                                 idList=NULL,
#'                                 prodCodesVector=requiredProds,
#'                                 descriptionFile=NULL)

#' filteredTherapyDF <- filterPatientsByDrugMatchingDisease(testClinicalDF,
#'                                                 NULL,
#'                                                 testTherapyDF,
#'                                                 medcodeList = headacheCodes,
#'                                                 drugcodeList = requiredProds,
#'                                                 severity = 1)
filterPatientsByDrugMatchingDisease <- function(clinicalDF, referralDF=NULL, therapyDF, medcodeList=NULL, drugcodeList=NULL, severity=1, evidenceDF=NULL) {

  #if severity == 1 - don't care about comorbidities. Therefore, only retrieve those conditions
  #which map the specified medcodeList or take them direct for the therapyDF and treat every
  #condition as a condition of interest.
  if(severity == 1) {
    print("Selected to filter therapies by coincidental medcode events without consideration of comorbidities.")
    if(!is.null(medcodeList)) {
      clinicalDF <- subset(clinicalDF, clinicalDF$medcode %in% unlist(medcodeList))
    } else {
      medcodeList <- unique(clinicalDF$medcode)
    }

    #if there is referral data use that too!
    if(!is.null(referralDF)) {
      if(!is.null(medcodeList)) {
        referralDF <- subset(referralDF, referralDF$medcode %in% unlist(medcodeList))
      } else {
        medcodeList <- unique(c(medcodeList, unique(referralDF$medcode)))
      }
    }
  } else if(severity == 2) {
    print("Selected to filter therapies by coincidental medcode events whilst considering comorbidities.")
    print("Severity 2 is currently not supported but will be in a later release.")
    return(NULL)
    #if severity is 2 we must not remove anything yet! We build with the whole clinical and referral data and then
    #look to remove any referral or clinical data that isn't on the same date as the medication.
  }

  if(!is.null(drugcodeList)) {
    therapyDF <- subset(therapyDF, therapyDF$prodcode %in% unlist(drugcodeList))
  } else {
    drugcodeList <- unique(therapyDF$prodcode)
  }

  #make the medhistory of anything clinical or referral - I'll use that to check against.
  medHistoryDF <- constructMedicalHistory(clinicalDF, referralDF, NULL)
  print(paste("Searching over", length(getUniquePatidList(medHistoryDF)), "patients."))

  #contain the new therapy data to then rbind
  therapyList <- list()
  counter <- 1

  #Check what severity has been selected
  if(severity == 1) {
    #find all the therapy dates and ensure that all clinical and referral events only happen at those times
    #do this on a patient basis

    #get out all the headaches so we don't need to worry about comorbidities
    headacheMedHistoryDF <- subset(medHistoryDF, medHistoryDF$code %in% unlist(medcodeList), drop=FALSE)
    #we search over the therapy data
    therapyPatidList <- getUniquePatidList(therapyDF)
    #headache medhistory as a matrix to speed things up!
    headacheMatrix <- trimws(as.matrix(headacheMedHistoryDF, drop=FALSE))

    for(i in 1:length(therapyPatidList)) {
      id <- therapyPatidList[[i]]
      #get the patid from
      indTherapyDF <- subset(therapyDF, therapyDF$patid == id, drop=FALSE)
      indClinicalMat <- headacheMatrix[headacheMatrix[,1] == id,, drop=FALSE]
      if(nrow(indClinicalMat)>0) {
        #was prescribed on a day of headache
        #match the dates
        indClinicalDates <- as.Date(indClinicalMat[,2]) #convert to dates
        indTherapyDF <- subset(indTherapyDF, indTherapyDF$eventdate %in% indClinicalDates, drop=FALSE)

        #store only those dates
        therapyList[[counter]] <- indTherapyDF
        counter <- counter + 1
      }
    }

  } else if(severity == 2) {
    #find all the therapy dates and ensure that all clinical and referral events only happen at those times but with no comorbidities
    therapyPatidList <- getUniquePatidList(therapyDF)
    headacheMedHistoryDF <- subset(medHistoryDF, medHistoryDF$code %in% unlist(medcodeList), drop=FALSE)
    comorbiditiesMedHistoryDF <- subset(medHistoryDF, !(medHistoryDF$code %in% unlist(medcodeList)), drop=FALSE)

    #headache medhistory as a matrix to speed things up!
    headacheMatrix <- trimws(as.matrix(headacheMedHistoryDF))
    comorbidityMatrix <- trimws(as.matrix(comorbiditiesMedHistoryDF))

    #go through the therapy data
    for(i in 1:length(therapyPatidList)) {
      id <- therapyPatidList[[i]]
      #get the patid from
      indTherapyDF <- subset(therapyDF, therapyDF$patid == id, drop=FALSE)

      #first do I have headache data for the patient?
      indHeadacheMat <- headacheMatrix[headacheMatrix[,1] == id, ,drop=FALSE]

      #Yes - then check whether it is confounded with comorbidities
      if(nrow(indHeadacheMat)>0) {
        indComorbidityMat <- comorbidityMatrix[comorbidityMatrix[,1] == id, ,drop=FALSE]

        #if there are comorbidities
        if(nrow(indComorbidityMat)>0) {
          #subset out headaches that do not have comorbidities by matching dates
          comorbidityDates <- as.Date(indComorbidityMat[,2])
          indHeadacheMat <- indHeadacheMat[indHeadacheMat[,2] %in% comorbidityDates, ,drop=FALSE]
        }
        #subset out therapy events that match headache events - the individual headache matrix may have been adjusted at this stage
        headacheDates <- as.Date(indHeadacheMat[,2])
        indTherapyDF <-  subset(indTherapyDF, indTherapyDF$eventdate %in% headacheDates, drop=FALSE)

        #if at least one therapy event has matched a headache date after they have been adjusted
        #for the presence of a comorbidity confound then add it!
        if(nrow(indTherapyDF) > 0) {
          therapyList[[counter]] <- indTherapyDF
          counter <- counter + 1
        }
      }
    }
  }

  #return a therapy data frame that only contains therapy data that meets the criteria (1) or (2).
  if(length(therapyList)==0) {
    print("Warnings: could not find any matching drug-disease relationships. Returning NULL")
    return(NULL)
  }
  resultDF <- do.call("rbind",therapyList)
  return(resultDF)
}






