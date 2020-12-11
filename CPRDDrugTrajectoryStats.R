#===================================================================================================================
#' Structure data for a single-group mean cumulative frequency estimate calculation
#'
#' @description
#' Performs a semi-parametric mean cumulative frequency analysis over CPRD clinical, referral or therapy data. It gives an indication of the burden/demand on primary care services
#' using a single study group (for example, by gender or age).
#'
#' @details
#' Groups must be manually defined by the user, typically through a series of subsetting steps, and presented in the form of a clinical, referral and/or therapy dataframes.
#' For multiple group member comparisons e.g., for two drugs amitriptyline and topiramate, run this function per drug then perform a cbind
#' on each returning data frame with the name of the drug, then finally, rbind both data frames together. Perform: resultMCF <- mcf(Recur(week, id, No.) ~ Drug, data = drugDF).
#' Default variance by Lawless and Nadeau (1995).
#'
#' @param clinicalDF Dataframe of clinical events. Must contain columns "patid", "eventdate", medcode".
#' @param referralDF Dataframe of referral events.  Must contain columns "patid", "eventdate", medcode".
#' @param therapyDF Dataframe of therapy events.  Must contain columns "patid", "eventdate", prodcode".
#' @param startDateCharVector Character vector to denote the date from when patient events are included, e.g., "2000-03-21".
#' @param minRecords Number of records a patient must have to be included in the analysis. Minimum (and default) is 2.
#'
#' @return MCF object which can be used to plot. Alternatively, the mean, upper and lower bound confidence intervals can be extracted and used for bespoke plotting.
#' @export
#'
#' @examples
#' numPatients <- length(getUniquePatidList(testTherapyDF))
#' idList <- getUniquePatidList(testTherapyDF)
#' maleIDs <- idList[1:1500]
#' maleClinicalDF <- testClinicalDF[testClinicalDF$patid %in% maleIDs,]
#' maleTherapyDF <- testTherapyDF[testTherapyDF$patid %in% maleIDs,]
#' femaleIDs <- idList[1501:length(idList)]
#' femaleClinicalDF <- testClinicalDF[testClinicalDF$patid %in% femaleIDs,]
#' femaleTherapyDF <- testTherapyDF[testTherapyDF$patid %in% femaleIDs,]
#' maleMCFDF <- generateMCFOneGroup(maleClinicalDF,
#'                                  NULL,
#'                                  maleTherapyDF)
#' femaleMCFDF <- generateMCFOneGroup(femaleClinicalDF,
#'                                    NULL,
#'                                    femaleTherapyDF)
#' maleMCFDF <- cbind(maleMCFDF, Gender="Male")
#' femaleMCFDF <- cbind(femaleMCFDF, Gender="Female")
#' genderMCF <- rbind(maleMCFDF, femaleMCFDF)
#' resultMCF <- reda::mcf(reda::Recur(week, id, No.) ~ Gender, data = genderMCF)
generateMCFOneGroup <- function(clinicalDF=NULL, referralDF=NULL, therapyDF=NULL, startDateCharVector="2000-01-01", minRecords=2) {

  clinicalDFCheck <- TRUE
  referralDFCheck <- TRUE
  therapyDFCheck <- TRUE

  if(!is.null(clinicalDF)) {
    clinicalDFCheck <- errorChecker(clinicalDF)
  }
  if(!is.null(referralDF)) {
    referralDFCheck <- errorChecker(referralDF)
  }
  if(!is.null(therapyDF)) {
    therapyDFCheck <- errorChecker(therapyDF)
  }

  #check to ensure at least one of the data.frames is not null
  if(isFALSE(clinicalDFCheck) & isFALSE(referralDFCheck) & isFALSE(therapyDFCheck)) {
    print("Is is impossible to run generateMCFOneGroup without adding data! Check your code. Returning NULL.")
    return(NULL)
  }

  startDateCharVectorCheck <- errorChecker(startDateCharVector)
  minRecordsCheck <- errorChecker(minRecords)

  if(isFALSE(startDateCharVectorCheck) | isFALSE(minRecordsCheck)) {
    print("There was a problem with one of the arguments startDateCharVector or minRecords. Returning NULL.")
    return(NULL)
  }

  numCores <- parallel::detectCores()
  if(numCores > 1) {
    numCores <- ceiling(numCores/2)
  }

  ###THIS IS A TERRIBLE SHAME
  if(numCores > 2) {
    numCores <- 2
  }

  #build the codes to use according to what data was passed into this function
  codesToUse <- as.character()
  if(!is.null(clinicalDF)) { codesToUse <- c(codesToUse, "c")}
  if(!is.null(referralDF)) { codesToUse <- c(codesToUse, "r")}
  if(!is.null(therapyDF)) { codesToUse <- c(codesToUse, "t")}
  print(paste("Using code types:",codesToUse))

  #construct the origin time point for the MCF
  startDate <- as.Date(startDateCharVector)

  print("Building an internal medical history")
  medHistoryDF <- constructMedicalHistory(clinicalDF, referralDF, therapyDF)
  print(paste("Generated a medical history of", length(getUniquePatidList(medHistoryDF)), "patients."))

  #Run one either one code else run across multiple cores.
  if(numCores > 1) {
    parallelMedHistoryList <- getParallelDF(medHistoryDF, numCores)

    print("Running MCF data.frame construction. Approximately 40,000 patients takes around 30 minutes to build on one core.")
    print(paste("Running on", numCores, "cores."))

    `%dopar%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(numCores, outfile="")
    on.exit(parallel::stopCluster(cl)) #always put this immediately after makeCluster
    parallel::clusterExport(cl, c("startDate"), envir=environment())
    #parallel::clusterExport(cl, c("getUniquePatidList", "constructMCF","errorChecker"))
    doParallel::registerDoParallel(cl)
    mcfDF <- foreach::foreach(i = 1:numCores, .combine = 'rbind') %dopar% {
      mcfDF <- constructMCF(medHistoryDF, codesToUse, startDate, 2, TRUE)
    }
  } else {
    print("Running MCF data.frame construction. Approximately 40,000 patients takes around 30 minutes to build on one core.")
    print("Running on one core.")
    mcfDF <- constructMCF(medHistoryDF, codesToUse, startDate, minRecords, TRUE)
  }

  return(mcfDF)
}

#===================================================================================================================
#' Build a dataframe for a mean cumulative frequency calculation
#'
#' @description
#' This function is called by \code{\link{generateMCFOneGroup}} (see for further information). However, if the user
#' has a medHistory dataframe, they can call this function explicitly
#' to generate the data structure necessary to run an MCF estimate over those patients.
#'
#' @details
#' Each patient must have at least two records to contribute.
#'
#' @param medHistoryDF Dataframe containing a combination of clinical, referral or therapy data.
#' @param codetypeVector Vector of desired code types to consider: "c" clinical, "r" referral, and "t" therapy.
#' @param startDate Start of the cohort observation period. Patients are adjusted so records run concurrently i.e., a patient enters 2000 and another 2002, both run for one year. They are counted equally.
#' @param minRecords Removes anyone with fewer than the minimum number (2 by default) of medHistoryDF entries.
#' @param returnData Boolean to indicate whether the MCF data is returned as a dataframe or an MCF object is returned having first perform the MCF estimate with default variance by lawless and Nadeau (1995).
#'
#' @return Dataframe with structured data for an MCF estimate calculation or
#' an MCF result object.
#' @export
#'
#' @examples
#' medHistoryDF <- constructMedicalHistory(testClinicalDF, NULL, testTherapyDF)
#' MCF_df <- constructMCF(medHistoryDF,
#'                        c("c","t"),
#'                        as.Date("2000-01-01"),
#'                        2,
#'                        TRUE)
#' resultMCF <- reda::mcf(reda::Recur(week, id, No.) ~ 1, data = MCF_df)
constructMCF <- function(medHistoryDF, codetypeVector, startDate, minRecords=2, returnData=TRUE) {

  medHistoryDFCheck <- errorChecker(medHistoryDF)
  codetypeVectorCheck <- errorChecker(codetypeVector)
  startDateCheck <- errorChecker(startDate)
  minRecordsCheck <- errorChecker(minRecords)
  returnDataCheck <- errorChecker(returnData)

  if(isFALSE(medHistoryDFCheck)) {
    print("Warning: These is a problem with the medHistoryDF argument in constructMCF. Returning NULL.")
    return(NULL)
  }
  if(isFALSE(codetypeVectorCheck)) {
    print("Warning: These is a problem with the codetypeVector argument in constructMCF. Returning NULL.")
    return(NULL)
  }
  if(isFALSE(startDateCheck)) {
    print("Warning: These is a problem with the startDate argument in constructMCF. Returning NULL.")
    return(NULL)
  }
  if(isFALSE(minRecordsCheck)) {
    print("Warning: These is a problem with the minRecords (default=2) argument in constructMCF. Returning NULL.")
    return(NULL)
  }
  if(isFALSE(returnDataCheck)) {
    print("Warning: These is a problem with the returnData (default=TRUE) argument in constructMCF. Returning NULL.")
    return(NULL)
  }

  print(paste("You supplied", length(getUniquePatidList(medHistoryDF)), "patients."))
  #subset for only those codetypes I am interested in
  medHistoryDF <- subset(medHistoryDF, medHistoryDF$codetype %in% codetypeVector)
  if(nrow(medHistoryDF)==0) {
    print("Having filtered for desired codetypes there were no records left. Returning NULL.")
    return(NULL)
  }
  print(paste("Of those", length(getUniquePatidList(medHistoryDF)), "had the correct codetypes i.e.,", codetypeVector))

  #remove all duplicate dates in a patients record.. you can't cumulate with zero time progression
  #but it always keeps at least one duplicated value
  `%>%` <- dplyr::`%>%`
  y <- medHistoryDF %>% dplyr::group_by(medHistoryDF$patid, medHistoryDF$eventdate) %>% duplicated(fromLast=T)
  medHistoryDF <- as.data.frame(medHistoryDF[!y,])
  print(paste("Made sure that every patient had unique time points only. There should still be", length(getUniquePatidList(medHistoryDF)), "patients compared with before."))

  #get the number of medical instances instances per patient
  occurencesDF <- as.data.frame(table(unlist(medHistoryDF$patid)), stringsAsFactors=FALSE)
  colnames(occurencesDF) <- c("patid","Freq")
  #Keep only those patients with enough medical instances
  occurencesDF <- occurencesDF[occurencesDF$Freq >= minRecords,]
  idList <- getUniquePatidList(occurencesDF)
  medHistoryDF <- subset(medHistoryDF, medHistoryDF$patid %in% idList)
  print(paste("And", length(getUniquePatidList(medHistoryDF)), "had at least", minRecords, "codes to use."))

  medHistoryMatrix <- as.matrix(medHistoryDF)
  medHistoryMatrix <- apply(medHistoryMatrix,2,function(x)gsub('\\s+', '',x))

  newdateTotalVector <- as.Date(character())
  durationTotalVector <- as.integer()
  patidTotalVector <- as.integer()
  censoringTotalVector <- as.integer()

  for(id in idList) {
    ind <- medHistoryMatrix[which(medHistoryMatrix[,'patid']==id),,drop=F]

    indEventdates <- as.Date(ind[,2])

    offsetDate <- difftime(indEventdates[1], startDate, units="days")
    newdate <- as.Date(character())
    duration <- as.integer()
    censoring <- integer()

    newdateCount <- 1
    indEventdatesLength <- length(indEventdates)
    for(i in 1:indEventdatesLength) {
      newdate[i] <- indEventdates[i] - offsetDate #ind[i,2] - offsetDate

      #new stuff added in here
      duration[i] <- round(difftime(newdate[i], startDate, units="weeks"))
      if(i==indEventdatesLength) {
        censoring[i] <- 0  #censor
      } else {
        censoring[i] <- 1  #an event occurred
      }
    }
    patidTotalVector <- c(patidTotalVector, rep(id, length(newdate)))
    newdateTotalVector <- c(newdateTotalVector, newdate)
    durationTotalVector <- c(durationTotalVector, duration)
    censoringTotalVector <- c(censoringTotalVector, censoring)
  }

  mcfDF <- data.frame(id=patidTotalVector, week=durationTotalVector, No.=censoringTotalVector, stringsAsFactors=FALSE)
  print(paste("Final valid patient count:",  length(unique(mcfDF$id))))

  #this will cause a memory leak until the gc is called. This should be removed.
  if(isFALSE(returnData)) {
    #default variance by lawless and Nadeau (1995)
    resultMCF <- reda::mcf(reda::Recur(week,id, No.) ~ 1, data = mcfDF)
    return(resultMCF)
  } else {
    return(mcfDF)
  }
}

#' Constructs a time-to-event survival time dataframe for fixed and time-dependent covariates
#'
#' @description
#' Time-to-event data for Cox proportional hazard regression and KM survival curves.
#' Please read the documentation of each argument carefully. Events and an index are
#' represented using the codes in a patient's medical history dataframe. This enables
#' both medical and drug data to define an index, event, and covariates.
#'
#' @details
#' This function will look for the first/last event that comes after the index date.
#' The code runs in parallel (numCores/2) unless there are fewer than 100 patients. Please do not overlap codes between the event codes for index, covariates and time-to-event (doing so will compromise the results).
#' For example, ensure that a set of codes for the event e.g., TIA, are exclusive
#' from a set of codes that indicate indexVector and tdCovariateList.
#' Patients will be ignored if: (1) they have no index or time-to-event codes, (2) if all index codes (or single date) happen after event codes (or single date), (3) a time-to-event happens on the same day as a code for an index date.
#'
#'
#' @param medHistoryDF Dataframe containing 'medical history' (generated using \code{\link{constructMCF}}). To increase speed remove all events which are not explicitly specified are removed.
#' @param indexVector Vector of event codes or a single date to indicate when the observation begins Patient records that do not satisfy this
#' are ignored. If a date, it must be of the Date (R object) format, i.e., 2005-02-24. It must not be of type POSIXct. If multiple index events are found in a patient the first is used.
#' @param indexPosition String denoting either "FIRST" or "LAST" for an index event before the time-to-event. Useful for "first drug X before event A" and "last drug X before event A".
#' @param eventVector Vector of event codes or a single date that indicates the time-to-event. If a date it must be of the Data format, e.g.: 2005-02-24. It must not be of type POSIXct.
#' @param covariateBooleanList A named-list of non-time-dependent covariates (event codes). If the covariate is present then a 1 is assigned to that patid in a column named using the List entry name.
#' @param tdCovariateList A named List of time-invarient covariants (event codes) e.g., tdCovariateList<-list(analgesic=c(drugcode1,drugcode2,...), Triptan=(drugcode1,drugcode2,...)). The named element is used
#' as the column name in the returning data frame. For every patient with a matching covariate a 0 is supplied. For every patient withh a matching covariate
#' a 1 is initially supplied. If a '+' is specified in the tdCovariateBehaviourVector for a particular covariate, further occurences of a covariate event cause the covariate value to incremement by 1.
#' @param tdCovariateBehaviourVector A vector which is equal in length with tdCovariateList. A '+' indicates that the corresponding tdCovariateList entry increments with each new discovery (a new row).
#' A '-' keeps that tdCovariate value fixed even if further identical covariates are found.
#' @param obsTime Number of days to follow. If the event happens before a status = 1, otherwise a status = 0.
#' @param fixedCovariateDF Dataframe of covariate codes asigned with a patid. The column names are the covariate types, and the corresponding data.frame element
#' values are the covariate values e.g., data.frame(patid=patid, smoking=smokingStatus, BMI=BMIValue, age=ageAt2017). Patients missing a covariate value are assigned NA.
#' @param keepEventlessPatients If TRUE (default), any patients who don't have a time-to-event event on record are still kept. They will yield a 0 in the status. If
#' FALSE, only patientswith an event status of 1 are kept.
#' @param endDate Date to indicate the end of the study.
#'
#' @return Dataframe with columns: patid, time (for non-TD survival), status (0/1), columns per covariates.
#' @export
#'
#' @examples
#' medHistoryMainDF <- constructMedicalHistory(NULL,NULL,testTherapyDF)
#' indexVector <- c(769, 707,83, 1888,49)
#' indexPosition <- "FIRST"
#' tdCovariateList <- list(firstCovariate=c(227,78))
#' tdCovariateBehaviourVector <- c("+")
#' eventVector <- c(11237,5,26,65)
#' obsTime <- 365
#' covariateBooleanList <- NULL
#' fixedCovariateDF <- NULL
#' endDate <- as.Date("2018-01-01")
#' coxDF <- constructSurvivalTimeline(medHistoryMainDF,
#'                                    indexVector,
#'                                    indexPosition,
#'                                    eventVector,
#'                                    covariateBooleanList=NULL,
#'                                    tdCovariateList=NULL,
#'                                    tdCovariateBehaviourVector=NULL,
#'                                    obsTime,
#'                                    fixedCovariateDF=NULL,
#'                                    keepEventlessPatients = TRUE,
#'                                    endDate = endDate)
#'
#' ###A time-dependent example is needed in a later release.
constructSurvivalTimeline <- function(medHistoryDF, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, tdCovariateList=NULL, tdCovariateBehaviourVector=NULL,
                                      obsTime, fixedCovariateDF=NULL, keepEventlessPatients=TRUE, endDate) {

  #test incoming parameters
  if(sum(indexVector %in% eventVector) > 0) {
    print("Warning: Cannot overlap index and time-to-event codes. Returning NULL.")
    return(NULL)
  }
  if(sum(indexVector %in% unlist(tdCovariateList)) > 0) {
    print("Warning: Cannot overlap index and covariate codes. Returning NULL.")
    return(NULL)
  }
  if(sum(eventVector %in% unlist(tdCovariateList)) > 0) {
    print("Warning: Cannot overlap time-to-event and covariate codes. Returning NULL.")
    return(NULL)
  }

  if(!is.null(tdCovariateList)) {
    if(length(tdCovariateList) != length(tdCovariateBehaviourVector)) {
      print("Warning: The time-dependent covarate list is not the same length as the time-dependent covariate behaviour vector. Returning NULL.")
      return(NULL)
    }
  }

  dateConstant <- "DATE"
  eventConstant <- "EVENT"
  indexEventOrDate <- "NONE"
  timeDependent <- FALSE
  #perform checks on inputs

  #does everyone have an index date? as an event? #as a date?
  if(sum(is.numeric(indexVector) | is.character(indexVector))>0) {
    indexEventOrDate <- eventConstant
  } else if(is.date(indexVector)) {
    indexEventOrDate <- dateConstant
    print("An index date as a date is not supported yet. Returning NULL")
    return(NULL)
  } else {
    print("Unrecognised index type. Unrecognised Date or vector of events. Returning NULL.")
    return(NULL)
  }

  #get all patients who have the index event date
  if(indexEventOrDate == eventConstant) {
    thoseWithIndexEventDF <- subset(medHistoryDF, medHistoryDF$code %in% indexVector)
    if(nrow(thoseWithIndexEventDF)==0) {
      print("Could not identify any patients who had an index event date. Returninng NULL.")
      return(NULL)
    }
    thoseWithIndexEventIDList <- getUniquePatidList(thoseWithIndexEventDF)
    medHistoryDF <- subset(medHistoryDF, medHistoryDF$patid %in% thoseWithIndexEventIDList)

    #now get all those with a time-to-event code
    thoseWithEventDF <- subset(medHistoryDF, medHistoryDF$code %in% eventVector)
    if(nrow(thoseWithEventDF)==0) {
      print("Could not identify any patients who had an index and event date. Returninng NULL.")
      return(NULL)
    }
    thoseWithEventIDList <- getUniquePatidList(thoseWithEventDF)
    if(keepEventlessPatients==FALSE) {
      medHistoryDF <- subset(medHistoryDF, medHistoryDF$patid %in% thoseWithEventIDList)
    }
    print(paste("Identified", length(thoseWithEventIDList), "with an index and a time-to-event event."))
  } else {
    #For when a date for index date is supported
  }

  #check whether covariates are time dependent
  if(!is.null(tdCovariateList)) {
    timeDependent <- TRUE
  }

  #if there are only a few records e.g., 100, do in serial, elso parallel
  if(length(thoseWithIndexEventIDList) > 100) {
    #do this in parallel
    print("There are more than 100 patient records. Running in parallel.")
    numCores <- parallel::detectCores()
    if(numCores > 1) {
      numCores <- ceiling(numCores/2)
    }
  } else {
    print("Fewer than 100 patient records. Runing in serial.")
    numCores <- 1
  }

  #Execute in parallel
  if(numCores > 1) {
    ##this is for CRAN!
    if(numCores > 2) {
      numCores <- 2
    }
    parallelMedHistoryList <- getParallelDF(medHistoryDF, numCores)
    print(paste("Running on", numCores, "cores."))

    `%dopar%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(numCores, outfile="")
    on.exit(parallel::stopCluster(cl))
    parallel::clusterExport(cl, c("indexPosition","timeDependent","errorChecker","calculateSurvivalTime","calculateTDSurvivalTime","getUniquePatidList","indexVector","eventVector","covariateBooleanList","tdCovariateList","tdCovariateBehaviourVector", "obsTime", "getTDTimeline","getTimeline","endDate"), envir = environment())
    doParallel::registerDoParallel(cl)
    coxDF <- foreach::foreach(i = 1:numCores, .combine = 'rbind') %dopar% {
      if(isTRUE(timeDependent)) {
        #time-dependent
        coxDF <- getTDTimeline(parallelMedHistoryList[[i]], indexVector, indexPosition, eventVector, covariateBooleanList, tdCovariateList, tdCovariateBehaviourVector, obsTime, endDate = endDate)
      } else {
        #non-time-dependent
        coxDF <- getTimeline(parallelMedHistoryList[[i]], indexVector, indexPosition, eventVector, covariateBooleanList, obsTime, endDate = endDate)
      }
    }
  } else { #execute serial
    print("Running on one core.")
    if(isTRUE(timeDependent)) {
      #time-dependent
      coxDF <- getTDTimeline(medHistoryDF, indexVector, indexPosition, eventVector, covariateBooleanList, tdCovariateList, tdCovariateBehaviourVector, obsTime, endDate = endDate)
    } else {
      #non-time-dependent
      coxDF <- getTimeline(medHistoryDF, indexVector, indexPosition, eventVector, covariateBooleanList, obsTime, endDate = endDate)
    }
  }

  #right at the end we fix patients with any static covariates e.g., age, gender, IMD.
  if(!is.null(fixedCovariateDF)) {

    #get the covariate names (to be used as column in the coxDF)
    covariateNames <- names(fixedCovariateDF)[2:length(fixedCovariateDF)]

    covariateIDList <- getUniquePatidList(fixedCovariateDF)
    #go through each covariate (by name)
    for(i in 1:length(covariateNames))

      #make the column in the coxDF and assigned NA
      nameToUse <- covariateNames[i]
      coxDF <- cbind(coxDF, placeholderName=NA)

      #go through the patient IDs provided in the covariate and assign them to the coxDF (check first thether they exist)
      for(j in 1:length(covariateIDList)) {
        #check whether the coxDF has this particular patient
        if(sum(covariateIDList %in% coxDF$patis)>0) {
          coxDF[coxDF$patid==covariateIDList[[j]],]$placeholderName <- fixedCovariateDF[fixedCovariateDF$patid==covariateIDList[[j]],(i+1)]
        }
      }
      names(coxDF) <- c(names(coxDF[1:(length(coxDF)-1)]), nameToUse)
  }

  #this is a little naughty!
  #add a new column to be total time
  coxDF <- cbind(coxDF, time=as.numeric((coxDF$eventDate-coxDF$indexDate)))
  colnames(coxDF) <- c(colnames(coxDF)[1],"obs_time",colnames(coxDF)[3:length(colnames(coxDF))])

  return(coxDF)
}

#' Not for external call: builds the cohort level time dependent survival time data
#'
#' Builds the cohort level time dependent survival time data through a series
#' of individual calls to the corresponding \code{\link{calculateSurvivalTime}}.
#'
#' @param medHistoryDF Dataframe of all necessary medical history for the patients.
#' @param indexVector Vector of codes that denote the index date for the patient.
#' @param indexPosition A string denoting either "FIRST" or "LAST" for an index event before the time-to-event. Useful for "first drug X before event A" or "last drug X before event A".
#' @param eventVector Vector of codes that indicate an event.
#' @param covariateBooleanList List of named vectors to be used as codes indicating covariates. List element names are used to build data.frame columns.
#' @param obsTime Number of days to observe events.
#' @param endDate Date of when the study ends.
#'
#' @return List with the named elements: survivalTime, indexDate and eventDate.
#' @export
#'
getTimeline <- function(medHistoryDF, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, obsTime, endDate) {
  patidVector <- NA
  survivalTimeVector <- NA
  indexDateVector <- as.Date(as.character())
  eventDateVector <- as.Date(as.character())
  statusVector <- NA
  count <- 1

  idList <- getUniquePatidList(medHistoryDF)
  for(i in 1:length(idList)) {
    indDF <- medHistoryDF[medHistoryDF$patid == idList[[i]],]
    survivalTimeList <- calculateSurvivalTime(indDF, indexVector, indexPosition, eventVector, endDate = endDate)
    if(is.null(survivalTimeList)) {
      next()
    }
    if(length(survivalTimeList)==0) {
      next()
    }
    survivalTime <- survivalTimeList$survivalTime
    indexDate <- survivalTimeList$indexDate
    eventDate <- survivalTimeList$eventDate

    #assign status
    if(isTRUE(survivalTime > obsTime)) {
      status <- 0
      survivalTime <- obsTime
    } else {
      status <- 1
    }
    patidVector[count] <- indDF$patid[1]
    survivalTimeVector[count] <- survivalTime
    indexDateVector[count] <- indexDate
    eventDateVector[count] <- eventDate
    statusVector[count] <- status
    count <- count + 1
  }

  if(sum(is.na(patidVector))>0) {
    print("No patients had survival times.")
    return(NULL)
  }

  coxDF <- data.frame(patid=patidVector, time=survivalTimeVector, indexDate=indexDateVector, eventDate=eventDateVector, status=statusVector, obsWindow=obsTime)

  if(!is.null(covariateBooleanList)) {
    coxDF <- addCovariateBooleanList(coxDF, medHistoryDF, covariateBooleanList, patidVector)
  }

  return(coxDF)

}

#' Not for external call: builds the cohort level time dependent survival time data.
#'
#' @description
#' Builds the cohort level time dependent survival time data through a series
#' of individual patient calls to the corresponding \code{\link{calculateTDSurvivalTime}}.
#'
#' @param medHistoryDF Dataframe of all necessary medical history of the patients.
#' @param indexVector Vector of codes that denote the index date for the patient.
#' @param indexPosition A string denoting either "FIRST" or "LAST" or an index event
#' before the time-to-event. Useful for "first drug X before event A" and "last drug X before event A".
#' @param eventVector Vector of codes that indicate an event.
#' @param covariateBooleanList List of named vectors to be used as codes indicating covariates. List element names are used to build dataframe columns.
#' @param tdCovariateList List of covariate vectors. Each covariate vector must be named. e.g., List(nameA=codeVectorA).
#' @param tdCovariateBehaviourVector As with the tdCovariateList but used to denote whether the covariate stays fixed after having been discovered, or
#' whether the covariate increments per new instance of the covariate on record.
#' @param obsTime Number of days to observe events.
#' @param endDate Date indicating when the study ended. Used to determine lost to follow-up patients.
#'
#' @return List with the named elements: survivalTime, indexDate and eventDate.
#' @export
#'
getTDTimeline <- function(medHistoryDF, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, tdCovariateList, tdCovariateBehaviourVector, obsTime, endDate) {
  idList <- getUniquePatidList(medHistoryDF)
  coxList <- list()
  count <- 1
  for(i in 1:length(idList)) {
    print(idList[[i]])
    indDF <- medHistoryDF[medHistoryDF$patid == idList[[i]],]
    survivalCoxDF <- calculateTDSurvivalTime(indDF, indexVector, indexPosition, eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime, endDate = endDate)
    if(!is.null(survivalCoxDF)) {
      coxList[[count]] <- survivalCoxDF
      count <- count + 1
    }
  }

  if(length(coxList)>0) {
    survivalCoxDF <- do.call(rbind, coxList)
  } else {
    print("No patient Cox data. Returning NULL.")
    return(NULL)
  }

  #add static covariates
  if(!is.null(survivalCoxDF)) {
    if(!is.null(covariateBooleanList)) {
      survivalCoxDF <- addCovariateBooleanList(survivalCoxDF, medHistoryDF, covariateBooleanList, idList)
    }
    return(survivalCoxDF)
  } else {
    print("No patient Cox data. Returning NULL.")
    return(NULL)
  }
}

#' Not for external call: calculates time-dependent time-to-event survival time for a single patient
#'
#' @description
#' See \code{\link{constructSurvivalTimeline}} for shared documentation. Per event code
#' found after the index, a new row is added to the returned column. See cran-r-project.org/web/packages/survival/vignettes/timedef.pdf
#' to see how a subject's time is broken up into time intervals.
#'
#' @param indDF Dataframe for a single patient.
#' @param indexVector Vector of codes that denote the index date for the patient.
#' @param indexPosition Vector of either "FIRST" or "LAST" to determine the position from index date/event
#' before the time-to-event. Useful for "first drug X before event A" and "last drug X before event A".
#' @param eventVector Vector of codes that indicate an event.
#' @param tdCovariateList List of covariate vectors. Each covariate vector must be named. e.g., List(nameA=codeVectorA)
#' @param tdCovariateBehaviourVector Logical vector to indicate whether the covariates increment or stay fixed. The List structure must follow that of tdCovariateList.
#' @param obsTime Number of days follow-up from index date before stop recording event.
#' @param endDate Date indicating when the study ended. Used to determine lost to follow-up patients.
#'
#' @return List with the named elements: survivalTime, indexDate and eventDate.
#' @export
#'
calculateTDSurvivalTime <- function(indDF, indexVector, indexPosition, eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime, endDate) {
  #only keep the essential codes to keep memory footprint small
  allCodes <- c(indexVector, eventVector, unlist(tdCovariateList))
  indDF <- subset(indDF, indDF$code %in% allCodes)

  coxDF <- NULL
  covariateDF <- NULL
  status <- NA
  survivalTime <- NA
  patid <- indDF$patid[1]
  covariateLineCount <- 1
  proposedIndexDate <- NULL
  eventDate <- NA
  previousDate <- NA
  foundIndex <- FALSE
  foundEvent <- FALSE
  coxDFLength <- 0
  indexStep <- 0

  previousDate <- indDF$eventdate[1]
  nIndDF <- nrow(indDF)

  #go through an individual's medical history
  for(i in 1:nIndDF) {

    #the algorithm found an index position in the first iteration and now it is waiting for the i iteration to catch up with where the index is.
    if(indexPosition == "LAST" & isTRUE(foundIndex)) {
      if(indexStep > i) {
        next()
      }
    }

    #if a time-to-event event has already been parsed then leave
    if(isTRUE(foundEvent)) {
      break()
    }

    currentEvent <- indDF$code[i]
    currentDate <- indDF$eventdate[i]

    #check whether this gets to the end of a record and still hasn't found an index date!
    if(i == nIndDF) {
      if(isFALSE(foundIndex)) {
        print("Warning: calculateTDSurvivalTime got to the end of a patient record and didn't find an index date. This should never happen. Please report. Returning NULL.")
        return(NULL)
      } else if(sum(currentEvent %in% eventVector) == 0) {
        print("Warning: calculateTDSurvivalTime got to the end of a patient record and the final date was not a time-to-event event. This should never happen. Please report. Returning NULL.")
        return(NULL)
      }
    }

    #test to see whether an index date has been found
    if(isFALSE(foundIndex)) {
      if(sum(currentEvent %in% eventVector) > 0) {
        #found an event before an index! Ignoring this patient
        print("Patient had a time-to-event event before the index event. Ignoring patient.")
        return(NULL)
      } else if(sum(currentEvent %in% indexVector) > 0) { #is the current event an index?
        covariateDF <- as.data.frame(matrix(0,ncol=length(tdCovariateList), nrow=1))
        names(covariateDF) <- names(tdCovariateList)
        #============================FOUND INDEX===========================================
        #Check whether the index type is FIRST or LAST
        if(indexPosition == "FIRST") {
          foundIndex <- TRUE
          proposedIndexDate <- currentDate
          previousDate <- currentDate
          print("TEST: found a FIRST index date.")
        } else {
          #The last index before a time-to-event. go through each event from the index date
          for(j in i:nrow(indDF)) {
            futureEvent <- indDF$code[j] #get the next event
            futureDate <- indDF$eventdate[j]
            #is the event an index date? - this will skip over covariates as their codes shouldn't be in index or event codes
            if(sum(futureEvent %in% indexVector)>0) {
              proposedIndexDate <- futureDate
              previousDate <- futureDate
            } else if(sum(futureEvent %in% eventVector)>0) {
              #then step out (having defined the proposed index date above) when a time-to-event has been found
              foundIndex <- TRUE
              indexStep <- j-1
              break()
            }
          }
        } #FIRST or LAST end of loop
        #ensure the proposed index date is not the same as an end date. If so, then ignore patient.
        endIndDF <- subset(indDF, indDF$code %in% eventVector)
        if(sum(endIndDF$eventdate %in% proposedIndexDate) > 0) {
          #the end-event date was the same as the locate index date
          #this is zero survival time - ignore this patient.
          return(NULL)
        }

        #are there any time dependent covariates on the same day as the index day?
        if(!is.null(tdCovariateList)) {
          if(sum(proposedIndexDate %in% indDF$eventdate) > 0) { #i.e., index date + any other event of the same date? >0 because this will only return a single true or false as it's one-date %in% many-dates

            #just get the covariates - this might be empty
            potentialCovDF <- subset(indDF, indDF$code %in% unlist(tdCovariateList))
            potentialCovDF <- subset(potentialCovDF, potentialCovDF$eventdate == proposedIndexDate)
            if(nrow(potentialCovDF) > 0) {
              #find the time dep cov that happen on the same day as the inex date and then increment
              for(n in 1:length(tdCovariateList)) {
                covCodes <- tdCovariateList[[n]]
                if(sum(covCodes %in% potentialCovDF$code) > 0) {
                  print("Adding covariate during the index date")
                  covariateDF[1,n] <- 1
                }
              }
            }
          }
        }
      }
    } else { #if the index date has been satisfied...
      #check if the currentEvent is a time-to-event
      if(sum(currentEvent %in% eventVector)>0) {
        #============================FOUND TIME-TO-EVENT EVENT===========================================
        foundEvent <- TRUE
        eventDate <- currentDate
        print("Found end-event.")

        #discount patients when the index date and time-to-event date happen on the same time.
        if(isTRUE(eventDate == proposedIndexDate)) {
          return(NULL)
        }

        #If a covariate has not been found
        if(covariateLineCount == 1) { #the empty covariate line (but does this take into account covariates at the same time as index?)
          #no covariates found
          survivalTime <- as.numeric(eventDate - proposedIndexDate)
          if(survivalTime < obsTime) {
            #the event happened - only when a time to event has been found
            if(isTRUE(foundEvent)) {
              status <- 1
            } else {
              status <- 0
            }
          } else {
            #censor
            status <- 0
            survivalTime <- obsTime

          }
          coxDF <- data.frame(patid=patid, time=0, tstart=0, tstop=survivalTime, indexDate=proposedIndexDate, eventDate=eventDate, status=status, obsWindow=obsTime)
        } else {
          #if a covariates has been found
          tempCoxDF <- coxDF #make a complete copy of the data.frame
          coxDF <- coxDF[nrow(coxDF),] #take the last row
          coxDF$tstart <- tempCoxDF[coxDFLength,]$tstop #set the tstart date to be the time from the tstop of the last row
          survivalTime <- coxDF$tstart + as.numeric(currentDate - previousDate)
          if(survivalTime < obsTime) {
            #the event happened
            status <- 1
          } else {
            #censor
            status <- 0
            survivalTime <- obsTime
          }
          coxDF$tstop <- survivalTime
          coxDF$status <- status
          coxDF$eventDate <- as.Date(coxDF$eventDate)
          coxDF <- rbind(tempCoxDF, coxDF)

          for(j in 1:length(tdCovariateList)) {
            previousCovariateValue <- covariateDF[nrow(covariateDF)-1,j]
            if(previousCovariateValue > covariateDF[nrow(covariateDF),j]) {
              covariateDF[nrow(covariateDF),j] <- covariateDF[nrow(covariateDF)-1,j]
            }
          }
        }

        #============================FOUND COVARIATE===========================================
      } else if(sum(currentEvent %in% unlist(tdCovariateList))>0) {

        #ensure that the covariate didn't fall on the same day as the index day; it would have been taken care of
        if(currentDate == proposedIndexDate) {
          next()
        }

        #go through each covariate vector in the list, see which one it matches, then work out whether to incremement or keep fixed
        #go through the covariate list elements and find a match using: currentEvent %in% covariateEvents
        for(j in 1:length(tdCovariateList)) {
          covariateEvents <- tdCovariateList[[j]] #this is a vector of events for that particular covariates e.g., codes for Triptan
          if(sum(currentEvent %in% covariateEvents)>0) {
            covariateType <- tdCovariateBehaviourVector[j]

            if(previousDate == currentDate) { #<-----events can be on the same day
              #therefore, do not make a new covariate line, add to the current one (and do not update the timings)
              if(isTRUE(covariateType == "+")) {
                covariateDF[covariateLineCount,j] <- covariateDF[covariateLineCount-1,j] + 1
              } else {
                covariateDF[covariateLineCount,j] <- 1
              }
            } else {
              #make a new coxDF data.frame line as we already have an index date, now we have the first covariate event (copy the older line)
              #first covariate found
              if(covariateLineCount == 1) {
                #populate the first entry start tstart=0
                survivalTime <- as.numeric(currentDate-proposedIndexDate)
                status <- 0
                tstart <- 0
                if(survivalTime > obsTime) {
                  #the covariate happens after the desired observation time. It is not counted, and the final coxDF is built and we exit this patient.
                  tstop <- obsTime
                  foundEvent <- TRUE #artificially say we found the time-to-event so we can leave this patients as they shot over the observation window
                } else {
                  tstop <- survivalTime
                }

                coxDF <- data.frame(patid=patid, time=0, tstart=0, tstop=tstop, indexDate=proposedIndexDate, eventDate=currentDate, status=status, obsWindow=obsTime)
                coxDFLength <- 1

                covariateDF <- rbind(covariateDF, covariateDF[(covariateLineCount),]) #then append the covaraite data to the existing covaraite data
                covariateDF[nrow(covariateDF),j] <- 1
                covariateLineCount <- covariateLineCount + 1
              } else {
                #always check whether the time to the currentDate from the previous date takes the survival time past the observation time
                tempCovariateDF <- covariateDF[(covariateLineCount-1), ,drop=FALSE] #get the previous single row to overwrite (it is only a copy, saves me from writing a data.frame line)
                tempCoxDF <- coxDF[coxDFLength, , drop=FALSE] #get the previous single row to overwrite (it is only a copy, saves me from writing a data.frame line)

                tempCoxDF$tstart[1] <- coxDF[coxDFLength,]$tstop #get the last tstop date to become the new start date
                survivalTime <- tempCoxDF$tstart[1] + as.numeric(currentDate - previousDate)
                status <- 0
                if(survivalTime < obsTime) {
                  status <- 0
                  tempCoxDF$tstop[1] <- survivalTime
                } else {
                  status <- 0
                  tempCoxDF$tstop[1] <- obsTime
                  foundEvent <- TRUE
                }

                tempCoxDF$status[1] <- status
                tempCoxDF$time[1] <- 0
                if(isTRUE(covariateType == "+")) {
                  tempCovariateDF[1,j] <- covariateDF[covariateLineCount,j] + 1
                } else {
                  tempCovariateDF[1,j] <- 1
                }

                coxDF <- rbind(coxDF, tempCoxDF) #then append the cox data to the existing cox data
                coxDFLength <- coxDFLength + 1

                if(isTRUE(foundEvent)) { #got to the end of the observation time without finding the time-to-event event
                  break()
                } else {
                  covariateDF <- rbind(covariateDF, tempCovariateDF) #then append the covaraite data to the existing covaraite data
                }
                covariateLineCount <- covariateLineCount + 1

              }
            }
          }
        }
        previousDate <- currentDate
      } else {
        #print("TEST: an event that is not a covariate, index or time-to-event.")
        #place holder
      }
    }
  }
  coxDF <- cbind(coxDF, step=1:nrow(coxDF))
  row.names(coxDF) <- NULL
  coxDF <- cbind(coxDF, covariateDF)
  return(coxDF)
}


#' Not for external call: calculates time-to-event survival time for a single patient.
#'
#' @description
#' Calculates time-to-event time for a single patient.
#'
#' @details
#' Time calculated between an index position (denoted as codes in indexVector)
#' and an event (denoted as codes in eventVector). Patients are lost to follow-up
#' if the time-to-event duration goes beyond the endDate.
#'
#' @param indDF Dataframe for a single patient.
#' @param indexVector Vector of codes that denote the index date for the patient.
#' @param indexPosition A string of either "FIRST" or "LAST" to determine the
#' position of the index date/event.
#' before the time-to-event. Useful for "first drug X before event A" and "last drug X before event A".
#' @param eventVector Vector of codes that denote the event for the patient.
#' @param endDate Date indicating when the study ended. Used to determine lost to follow-up patients.
#'
#' @return List with the named elements: survivalTime, indexDate and eventDate.
#' @export
#'
calculateSurvivalTime <- function(indDF, indexVector, indexPosition, eventVector, endDate) {

  #get the  index from indexVector
  indexDatesDF <- indDF[indDF$code %in% indexVector,]
  if(nrow(indexDatesDF)==0) {
    print(paste("Could not find index date in patient", indDF$patid[1]))
    return(NULL)
  }

  #get the possible time-to-events - we want the first one
  eventDatesDF <- indDF[indDF$code %in% eventVector,]
  if(nrow(eventDatesDF)==0) {
    #print(paste("Could not find time-to-event date in patient", indDF$patid[1]))
    #return(NULL)
    #use the end of record time
    eventDate <- endDate
  } else {
    eventDate <- eventDatesDF$eventdate[1]
  }


  if(indexPosition=="FIRST") {
    indexDate <- indexDatesDF$eventdate[1]

    if(isFALSE(indexDate < eventDate)) {
      return(NULL)
    }
  } else {
    #this is for the last index date before an eventi.e., the last prescription of drug X before an event
    storedIndexDate <- NULL
    for(i in 1:length(indexDatesDF$eventdate)) {
      indexDate <- indexDatesDF$eventdate[i]
      if(isTRUE(indexDate < eventDate)) {
        storedIndexDate <- indexDate
        #then move onto the next
      } else {
        #we either found the last index before the time-to-event or we didn't find an index at all
        break()
      }
    }
    if(is.null(storedIndexDate))  {
      return(NULL)
    }
    indexDate <- storedIndexDate
  }

  survivalTime <- as.numeric(eventDate - indexDate)

  returnList <- list(survivalTime=survivalTime, indexDate=indexDate, eventDate=eventDate)
  return(returnList)
}


#' Not for external call. Adds time-independent TRUE/FALSE covariate values to a Cox dataframe
#'
#' @description
#' This function appends covariate columns to a Cox dataframe (having called \code{\link{constructSurvivalTimeline}} first) for all those codes stored in the named covariateBooleanList argument. The
#' covariate columns are named after the named list elements in the covariateBooleanList. The covariate search can be restricted by providing a
#' list of patient patids in the patidIDVector. A patient's record is checked for codes by searching through the the medHistoryDF.
#'
#' @details
#' The covariateBooleanList is a named list of medcodes and/or prodcodes e.g., List(drugA=c(1,2,3,4), drugB=c(5,6,7,8), conA=c(97,98,501)). Boolean covariates are
#' stored as either 0 or 1 to represent FALSE/TRUE, NO/YES, ABSENT/PRESENT, etc.
#'
#' @param coxDF Dataframe generated by first calling \code{\link{constructSurvivalTimeline}}.
#' @param medHistoryDF Dataframe of patient medical (clinical, therapy etc) data.
#' @param covariateBooleanList A named list of codes (can be a mix of medcodes and prodcodes). Each named element is used as the covariate column name.
#' @param patidVector List or vector of patient patids. If NULL (default) then the medHistoryDF is filtered for all patients in the coxDF.
#'
#' @return Dataframe similar to the coxDF argument but with additional columns to denote each boolean covariate.
#' @export
#'
addCovariateBooleanList <- function(coxDF, medHistoryDF, covariateBooleanList, patidVector=NULL) {
    #assign covariates (do this outside the loop) - go through each and check
    #make the medHistory only relative to those with survival times
    covariatesNames <- names(covariateBooleanList)

    covariateDF <- data.frame(patid=patidVector)
    for(i in 1:length(covariateBooleanList)) {
      covariateDF <- cbind(covariateDF, placeholder=NA)
    }
    names(covariateDF) <- c("patid", covariatesNames)


  if(is.null(patidVector)) {
    medHistoryDF <- subset(medHistoryDF, medHistoryDF$patid %in% getUniquePatidList(coxDF)) #only patients with survival times
  } else {
    medHistoryDF <- subset(medHistoryDF, medHistoryDF$patid %in% patidVector) #only patients with survival times
  }


  idList <- getUniquePatidList(medHistoryDF)
  #go through each covariate type
    for(j in 1:length(covariateBooleanList)) {
      #covariateName <- covariatesNames[[j]]
      covariatesEvents <- covariateBooleanList[[j]]
      #go through the relevent medhistory entries
      for(i in 1:length(idList)) {
        indDF <- subset(medHistoryDF, medHistoryDF$patid == idList[[i]])
        indDF <- subset(indDF, indDF$code %in% covariatesEvents)

        #no covariates for this individual
        if(nrow(indDF)==0) {
          #set covariate to 0
          #covariatesToUseList$`covariatesName`[i] <- 0
          #covariatesToUseList[i,(j+1)] <- 0
          covariateDF[covariateDF$patid==idList[[i]],][(j+1)] <- 0
        } else {
          #do any covariate event dates fall between index and event date?
          eventDatesFound <- indDF$eventdate #get the eventdates for that event

          index <- coxDF[coxDF$patid == idList[[i]],]$indexDate[1]
          event <- coxDF[coxDF$patid == idList[[i]],]$eventDate[1]

          #do any events happens after indexdate?
          if(sum(index < eventDatesFound) > 0) {

            #get all those events less than eventdate
            if(sum(event < eventDatesFound) > 0) {
              #covariatesToUseList$`covariatesName`[i] <- 1
              #covariatesToUseList[i,(j+1)] <- 1
              covariateDF[covariateDF$patid==idList[[i]],][(j+1)] <- 1
            } else {
              #covariates are beyond the event date
              #covariatesToUseList$`covariatesName`[i] <- 0
              #covariatesToUseList[i,(j+1)] <- 0
              covariateDF[covariateDF$patid==idList[[i]],][(j+1)] <- 0
            }

            #get all those events equal to the indexdate - they happen on the index date
          } else if(sum(index > eventDatesFound) == 1) {
            #covariatesToUseList$`covariatesName`[i] <- 1
            #covariatesToUseList[i,(j+1)] <- 1
            covariateDF[covariateDF$patid==idList[[i]],][(j+1)] <- 1
          } else {
            #when events happen before index date
            #covariatesToUseList$`covariatesName`[i] <- 0
            #covariatesToUseList[i,(j+1)] <- 0
            covariateDF[covariateDF$patid==idList[[i]],][(j+1)] <- 0
          } #location of the covariate
        }#whether there is a covariate
      }#go through each patient
    } #go through each covariate

    coxDF <- cbind(coxDF, covariateDF)

  return(coxDF)
}


#===============================================================================

#' Calculates the proportion of patients still taking a particular drug
#' n days after first-prescription
#'
#' @description
#'  Provides information on
#' the presence of a drug N days after first-prescription with some degree of buffer.
#' This is not for observing a change in medication. This does not provide information
#' on the events between the first instance of a drug prescription and any instance n days later.
#'
#' @details
#' This is very useful to simply monitor the proportion of patients still being prescribed
#' a medication n days later whilst being blind to the events that took place in between.
#'
#' Due to the nature of some datasets, CPRD especially, observing N days later for a prescription is very unlikely to yield any interesting results.
#' Rather, a buffer is provided to define a window of time before the duration cut-off to
#' look for records of drugs of interest. For example, given a duration of 365 days and
#' a buffer of 30 day, all prescriptions for example, amitriptyline, are valid if they fall within
#' the interval [365-30, 365] days after their first amitriptyline prescription.
#'
#' The proportion of those patients still on the drugs (i.e., meeting the requirements specified
#' in the function parameters) is the length of the returning list (length(resultList)) divided by
#' the number of patients sampled
#'
#' Therefore, two individuals would yield identical results if their record were structured:
#' start--D--D---D---D---D----D---D--|--D--end
#' start--D-----------------------D--|--D--end
#' even though the first patient continued taking the drug D from their first prescription
#' up to n days later (with buffer) whilst the second patient had a significantly long
#' break from the prescription, despite then being prescribed the medication shortly before
#' the end of the period of interest.
#'
#' @param therapyDF Dataframe of therapy events.
#' @param idList Patients of interest. If left NULL all the patients in therapyDF are used.
#' @param prodcodeList List or vector of prodcodes to identify drugs of interest. If
#' left NULL then all prodcodes in the therapyDF are used.
#' @param duration Number of days to check whether a patient was prescribed the same drug
#' some N days after their first prescription.
#' @param buffer Number of days from before the duration cut-off where the presence of a drug
#' is valid.
#' @param endOfRecordDate A date in the form as.Date("yyyy-mm-dd"). Any patients with a first drug of interest plus duration time
#' exceeds this date are ignored. This provides an upper boundary to the data set and prevents
#' pulling in patients who are lost to follow up.
#'
#'
#' @return A list where each element contains a nested list of two elements patid and
#' prodcode. Patid is the patient ID and prodcode is a vector of all prodcodes found within the time
#' of interest. Also prints the proportion of those patients with the desired medication N days after the
#' first prescription.
#' @export
#'
#' @examples
#' patientList <- getDrugPersistence(testTherapyDF,
#'                                   idList=NULL,
#'                                   prodcodeList=NULL,
#'                                   duration=395,
#'                                   buffer=60,
#'                                   endOfRecordDate="2017-12-31")
getDrugPersistence <- function(therapyDF, idList=NULL, prodcodeList=NULL, duration=365, buffer=30, endOfRecordDate="2017-12-31") {
  therapyDFCheck <- errorChecker(therapyDF)
  durationCheck <- errorChecker(duration)
  bufferCheck <- errorChecker(buffer)
  endOfRecordDateCheck <- errorChecker(endOfRecordDate)
  idListCheck <- TRUE
  if(!is.null(idList)) {
    idListCheck <- errorChecker(idList)
  }
  prodcodeListCheck <- TRUE
  if(!is.null(prodcodeList)) {
    prodcodeListCheck <- errorChecker(prodcodeList)
  }

  if(isFALSE(endOfRecordDate) | isFALSE(therapyDFCheck) | isFALSE(durationCheck) | isFALSE(bufferCheck) | isFALSE(idListCheck) | isFALSE(prodcodeListCheck)) {
    print("Error: There is a problem with one of the argument to getDrugPersistence(). Returning NULL")
    return(NULL)
  }

  if(duration < 1) {
    print("Error: duration argument to getDrugPersistence cannot be less than 1. Returning NULL")
    return(NULL)
  }

  if(buffer < 0) {
    print("Error: buffer argument to getDrugPersistence cannot be less than 0. Returning NULL")
    return(NULL)
  }

  if(buffer == 0) {
    print("Warning: buffer time was 0 days. It is very unlikely that getDrugPersistence will produce anything meaingful.")
  }

  if(is.character(endOfRecordDate) == FALSE) {
    print("Error: the endOfRecordDate must be a character of the format: yyyy-mm-dd. Returning NULL")
    return(NULL)
  }
  endOfRecordDate <- as.Date(endOfRecordDate)

  #no ids were provided so take directly from all those patients with a drug record
  if(is.null(idList)) {
    idList <- getUniquePatidList(therapyDF)
  } else {
    therapyDF <- subset(therapyDF, therapyDF$patid %in% idList)
  }
  #no prodcodes were provided so take directly from all those patients with a drug
  if(is.null(prodcodeList)) {
    prodcodeList <- unique(therapyDF$prodcode)
  } else {
    therapyDF <- subset(therapyDF, therapyDF$prodcode %in% prodcodeList)
  }

  #check to see if there are any records left?
  if(nrow(therapyDF) == 0) {
    print("Warning: After refining the therapyDF by the idList and prodcodeList there were no patients left. Returning NULL.")
    return(NULL)
  }

  therapyMatrix <- trimws(as.matrix(therapyDF))
  #go through each patient
  #1)take out their therapy data
  #2)get the first drug (it will already match what we want to start with)
  #3) check whethet they are lost to followup
  #4) define a lower bound and an upper bound date using
  #4a) eventdate + (duration - buffer)
  #4b) eventdate + (duration)
  #5) check whether a drug exists between (inclusive) of these two times. If yes, which ones?
  resultList <- lapply(idList, function(id) {
    indMat <- therapyMatrix[therapyMatrix[,c("patid")]==id,, drop=FALSE]
    #if only one prescription event happened for that patient then we are forced to skip
    if(nrow(indMat)==1) {
      #listReturn <- list(patid=id, prodcode=NA)
      listReturn <- NULL
    } else {
      firstDate <- as.Date(indMat[1,2])
      upperDate <- as.Date(firstDate+duration)
      #patient lost to follow up.
      if(upperDate > endOfRecordDate) {
        #listReturn <- list(patid=id, prodcode=NA)
        listReturn <- NULL
      } else {
        lowerDate <- as.Date(upperDate - buffer)
        if(lowerDate > firstDate) {
          #trim the rows of indMat to see if any drugs remains
          indMat <- indMat[as.Date(indMat[,c("eventdate")])>=lowerDate, ,drop=FALSE]
          indMat <- indMat[as.Date(indMat[,c("eventdate")])<=upperDate, ,drop=FALSE]
          if(nrow(indMat) > 0) {
            listReturn <- list(patid=id, prodcode=unique(indMat[,3]))
          } else {
            #listReturn <- list(patid=id, prodcode=NA)
            listReturn <- NULL
          }

        } else {
          #if this happens then date is too close to the original prescription and buffer
          #is too wide, putting the date to begin observing behind the original prescription.
          #listReturn <- list(patid=id, prodcode=NA)
          listReturn <- NULL
        }
      }
    }
    listReturn
  })

  print(paste("Checked", length(resultList), "patients."))
  totalPatients <- length(resultList)
  resultList <- resultList[!sapply(resultList, is.null)]
  print(paste(length(resultList), "patients had a drug prescription", duration,
              "days later whilst allowing for a pre-buffer of", buffer, "days, from their first prescription."))
  print(paste("That's", length(resultList)/totalPatients, "of the input cohort still being prescribed these prodcodes."))

  return(resultList)

}






