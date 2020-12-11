testAll <- function() {
  numFails <- 0
  numFailsAA <- testConstructMedHistory()
  numFails <- numFailsAA + numFails
  numFailsAB <- testErrorChecker()
  numFails <- numFailsAB + numFails
  numFailsAC <- testGetUniquePatidList()
  numFails <- numFailsAC + numFails
  numFailsAD <- testGetBurnInPatients()
  numFails <- numFailsAD + numFails
  numFailsAE <- testGetPatientsWithProdCode()
  numFails <- numFailsAE + numFails
  numFailsAF <- testGetFirstDrugPrescription()
  numFails <- numFailsAF + numFails
  numFailsAG <- testCombinePrescriptionFrequencies()
  numFails <- numFailsAG + numFails
  numFailsAH <- testGetEventdateSummaryByPatient()
  numFails <- numFailsAH + numFails
  numFailsAI <- testGetPopulationDrugSummary()
  numFails <- numFailsAI + numFails
  numFailsAJ <- testMapDrugTrajectory()
  numFails <- numFailsAJ + numFails
  numFailsAK <- testGetPatientsWithFirstDrugWithDisease()
  numFails <- numFailsAK + numFails
  numFailsAL <- testGetGenderOfPatients()
  numFails <- numFailsAL + numFails
  numFailsAM <- testGetIMDOfPatients()
  numFails <- numFailsAM + numFails
  numFailsAN <- testGetAgeGroupByEvents()
  numFails <- numFailsAN + numFails
  numFailsAO <- testMatchDrugWithDisease()
  numFails <- numFailsAO + numFails
  numFailsAP <- testGetParallelDF()
  numFails <- numFailsAP + numFails
  numFailsAQ <- testConvertCodesToDescriptions()
  numFails <- numFailsAQ + numFails
  numFailsAR <- testGetMultiPrescriptionSameDayPatients()
  numFails <- numFailsAR + numFails
  numFailsAS <- testGetSettingFromFirstPrescription()
  numFails <- numFailsAS + numFails
  numFailsAU <- testGetAnyDrugPrescriptionByYear()
  numFails <- numFailsAU + numFails
  numFailsAV <- testGetFirstDrugPrescriptionByYear()
  numFails <- numFailsAV + numFails
  numFailsAW <- testRemovePatientsByDuration()
  numFails <- numFailsAW + numFails
  numFailsAX <- testGetPatientsWithDiseaseButNoDrug()
  numFails <- numFailsAX + numFails
  numFailsAY <- testGetPatientsWithFirstDrugWithNoDisease()
  numFails <- numFailsAY + numFails
  numFailsAAA <- testFilterPatientsByDrugMatchingDisease()
  numFails <- numFailsAAA + numFails



  numFailsCA <- testGenerateMCFOneGroup()
  numFails <- numFailsCA + numFails
  numFailsCB <- testCalculateTDSurvivalTime()
  numFails <- numFailsCB + numFails
  numFailsCC <- testGetTDTimeline()
  numFails <- numFailsCC + numFails
  numFailsCD <- testGetTimeline()
  numFails <- numFailsCD + numFails
  numFailsCE <- testCalculateSurvivalTime()
  numFails <- numFailsCE + numFails
  numFailsCF <- testGetDrugPersistence()
  numFails <- numFailsCF + numFails
  numFailsCG <- testGetFirstDrugIncidenceRate()
  numFails <- numFailsCG + numFails
  numFailsCH <- testConstructSurvivalTimeline()
  numFails <- numFailsCH + numFails

  numFailsDA <- testSumAcrossAgeGroups()
  numFails <- numFailsDA + numFails
  numFailsDB <- testCheckCPRDRecord()
  numFails <- numFailsDB + numFails
  numFailsDC <- testConstructMCF()
  numFails <- numFailsDC + numFails
  numFailsDD <- testSaveCPRDDataframe()
  numFails <- numFailsDD + numFails
  numFailsDE <- testSaveCPRDDataframeAsText()
  numFails <- numFailsDE + numFails
  numFailsDF <- testSaveCPRDList()
  numFails <- numFailsDF + numFails


  print("=====================TEST RESULT BY FUNCTION=======================================")
  print(paste("constructMedHistory returned",numFailsAA ,"errors."))
  print(paste("errorChecker returned",numFailsAB ,"errors."))
  print(paste("getUniquePatidList returned",numFailsAC ,"errors."))
  print(paste("getBurnInPatients returned",numFailsAD ,"errors."))
  print(paste("getPatientsWithProdCode returned",numFailsAE ,"errors."))
  print(paste("getFirstDrugPrescription returned",numFailsAF ,"errors."))
  print(paste("combinePrescriptionFrequencies returned",numFailsAG ,"errors."))
  print(paste("getEventdateSummaryByPatient returned",numFailsAH ,"errors."))
  print(paste("getPopulationDrugSummary returned",numFailsAI ,"errors."))
  print(paste("mapDrugTrajectory returned",numFailsAJ ,"errors."))
  print(paste("getPatientsWithFirstDrugWithDisease returned",numFailsAK ,"errors."))
  print(paste("getGenderOfPatients returned",numFailsAL ,"errors."))
  print(paste("getIMDOfPatients returned",numFailsAM ,"errors."))
  print(paste("getAgeGroupByEvents returned",numFailsAN ,"errors."))
  print(paste("matchDrugWithDisease returned",numFailsAO ,"errors."))
  print(paste("getParallelDF",numFailsAP ,"errors."))
  print(paste("convertCodesToDescriptions returned",numFailsAQ ,"errors."))
  print(paste("getMultiPrescriptionSameDayPatients returned",numFailsAR ,"errors."))
  print(paste("getSettingFromFirstPrescription returned",numFailsAS ,"errors."))
  print(paste("getAnyDrugPrescriptionByYear returned",numFailsAU ,"errors."))
  print(paste("getFirstDrugPrescriptionByYear returned",numFailsAV ,"errors."))
  print(paste("removePatientsByDuration returned",numFailsAW ,"errors."))
  print(paste("getPatientsWithDiseaseButNoDrug returned",numFailsAX ,"errors."))
  print(paste("getPatientsWithFirstDrugWithNoDisease returned",numFailsAY ,"errors."))
  print(paste("filterPatientsByDrugMatchingDisease returned",numFailsAAA ,"errors."))

  print(paste("generateMCFOneGroup returned",numFailsCA ,"errors."))
  print(paste("calculateTDSurvivalTime returned",numFailsCB ,"errors."))
  print(paste("getTDTimeline returned",numFailsCC ,"errors."))
  print(paste("getTimeline returned",numFailsCD ,"errors."))
  print(paste("calculateSurvivalTime returned",numFailsCE ,"errors."))
  print(paste("getDrugPersistence returned",numFailsCF ,"errors."))
  print(paste("getFirstDrugIncidenceRate returned",numFailsCG ,"errors."))
  print(paste("constructSurvivalTimeline returned",numFailsCH ,"errors."))

  print(paste("sumAcrossAgeGroups", numFailsDA, "errors."))
  print(paste("checkCPRDRecord", numFailsDB, "errors."))
  print(paste("constructMCF", numFailsDC, "errors."))
  print(paste("saveCPRDDataframe", numFailsDD, "errors."))
  print(paste("saveCPRDDataframeAsText", numFailsDE, "errors."))
  print(paste("saveCPRDList", numFailsDF, "errors."))


  print("=====================TEST SUMMARY==================================================")
  print(paste("There were",numFails,"failures."))
}

#============CPRDDrugTrajectoryUtils TESTS=========================================

testCheckCPRDRecord <- function() {
  numFails <- 0
  print("TESTING: checkCPRDRecord")

  checkBool <- checkCPRDRecord(testTherapyDF, "therapy")
  if(isTRUE(checkBool)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}

  checkBool <- checkCPRDRecord(testClinicalDF, "clinical")
  if(isTRUE(checkBool)) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}

  checkBool <- checkCPRDRecord(testClinicalDF, "unknown")
  if(isFALSE(checkBool)) {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}

  testDF <- data.frame(Nothing=NA)
  checkBool <- checkCPRDRecord(testDF, "clinical")
  if(isFALSE(checkBool)) {print("4. TRUE")} else {print("4. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

testSumAcrossAgeGroups <- function() {
  numFails <- 0
  print("TESTING: sumAcrossAgeGroups and getAgeGroupByEvents")
  fileLocation <- "../RDrugTrajectory/product.txt"

  #specific drugs
  drugList <- unique(testTherapyDF$prodcode)
  requiredProds <- drugList[1:10]
  names(ageGenderDF) <- c("patid","age","gender")
  ageDF <- ageGenderDF
  ageGroupVector <- c(0,18,25,30,35,40,45,50,55,60)
  ageAtYear <- "2017"

  df <- getFirstDrugPrescription(testTherapyDF, idList=NULL, prodCodesVector=requiredProds, descriptionFile=fileLocation)
  if(is.list(df)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(length(df)==4) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}
  idList <- as.list(df[[1]][1:2])
  eventdateList <- as.list(df[[2]][1:2])

  ageListResult <- getAgeGroupByEvents(idList, eventdateList, ageDF, ageGroupVector, ageAtYear)
  if(is.list(ageListResult)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1 }
  if(length(ageListResult)==2) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1 }
  ageVectorResult <- sumAcrossAgeGroups(ageListResult)
  if(ageVectorResult[2] == 149) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1 }

  ageListResult <- getAgeGroupByEvents(idList, eventdateList, ageDF, 0, ageAtYear)
  if((ageListResult)[[1]][1]==1498) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1 }

  return(numFails)
}

#FINISHED
testConstructMedHistory <- function() {
  numFails <- 0
  print("TESTING: constructMedicalHistory")
  referralDF <- testClinicalDF
  df <- data.frame()
  medHistory <- constructMedicalHistory(testClinicalDF,referralDF,testTherapyDF)
  if(nrow(medHistory) > 0) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1 }
  if(sum(unique(medHistory$codetype) %in% c("c","r","t"))==3) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(is.null(constructMedicalHistory())) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}
  if(is.null(constructMedicalHistory(df,df,df))) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}
  if(is.null(constructMedicalHistory(testClinicalDF,referralDF,df))) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}
  if(is.null(constructMedicalHistory(testClinicalDF,df,testTherapyDF))) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}
  if(is.null(constructMedicalHistory(df,referralDF,testTherapyDF))) {print("7. PASSED")} else {print("7. FAILED"); numFails <- numFails + 1}
  if(is.null(constructMedicalHistory(testTherapyDF))) {print("8. PASSED")} else {print("8. FAILED"); numFails <- numFails + 1}
  if(is.null(constructMedicalHistory(testClinicalDF, testTherapyDF))) {print("9. PASSED")} else {print("9. FAILED"); numFails <- numFails + 1}
  if(is.null(constructMedicalHistory(testClinicalDF, referralDF, testClinicalDF))) {print("10. PASSED")} else {print("10. FAILED"); numFails <- numFails + 1}
  if(sum(colnames(medHistory) %in% c("patid","eventdate","code","codetype")) == 4 ) {print("11. PASSED")} else {print("11. FAILED"); numFails <- numFails + 1}
  if(isTRUE(is.data.frame(medHistory))) {print("12. PASSED")} else {print("12. FAILED"); numFails <- numFails + 1}
  return(numFails)
}

#FINISHED
testErrorChecker <- function() {
  numFails <- 0
  print("TESTING: testErrorChecker")
  df <- data.frame()
  intVec <- integer()
  list <- list()
  somethingNULL <- NULL
  firstDrugObjectEmpty <- list()
  class(firstDrugObjectEmpty) <- "FirstDrugObject"
  drugList <- list(unique(testTherapyDF$prodcode))
  drugVector <- unique(testTherapyDF$prodcode)

  x <- c(1,2,3)
  y <- c(4,5,6)
  patidList <- as.list("x"=x, "y"=y)
  eventList <- patidList
  frequencyDF <- data.frame(stuff="stuff")
  firstDrugObject <- list(patidList, eventList, frequencyDF)
  class(firstDrugObject) <- "FirstDrugObject"

  if(isFALSE(errorChecker(df))) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(isFALSE(errorChecker(intVec))) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(isFALSE(errorChecker(list))) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}
  if(isFALSE(errorChecker(somethingNULL))) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}
  if(isFALSE(errorChecker(firstDrugObjectEmpty))) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}
  if(isTRUE(errorChecker(testClinicalDF))) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}
  if(isTRUE(errorChecker(drugList))) {print("7. PASSED")} else {print("7. FAILED"); numFails <- numFails + 1}
  if(isTRUE(errorChecker(drugVector))) {print("8. PASSED")} else {print("8. FAILED"); numFails <- numFails + 1}
  if(isTRUE(errorChecker(firstDrugObject))) {print("9. PASSED")} else {print("9. FAILED"); numFails <- numFails + 1}

  if(isTRUE(errorChecker(as.integer(x)))) {print("10. PASSED")} else {print("10. FAILED"); numFails <- numFails + 1}
  if(isTRUE(errorChecker(as.numeric(x)))) {print("11. PASSED")} else {print("11. FAILED"); numFails <- numFails + 1}

  startDateChar <- "2000-01-01"
  startDate <- as.Date(startDateChar)
  if(isTRUE(errorChecker(startDate))) {print("12. PASSED")} else {print("12. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#FINISHED
testGetUniquePatidList <- function() {
  numFails <- 0
  print("TESTING: getUniquePatidList")
  idList <- getUniquePatidList(testClinicalDF)
  if(!is.null(idList)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(length(idList)>0) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(isTRUE(length(idList) == length(unique(testClinicalDF$patid)))) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}
  return(numFails)
}

#FINISHED
testGetParallelDF <- function() {
  numFails <- 0
  print("TESTING: getParallelDF")
  n <- nrow(testTherapyDF)

  listDF <- getParallelDF(testTherapyDF, 1)
  if(isTRUE( length(listDF)==1 )) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(isTRUE( nrow(do.call(rbind,listDF))==n )) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}
  listDF <- getParallelDF(testTherapyDF, 2)
  if(isTRUE( length(listDF)==2 )) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}
  if(isTRUE( nrow(do.call(rbind,listDF))==n )) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}
  listDF <- getParallelDF(testTherapyDF, 3)
  if(isTRUE( length(listDF)==3 )) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}
  if(isTRUE( nrow(do.call(rbind,listDF))==n )) {print("7. PASSED")} else {print("7. FAILED"); numFails <- numFails + 1}
  listDF <- getParallelDF(testTherapyDF, 4)
  if(isTRUE( length(listDF)==4 )) {print("8. PASSED")} else {print("8. FAILED"); numFails <- numFails + 1}
  if(isTRUE( nrow(do.call(rbind,listDF))==n )) {print("9. PASSED")} else {print("9. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

testConvertCodesToDescriptions <- function() {
  numFails <- 0
  print("TESTING: testConvertCodesToDescriptions")

  return(numFails)
}

#============CPRDDrugTrajectory TESTS=========================================

testFilterPatientsByDrugMatchingDisease <- function() {
  numFails <- 0
  print("TESTING: filterPatientsByDrugMatchingDisease")
  fileLocation <- "../RDrugTrajectory/product.txt"
  requiredProds <- unique(testTherapyDF$prodcode)
  allMedCodes <- unique(testClinicalDF$medcode)

  headacheCodes <- allMedCodes[1:10]
  mockComorbidityCodes <- allMedCodes[11:52]

  fdo <- getFirstDrugPrescription(testTherapyDF, idList=NULL, prodCodesVector=requiredProds, descriptionFile=fileLocation)
  filteredTherapyDF <- filterPatientsByDrugMatchingDisease(testClinicalDF,NULL,testTherapyDF, medcodeList = headacheCodes, drugcodeList = requiredProds, severity = 1)
  if(is.data.frame(filteredTherapyDF)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(nrow(filteredTherapyDF)==1644) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

testMatchDrugWithDisease <- function() {
  numFails <- 0
  print("TESTING: matchDrugWithDisease")

  prodcodes <- unique(testTherapyDF$prodcode)
  medcodeList <- unique(testClinicalDF$medcode)
  returnIDList <- matchDrugWithDisease(testClinicalDF, referralDF = NULL, testTherapyDF, patidList=NULL, medcodeList=medcodeList, drugcodeList=prodcodes, severity=1, dateDF=NULL)
  if(length(getUniquePatidList(testTherapyDF)) == length(returnIDList)) {
    print("1. PASSED")
  } else {
    print("1. FAILED")
    numFails <- numFails + 1
  }

  headacheCodes <- medcodeList[1:10]
  comorbidityCodes <- medcodeList[11:52]
  returnIDList <- matchDrugWithDisease(testClinicalDF, referralDF = NULL, testTherapyDF, patidList=NULL, medcodeList=headacheCodes, drugcodeList=prodcodes, severity=2, dateDF=NULL)
  if(1093 == length(returnIDList)) { print("2. PASSED") } else { print("2. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#FINISHED
testCombinePrescriptionFrequencies <- function() {
  numFails <- 0
  print("TESTING: combinePrescriptionFrequencies")
  fileLocation <- "../RDrugTrajectory/product.txt"

  #leave topiramate
  combineList <- list(Amitriptyline=c(83,49,1888),
                      Propranolol=c(707,297,769),
                      Lisinopril=c(78,65),
                      Venlafaxine=c(470)
  )

  #specific drugs
  drugList <- unique(testTherapyDF$prodcode)
  requiredProds <- drugList[1:10]

    fdo <- getFirstDrugPrescription(testTherapyDF, idList=NULL, prodCodesVector=requiredProds, descriptionFile=fileLocation)
    combinedDF <- combinePrescriptionFrequencies(fdo[[3]], combineList)
    if(is.data.frame(combinedDF)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
    if(nrow(combinedDF)==4) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
    if(nrow(combinedDF)==4) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}
    if(combinedDF[combinedDF$description=="Amitriptyline",]$Frequency==1937 ) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}

    combinedDF <- combinePrescriptionFrequencies(fdo[[3]], NULL)
    if(is.null(combinedDF)) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}
    combinedDF <- combinePrescriptionFrequencies(NULL, combineList)
    if(is.null(combinedDF)) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}

    #no codes match
    combineList <- list(Amitriptyline=c(1,2,3))
    combinedDF <- combinePrescriptionFrequencies(fdo[[3]], combineList)
    if(is.null(combinedDF)) {print("7. PASSED")} else {print("7. FAILED"); numFails <- numFails + 1}

    #row entries
    combineList <- list(Amitriptyline=c(1,6,7),
                        Propranolol=c(2,3,4))
    combinedDF <- combinePrescriptionFrequencies(fdo[[3]], combineList, TRUE)
    if(combinedDF[combinedDF$description=="Amitriptyline",]$Frequency==1937 ) {print("8. PASSED")} else {print("8. FAILED"); numFails <- numFails + 1}

    #incorrect row entries
    combineList <- list(Amitriptyline=c(0,1,6,7),
                        Propranolol=c(2,3,4))
    combinedDF <- combinePrescriptionFrequencies(fdo[[3]], combineList, TRUE)
    if(is.null(combinedDF)) {print("9. PASSED")} else {print("9. FAILED"); numFails <- numFails + 1}

    combineList <- list(Amitriptyline=c(1,6,7),
                        Propranolol=c(2,3,4,99))
    combinedDF <- combinePrescriptionFrequencies(fdo[[3]], combineList, TRUE)
    if(is.null(combinedDF)) {print("10. PASSED")} else {print("10. FAILED"); numFails <- numFails + 1}

    combineWithMissingList <- list(Amitriptyline=c(83,49,1888,1234567),
                                   Propranolol=c(707,297,769),
                                   Lisinopril=c(78,65),
                                   Imaginary=c(1,2,3),
                                   Venlafaxine=c(470)
    )

    #only some codes match
    combinedDF <- combinePrescriptionFrequencies(fdo[[3]], combineWithMissingList)
    if(is.data.frame(combinedDF)) {print("11. PASSED")} else {print("11. FAILED"); numFails <- numFails + 1}
    if(nrow(combinedDF)==4) {print("12. PASSED")} else {print("12. FAILED"); numFails <- numFails + 1}


  return(numFails)
}

#FINISHED
testGetBurnInPatients <- function() {
  numFails <- 0
  print("TESTING: getBurnInPatients")
  drugVector <- c(83,49,297,1888,940,5)

  df <- data.frame()
  patientList <- getBurnInPatients(testTherapyDF, drugVector, 172)
  if(isTRUE(is.list(patientList))) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(length(patientList)==426) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
  x<-getBurnInPatients(df, drugVector, 172)
  if(is.null(x)) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}
  drugEmptyVector <- c(0000000)
  patientList <- getBurnInPatients(testTherapyDF, drugEmptyVector, 172)
  if(is.null(patientList)) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}
  if(is.null(getBurnInPatients(testTherapyDF, drugVector, 0))) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}
  if(is.null(getBurnInPatients(testTherapyDF, drugVector, -10))) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#FINISHED
testGetPatientsWithProdCode <- function() {
  numFails <- 0
  print("TESTING: getPatientsWithProdCode")

  drugList <- unique(testTherapyDF$prodcode)
  requiredProds <- drugList[1:10]

  df <- getPatientsWithProdCode(testTherapyDF, requiredProds, removeExcessDrugs=TRUE, returnIDList=FALSE)
  if(isTRUE(nrow(df) == 57686)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(isTRUE(length(getUniquePatidList(df))==3453)) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}
  df <- getPatientsWithProdCode(testTherapyDF, requiredProds, removeExcessDrugs=FALSE, returnIDList=FALSE)
  if(isTRUE(nrow(df)==78827)) {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}
  df <- getPatientsWithProdCode(testTherapyDF, requiredProds, removeExcessDrugs=TRUE, returnIDList=TRUE)
  if(isTRUE(is.list(df))) {print("4. TRUE")} else {print("4. FAILED"); numFails <- numFails + 1}
  if(isTRUE(length(df)==3453)) {print("5. TRUE")} else {print("5. FAILED"); numFails <- numFails + 1}
  df <- getPatientsWithProdCode(data.frame(), requiredProds, removeExcessDrugs=TRUE, returnIDList=TRUE)
  if(isTRUE(is.null(df))) {print("6. TRUE")} else {print("6. FAILED"); numFails <- numFails + 1}
  df <- getPatientsWithProdCode(testTherapyDF, as.integer(), removeExcessDrugs=TRUE, returnIDList=TRUE)
  if(isTRUE(is.null(df))) {print("7. TRUE")} else {print("7. FAILED"); numFails <- numFails + 1}

  #any others?


  return(numFails)
}

#FINISHED
testGetIMDOfPatients <- function() {
  numFails <- 0
  print("TESTING: getIMDOfPatients")

  #everyone
  idList <- getUniquePatidList(testTherapyDF)
  df <- getIMDOfPatients(idList, imdDF)
  if(is.data.frame(df)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==2123) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(sum(unique(df$score) %in% c(1,2,3,4,5))==5) {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}

  df <- getIMDOfPatients(idList, imdDF, 1)
  if(is.data.frame(df)) {print("4. TRUE")} else {print("4. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==465) {print("5. TRUE")} else {print("5. FAILED"); numFails <- numFails + 1}
  if(sum(unique(df$score) %in% c(1,2,3,4,5))==1) {print("6. TRUE")} else {print("6. FAILED"); numFails <- numFails + 1}

  df <- getIMDOfPatients(idList, imdDF, 2)
  if(is.data.frame(df)) {print("7. TRUE")} else {print("7. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==402) {print("8. TRUE")} else {print("8. FAILED"); numFails <- numFails + 1}
  if(sum(unique(df$score) %in% c(1,2,3,4,5))==1) {print("9. TRUE")} else {print("9. FAILED"); numFails <- numFails + 1}

  df <- getIMDOfPatients(idList, imdDF, 3)
  if(is.data.frame(df)) {print("10. TRUE")} else {print("10. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==450) {print("11. TRUE")} else {print("11. FAILED"); numFails <- numFails + 1}
  if(sum(unique(df$score) %in% c(1,2,3,4,5))==1) {print("12. TRUE")} else {print("12. FAILED"); numFails <- numFails + 1}

  df <- getIMDOfPatients(idList, imdDF, 4)
  if(is.data.frame(df)) {print("13. TRUE")} else {print("13. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==413) {print("14. TRUE")} else {print("14. FAILED"); numFails <- numFails + 1}
  if(sum(unique(df$score) %in% c(1,2,3,4,5))==1) {print("15. TRUE")} else {print("15. FAILED"); numFails <- numFails + 1}

  df <- getIMDOfPatients(idList, imdDF, 5)
  if(is.data.frame(df)) {print("16. TRUE")} else {print("16. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==393) {print("17. TRUE")} else {print("17. FAILED"); numFails <- numFails + 1}
  if(sum(unique(df$score) %in% c(1,2,3,4,5))==1) {print("18. TRUE")} else {print("18. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#FINISHED
testGetGenderOfPatients <- function() {
  numFails <- 0
  print("TESTING: getGenderOfPatients")

  justMaleDF <- subset(ageGenderDF, ageGenderDF$gender == 1)
  maleIDList <- getUniquePatidList(justMaleDF)
  justFemaleDF <- subset(ageGenderDF, ageGenderDF$gender == 2)
  femaleIDList <- getUniquePatidList(justFemaleDF)

  #everyone
  idList <- getUniquePatidList(testTherapyDF)
  df <- getGenderOfPatients(idList, ageGenderDF)
  if(is.data.frame(df)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==length(idList)) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(sum(unique(df$gender) %in% c(1,2))==2) {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}

  #all subset idList
  subsetIDList <- idList[1:1000]
  df <- getGenderOfPatients(subsetIDList, ageGenderDF)
  if(is.data.frame(df)) {print("4. TRUE")} else {print("4. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==1000) {print("5. TRUE")} else {print("5. FAILED"); numFails <- numFails + 1}
  if(sum(unique(df$gender) %in% c(1,2))==2) {print("6. TRUE")} else {print("6. FAILED"); numFails <- numFails + 1}

  #all males in subset idlist
  df <- getGenderOfPatients(subsetIDList, ageGenderDF, 1)
  if(is.data.frame(df)) {print("7. TRUE")} else {print("7. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==329) {print("8. TRUE")} else {print("8. FAILED"); numFails <- numFails + 1}
  if(sum(unique(df$gender) %in% c(1))==1) {print("9. TRUE")} else {print("9. FAILED"); numFails <- numFails + 1}

  #all female in subset idlist
  df <- getGenderOfPatients(subsetIDList, ageGenderDF, 2)
  if(is.data.frame(df)) {print("10. TRUE")} else {print("10. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==671) {print("11. TRUE")} else {print("11. FAILED"); numFails <- numFails + 1}
  if(sum(unique(df$gender) %in% c(2))==1) {print("12. TRUE")} else {print("12. FAILED"); numFails <- numFails + 1}

  #request females in male
  df <- getGenderOfPatients(maleIDList, ageGenderDF, 2)
  if(is.null(df)) {print("13. TRUE")} else {print("13. FAILED"); numFails <- numFails + 1}

  #request males in female
  df <- getGenderOfPatients(femaleIDList, ageGenderDF, 1)
  if(is.null(df)) {print("14. TRUE")} else {print("14. FAILED"); numFails <- numFails + 1}

  df <- getGenderOfPatients(femaleIDList, NULL)
  if(is.null(df)) {print("15. TRUE")} else {print("15. FAILED"); numFails <- numFails + 1}

  df <- getGenderOfPatients(NULL, ageGenderDF)
  if(is.null(df)) {print("16. TRUE")} else {print("16. FAILED"); numFails <- numFails + 1}

  df <- getGenderOfPatients(NULL,NULL)
  if(is.null(df)) {print("16. TRUE")} else {print("16. FAILED"); numFails <- numFails + 1}

  df <- getGenderOfPatients(subsetIDList, ageGenderDF, c(4,5))
  if(is.null(df)) {print("17. TRUE")} else {print("17. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#EMPTY
testGetMultiPrescriptionSameDayPatients <- function() {
  numFails <- 0
  print("TESTING: getMultiPrescriptionSameDayPatients")

  df <- getMultiPrescriptionSameDayPatients(testTherapyDF)
  if(nrow(df) == 39502) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
  df <- getMultiPrescriptionSameDayPatients(testTherapyDF,
                                            removePatientsWithoutDrugs=TRUE)
  if(is.null(df)) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}

  prodcodesVector <- unique(testTherapyDF$prodcode[1:20])
  medHistoryDF <- constructMedicalHistory(NULL,
                                         NULL,
                                        testTherapyDF)
  df <- getMultiPrescriptionSameDayPatients(medHistoryDF,
                                            prodcodesVector,
                                            removePatientsWithoutDrugs=TRUE)
  if(nrow(df)==19791) {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}

  df <- getMultiPrescriptionSameDayPatients(medHistoryDF,
                                            prodcodesVector[4],
                                            removePatientsWithoutDrugs=TRUE)
  if(nrow(df)==3975) {print("4. TRUE")} else {print("4. FAILED"); numFails <- numFails + 1}


  return(numFails)
}

testGetAgeGroupByEvents <- function() {
  numFails <- 0
  print("TESTING: getAgeGroupByEvents")
  fileLocation <- "../RDrugTrajectory/product.txt"

  #specific drugs
  drugList <- unique(testTherapyDF$prodcode)
  requiredProds <- drugList[1:10]
  names(ageGenderDF) <- c("patid","age","gender")
  ageDF <- ageGenderDF
  ageGroupVector <- c(18,25,30,35,40,45,50,55,60)
  ageAtYear <- "2017"


    df <- getFirstDrugPrescription(testTherapyDF, idList=NULL, prodCodesVector=requiredProds, descriptionFile=fileLocation)
    if(is.list(df)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
    idList <- as.list(df[[1]][1:2])
    eventdateList <- as.list(df[[2]][1:2])

    ageListResult1 <- getAgeGroupByEvents(idList, eventdateList, ageDF, ageGroupVector, ageAtYear)
    if(isFALSE(is.null(ageListResult1))) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}
    ageListResult2 <- getAgeGroupByEvents(idList, eventdateList, ageDF, ageGroupVector, "2")
    if(is.null(ageListResult2)) {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}

    ageGroupMixedUpVector <- c(18,30,25,35,40,45,50,55,60)
    ageListResult3 <- getAgeGroupByEvents(idList, eventdateList, ageDF, ageGroupMixedUpVector, ageAtYear)
    if(!is.null(ageListResult3)) {print("4. TRUE")} else {print("4. FAILED"); numFails <- numFails + 1}
    if(sum(useful::compare.list(ageListResult1,ageListResult3))==2 ) {print("5. TRUE")} else {print("5. FAILED"); numFails <- numFails + 1}

    idListEmpty <- list()
    ageListResult <- getAgeGroupByEvents(idListEmpty, eventdateList, ageDF, ageGroupVector, ageAtYear)
    if(isTRUE(is.null(ageListResult))) {print("6. TRUE")} else {print("6. FAILED"); numFails <- numFails + 1}

    eventdateListEmpty <- list()
    ageListResult <- getAgeGroupByEvents(idList, eventdateListEmpty, ageDF, ageGroupVector, ageAtYear)
    if(isTRUE(is.null(ageListResult))) {print("7. TRUE")} else {print("7. FAILED"); numFails <- numFails + 1}


  return(numFails)

}


testGetFirstDrugIncidenceRate <- function() {
  numFails <- 0
  print("TESTING: getFirstDrugIncidence")
  fileLocation <- "../RDrugTrajectory/product.txt"

  drugList <- unique(testTherapyDF$prodcode)
  requiredProds <- drugList[1:10]

  firstDrugObject <- getFirstDrugPrescription(testTherapyDF, idList=NULL, prodCodesVector=requiredProds, descriptionFile=fileLocation)
  enrollmentDate <- as.Date("2000-01-01")
  studyEndDate <- as.Date("2016-12-31")
  medhistoryDF <- constructMedicalHistory(testClinicalDF, NULL, testTherapyDF)
  patidList <- unlist(firstDrugObject$patidList)
  resultMatrix <- getFirstDrugIncidenceRate(firstDrugObject,
                                            medhistoryDF,
                                            enrollmentDate,
                                            studyEndDate)

  if(is.matrix(resultMatrix)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(dim(resultMatrix)[1]) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(dim(resultMatrix)[2]) {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#FINISHED
testGetFirstDrugPrescription <- function() {
  numFails <- 0
  print("TESTING: getFirstDrugPrescription")
  fileLocation <- "../RDrugTrajectory/product.txt"

  #specific drugs
  drugList <- unique(testTherapyDF$prodcode)
  requiredProds <- drugList[1:10]

    df <- getFirstDrugPrescription(testTherapyDF, idList=NULL, prodCodesVector=requiredProds, descriptionFile=fileLocation)
    if(is.list(df)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
    df1 <- df[[1]]
    df2 <- df[[2]]
    df3 <- df[[3]]
    df4 <- df[[4]]
    if(is.list(df1)) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}
    if(is.list(df2)) {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}
    if(is.data.frame(df3)) {print("4. TRUE")} else {print("4. FAILED"); numFails <- numFails + 1}
    if(is.data.frame(df4)) {print("5. TRUE")} else {print("5. FAILED"); numFails <- numFails + 1}
    if(length(df1)==10) {print("6. TRUE")} else {print("6. FAILED"); numFails <- numFails + 1}
    if(length(df2)==10) {print("7. TRUE")} else {print("7. FAILED"); numFails <- numFails + 1}
    if(nrow(df3)==10) {print("8. TRUE")} else {print("8. FAILED"); numFails <- numFails + 1}
    if(nrow(df4)==9) {print("9. TRUE")} else {print("9. FAILED"); numFails <- numFails + 1}

    #check when no IDs or Prodcode have been specified and the description comes from a file
    df <- getFirstDrugPrescription(testTherapyDF, idList=NULL, prodCodesVector=NULL, descriptionFile=fileLocation)
    if(is.list(df)) {print("10. TRUE")} else {print("10. FAILED"); numFails <- numFails + 1}
    df1 <- df[[1]]
    df2 <- df[[2]]
    df3 <- df[[3]]
    df4 <- df[[4]]
    if(is.list(df1)) {print("11. TRUE")} else {print("11. FAILED"); numFails <- numFails + 1}
    if(is.list(df2)) {print("12. TRUE")} else {print("12. FAILED"); numFails <- numFails + 1}
    if(is.data.frame(df3)) {print("13. TRUE")} else {print("13. FAILED"); numFails <- numFails + 1}
    if(length(df1)==19) {print("14. TRUE")} else {print("14. FAILED"); numFails <- numFails + 1}
    if(length(df2)==19) {print("15. TRUE")} else {print("15. FAILED"); numFails <- numFails + 1}
    if(nrow(df3)==19) {print("16. TRUE")} else {print("16. FAILED"); numFails <- numFails + 1}
    if(is.null(df4)) {print("17. TRUE")} else {print("17. FAILED"); numFails <- numFails + 1}

    #check for duplicate prodcodes
    duplicateProds <- requiredProds
    duplicateProds[2] <-83
    df <- getFirstDrugPrescription(testTherapyDF, idList=NULL, prodCodesVector=duplicateProds, descriptionFile=fileLocation)
    if(is.null(df)) {print("18. TRUE")} else {print("18. FAILED"); numFails <- numFails + 1}

    #check specific patids
    idList <- getUniquePatidList(testTherapyDF)
    specific <- idList[1:50]
    df <- getFirstDrugPrescription(testTherapyDF, idList=specific, prodCodesVector=NULL, descriptionFile=fileLocation)
    if(is.list(df)) {print("19. TRUE")} else {print("19. FAILED"); numFails <- numFails + 1}
    df1 <- df[[1]]
    df2 <- df[[2]]
    df3 <- df[[3]]
    df4 <- df[[4]]
    if(is.list(df1)) {print("20. TRUE")} else {print("20. FAILED"); numFails <- numFails + 1}
    if(is.list(df2)) {print("21. TRUE")} else {print("21. FAILED"); numFails <- numFails + 1}
    if(is.data.frame(df3)) {print("22. TRUE")} else {print("22. FAILED"); numFails <- numFails + 1}
    if(length(df1)==12) {print("23. TRUE")} else {print("23. FAILED"); numFails <- numFails + 1}
    if(length(df2)==12) {print("24. TRUE")} else {print("24. FAILED"); numFails <- numFails + 1}
    if(nrow(df3)==12) {print("25. TRUE")} else {print("25. FAILED"); numFails <- numFails + 1}
    if(is.null(df4)) {print("26. TRUE")} else {print("26. FAILED"); numFails <- numFails + 1}
    if(isTRUE(df3[df3$code==83,]$Frequency==15)) {print("27. TRUE")} else {print("27. FAILED"); numFails <- numFails + 1}

    #check for duplicate patid
    idList[2] <- idList[1]
    df <- getFirstDrugPrescription(testTherapyDF, idList=idList, prodCodesVector=requiredProds, descriptionFile=fileLocation)
    if(is.null(df)) {print("28. TRUE")} else {print("28. FAILED"); numFails <- numFails + 1}


  #check NA descriptions
  df <- getFirstDrugPrescription(testTherapyDF, idList=NULL, prodCodesVector=requiredProds, descriptionFile=NULL)
  df1 <- df[[1]]
  df2 <- df[[2]]
  df3 <- df[[3]]
  df4 <- df[[4]]
  if(is.list(df1)) {print("29. TRUE")} else {print("29. FAILED"); numFails <- numFails + 1}
  if(is.list(df2)) {print("30. TRUE")} else {print("30. FAILED"); numFails <- numFails + 1}
  if(is.data.frame(df3)) {print("31. TRUE")} else {print("31. FAILED"); numFails <- numFails + 1}
  if(is.data.frame(df4)) {print("32. TRUE")} else {print("32. FAILED"); numFails <- numFails + 1}
  if(length(df1)==10) {print("33. TRUE")} else {print("33. FAILED"); numFails <- numFails + 1}
  if(length(df2)==10) {print("34. TRUE")} else {print("34. FAILED"); numFails <- numFails + 1}
  if(nrow(df3)==10) {print("35. TRUE")} else {print("35. FAILED"); numFails <- numFails + 1}
  if(nrow(df4)==9) {print("36. TRUE")} else {print("36. FAILED"); numFails <- numFails + 1}
  if(is.na(unique(df3$description))) {print("37. TRUE")} else {print("37. FAILED"); numFails <- numFails + 1}
  if(is.na(unique(df4$description))) {print("38. TRUE")} else {print("38. FAILED"); numFails <- numFails + 1}

  smallDF <- subset(testTherapyDF, testTherapyDF$prodcode %in% drugList[1:7])
  #check structure descriptionns
  structureList <- list(DrugA=drugList[[1]],
                        DrugB=drugList[[2]],
                        DrugC=drugList[[3]],
                        DrugD=drugList[[4]],
                        DrugE=drugList[[5]],
                        DrugF=drugList[[6]],
                        DrugG=drugList[[7]],
                        DrugH=drugList[[8]],
                        DrugI=drugList[[9]])
  df <- getFirstDrugPrescription(smallDF, idList=NULL, prodCodesVector=drugList[1:4], descriptionFile=structureList)
  df1 <- df[[1]]
  df2 <- df[[2]]
  df3 <- df[[3]]
  df4 <- df[[4]]
  if(is.list(df1)) {print("39. TRUE")} else {print("39. FAILED"); numFails <- numFails + 1}
  if(is.list(df2)) {print("40. TRUE")} else {print("40. FAILED"); numFails <- numFails + 1}
  if(is.data.frame(df3)) {print("41. TRUE")} else {print("41. FAILED"); numFails <- numFails + 1}
  if(is.data.frame(df4)) {print("42. TRUE")} else {print("42. FAILED"); numFails <- numFails + 1}
  testNames <- c("DrugA","DrugB","DrugC","DrugD")
  if(sum(testNames %in% df3$description) == 4) {print("43. TRUE")} else {print("43. FAILED"); numFails <- numFails + 1}
  testNames <- c("DrugE","DrugF","DrugG")
  if(sum(testNames %in% df4$description) == 3) {print("44. TRUE")} else {print("44. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#FINISHED
testGetEventdateSummaryByPatient <- function() {
  numFails <- 0
  print("TESTING: getEventdateSummaryByPatient")
  idList <- getUniquePatidList(testTherapyDF)
  resultList <- getEventdateSummaryByPatient(testTherapyDF[testTherapyDF$patid==idList[[1]],] )
  if(is.list(resultList)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(is.vector(resultList[[1]])) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(is.data.frame(resultList[[2]])) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}

  timeList <- resultList[[1]]
  summaryEventsDF <- resultList[[2]]
  numEvents <- summaryEventsDF$numberOfEvents
  if(numEvents == 7) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}
  if((numEvents-1) == length(timeList)) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}
  if(timeList[1]==336) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}

  #test what happens if the prodcodes work
  prodcodes <- c(83,707)
  resultList <- getEventdateSummaryByPatient(testTherapyDF[testTherapyDF$patid==idList[[1]],], prodcodes)
  prodcodes <- c(83)
  resultList <- getEventdateSummaryByPatient(testTherapyDF[testTherapyDF$patid==idList[[1]],], prodcodes)
  prodcodes <- c(83707,83)
  resultList <- getEventdateSummaryByPatient(testTherapyDF[testTherapyDF$patid==idList[[1]],], prodcodes)
  prodcodes <- c(8,70)
  resultList <- getEventdateSummaryByPatient(testTherapyDF[testTherapyDF$patid==idList[[1]],], prodcodes)


  return(numFails)
}

#NOT FINISHED
testGetPopulationDrugSummary <- function() {
  numFails <- 0
  print("TESTING: getPopulationDrugSummary")
  resultList <- getPopulationDrugSummary(testTherapyDF, prodCodesVector=NULL)
  if(is.list(resultList)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(class(resultList) == "PopulationEventdateSummary") {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}

  summaryDF <- resultList$SummaryDF
  if(is.data.frame(summaryDF)) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}
  if(nrow(summaryDF) == 3838) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}
  #I know that some records were broken
  if(sum(unique(stats::complete.cases(summaryDF)))==1) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}

  timeSeries <- resultList$TimeSeriesList
  if(is.list(timeSeries)) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}
  if(length(timeSeries)==3838) {print("7. PASSED")} else {print("7. FAILED"); numFails <- numFails + 1}


  return(numFails)
}

#NOT FINISHED
testGetSettingFromFirstPrescription <- function() {
  numFails <- 0
  print("TESTING: getSettingFromFirstPrescription")

  return(numFails)
}

#NOT FINISHED
testGetAnyDrugPrescriptionByYear <- function() {
  numFails <- 0
  print("TESTING: getAnyDrugPrescriptionByYear")

  return(numFails)
}

#NOT FINISHED
testGetFirstDrugPrescriptionByYear <- function() {
  numFails <- 0
  print("TESTING: getFirstDrugPrescriptionByYear")

  fileLocation <- "../RDrugTrajectory/product.txt"

  #specific drugs
  drugList <- unique(testTherapyDF$prodcode)
  requiredProds <- drugList[1:10]

  df <- getFirstDrugPrescription(testTherapyDF, idList=NULL, prodCodesVector=requiredProds, descriptionFile=fileLocation)
  yearList <- getFirstDrugPrescriptionByYear(df, "2001")
  if(is.list(yearList)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(length(yearList)==1) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}

  yearList <- getFirstDrugPrescriptionByYear(df, c("2001","2002","2003"))
  if(length(yearList)==3) {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#NOT FINISHED
testRemovePatientsByDuration <- function() {
  numFails <- 0
  print("TESTING: removePatientsByDuration")

  return(numFails)
}

#NOT FINISHED
testGetPatientsWithDiseaseButNoDrug <- function() {
  numFails <- 0
  print("TESTING: getPatientsWithDiseaseButNoDrug")

  return(numFails)
}

#NOT FINISHED
testGetPatientsWithFirstDrugWithNoDisease <- function() {
  numFails <- 0
  print("TESTING: getPatientsWithFirstDrugWithNoDisease")

  returnTherapyDF <- getPatientsWithFirstDrugWithDisease(
    clinicalDF=testClinicalDF,
    therapyDF=testTherapyDF,
    medCodesVector=NULL,
    drugCodesVector = NULL,
    FALSE, c(0,0))
  if(!is.null(returnTherapyDF)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}

  df <- getPatientsWithFirstDrugWithNoDisease(
      therapyDF=testTherapyDF,
      therapyOfDrugOnDiseaseDF=returnTherapyDF)

  if(!is.null(df)) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(nrow(df)==83924) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

testGetPatientsWithFirstDrugWithDisease <- function() {
  numFails <- 0
  print("TESTING: getPatientsWithFirstDrugWithDisease")

  drugList <- unique(testTherapyDF$prodcode)
  requiredProds <- drugList[1:18]
  conditions <- unique(testClinicalDF$medcode)
  subConditions <- conditions[1:30]

  #The first four scenarios should be the same.
  returnTherapyDF <- getPatientsWithFirstDrugWithDisease(clinicalDF=testClinicalDF, therapyDF=testTherapyDF, medCodesVector=NULL, drugCodesVector = NULL, FALSE, c(0,0))
  if(is.data.frame(returnTherapyDF)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(isTRUE(length(getUniquePatidList(returnTherapyDF))==667)) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}

  returnTherapyDF <- getPatientsWithFirstDrugWithDisease(clinicalDF=testClinicalDF, therapyDF=testTherapyDF, medCodesVector=NULL, drugCodesVector = drugList, FALSE, c(0,0))
  if(is.data.frame(returnTherapyDF)) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}
  if(isTRUE(length(getUniquePatidList(returnTherapyDF))==667)) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}
  if(sum(unique(returnTherapyDF$prodcode %in% drugList))==1) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}

  returnTherapyDF <- getPatientsWithFirstDrugWithDisease(clinicalDF=testClinicalDF, therapyDF=testTherapyDF, medCodesVector=conditions, drugCodesVector = NULL, FALSE, c(0,0))
  if(is.data.frame(returnTherapyDF)) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}
  if(isTRUE(length(getUniquePatidList(returnTherapyDF))==667)) {print("7. PASSED")} else {print("7. FAILED"); numFails <- numFails + 1}

  returnTherapyDF <- getPatientsWithFirstDrugWithDisease(clinicalDF=testClinicalDF, therapyDF=testTherapyDF, medCodesVector=conditions, drugCodesVector = drugList, FALSE, c(0,0))
  if(is.data.frame(returnTherapyDF)) {print("8. PASSED")} else {print("8. FAILED"); numFails <- numFails + 1}
  if(isTRUE(length(getUniquePatidList(returnTherapyDF))==667)) {print("9. PASSED")} else {print("9. FAILED"); numFails <- numFails + 1}
  if(sum(unique(returnTherapyDF$prodcode %in% drugList))==1) {print("10. PASSED")} else {print("10. FAILED"); numFails <- numFails + 1}


  returnTherapyDF <- getPatientsWithFirstDrugWithDisease(clinicalDF=testClinicalDF, therapyDF=testTherapyDF, medCodesVector=NULL, drugCodesVector = requiredProds, FALSE, c(0,0))
  if(is.data.frame(returnTherapyDF)) {print("11. PASSED")} else {print("11. FAILED"); numFails <- numFails + 1}
  if(isTRUE(length(getUniquePatidList(returnTherapyDF))==665)) {print("12. PASSED")} else {print("12. FAILED"); numFails <- numFails + 1}

  returnTherapyDF <- getPatientsWithFirstDrugWithDisease(clinicalDF=testClinicalDF, therapyDF=testTherapyDF, medCodesVector=subConditions, drugCodesVector = requiredProds, FALSE, c(0,0))
  if(is.data.frame(returnTherapyDF)) {print("13. PASSED")} else {print("13. FAILED"); numFails <- numFails + 1}
  if(isTRUE(length(getUniquePatidList(returnTherapyDF))==662)) {print("14. PASSED")} else {print("14. FAILED"); numFails <- numFails + 1}

  returnTherapyDF <- getPatientsWithFirstDrugWithDisease(clinicalDF=testClinicalDF, therapyDF=testTherapyDF, medCodesVector=subConditions, drugCodesVector = requiredProds, FALSE, c(7,7))
  if(is.data.frame(returnTherapyDF)) {print("15. PASSED")} else {print("15. FAILED"); numFails <- numFails + 1}
  if(isTRUE(length(getUniquePatidList(returnTherapyDF))==722)) {print("16. PASSED")} else {print("16. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#Still more to do
testMapDrugTrajectory <- function() {
  numFails <- 0
  print("TESTING: mapDrugTrajectory")

  drugList <- unique(testTherapyDF$prodcode)
  requiredProds <- drugList[1:18]

  structureList <- list(
    Amitriptyline = c(83,49,1888),
    Propranolol = c(707,297,769),
    Topiramate = c(11237),
    Venlafaxine = c(470,301,39359),
    Lisinopril = c(78,65,277),
    Atenolol = c(5,24,26),
    Candesartan = c(531)
  )

  resultList <- mapDrugTrajectory(testTherapyDF, NULL, NULL, 5, 5, groupingList=structureList, removeUndefinedCode=TRUE)
  if(is.list(resultList)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  df3 <- resultList[[3]]
  if(is.data.frame(df3)) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(isTRUE(ggalluvial::is_alluvia_form(as.data.frame(df3), axes = 1:5, silent = TRUE))) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}

  #StatStratum <- ggalluvial::StatStratum
  uniqueNames <- c(names(structureList),"Stopped")
  gg <- ggplot2::ggplot(df3, ggplot2::aes(y = df3$Freq, axis1 = df3$FirstDrug, axis2 = df3$Switch1, axis3 = df3$Switch2, axis4 = df3$Switch3, axis5 = df3$Switch4)) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = df3$FirstDrug), width = 1/12) +
    ggalluvial::geom_stratum(width = 1/12, fill = "black", color = "grey") +
    #ggplot2::geom_label(stat = "stratum", infer.label = TRUE) +
    ggplot2::scale_fill_brewer(type = "qual", palette = "Set1") +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "none") +
    ggplot2::scale_x_discrete(limits = c("First Drug", "1st Switch", "2nd Switch", "3rd Switch","4th Switch"), expand = c(.05, .05)) +
    ggplot2::ggtitle("Migraine Preventative Switching")
  if(is.list(gg)) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#continue working on....
testMatchDrugWithDisease <- function() {
  numFails <- 0

  medcodeList <- c(129, 161)
  drugcodeList <- unique(testTherapyDF$prodcode)
  patidList <- getUniquePatidList(testClinicalDF)
  startDateVector <- NULL
  stopDateVector <- NULL

  resultPatidList <- matchDrugWithDisease(testClinicalDF, NULL, testTherapyDF, patidList, medcodeList, drugcodeList, severity=1)
  if(!is.null(resultPatidList)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(length(resultPatidList)==3838) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}

  resultPatidList <- matchDrugWithDisease(testClinicalDF, NULL, testTherapyDF, patidList, medcodeList, drugcodeList, severity=2)
  if(!is.null(resultPatidList)) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}
  if(length(resultPatidList)==682) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}

  resultPatidList <- matchDrugWithDisease(testClinicalDF, NULL, testTherapyDF, patidList, medcodeList, drugcodeList, severity=3)
  if(!is.null(resultPatidList)) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}
  if(length(resultPatidList)==674) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}

  medcodeList <- c(1,2)
  resultPatidList <- matchDrugWithDisease(testClinicalDF, NULL, testTherapyDF, patidList, medcodeList, drugcodeList, severity=2)
  if(is.null(resultPatidList)) {print("7. PASSED")} else {print("7. FAILED"); numFails <- numFails + 1}


  resultPatidList <- matchDrugWithDisease(testClinicalDF, NULL, testTherapyDF, patidList, medcodeList, drugcodeList, severity=3)
  if(!is.null(resultPatidList)) {print("8. PASSED")} else {print("8. FAILED"); numFails <- numFails + 1}

  #if you don't pass in a medcode list an empty list is returned. This isn't right.
  #If no medcode is present then only 1 or 2 are possible and it should use all conditions in the clinical DF.
  #resultPatidList <- matchDrugWithDisease(testClinicalDF, NULL, testTherapyDF, patidList, NULL, drugcodeList, severity=3)
  #if(!is.null(resultPatidList)) {print("8B. PASSED")} else {print("8B. FAILED"); numFails <- numFails + 1}


  medcodeList <- c(161)
  drugcodeList <- c(707)

  #comorbidity on the same day as disease of innterest with a drug of interest
  smallClinicalDF <- subset(testClinicalDF, testClinicalDF$patid == 3235001)
  smallClinicalDF$eventdate[2] <- smallClinicalDF$eventdate[1]
  smallClinicalDF$medcode[2] <- 777 #comorbidity
  smallTherapyDF <- subset(testTherapyDF, testTherapyDF$patid == 3235001)
  smallTherapyDF$eventdate[8] <- smallClinicalDF$eventdate[1]

  resultPatidList <- matchDrugWithDisease(smallClinicalDF, NULL, smallTherapyDF, getUniquePatidList(smallClinicalDF), medcodeList, drugcodeList, severity=3)
  if(length(resultPatidList)==0) {print("9. PASSED")} else {print("9. FAILED"); numFails <- numFails + 1}

  #comorbidity on a different same day as disease of innterest with a drug of interest
  smallClinicalDF <- subset(testClinicalDF, testClinicalDF$patid == 3235001)
  smallClinicalDF$medcode[2] <- 777 #comorbidity
  smallTherapyDF <- subset(testTherapyDF, testTherapyDF$patid == 3235001)
  smallTherapyDF$eventdate[8] <- smallClinicalDF$eventdate[1]

  resultPatidList <- matchDrugWithDisease(smallClinicalDF, NULL, smallTherapyDF, getUniquePatidList(smallClinicalDF), medcodeList, drugcodeList, severity=3)
  if(length(resultPatidList)==1) {print("10. PASSED")} else {print("10. FAILED"); numFails <- numFails + 1}

  resultPatidList <- matchDrugWithDisease(smallClinicalDF, NULL, smallTherapyDF, getUniquePatidList(smallClinicalDF), medcodeList, drugcodeList, severity=2)
  if(length(resultPatidList)==1) {print("11. PASSED")} else {print("11. FAILED"); numFails <- numFails + 1}

  smallClinicalDF$eventdate[2] <- smallClinicalDF$eventdate[1]
  resultPatidList <- matchDrugWithDisease(smallClinicalDF, NULL, smallTherapyDF, getUniquePatidList(smallClinicalDF), medcodeList, drugcodeList, severity=2)
  if(length(resultPatidList)==1) {print("12. PASSED")} else {print("12. FAILED"); numFails <- numFails + 1}


  #I now need to check whether the date code works
  clusterDF <- loadCPRDDataframe("../Comorbidity_Cox/survDFNoLTFNoAbsent.rda")
  clusterDF <- subset(clusterDF, clusterDF$patid %in% patidList) #this gives me 1538 patients who had RHP
  clusterDF <- clusterDF[order(clusterDF$patid),]

  clusterClinicalDF <- subset(testClinicalDF, testClinicalDF$patid %in% getUniquePatidList(clusterDF))
  clusterTherapyDF <- subset(testTherapyDF, testTherapyDF$patid %in% getUniquePatidList(clusterDF))
  clusterClinicalDF <- clusterClinicalDF[order(clusterClinicalDF$patid),]
  clusterTherapyDF <- clusterTherapyDF[order(clusterTherapyDF$patid),]

  patidList <- unlist(getUniquePatidList(clusterDF))
  startDateVector <- clusterDF$clusterStart
  stopDateVector <- clusterDF$clusterEnd

  dateDF <- data.frame(patid=unlist(patidList), start=startDateVector, stop=stopDateVector)

  #test with a reduced medcode of interest data set
  medcodeList <- c(129, 161)
  resultPatidList <- matchDrugWithDisease(testClinicalDF, NULL, testTherapyDF, patidList, medcodeList, drugcodeList, severity=1, dateDF)
  if(length(resultPatidList)==847) {print("13. PASSED")} else {print("13. FAILED"); numFails <- numFails + 1}

  resultPatidList <- matchDrugWithDisease(testClinicalDF, NULL, testTherapyDF, patidList, medcodeList, drugcodeList, severity=2, dateDF)
  if(length(resultPatidList)==67) {print("14. PASSED")} else {print("14. FAILED"); numFails <- numFails + 1}

  resultPatidList <- matchDrugWithDisease(testClinicalDF, NULL, testTherapyDF, patidList, medcodeList, drugcodeList, severity=3, dateDF)
  if(length(resultPatidList)==674) {print("15. PASSED")} else {print("15. FAILED"); numFails <- numFails + 1}


  return(numFails)
}

#============CPRDDrugTrajectoryStatistics TESTS=========================================


#Almost finished
testCalculateTDSurvivalTime <- function() {
  numFails <- 0
  print("TESTING: calculateTDSurvivalTime")

  medHistoryMainDF <- constructMedicalHistory(NULL,NULL,testTherapyDF)
  idList <- getUniquePatidList(medHistoryMainDF)
  indDF <- medHistoryMainDF[medHistoryMainDF$patid==8204,]

  indexVector <- c(769)
  indexPosition <- "FIRST"
  tdCovariateList <- list(firstCovariate=c(83))
  tdCovariateBehaviourVector <- c("+")
  eventVector <- c(11237)
  obsTime <- 3000
  covariateBooleanList <- NULL
  fixedCovariateDF <- NULL

  #I-C-E
  coxDF <- calculateTDSurvivalTime(indDF, indexVector, indexPosition, eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==2) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[2]==597) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}

  #I-E
  indDF1 <- indDF
  indDF1$code[2] <- 11237
  coxDF <- calculateTDSurvivalTime(indDF1, indexVector, indexPosition, eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==1) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}
  if(coxDF$status[1]==1) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[1]==61) {print("7. PASSED")} else {print("7. FAILED"); numFails <- numFails + 1}

  #I-I-E #FIRST
  indDF2 <- indDF
  indDF2$code[2] <- 769
  indDF2$code[3] <- 11237
  coxDF <- calculateTDSurvivalTime(indDF2, indexVector, "FIRST", eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("8. PASSED")} else {print("8. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==1) {print("9. PASSED")} else {print("9. FAILED"); numFails <- numFails + 1}
  if(coxDF$status[1]==1) {print("10. PASSED")} else {print("10. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[1]==597) {print("11. PASSED")} else {print("11. FAILED"); numFails <- numFails + 1}

  #I-I-E #LAST
  coxDF <- calculateTDSurvivalTime(indDF2, indexVector, "LAST", eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("8. PASSED")} else {print("8. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==1) {print("9. PASSED")} else {print("9. FAILED"); numFails <- numFails + 1}
  if(coxDF$status[1]==1) {print("10. PASSED")} else {print("10. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[1]==536) {print("11. PASSED")} else {print("11. FAILED"); numFails <- numFails + 1}

  #I-I-C-E #FIRST
  indDF3 <- indDF
  indDF3$code[2] <- 769
  indDF3$code[3] <- 83
  indDF3$code[4] <- 11237
  coxDF <- calculateTDSurvivalTime(indDF3, indexVector, "FIRST", eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("12. PASSED")} else {print("12. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==2) {print("13. PASSED")} else {print("13. FAILED"); numFails <- numFails + 1}
  if(coxDF$status[2]==1) {print("14. PASSED")} else {print("14. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[2]==698) {print("15. PASSED")} else {print("15. FAILED"); numFails <- numFails + 1}

  #I-I-C-E #LAST
  coxDF <- calculateTDSurvivalTime(indDF3, indexVector, "LAST", eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("16. PASSED")} else {print("16. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==2) {print("17. PASSED")} else {print("17. FAILED"); numFails <- numFails + 1}
  if(coxDF$status[2]==1) {print("18. PASSED")} else {print("18. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[2]==637) {print("19. PASSED")} else {print("19. FAILED"); numFails <- numFails + 1}

  #E-I
  indDF4 <- indDF
  indDF4$code[1] <- 11237
  indDF4$code[2] <- 769
  coxDF <- calculateTDSurvivalTime(indDF4, indexVector, "FIRST", eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(is.null(coxDF)) {print("20. PASSED")} else {print("20. FAILED"); numFails <- numFails + 1}

  #C-I-E
  indDF5 <- indDF
  indDF5$code[1] <- 83
  indDF5$code[2] <- 769
  indDF5$code[3] <- 11237
  coxDF <- calculateTDSurvivalTime(indDF5, indexVector, "FIRST", eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("21. PASSED")} else {print("21. FAILED"); numFails <- numFails + 1}

  #C-C-I-E
  indDF6 <- indDF
  indDF6$code[1] <- 83
  indDF6$code[2] <- 83
  indDF6$code[3] <- 769
  indDF6$code[4] <- 11237
  coxDF <- calculateTDSurvivalTime(indDF6, indexVector, "FIRST", eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("22. PASSED")} else {print("22. FAILED"); numFails <- numFails + 1}

  #C-C-I-I-E First
  indDF7 <- indDF
  indDF7$code[1] <- 83
  indDF7$code[2] <- 83
  indDF7$code[3] <- 769
  indDF7$code[4] <- 769
  indDF7$code[5] <- 11237
  coxDF <- calculateTDSurvivalTime(indDF7, indexVector, "FIRST", eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("23. PASSED")} else {print("23. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[1]==154) {print("24 PASSED")} else {print("24 FAILED"); numFails <- numFails + 1}

  #C-C-I-I-E Last
  coxDF <- calculateTDSurvivalTime(indDF7, indexVector, "LAST", eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("25. PASSED")} else {print("25. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[1]==53) {print("26 PASSED")} else {print("26 FAILED"); numFails <- numFails + 1}

  #I-C1-C1-C2-E #diff day
  tdCovariateList2 <- list(firstCovariate=c(83),secondCovariate=c(77))
  tdCovariateBehaviourVector2 <- c("+","+")
  indDF8 <- indDF
  indDF8$code[1] <- 769
  indDF8$code[2] <- 83
  indDF8$code[3] <- 83
  indDF8$code[4] <- 77
  indDF8$code[5] <- 769
  indDF8$code[6] <- 11237
  coxDF <- calculateTDSurvivalTime(indDF8, indexVector, "FIRST", eventVector, tdCovariateList2, tdCovariateBehaviourVector2, obsTime)
  if(!is.null(coxDF)) {print("27. PASSED")} else {print("27. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[4]==796) {print("28 PASSED")} else {print("28 FAILED"); numFails <- numFails + 1}

  #no incrememnt on the first covariate
  tdCovariateBehaviourVector3 <- c("-","+")
  coxDF <- calculateTDSurvivalTime(indDF8, indexVector, "FIRST", eventVector, tdCovariateList2, tdCovariateBehaviourVector3, obsTime)
  if(!is.null(coxDF)) {print("29. PASSED")} else {print("29. FAILED"); numFails <- numFails + 1}
  if(coxDF$firstCovariate[4]==1) {print("30. PASSED")} else {print("30. FAILED"); numFails <- numFails + 1}

  #I-C1-C2-E #same day
  tdCovariateList2 <- list(firstCovariate=c(83),secondCovariate=c(77))
  tdCovariateBehaviourVector2 <- c("+","+")
  indDF9 <- indDF
  indDF9$code[1] <- 769
  indDF9$code[2] <- 83
  indDF9$code[3] <- 83
  indDF9$code[4] <- 77
  indDF9$eventdate[4] <- indDF9$eventdate[3]
  indDF9$code[5] <- 769
  indDF9$code[6] <- 11237
  coxDF <- calculateTDSurvivalTime(indDF9, indexVector, "FIRST", eventVector, tdCovariateList2, tdCovariateBehaviourVector2, obsTime)
  if(!is.null(coxDF)) {print("27. PASSED")} else {print("27. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[3]==796) {print("28 PASSED")} else {print("28 FAILED"); numFails <- numFails + 1}

  #I-C1-C2-C3-C4-E #same day diff day
  tdCovariateList2 <- list(firstCovariate=c(83),secondCovariate=c(77))
  tdCovariateBehaviourVector2 <- c("+","+")
  indDF10 <- indDF
  indDF10$code[1] <- 769  #index
  indDF10$code[2] <- 83  #cov1
  indDF10$code[3] <- 83 #cov2
  indDF10$code[4] <- 77 #cov3   same day as cov2
  indDF10$eventdate[4] <- indDF9$eventdate[3]
  indDF10$code[5] <- 83 #cov4
  indDF10$code[6] <- 769 # another event that could be the index day if looking for last
  indDF10$code[7] <- 11237
  coxDF <- calculateTDSurvivalTime(indDF10, indexVector, "FIRST", eventVector, tdCovariateList2, tdCovariateBehaviourVector2, obsTime)
  if(!is.null(coxDF)) {print("29. PASSED")} else {print("29. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[4]==855) {print("30 PASSED")} else {print("30 FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==4) {print("31. PASSED")} else {print("31. FAILED"); numFails <- numFails + 1}

  #if index is set to last it should ignore all the previous covariates
  coxDF <- calculateTDSurvivalTime(indDF10, indexVector, "LAST", eventVector, tdCovariateList2, tdCovariateBehaviourVector2, obsTime)
  if(nrow(coxDF)==1) {print("32. PASSED")} else {print("32. FAILED"); numFails <- numFails + 1}

  #what happens if there are covariates on the same days as the index date? Organise by date and event type
  #index first on the same day as a covariate
  indDF11 <- indDF
  indDF11$code[1] <- 83  #index
  indDF11$code[2] <- 83  #cov1
  indDF11$code[3] <- 769 #index
  indDF11$code[4] <- 77 #cov3   same day as cov2
  indDF11$eventdate[4] <- indDF11$eventdate[3]
  #resultIndDF <- rearrangeIndexDateCovariates(indDF11, indDF11$eventdate[3], indexVector)
  #if(nrow(resultIndDF)==nrow(indDF11)) {print("33. PASSED")} else {print("33. FAILED"); numFails <- numFails + 1}

  #covarite first on same day as index
  indDF12 <- indDF
  indDF12$code[1] <- 83  #index
  indDF12$code[2] <- 83  #cov1
  indDF12$code[3] <- 77 #cov2
  indDF12$code[4] <- 769 #index   same day as cov2
  indDF12$eventdate[4] <- indDF12$eventdate[3]
  #resultIndDF <- rearrangeIndexDateCovariates(indDF12, indDF12$eventdate[4], indexVector)
  #if(nrow(resultIndDF)==nrow(indDF12)) {print("34. PASSED")} else {print("34. FAILED"); numFails <- numFails + 1}

  #start testing the observation time
  #I-C-E
  obsTime2 <- 300
  coxDF <- calculateTDSurvivalTime(indDF, indexVector, indexPosition, eventVector, tdCovariateList, tdCovariateBehaviourVector, obsTime2)
  if(!is.null(coxDF)) {print("35. PASSED")} else {print("35. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==2) {print("36. PASSED")} else {print("36. FAILED"); numFails <- numFails + 1}
  if(coxDF$tstop[2]==300) {print("37. PASSED")} else {print("37. FAILED"); numFails <- numFails + 1}
  if(coxDF$status[2]==0) {print("38. PASSED")} else {print("38. FAILED"); numFails <- numFails + 1}

  #covarite first on same day as index
  indDF13 <- indDF
  indDF13$code[1] <- 83  #cov1
  indDF13$code[2] <- 83  #cov1
  indDF13$code[3] <- 83 #cov2
  indDF13$code[4] <- 769 #index   same day as cov2
  indDF13$eventdate[4] <- indDF13$eventdate[3]
  coxDF <- calculateTDSurvivalTime(indDF13, indexVector, "FIRST", eventVector, tdCovariateList2, tdCovariateBehaviourVector2, obsTime)
  if(!is.null(coxDF)) {print("39. PASSED")} else {print("39. FAILED"); numFails <- numFails + 1}
  if(coxDF$firstCovariate[1]==1) {print("40. PASSED")} else {print("40. FAILED"); numFails <- numFails + 1}


  return(numFails)
}

#Almost finished
testGetTDTimeline <- function() {
  numFails <- 0

  print("TESTING: getTDTimeline")

  medHistoryMainDF <- constructMedicalHistory(NULL,NULL,testTherapyDF)
  idList <- getUniquePatidList(medHistoryMainDF)
  indDF <- medHistoryMainDF[medHistoryMainDF$patid %in% c(8204,1162),]
  indDF$code[2] <- 11237

  indexVector <- c(769, 707,83, 1888,49)
  indexPosition <- "FIRST"
  tdCovariateList <- list(firstCovariate=c(227,78))
  tdCovariateBehaviourVector <- c("+")
  eventVector <- c(11237,5,26,65)
  obsTime <- 3000
  covariateBooleanList <- NULL
  fixedCovariateDF <- NULL

  coxDF <- getTDTimeline(indDF, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}

  #smallPopulationDF <- medHistoryMainDF[medHistoryMainDF$patid %in% idList[1:100],]
  coxDF <- getTDTimeline(medHistoryMainDF, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
  statusVector <- coxDF$status
  tstopVector <- coxDF$tstop
  failedCount <- 0
  for(i in 1:nrow(coxDF)) {
    if(statusVector[i]==1) {
      if(tstopVector[i] >= obsTime) {
        failedCount <- failedCount + 1
      }
    }
  }

  indDF0 <- medHistoryMainDF[medHistoryMainDF$patid == 1198152,]
  coxDF <- getTDTimeline(indDF0, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}

  indDF1 <- medHistoryMainDF[medHistoryMainDF$patid == 179246,]
  coxDF <- getTDTimeline(indDF1, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, tdCovariateList, tdCovariateBehaviourVector, obsTime)
  if(!is.null(coxDF)) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==12) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}
  if(coxDF[12,]$status==0) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}
  if(coxDF[12,]$tstop==3000) {print("7. PASSED")} else {print("7. FAILED"); numFails <- numFails + 1}

  coxDF <- getTDTimeline(indDF1, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, tdCovariateList, tdCovariateBehaviourVector, 10000)
  if(!is.null(coxDF)) {print("8. PASSED")} else {print("8. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==18) {print("9. PASSED")} else {print("9. FAILED"); numFails <- numFails + 1}
  if(coxDF[18,]$status==1) {print("10. PASSED")} else {print("10. FAILED"); numFails <- numFails + 1}
  if(coxDF[18,]$tstop==3373) {print("11. PASSED")} else {print("11. FAILED"); numFails <- numFails + 1}


  return(numFails + failedCount)
}

#working on
testGetTimeline <- function() {
  numFails <- 0

  print("TESTING: getTimeline")

  medHistoryMainDF <- constructMedicalHistory(NULL,NULL,testTherapyDF)
  idList <- getUniquePatidList(medHistoryMainDF)
  indDF <- medHistoryMainDF[medHistoryMainDF$patid==8204,]

  indexVector <- c(769)
  indexPosition <- "FIRST"
  eventVector <- c(11237)
  obsTime <- 3000
  covariateBooleanList <- NULL
  fixedCovariateDF <- NULL

  coxDF <- getTimeline(indDF, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, obsTime)
  if(!is.null(coxDF)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}


  indDF <- medHistoryMainDF[medHistoryMainDF$patid %in% c(8204,1162),]
  indDF$code[2] <- 11237

  indexVector <- c(769, 707,83, 1888,49)
  indexPosition <- "FIRST"
  eventVector <- c(11237,5,26,65)
  obsTime <- 3000
  covariateBooleanList <- NULL
  fixedCovariateDF <- NULL

  endDate <- as.Date("2018-01-01")

  coxDF <- getTimeline(indDF, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, obsTime, endDate)
  if(!is.null(coxDF)) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}

  #smallPopulationDF <- medHistoryMainDF[medHistoryMainDF$patid %in% idList[1:100],]
  coxDF <- getTimeline(medHistoryMainDF, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, obsTime, endDate)
  if(!is.null(coxDF)) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}
  statusVector <- coxDF$status
  tstopVector <- coxDF$time
  failedCount <- 0
  for(i in 1:nrow(coxDF)) {
    if(statusVector[i]==1) {
      if(tstopVector[i] > obsTime) {
        failedCount <- failedCount + 1
      }
    }
  }

  indDF0 <- medHistoryMainDF[medHistoryMainDF$patid == 1198152,]
  coxDF <- getTimeline(indDF0, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, obsTime, endDate)
  if(!is.null(coxDF)) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}

  indDF1 <- medHistoryMainDF[medHistoryMainDF$patid == 179246,]
  coxDF <- getTimeline(indDF1, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, obsTime, endDate)
  if(!is.null(coxDF)) {print("5. PASSED")} else {print("5. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==1) {print("6. PASSED")} else {print("6. FAILED"); numFails <- numFails + 1}
  if(coxDF$status==0) {print("7. PASSED")} else {print("7. FAILED"); numFails <- numFails + 1}
  if(coxDF$time==3000) {print("8. PASSED")} else {print("8. FAILED"); numFails <- numFails + 1}

  coxDF <- getTimeline(indDF1, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, 10000, endDate)
  if(!is.null(coxDF)) {print("9. PASSED")} else {print("9. FAILED"); numFails <- numFails + 1}
  if(nrow(coxDF)==1) {print("10. PASSED")} else {print("10. FAILED"); numFails <- numFails + 1}
  if(coxDF$status==1) {print("11. PASSED")} else {print("11. FAILED"); numFails <- numFails + 1}
  if(coxDF$time==3373) {print("12. PASSED")} else {print("12. FAILED"); numFails <- numFails + 1}


  return(numFails + failedCount)
}

#working on
testCalculateSurvivalTime <- function() {
  numFails <- 0
  print("TESTING: calculateSurvivalTime")

  medHistoryMainDF <- constructMedicalHistory(NULL,NULL,testTherapyDF)
  idList <- getUniquePatidList(medHistoryMainDF)
  indDF <- medHistoryMainDF[medHistoryMainDF$patid==8204,]

  indexVector <- c(769)
  indexPosition <- "FIRST"
  eventVector <- c(11237)
  obsTime <- 3000
  covariateBooleanList <- NULL
  fixedCovariateDF <- NULL

  #I-C-E
  coxList <- calculateSurvivalTime(indDF, indexVector, indexPosition, eventVector)
  if(!is.null(coxList)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(length(coxList)==3) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}
  if(coxList$survivalTime==597) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

#working on
testConstructSurvivalTimeline <- function() {
  numFails <- 0

  medHistoryMainDF <- constructMedicalHistory(NULL,NULL,testTherapyDF)
  indexVector <- c(769, 707,83, 1888,49)
  indexPosition <- "FIRST"
  tdCovariateList <- list(firstCovariate=c(227,78))
  tdCovariateBehaviourVector <- c("+")
  eventVector <- c(11237,5,26,65)
  obsTime <- 365
  covariateBooleanList <- NULL
  fixedCovariateDF <- NULL
  endDate <- as.Date("2018-01-01")

  coxDF <- constructSurvivalTimeline(medHistoryMainDF, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, tdCovariateList=NULL, tdCovariateBehaviourVector=NULL,
                                        obsTime, fixedCovariateDF=NULL, keepEventlessPatients = TRUE, endDate = endDate)

  if(!is.null(coxDF)) {print("1. PASSED")} else {print("1. FAILED"); numFails <- numFails + 1}
  coxDF <- constructSurvivalTimeline(medHistoryMainDF, indexVector, "LAST", eventVector, covariateBooleanList=NULL, tdCovariateList=NULL, tdCovariateBehaviourVector=NULL,
                                     obsTime, fixedCovariateDF=NULL, keepEventlessPatients = TRUE, endDate = endDate)
  if(!is.null(coxDF)) {print("2. PASSED")} else {print("2. FAILED"); numFails <- numFails + 1}

  coxDF <- constructSurvivalTimeline(medHistoryMainDF, indexVector, indexPosition, eventVector, covariateBooleanList=NULL, tdCovariateList=tdCovariateList, tdCovariateBehaviourVector=tdCovariateBehaviourVector,
                                     obsTime, fixedCovariateDF=NULL, keepEventlessPatients = TRUE, endDate = endDate)
  if(!is.null(coxDF)) {print("3. PASSED")} else {print("3. FAILED"); numFails <- numFails + 1}

  coxDF <- constructSurvivalTimeline(medHistoryMainDF, indexVector, "LAST", eventVector, covariateBooleanList=NULL, tdCovariateList=tdCovariateList, tdCovariateBehaviourVector=tdCovariateBehaviourVector,
                                     obsTime, fixedCovariateDF=NULL, keepEventlessPatients = TRUE, endDate = endDate)
  if(!is.null(coxDF)) {print("4. PASSED")} else {print("4. FAILED"); numFails <- numFails + 1}


  return(numFails)

}

#continue working on
testGenerateMCFOneGroup <- function() {
  numFails <- 0
  print("TESTING: generateMCFOneGroup")

  numPatients <- length(getUniquePatidList(testTherapyDF))
  idList <- getUniquePatidList(testTherapyDF)
  maleIDs <- idList[1:1500]
  maleClinicalDF <- testClinicalDF[testClinicalDF$patid %in% maleIDs,]
  maleTherapyDF <- testTherapyDF[testTherapyDF$patid %in% maleIDs,]
  femaleIDs <- idList[1501:length(idList)]
  femaleClinicalDF <- testClinicalDF[testClinicalDF$patid %in% femaleIDs,]
  femaleTherapyDF <- testTherapyDF[testTherapyDF$patid %in% femaleIDs,]

  maleMCFDF <- generateMCFOneGroup(maleClinicalDF,
                                  NULL,
                                  maleTherapyDF)
  x <- nrow(maleMCFDF)
  if(x == 79542) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}

  femaleMCFDF <- generateMCFOneGroup(femaleClinicalDF,
                                    NULL,
                                    femaleTherapyDF)
  x <- nrow(femaleMCFDF)
  if(x == 124146) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}

  maleMCFDF <- cbind(maleMCFDF, Gender="Male")
  femaleMCFDF <- cbind(femaleMCFDF, Gender="Female")
  genderMCF <- rbind(maleMCFDF, femaleMCFDF)
  resultMCF <- reda::mcf(reda::Recur(week, id, No.) ~ Gender, data = genderMCF)

  if(class(resultMCF) == "mcf.formula") {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

testConstructMCF <- function() {
  numFails <- 0
  print("TESTING: constructMCF")

  startDate <- as.Date("2000-01-01")
  testMedHistoryDF <- constructMedicalHistory(NULL,NULL,testTherapyDF)
  codestypes <- "t"
  minRecords <- 2

  mcfDF <- constructMCF(testMedHistoryDF, codestypes, startDate, minRecords, returnData=TRUE)
  if(is.data.frame(mcfDF)) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
  if(nrow(mcfDF)==89370) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}
  mcfDF <- constructMCF(testMedHistoryDF, "c", startDate, minRecords, returnData=TRUE)
  if(is.null(mcfDF)) {print("3. TRUE")} else {print("3. FAILED"); numFails <- numFails + 1}

  mcfDF <- constructMCF(testMedHistoryDF, codestypes, startDate, 1, returnData=TRUE)
  if(nrow(mcfDF)==90379) {print("4. TRUE")} else {print("4. FAILED"); numFails <- numFails + 1}

  mcfDF <- constructMCF(testMedHistoryDF, codestypes, startDate, 6, returnData=TRUE)
  if(nrow(mcfDF)==86161) {print("5. TRUE")} else {print("5. FAILED"); numFails <- numFails + 1}

  mcfDF <- constructMCF(testMedHistoryDF, codestypes, startDate, minRecords, returnData=FALSE)
  if(class(mcfDF) == "mcf.formula") {print("6. TRUE")} else {print("6. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

testGetDrugPersistence <- function() {
  numFails <- 0
  print("TESTING: getDrugPersistence")

  patientList <- getDrugPersistence(testTherapyDF,idList=NULL,prodcodeList=NULL,duration=395,buffer=60,endOfRecordDate="2017-12-31")
  if(length(patientList)==954) {print("1. TRUE")} else {print("1. FAILED"); numFails <- numFails + 1}
  patientList <- getDrugPersistence(testTherapyDF,idList=NULL,prodcodeList=NULL,duration=395,buffer=-3,endOfRecordDate="2017-12-31")
  if(is.null(patientList)) {print("2. TRUE")} else {print("2. FAILED"); numFails <- numFails + 1}

  return(numFails)
}

testSaveCPRDDataframe <- function() {
  numFails <- 0
  print("TESTING: saveCPRDDataframe")

  return(numFails)
}

testSaveCPRDDataframeAsText <- function() {
  numFails <- 0
  print("TESTING: saveCPRDDataframeAsText")

  return(numFails)
}

testSaveCPRDList <- function() {
  numFails <- 0
  print("TESTING: saveCPRDList")

  return(numFails)
}
