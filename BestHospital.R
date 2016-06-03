#Function that returns the best hospital in a state for a given outcome (heart attack, heart failure, pneumonia) - ties are resolved using alphabetical order of Hospital.Name
best <- function(state, outcome){
	
	##Initialize utilities
	if(!exists("readOutcomeData", mode = "function") || !	exists("isStateValid", mode = "function") || 		exists("isOutcomeValid", mode = "function")  ){
		source("Utilities.R")
	}



	bestHeartAttackRateHospital <- function(inputState){
		##get cols with Heart Attack
		haCols <- grep("Heart.Attack", colnames(o))
		##add HospitalName and state
		haCols <- c(grep("Hospital.Name", colnames(o)), haCols)	
		haCols <- c(haCols, grep("State", colnames(o)))	

		##get subset
		haStats <- o[,haCols]
		##remove not applicable and filter by state
		haStats <- subset(haStats, !is.na(haStats[,2]))
		haStats <- subset(haStats, haStats$State == inputState)
		##get minimum heart attack rate
		minHa <- min(haStats[,2])
		##get best hospitals names
		bestHaHospitals <- subset(haStats, haStats[,2] == minHa)
		##return best hospital (alphabetical)
		min(bestHaHospitals[,1])
	}


	
	bestHeartFailureRateHospital <- function(inputState){
		##get cols with Heart Failure
		hfCols <- grep("Heart.Failure", colnames(o))
		##get cols with State and Hospital Name	
		hfCols <- c(grep("State", colnames(o)), hfCols)
		hfCols <- c(grep("Hospital.Name", colnames(o)), hfCols)
		#do a project
		hfStats <- o[,hfCols]
		##remove "Not Applicable" mortality rates and filter by state
		hfStats <- subset(hfStats, hfStats$State == inputState)
		##convert heart failure rate to numeric since there was character data in the raw file
		hfStats[,3] <- suppressWarnings(as.numeric(hfStats[,3]))
		##min heart failure rate
		minHf <- min(hfStats[,3], na.rm = TRUE)
		##best heart failure hospital
		bestHf <- subset(hfStats, hfStats[,3] == minHf)[,1]
		##best hospital	
		min(bestHf)
	}	



	bestPneumoniaRateHospital <- function(inputState){
		##get cols with pneumonia; 2-> hospital name, 7-> state, 23->pneumonia mortality rate
		pCols <- c(2,7,23)
		##project data
		pStats <- o[,pCols]
		##get state values
		pStats <- subset(pStats, pStats$State == inputState)
		##mortality rate as numeric
		pStats[,3] <- as.numeric(pStats[,3])
		##find min ignoring NA
		minP <- min(pStats[,3], na.rm = TRUE)
		##get best hospitals
		bestPHospitals <- subset(pStats, pStats[,3] == minP)[,1]
		##return best (alphabetical)
		min(bestPHospitals)

	}

	

	##read outcome data
	o <- readOutcomeData();

	##Check if inputs are valid
	if(!isStateValid(state, o)){
		errmsg <- "invalid state"
		stop(errmsg)	
	}
	if(!isOutcomeValid(outcome)){
		
		errmsg <- "invalid outcome"
		stop(errmsg)	
	}
	
	
	if(outcome == "heart attack"){
		answer <- bestHeartAttackRateHospital(state)
	} 
	else if(outcome == "heart failure"){
		answer <- bestHeartFailureRateHospital(state)
	}
	else{
		answer <- bestPneumoniaRateHospital(state)
	}
	
	answer
}	


