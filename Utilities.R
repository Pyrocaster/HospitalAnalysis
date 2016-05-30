	
#Read contents of 'hospital-data.csv' from folder to a data table
readHospitalData <- function(folder = NULL){
	#use current WD as folder location if nothing is explicitly passed	
	if(is.null(folder)){
	 folder = getwd()
	}

	filename = 	paste(folder, "//hospital-data.csv", sep = "")

	read.table(filename, header = TRUE, sep = ",", colClasses = "character")
}



#Read contents of 'outcome-of-care-measures.csv' from folder to a data table
readOutcomeData <- function(folder = NULL) {
	#use current WD as folder location if nothing is explicitly passed	
	if(is.null(folder)){
	 folder = getwd()
	}
	
	filename = paste(folder, "//outcome-of-care-measures.csv", sep = "")

	read.table(filename, header = TRUE, sep = ",", colClasses = "character")
}



#check if the state is valid; o is an object with a column called 'State'
isStateValid <- function(inputState, o){
	isValid <- FALSE
	if(sum(o$State == inputState) >= 1){
		isValid <- TRUE
	}
	isValid
}	



#check if the outcome is valid
isOutcomeValid <- function(inputOutcome){
	validOutcomes <- c("heart attack", "heart failure", "pneumonia")

	isValid <- FALSE
	if(sum(validOutcomes == inputOutcome) >= 1){
		isValid <- TRUE
	}
	isValid
}


#clean workspace
clearObjects <- function(){
	rm(best)
	rm(readHospitalData)
	rm(readOutcomeData)
	rm(isOutcomeValid)
	rm(isStateValid)
}
