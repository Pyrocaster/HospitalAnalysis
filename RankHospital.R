# Function that returns a character vector of hospital with the 'rank' lowest 30 day mortality rate for the input outcome (heart attack, heart failure, pneumonia).
## Ties are resolved using alphabetical order of Hospital.Name; Returns the #1 ranked hospital by default
rankHospital <- function(state, outcome, num = "best"){
	##Initialize utilities
	if(!exists("readOutcomeData", mode = "function") || !exists("isStateValid", mode = "function") || !exists("isOutcomeValid", mode = "function")){
		source("Utilities.R")
	}

	##internal function to parse the input 'num' into correct rank
	getRank <- function(inputNum, df){
		answer <- -1
		if(inputNum == "best"){
			answer <- as.numeric(1)
		}
		else if(inputNum == "worst"){
			answer <- nrow(df)
		}
		else{
			suppressWarnings(answer <- as.numeric(inputNum))
		}

		if(is.na(answer)){
			errmsg <- "incorrect num"
			stop(errmsg)
		}
		answer	
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

	##Get the relevant column
	mrCol <- get30DayMRColumnNumber(outcome)

	##Project the outcome data to get only relevant columns
	hospitalNameCol <- 2
	hospitalStateCol <- 7 
	cols <- c(hospitalNameCol, hospitalStateCol, mrCol)
	mrData <- o[,cols]
	
	##subset by state
	sData <- subset(mrData, mrData$State == state)

	##remove NA
	data <- subset(sData, sData[,3] != NA | sData[,3] != "Not Applicable" | sData[,3] != "Not Available")

	##Coerce Mortality rate column (3) to numeric
	suppressWarnings(data[,3] <- as.numeric(data[,3]))

	##Order data by Mortality rate, then by Hospital name
	od <- data[order(data[,3],data[,1]),]	

	##Get rank from input 'num'
	rank <- getRank(num, od)

	##if rank out of bounds return NA
	if(rank > nrow(od)){
		answer <- NA
	}
	else{
		##Extract the hospital at 'rank'
		answer <- od[rank,]$Hospital.Name
	}

	answer
}	


