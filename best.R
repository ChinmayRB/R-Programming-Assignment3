source("rankhospital.R")
best <- function(state,outcome){
	besthosp <- rankhospital(state,outcome)
	besthosp
} 