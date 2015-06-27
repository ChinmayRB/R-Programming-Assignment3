rankhospital <- function(state,outcome,num="best") {
	outcomes <- c("heart attack","heart failure","pneumonia")
	if(!any(outcome %in% outcomes)) {
		stop("invalid outcome")
	}	

	df_hosp_data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	if(!any(state %in% df_hosp_data$State)) {
		stop("invalid state")
	}

	if(outcome == outcomes[1]) {
	df_hosp_data <- df_hosp_data[,c(7,2,11)]
	}

	if(outcome == outcomes[2]) {
	df_hosp_data <- df_hosp_data[,c(7,2,17)]
	}

	if(outcome == outcomes[3]) {
	df_hosp_data <- df_hosp_data[,c(7,2,23)]
	}

	df_hosp_data <- subset(df_hosp_data,subset=df_hosp_data[,1]==state & df_hosp_data[,3] != "Not Available")

#	print(df_hosp_data)	

	df_hosp_data[,3] <- as.numeric(df_hosp_data[,3])

	df_hosp_data <- df_hosp_data[order(df_hosp_data[,3]),]

	df_hosp_data[,3] <- as.factor(df_hosp_data[,3])

	ls_hosp_data <- split(df_hosp_data,df_hosp_data[,3])

	lsnames <- names(ls_hosp_data)
#	print(typeof(lsnames))
#	print(lsnames[1])

	i <- 1L
	ranks <- integer()
	NROW <- nrow(df_hosp_data)
	ranked_df <- data.frame()

	for(i in seq_along(lsnames)) {
		df_tmp <- ls_hosp_data[[lsnames[i]]]
#		print(df_tmp)	
		ranks <- c(ranks,length(ranks) + order(df_tmp[,2]))	
#		ranked_df <- rbind(ranked_df,df_tmp)
	}
	
	ranked_df <- cbind(df_hosp_data,ranks)
	ranked_df <- ranked_df[order(ranked_df[,4]),]
#	df_hosp_data <- cbind(df_hosp_data,ranks)
#	print(ranked_df)	
	
	if(num == "best") {
		answer <- ranked_df[1,2]
		return(answer)
	} else if(num == "worst") {
		NROW <- nrow(ranked_df)
		answer <- ranked_df[NROW,2]
		return(answer)
	} else if(is.numeric(num)){
		NROW <- nrow(ranked_df)
		if(num > NROW | num < 1){
			return(NA)
		} else {		
			return(ranked_df[num,2])		
		}
	}
	
}
