rankall <- function(outcome,num="best") {
  outcomes <- c("heart attack","heart failure","pneumonia")
  if(!any(outcome %in% outcomes)) {
    stop("invalid outcome")
  }	
  
  df_hosp_data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
#  if(!any(state %in% df_hosp_data$State)) {
#    stop("invalid state")
#  }
  
  
  if(outcome == outcomes[1]) {
    df_hosp_data <- df_hosp_data[,c(7,2,11)]
  }
  
  if(outcome == outcomes[2]) {
    df_hosp_data <- df_hosp_data[,c(7,2,17)]
  }
  
  if(outcome == outcomes[3]) {
    df_hosp_data <- df_hosp_data[,c(7,2,23)]
  }

  df_hosp_data <- subset(df_hosp_data,subset=df_hosp_data[,3] != "Not Available")
  
  df_hosp_data[,3] <- as.numeric(df_hosp_data[,3])
  
  df_hosp_data <- df_hosp_data[order(df_hosp_data[,3]),]
  
  df_hosp_data[,1] <- factor(df_hosp_data[,1])
  
  df_hosp_data[,3] <- factor(df_hosp_data[,3])

#  print(levels(df_hosp_data[,3]))
  
  ls_hosp_data <- split(df_hosp_data,list(df_hosp_data[,1],df_hosp_data[,3]),drop=TRUE)
  
  ls_state_hosp_data <- split(df_hosp_data,df_hosp_data[,1])
  
  lshdnames <- names(ls_state_hosp_data)
  
  lhdnames <- names(ls_hosp_data)
  
  for(lhdname in lhdnames){
    df_tmp <- ls_hosp_data[[lhdname]]
    df_tmp <- df_tmp[order(df_tmp[,2]),]
    ls_hosp_data[[lhdname]] <- df_tmp
  }
  
  for(lshdname in lshdnames) {
    df_tmp2 = data.frame()
    ss <- grep(lshdname,lhdnames)
    for(s in ss) {
      df_tmp2 <- rbind(df_tmp2,ls_hosp_data[[lhdnames[s]]])
    }
    ls_state_hosp_data[[lshdname]] <- df_tmp2
  }
  
  ans <- data.frame()
  hospital_col <- character()
  state_col <- character()
  
  for(lshdname in lshdnames){
    if(num == "best") {
    hospital_col <- c(hospital_col,ls_state_hosp_data[[lshdname]][1,2])
    state_col <- c(state_col,lshdname)
  } else if(num == "worst") {
    lastrow <- nrow(ls_state_hosp_data[[lshdname]])
    hospital_col <- c(hospital_col,ls_state_hosp_data[[lshdname]][lastrow,2])
    state_col <- c(state_col,lshdname)
  } else if(is.numeric(num)) {
    lastrow <- nrow(ls_state_hosp_data[[lshdname]])    
    if(num < 1 | num > lastrow) {
      hospital_col <- c(hospital_col,NA)
      state_col <- c(state_col,lshdname)      
    } else {
      hospital_col <- c(hospital_col,ls_state_hosp_data[[lshdname]][num,2])
      state_col <- c(state_col,lshdname)
    }

  }
    
  }
#  names(ls_hosp_data) <- lsnames
#  print(lsnames)
#  print(length(ls_hosp_data))
  ans <- cbind.data.frame(hospital_col,state_col)
  colnames(ans) <- c("hospital","state")
  rownames(ans) <- state_col

  ans
}