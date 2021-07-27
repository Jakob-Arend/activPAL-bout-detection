#FULL ALGORITHM ----
wd <- "C:/Users/User/Desktop/PNC_Lab/activPAL-bout-detection"
setwd(wd)
path1 <- "sample_data/SA008-SA008-AP840031 9Apr19 12-00am for 13d 16h 23m-VANE-PB08090417-Events.csv"
path2 <- "sample_data/SA009-SA009-AP840032 11Apr19 12-00am for 12d 16h 22m-VANE-PB08090417-Events.csv"
path3 <- "sample_data/SA010-SA010-AP840027 16Apr19 12-00am for 8d 17h 11m-VANE-PB08090417-Events.csv"
path4 <- "sample_data/SA011-SA011-AP840025 17Apr19 12-00am for 6d 17h 22m-VANE-PB08090417-Events.csv"
paths <- c(path1, path2, path3, path4)
rm(path1, path2, path3, path4)

for(i in 1:length(paths)){
  data <- activpalProcessing::activpal.file.reader(paths[2])
  #USER CONTROLLED OPTIONS ----
  num_days <- 7
  
  #REDUCE TO DAY RANGE ----
  curr_day <- as.Date(substr(data[1, 1], 1, 10))
  last_day <- as.Date(substr(data[nrow(data), 1], 1, 10))
  step_counts <- data.frame("remove_me" = 0)
  step_counts[, as.character(curr_day - 1)] <- 0
  step_counts$remove_me <- NULL
  while(as.numeric(difftime(last_day, curr_day)) != -1){
    step_counts[, as.character(curr_day)] <- 0
    curr_day <- curr_day + 1
  }
  for(i in 1:nrow(data)){
    date <- substr(data[i, 1], 1, 10)
    step_counts[1, date] <- data[i, 5]
  }
  for(i in 2:ncol(step_counts)){
    if(step_counts[1, i] == 0){
      step_counts[1, i] = step_counts[1, i-1]
    }
  }
  for(i in ncol(step_counts):2){
      step_counts[1, i] <- step_counts[1, i] - step_counts[1, i-1]
  }
  step_counts[, 1] <- NULL
  if(ncol(step_counts) < num_days){
    print("USER HAS SPECIFIED MORE DAYS THAN ARE AVAILABLE.  REDUCING 'num_days' TO:")
    print(ncol(step_counts))
    num_days <- ncol(step_counts)
  }
  start_index <- 0
  max <- 0
  for(i in (ncol(step_counts) - num_days + 1):1){
    possible_max <- sum(step_counts[,i:(i + num_days - 1)])
    if(possible_max > max){
      max <- possible_max
      start_index <- i
    }
  }
  valid_days <- c()
  for(i in 0:(num_days - 1)){
    valid_days <- c(valid_days, colnames(step_counts)[start_index + i])
  }
  step_counts <- step_counts[valid_days]
  days_to_remove <- c()
  for(i in 1:nrow(data)){
    if(!(substr(data[i, 1], 1, 10) %in% valid_days)){
      days_to_remove <- c(days_to_remove, i)
    }
  }
  if(length(days_to_remove) != 0){
    data <- data[-c(days_to_remove),]
  }
  rm(curr_day, date, days_to_remove, i, last_day, max, possible_max, start_index)
  
  #PROCESS DATA FOR SLNW ALGORITHM ----
  
  
}