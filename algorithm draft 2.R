#USER CONTROLLED OPTIONS ----
num_days <- 7

#READ IN DATA ----
wd <- "C:/Users/User/Desktop/PNC_Lab/activPAL-bout-detection"
setwd(wd)
data_path <- "sample_data/SA009-SA009-AP840032 11Apr19 12-00am for 12d 16h 22m-VANE-PB08090417-Events.csv"
data <- activpalProcessing::activpal.file.reader(data_path)

#FIND DAY RANGE ----
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
for(i in ncol(step_counts):2){
  step_counts[1, i] <- step_counts[1, i] - step_counts[1, i-1]
}
step_counts[, 1] <- NULL
if(ncol(step_counts) < num_days){
  print("USER HAS SPECIFIED MORE DAYS THAN ARE AVAILABLE.  REDUCING num_days TO:")
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
