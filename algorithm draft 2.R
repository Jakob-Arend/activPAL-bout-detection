#SET WD AND PATHS ----
wd <- "C:/Users/User/Desktop/PNC_Lab/activPAL-bout-detection"
setwd(wd)
#path1 <- "sample_data/SA008-SA008-AP840031 9Apr19 12-00am for 13d 16h 23m-VANE-PB08090417-Events.csv"
#path2 <- "sample_data/SA009-SA009-AP840032 11Apr19 12-00am for 12d 16h 22m-VANE-PB08090417-Events.csv"
#path3 <- "sample_data/SA010-SA010-AP840027 16Apr19 12-00am for 8d 17h 11m-VANE-PB08090417-Events.csv"
#path4 <- "sample_data/SA011-SA011-AP840025 17Apr19 12-00am for 6d 17h 22m-VANE-PB08090417-Events.csv"
#paths <- c(path1, path2, path3, path4)
paths <- c("sample_data/SA004-AP840029 20Feb19 4-14pm for 7d-VANE-PB08101257-Events.csv", "sample_data/SA006-AP840024 20Feb19 4-15pm for 7d-VANE-PB08101257-Events.csv", "sample_data/SA005-AP840030 20Feb19 4-17pm for 6d 23h 59m-VANE-PB08101257-Events.csv")

#rm(path1, path2, path3, path4)

#HELPER FUNCTIONS FOR LATER ----
search_forwards <- function(sleep_index, data){
  curr_position <- sleep_index + 1
  while(curr_position <= nrow(data) && (as.numeric(difftime(data[curr_position, 1], data[sleep_index, 1]), units = "secs") - data[sleep_index, 3]) <= 900){
    curr_position <- curr_position + 1
  }
  forward_SLNW <- c((sleep_index + 1):(curr_position - 1))
  small_sleep = FALSE
  for(i in length(forward_SLNW)){
    if(data[forward_SLNW[i], 3] >= 7200 && data[forward_SLNW[i], 4] < 2){
      return(forward_SLNW)
    }else if(data[forward_SLNW[i], 3] >= 1800 && data[forward_SLNW[i], 4] < 2){
      small_sleep = TRUE
    }
  }
  total_steps <- data[forward_SLNW[length(forward_SLNW)], 5] - data[sleep_index, 5]
  if((small_sleep && total_steps <= 20) || total_steps == 0){
    return(forward_SLNW)
  }
  return(c())
}

search_backwards <- function(sleep_index, data){
  curr_position <- sleep_index - 1
  while(curr_position >= 1 && (as.numeric(difftime(data[sleep_index, 1], data[curr_position, 1]), units = "secs") - data[curr_position, 3]) <= 900){
    curr_position <- curr_position - 1
  }
  backward_SLNW <- c((curr_position + 1):(sleep_index - 1))
  small_sleep = FALSE
  for(i in length(backward_SLNW)){
    if(data[backward_SLNW[i], 3] >= 7200 && data[backward_SLNW[i], 4] < 2){
      return(backward_SLNW)
    }else if(data[backward_SLNW[i], 3] >= 1800 && data[backward_SLNW[i], 4] < 2){
      small_sleep = TRUE
    }
  }
  total_steps <- data[sleep_index, 5] - data[backward_SLNW[1], 5]
  if((small_sleep && total_steps <= 20) || total_steps == 0){
    return(backward_SLNW)
  }
  return(c())
}

#LOOP FOR FULL ALGORITHM ----
data_frames <- list()
sleep_data_frames <- list()
invalid_data_frames <- list()
for(i in 1:length(paths)){
  data <- activpalProcessing::activpal.file.reader(paths[i])
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
  
  #SLNW ALGORITHM ----
  curr_day <- paste(valid_days[1], "12:00:00", sep = " ")
  noon_days <- c(1)
  i = 1
  while(i <= nrow(data)){
    if(as.numeric(difftime(data[i, 1], curr_day), units = "secs") > 86400) {
      noon_days <- c(noon_days, -1)
      curr_day <- paste(valid_days[length(noon_days)], "12:00:00", sep = " ")
      next
    }
    if(as.numeric(difftime(data[i, 1], curr_day), units = "secs") > 0) {
      noon_days <- c(noon_days, i)
      if(length(noon_days) > length(valid_days)){
        break
      } else {
        curr_day <- paste(valid_days[length(noon_days)], "12:00:00", sep = " ")
      }
    }
    i <- i + 1
  }
  noon_days <- c(noon_days, nrow(data) + 1)
  
  all_SLNW <- c()
  i = 1
  while(i < length(noon_days)){
    stop_index <- i + 1
    while(noon_days[stop_index] == -1){
      stop_index <- stop_index + 1
    }
    sleep_indices = c()
    longest_bout = -1
    for(j in noon_days[i]:(noon_days[stop_index] - 1)){
      if(data[j, 3] >= 18000 && data[j, 4] < 2){
        sleep_indices <- c(sleep_indices, j)
      } else if(data[j, 3] >= 7200 && data[j, 4] < 2){
        longest_bout = j
      }
    }
    if(length(sleep_indices) == 0 && longest_bout != -1){
      sleep_indices <- c(sleep_indices, longest_bout)
    }
    if(length(sleep_indices) > 0){
      for(j in 1:length(sleep_indices)){
        all_SLNW <- c(all_SLNW, search_backwards(sleep_indices[j], data), sleep_indices[j], search_forwards(sleep_indices[j], data))
      }
    }
    i <- stop_index
  }
  all_SLNW <- all_SLNW[!duplicated(all_SLNW)]
  sleep_data <- data.frame(data[all_SLNW,])
  data <- data[-c(all_SLNW),]
  
  #REMOVE INVALID DAYS ----
  day_end <- c()
  counter <- 1
  for(i in 1:nrow(data)){
    if(substr(data[i, 1], 1, 10) != valid_days[counter]){
      day_end <- c(day_end, i-1)
      counter <- counter + 1
    }
  }
  day_end <- c(day_end, nrow(data))
  
  hour_cutoffs <- c(28800, 36000, 50400)
  invalid_days <- c()
  for(i in 1:length(day_end)){
    check_time <- 0
    check_largest <- 0
    if(i == 1){
      start = 1
      prev_steps = 0
    }else{
      start = day_end[i-1] + 1
      prev_steps = data[start-1, 5]
    }
    for(j in start:day_end[i]){
      check_time <- check_time + data[j, 3]
      if(data[j, 3] > check_largest){
        check_largest <- data[j, 3]
      }
    }
    #--------------------------ADJUST VALID DAY PARAMETERS------------------------------
    if(check_time < 36000 || check_largest >= 0.95 * check_time || (data[day_end[i], 5] - prev_steps) < 500){
      #print(data[day_end[i], 5] - prev_steps)
      invalid_days <- c(invalid_days, valid_days[i])
    }
  }
  
  to_remove <- c()
  for(i in 1:nrow(data)){
    if(substr(data[i, 1], 1, 10) %in% invalid_days){
      to_remove <- c(to_remove, i)
    }
  }
  if(length(to_remove) != 0){
    invalid_data <- data.frame(data[to_remove,])
    data <- data[-c(to_remove),]
  } else{
    invalid_data <- data.frame()
  }
    
  data_frames[[length(data_frames) + 1]] <- data
  sleep_data_frames[[length(sleep_data_frames) + 1]] <- sleep_data
  invalid_data_frames[[length(invalid_data_frames) + 1]] <- invalid_data
  #CLEAN UP WORKING ENV ----
  rm(all_SLNW, check_largest, check_time, counter, curr_day, day_end, hour_cutoffs, i, invalid_days, j, longest_bout, noon_days, num_days, prev_steps, sleep_indices, start,stop_index, to_remove, valid_days, data, invalid_data, sleep_data, step_counts)
}

#example of summary statistics ----
f = 3
example_data <- data_frames[[f]]
example_invalid_data <- invalid_data_frames[[f]]
example_sleep_data <- sleep_data_frames[[f]]
non_SLNW_inactivity <- 0
for(i in 1:nrow(example_data)){
  if(example_data[i, 4] < 2){
    non_SLNW_inactivity <- non_SLNW_inactivity + example_data[i, 3]
  }
}
total_inactivity = non_SLNW_inactivity + sum(example_sleep_data$interval)
global_sedentary_time <- (total_inactivity / (sum(example_data$interval) + total_inactivity))
print(global_sedentary_time)
