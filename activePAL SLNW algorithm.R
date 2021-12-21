rm(list = ls())
library(activpalProcessing)
library(lubridate)
library(ggplot2)



SLNW <- function(folder, file){
  #Algorithm constants ----
  file_parts <- unlist(strsplit(file, "/"))
  num_parts <- length(file_parts)
  path <- paste(file_parts[num_parts-2], "/", file_parts[num_parts-1], "/", file_parts[num_parts], sep="")
  heatmap_path <- paste("OUTPUT/heat_maps/", basename(folder), "/", tools::file_path_sans_ext(basename(file)), "-HEAT MAP.png", sep="")
  
  consecutive_days <- 7
  always_slnw_min_hours <- 5
  longest_only_slnw_min_hours <- 2
  before_window_mins <- 15
  after_window_mins <- 15
  always_slnw_surrounding_hours <- 2
  longest_slnw_surrounding_mins <- 30
  slnw_surrounding_steps <- 20
  slnw_surrounding_steps_with_posture_changes <- 0
  invalid_day_largest_bout_percentage <- 0.95
  invalid_day_min_step_count <- 500
  invalid_day_min_hours <- 10
  
  #Helper functions for later in the code ----
  convert_time <- function(time_string) {
      to_return <- unlist(strsplit(paste(time_string), " "))
      if(length(to_return == 1)){
        to_return <- c(to_return, "00:00:00")
      }
      return(to_return)
  }
  
  search_forwards <- function(slnw_index, data){
    longest_slnw_bout <- 0
    curr_index <- slnw_index + 1
    while(curr_index <= nrow(data) && as.numeric(difftime(data[curr_index, 1], data[slnw_index+1, 1]), units = "secs") <= (after_window_mins * 60)){
      if(data[curr_index, 4] < 2 && data[curr_index, 3] > longest_slnw_bout){
        longest_slnw_bout <- data[curr_index, 3]
      }
      curr_index <- curr_index + 1
    }
    curr_index <- curr_index - 1
    total_steps <- data[curr_index, 5] - data[slnw_index, 5]
    if(longest_slnw_bout >= (always_slnw_surrounding_hours * 3600) || (longest_slnw_bout >= (longest_slnw_surrounding_mins * 30) && total_steps <= slnw_surrounding_steps) || total_steps <= slnw_surrounding_steps_with_posture_changes){
      return(curr_index)
    }else{
      return(slnw_index)
    }
  }
  
  search_backwards <- function(slnw_index, data){
    longest_slnw_bout <- 0
    curr_index <- slnw_index - 1
    while(curr_index >= 1 && as.numeric(difftime(data[slnw_index, 1], data[curr_index+1, 1]), units = "secs") <= (after_window_mins * 60)){
      if(data[curr_index, 4] < 2 && data[curr_index, 3] > longest_slnw_bout){
        longest_slnw_bout <- data[curr_index, 3]
      }
      curr_index <- curr_index - 1
    }
    curr_index <- curr_index + 1
    total_steps <- data[slnw_index, 5] - data[curr_index, 5]
    if(longest_slnw_bout >= (always_slnw_surrounding_hours * 3600) || (longest_slnw_bout >= (longest_slnw_surrounding_mins * 30) && total_steps <= slnw_surrounding_steps) || total_steps <= slnw_surrounding_steps_with_posture_changes){
      return(curr_index)
    }else{
      return(slnw_index)
    }
  }
  
  #Convert from .csv to data frame ----
  data <- activpalProcessing::activpal.file.reader(path)
  
  steps <- c(0)
  for(i in 2:nrow(data)){
    steps <- c(steps, data[i, 5] - data[i-1, 5])
  }
  data$steps <- steps
  
  #Cut data to valid window ----
  first_day <- convert_time(data[1, 1])[1]
  last_day <- convert_time(data[nrow(data), 1])[1]
  days <- as.character(seq(as.Date(first_day), as.Date(last_day), "days"))
  
  if(length(days) < consecutive_days){
    print(paste("ERROR: Consecutive days specified is greater than total number of days in input data, reducing consecutive days down to", as.character((length(days)))))
    consecutive_days <- length(days)
  }
  
  steps_per_day <- data.frame(matrix(0, nrow=1, ncol=length(days)))
  colnames(steps_per_day) <- days
  
  for(i in 1:nrow(data)){
    date <- convert_time(data[i, 1])[1]
    steps_per_day[1, date] <- data[i, 5]
  }
  for(i in ncol(steps_per_day):2){
    steps_per_day[1, i] <- steps_per_day[1, i] - steps_per_day[1, i-1]
  }
  
  window_start_index <- 0
  curr_max <- 0
  for(i in 1:(length(days)-consecutive_days+1)){
    potential_max <- 0
    for(j in 0:(consecutive_days-1)){
      potential_max <- potential_max + steps_per_day[1, (i+j)]
    }
    if(potential_max > curr_max){
      window_start_index <- i
      curr_max <- potential_max
    }
  }
  
  valid_days <- days[window_start_index:(window_start_index+consecutive_days-1)]
  excluded_indices <- c()
  for(i in 1:nrow(data)){
    date <- convert_time(data[i, 1])[1]
    if(!(date %in% valid_days)){
      excluded_indices <- c(excluded_indices, i)
    }
  }
  if(length(excluded_indices) != 0){
    experimental_window_excluded_data <- data[excluded_indices,]
    data <- data[-excluded_indices,]
  }else{
    experimental_window_excluded_data <- data.frame(matrix(ncol=ncol(data), nrow=0))
    colnames(experimental_window_excluded_data) <- colnames(data)
  }
  
  rm(steps_per_day, curr_max, date, days, first_day, i, j, last_day, potential_max, excluded_indices, window_start_index)
  
  #Remove SLNW bouts ----
  data <- transform(data, cumulativesteps=cumulativesteps-data[1,5])
  
  noon_day_maxes <- c()
  noon_day_indices <- c()
  for(i in 1:(length(valid_days)+1)){
    noon_day_maxes <- c(noon_day_maxes, -1)
    noon_day_indices <- c(noon_day_indices, -1)
  }
  
  get_noon_day_index <- function(first_day, current_date_time){
    start <- paste(first_day, "12:00:00", sep = " ")
    day_count <- as.numeric(difftime(current_date_time, start), units = "days")
    return(2 + floor(day_count))
  }
  
  slnw_indices <- c()
  for(i in 1:nrow(data)){
    noon_index <- get_noon_day_index(valid_days[1], data[i, 1])
    if(data[i, 4] < 2 && (data[i, 3] >= longest_only_slnw_min_hours * 3600)){
      if(data[i, 3] >= (always_slnw_min_hours * 3600)){
        slnw_indices <- c(slnw_indices, i)
      }
      if(data[i, 4] > noon_day_maxes[noon_index]) {
        noon_day_maxes[noon_index] <- data[i, 4]
        noon_day_indices[noon_index] <- i
      }
    }
  }
  
  for(i in 1:length(noon_day_indices)){
    if(noon_day_indices[i] != -1 && !(noon_day_indices[i] %in% slnw_indices)){
      slnw_indices <- c(slnw_indices, noon_day_indices[i])
    }
  }
  
  for(i in 1:length(slnw_indices)){
    curr_slnw_index <- slnw_indices[i]
    backward <- search_backwards(curr_slnw_index, data)
    forward <- search_forwards(curr_slnw_index, data)
    
    while(backward < curr_slnw_index){
      if(!(backward %in% slnw_indices)){
        slnw_indices <- c(slnw_indices, backward)
      }
      backward <- backward + 1
    }
    while(forward > curr_slnw_index){
      if(!(forward %in% slnw_indices)){
        slnw_indices <- c(slnw_indices, forward)
      }
      forward <- forward - 1
    }
  }
  
  slnw_indices <- sort(slnw_indices)
  
  if(length(slnw_indices) != 0){
    slnw_data <- data[slnw_indices,]
    data <- data[-slnw_indices,]
  }else{
    slnw_data <- data.frame(matrix(ncol=ncol(data), nrow=0))
    colnames(slnw_data) <- colnames(data)
  }
  
  rm(backward, forward, curr_slnw_index, slnw_indices, i, noon_day_indices, noon_day_maxes, noon_index)
  
  #Remove data from invalid days ----
  longest_waking_wear_hours <- c()
  total_steps <- c()
  total_waking_wear_hours <- c()
  for(i in 1:length(valid_days)){
    longest_waking_wear_hours <- c(longest_waking_wear_hours, -1)
    total_steps <- c(total_steps, -1)
    total_waking_wear_hours <- c(total_waking_wear_hours, -1)
  }
  
  if(nrow(data) <= 1){
    print("ERROR: Insufficient data remains after reducing to experimental window and removing SLNW bouts")
  }
  old_day <- convert_time(data[1, 1])[1]
  start_index <- 1
  day_index <- match(old_day, valid_days)
  edge_case <- FALSE
  for(i in 1:nrow(data)){
    curr_day <- convert_time(data[i, 1])[1]
    if(curr_day != old_day){
      total_steps[day_index] <- data[i-1, 5] - data[start_index, 5]
      total_waking_wear_hours[day_index] <- as.numeric(difftime(data[i-1, 1], data[start_index, 1], units="hours"))
      old_day <- curr_day
      start_index <- i
      day_index <- match(old_day, valid_days)
      if(i == nrow(data)){
        edge_case <- TRUE
      }
    }
    if((data[i, 3] / 3600)> longest_waking_wear_hours[day_index]){
      longest_waking_wear_hours[day_index] <- (data[i,3] / 3600)
    }
  }
  if(edge_case){
    old_day <- convert_time(data[nrow(data), 1])[1]
    day_index <- match(old_day, valid_days)
    longest_waking_wear_hours[day_index] <- (data[nrow(data), 3] / 3600)
    total_steps[day_index] <- (data[nrow(data), 5] - data[nrow(data)-1, 5])
    total_waking_wear_hours[day_index] <- (data[nrow(data), 3] / 3600)
  }else{
    total_steps[day_index] <- data[nrow(data), 5] - data[start_index, 5]
    total_waking_wear_hours[day_index] <- as.numeric(difftime(data[nrow(data), 1], data[start_index, 1], units="hours"))
  }
  
  invalid_days <- c()
  for(i in 1:length(valid_days)){
    if((longest_waking_wear_hours[i] / total_waking_wear_hours[i]) >= invalid_day_largest_bout_percentage || total_steps[i] < invalid_day_min_step_count || total_waking_wear_hours[i] < invalid_day_min_hours){
      invalid_days <- c(invalid_days, valid_days[i])
    }
  }
  
  invalid_day_indices <- c()
  for(i in 1:nrow(data)){
    curr_day <- convert_time(data[i, 1])[1]
    if(curr_day %in% invalid_days){
      invalid_day_indices <- c(invalid_day_indices, i)
    }
  }
  
  if(length(invalid_day_indices) != 0){
    invalid_day_data <- data[invalid_day_indices,]
    data <- data[-invalid_day_indices,]
  }else{
    invalid_day_data <- data.frame(matrix(ncol=ncol(data), nrow=0))
    colnames(invalid_day_data) <- colnames(data)
  }
  
  valid_days <- setdiff(valid_days, invalid_days)
  
  rm(curr_day, day_index, edge_case, i, invalid_day_indices, longest_waking_wear_hours, old_day, start_index, total_steps, total_waking_wear_hours)
  
  #Plot heat map ----
  if(nrow(data) > 0){
    dates <- c()
    times <- c()
    yends <- c()
    for(i in 1:nrow(data)){
      d <- convert_time(data[i, 1])[1]
      d <- gsub("-", "/", d)
      d <- substring(d, 6)
      dates <- c(dates, d)
      t <-convert_time(data[i, 1])[2]
      t <- period_to_seconds(hms(t))
      times <- c(times, t)
      yend <- min(t+data[i, 3], 86400)
      yends <- c(yends, yend)
    }
    
    data$dates <- dates
    data$times <- times
    data$yends <- yends
    data$activity <- as.factor(data$activity)
    
    nums <- seq(from = 0, to = 86400, length.out = 13)
    labels <- c("00:00", "02:00", "04:00", "06:00", "08:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00", "24:00")
    colors <- c("0" = "red", "1" = "yellow", "2" = "green4")
    ggplot() +
      geom_segment(data=data, size=12, aes(y=times, yend=yends , x=dates, xend=dates, color=activity)) +
      scale_color_manual(values = colors) +
      scale_y_continuous(breaks = nums, labels = labels, limits = c(nums[1], nums[length(nums)])) +
      ggtitle("ActivPAL Waking Wear Heat Map") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Assessement date") +
      ylab("24 hour time")
  
    ggsave(filename=heatmap_path, device=png)
    
    for(i in 1:3){
      data[,ncol(data)] <- NULL
    }
    
  }else{
    print("ERROR: No valid data left to plot.")
  }
  return(data)
}



get_summary_statistics <- function(folder, base_file, data, params) {
  write.csv(data, file=paste(folder, "/", base_file, ".csv", sep=""), row.names = FALSE)
}



working_directory <- "C:/Users/User/Desktop/PNC_Lab/activPAL-bout-detection"
setwd(working_directory)
folders <- Sys.glob(paste(working_directory, "/INPUT/*", sep=""))
dir.create(file.path(working_directory, "/OUTPUT"), showWarnings = FALSE)
output <- paste(working_directory, "/OUTPUT", sep="")
dir.create(file.path(output, "/heat_maps"), showWarnings = FALSE)
dir.create(file.path(output, "/valid_data"), showWarnings = FALSE)
dir.create(file.path(output, "/summary_statistics"), showWarnings = FALSE)
heat_maps <- paste(output, "/heat_maps", sep="")
valid_data <- paste(output, "/valid_data", sep="")
summary_statistics <- paste(output, "/summary_statistics", sep="")



all_data <- c()
for(folder in folders){
  dir.create(file.path(heat_maps, basename(folder)), showWarnings = FALSE)
  dir.create(file.path(valid_data, basename(folder)), showWarnings = FALSE)
  dir.create(file.path(summary_statistics, basename(folder)), showWarnings = FALSE)
  group_data <- data.frame(time=as.Date(character()),
                           datacount=integer(),
                           interval=double(),
                           activity=integer(),
                           cumulativesteps=integer(),
                           methrs=double(),
                           steps=integer())
  files <- Sys.glob(paste(folder, "/*", sep=""))
  for(file in files){
    print(paste("RUNNING SLNW ON FILE", file, sep=" "))
    rm(list=setdiff(ls(), c("SLNW", "working_directory", "folders", "output", "heat_maps", "valid_data", "folder", "files", "file", "get_summary_statistics", "summary_statistics", "group_data")))
    data <- SLNW(folder, file)
    group_data <- rbind(group_data, data)
    write.csv(data, file=paste(valid_data, "/", basename(folder), "/", tools::file_path_sans_ext(basename(file)), "-VALID DATA.csv", sep=""), row.names = FALSE)
    get_summary_statistics(paste(summary_statistics, "/", basename(folder), sep=""), tools::file_path_sans_ext(basename(file)), data, c())
  }
  get_summary_statistics(paste(summary_statistics, "/", basename(folder), sep=""), paste(basename(folder), "summary statistics", sep=" "), group_data, c())
}

