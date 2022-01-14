#Environment preparation ----
rm(list = ls())
library(activpalProcessing)
library(lubridate)
library(ggplot2)
#Helper functions to main ----
create_directories <- function(working_directory){
  dir.create(file.path(working_directory, "/OUTPUT"), showWarnings = FALSE)
  output <- paste(working_directory, "/OUTPUT", sep="")
  dir.create(file.path(output, "/heat_maps"), showWarnings = FALSE)
  dir.create(file.path(output, "/valid_data"), showWarnings = FALSE)
  dir.create(file.path(output, "/summary_statistics"), showWarnings = FALSE)
  heat_maps <- paste(output, "/heat_maps", sep="")
  valid_data <- paste(output, "/valid_data", sep="")
  summary_statistics <- paste(output, "/summary_statistics", sep="")
  for(folder in Sys.glob(paste(working_directory, "/INPUT/*", sep=""))){
    dir.create(file.path(heat_maps, paste(basename(folder), "heat maps", sep=" ")), showWarnings = FALSE)
    dir.create(file.path(valid_data, paste(basename(folder), "valid data", sep=" ")), showWarnings = FALSE)
    dir.create(file.path(summary_statistics, paste(basename(folder), "summary statistics", sep=" ")), showWarnings = FALSE)
  }
}

SLNW <- function(file){
  #Algorithm constants ----
  file_parts <- unlist(strsplit(file, "/"))
  num_parts <- length(file_parts)
  path <- paste(file_parts[num_parts-2], "/", file_parts[num_parts-1], "/", file_parts[num_parts], sep="")
  heatmap_path <- paste("OUTPUT/heat_maps/", paste(file_parts[num_parts - 1], "heat maps", sep=" "), "/", tools::file_path_sans_ext(basename(file)), "-HEAT MAP.png", sep="")
  
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

format_instructions <- function(instructions){
  name <- c()
  type <- c()
  lower_bound <- c()
  upper_bound <- c()
  lower_exclusive <- c()
  upper_exclusive <- c()
  for(i in 0:(length(instructions) / 6 - 1)){
    curr_start <- 6 * i + 1
    name <- c(name, instructions[[curr_start]])
    type <- c(type, instructions[[curr_start+1]])
    lower_bound <- c(lower_bound, instructions[[curr_start+2]])
    upper_bound <- c(upper_bound, instructions[[curr_start+3]])
    lower_exclusive <- c(lower_exclusive, instructions[[curr_start+4]])
    upper_exclusive <- c(upper_exclusive, instructions[[curr_start+5]])
  }
  return(data.frame(name, type, lower_bound, upper_bound, lower_exclusive, upper_exclusive))
}

filter_data <- function(data, type, lower_bound=0, upper_bound=Inf, lower_exclusive=TRUE, upper_exclusive=TRUE){
  if(type == "Sedentary"){
    filter <- c(0, 1)
  } else if(type == "Stepping"){
    filter <- c(2)
  } else if(type == "All"){
    filter <- c(0, 1, 2)
  } else{
    filter <- c()
  }
  if(lower_exclusive & upper_exclusive){return(subset(data, data$activity %in% filter & data$interval > lower_bound & data$interval < upper_bound))
  } else if(upper_exclusive){
    return(subset(data, data$activity %in% filter & data$interval >= lower_bound & data$interval < upper_bound))
  } else if(lower_exclusive){
    return(subset(data, data$activity %in% filter & data$interval > lower_bound & data$interval <= upper_bound))
  } else{
    return(subset(data, data$activity %in% filter & data$interval >= lower_bound & data$interval <= upper_bound))
  }
}

pnc_summary_statistics <- function(data){
  if(length(data > 0)){
    num_bouts <- length(data)
    avg <- mean(data)
    min <- min(data)
    max <- max(data)
    median <- median(data)
    iqr <- IQR(data)
    stddev <- sd(data)
    cv <- 100 * stddev / avg
    return(c(num_bouts, avg, min, max, median, iqr, stddev, cv))
  }
  return(rep(c(NaN), 8))
}

get_summary_statistics <- function(file, data, instructions) {
  if(nrow(data) == 0){
    names <- instructions$name
    colnames <- c()
    for(j in 1:length(names)){
      colnames <- c(colnames, paste("combined", names[j], sep=" "))
    }
    rownames <- c("num bouts", "average duration", "min duration", "max duration", "median duration", "duration interquartile range", "duration stddev", "duration coeff of variation")
    summ_stats <- data.frame(matrix(ncol=length(colnames), nrow=length(rownames)))
    colnames(summ_stats) <- colnames
    rownames(summ_stats) <- rownames
    write.csv(summ_stats, file=file)
  } else{
    days <- c()
    reduced_time <- c()
    for(i in 1:nrow(data)){
      curr_day <- unlist(strsplit(paste(data[i,1]), " "))[1]
      if(!(curr_day %in% days)){
        days <- c(days, curr_day)
      }
      reduced_time <- c(reduced_time, curr_day)
    }
    data$day <- reduced_time
    
    names <- instructions$name
    colnames <- c()
    for(i in 1:length(days)){
      for(j in 1:length(names)){
        colnames <- c(colnames, paste(days[i], names[j], sep=" "))
      }
    }
    for(j in 1:length(names)){
      colnames <- c(colnames, paste("combined", names[j], sep=" "))
    }
    rownames <- c("num bouts", "average duration", "min duration", "max duration", "median duration", "duration interquartile range", "duration stddev", "duration coeff of variation")
    summ_stats <- data.frame(matrix(ncol=length(colnames), nrow=length(rownames)))
    colnames(summ_stats) <- colnames
    rownames(summ_stats) <- rownames
    
    for(i in 1:length(days)){
      curr_data <- subset(data, data$day == days[i])
      for(j in 1:nrow(instructions)){
        curr <- instructions[j,]
        filtered_data <- filter_data(curr_data, curr$type, curr$lower_bound, curr$upper_bound, curr$lower_exclusive, curr$upper_exclusive)
        summ_stats[, paste(days[i], curr$name, sep=" ")] <- pnc_summary_statistics(filtered_data$interval)
      }
    }
    
    for(i in 1:nrow(instructions)){
      curr <- instructions[i,]
      filtered_data <- filter_data(data, curr$type, curr$lower_bound, curr$upper_bound, curr$lower_exclusive, curr$upper_exclusive)
      summ_stats[, paste("combined", curr$name, sep=" ")] <- pnc_summary_statistics(filtered_data$interval)
    }
    write.csv(summ_stats, file=file)
  }
}
#Main function ----
main <- function(working_directory){
  setwd(working_directory)
  folders <- Sys.glob(paste(working_directory, "/INPUT/*", sep=""))
  create_directories(working_directory)
  
  instructions <- format_instructions(list("sedentary less than 20 mins", "Sedentary", 0, 20 * 60, TRUE, TRUE,
                                           "sedentary more than 20 mins", "Sedentary", 20*60, Inf, FALSE, TRUE,
                                           "sedentary all bouts", "Sedentary", 0, Inf, TRUE, TRUE,
                                           "stepping less than 20 mins", "Stepping", 0, 20 * 60, TRUE, TRUE,
                                           "stepping more than 20 mins", "Stepping", 20*60, Inf, FALSE, TRUE,
                                           "stepping all bouts", "Stepping", 0, Inf, TRUE, TRUE,
                                           "all bouts less than 20 mins", "All", 0, 20 * 60, TRUE, TRUE,
                                           "all bouts more than 20 mins", "All", 20*60, Inf, FALSE, TRUE,
                                           "all bouts", "All", 0, Inf, TRUE, TRUE))
  for(folder in folders){
    group_data <- data.frame(time=as.Date(character()),
                             datacount=integer(),
                             interval=double(),
                             activity=integer(),
                             cumulativesteps=integer(),
                             methrs=double(),
                             steps=integer())
    files <- Sys.glob(paste(folder, "/*", sep=""))
    for(file in files){
      rm(list=setdiff(ls(), c("SLNW", "filter_data", "pnc_summary_statistics", "get_summary_statistics", "format_instructions", "create_directories", "working_directory", "folders", "instructions", "folder", "group_data", "files", "file")))
      data <- SLNW(file)
      group_data <- rbind(group_data, data)
      write.csv(data, file=paste(working_directory, "/OUTPUT/valid_data/", paste(basename(folder), "valid data", sep=" "), "/", tools::file_path_sans_ext(basename(file)), "-VALID DATA.csv", sep=""), row.names = FALSE)
      get_summary_statistics(paste(working_directory, "/OUTPUT/summary_statistics/", paste(basename(folder), "summary statistics", sep=" "), "/", tools::file_path_sans_ext(basename(file)), "-SUMMARY STATISTICS.csv", sep=""), data, instructions)
    }
  }
}

#Main execution ----
working_directory <- "C:/Users/User/Desktop/PNC_Lab/activPAL-bout-detection"
main(working_directory)
rm(list = ls())