#SETUP----
#first we import all the libraries which we utilize
#note here that you will need the activPAL package installed which has a dependency on the "chron" package
#the most current activPAL package can be found at https://cran.r-project.org/src/contrib/Archive/activpalProcessing/activpalProcessing_1.0.2.tar.gz
library(sjmisc)
library(NLP)
library(rlist)

#next up we need to set our working directory so that we can access our data
#set variable "wd" equal to a string containing the full path to the directory you intend to work from
wd <- "C:/Users/User/Desktop/PNC_Lab/activPAL-bout-detection"
setwd(wd)

#finishing our setup we now just have to provide the path to our data
#set variable "data_path" equal to a string containing the partial path FROM YOUR WORKING DIRECTORY
data_path <- "sample_data/SA009-SA009-AP840032 11Apr19 12-00am for 12d 16h 22m-VANE-PB08090417-Events.csv"

#now we're ready to go ahead and run our activPAL bout detection data reduction on our dataset
data <- activpalProcessing::activpal.file.reader(data_path)

#IDENTIFY 7 DAY WINDOW----
#we need to identify which 7 days our participant actually participated in data collection
#we'll do this by identifying the 7 days with the most steps

#first we count how many steps occur on each day
days <- 0
steps <- data.frame(days)
for(i in 1:nrow(data)){
  date <- substr(data[i, 1], 1, 10)
  if(!(date %in% colnames(steps))){
    steps[, date] <- data[i, 5]
    steps[1, "days"] <- steps[1, "days"] + 1
  } else {
    steps[1, date] <- data[i, 5]
  }
}
steps$days <- NULL

#next we remove down to the 7 days with the largest step counts and save those days into a list"valid_days"
while(ncol(steps) > 7){
  least_steps <- min(steps, na.rm = TRUE)
  for(i in 1:ncol(data)){
    if(least_steps == steps[1, i]){
      steps[i] <- NULL
      break
    }
  }
}
valid_days = colnames(steps)

#lastly just as a nicety we'll clean up our working env
rm(steps, date, days, i, least_steps)

#CUT DATA TO 7 DAY WINDOW----
#now that we have a list of valid days, we can cut out all data which took place outside of the experiment
days_to_remove <- c()
for(i in 1:nrow(data)){
  if(!(substr(data[i, 1], 1, 10) %in% valid_days)){
    days_to_remove <- c(days_to_remove, i)
  }
}
data <- data[-c(days_to_remove),]

#as before let's clean up our working env for clarity
rm(i, days_to_remove)

#IDENTIFY SLNW BOUTS----
#First we need to partition out our data into noon-noon days so we can effectively search for sleep
curr_day <- paste(valid_days[1], "12:00:00", sep = " ")
day_counter <- 1
noon_days <- c()
for(i in 1:nrow(data)){
  if(as.numeric(difftime(data[i, 1], curr_day), units = "secs") > 0) {
    noon_days <- c(noon_days, i)
    day_counter <- day_counter + 1
    if(day_counter > length(valid_days)){
      break
    } else {
      curr_day <- paste(valid_days[day_counter], "12:00:00", sep = " ")
      
    }
  }
}

#To account for the case that our day 7 data never crossed the noon threshold we add a fake index at one past the array
if(day_counter == length(valid_days)){
  noon_days <- c(noon_days, nrow(data) + 1)
}


#Here we'll define two helper functions for later--namely to search forwards and backwards for potential SLNW bouts
search_forwards <- function(sleep_index, curr_noon, noon_days, data){
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

search_backwards <- function(sleep_index, curr_noon, noon_days, data){
  backward_SLNW <- c()
  curr_position <- sleep_index - 1
  zero_check = 1
  if(curr_noon != 1){
    zero_check = noon_days[curr_noon - 1]
  }
  while(curr_position > zero_check && (as.numeric(difftime(data[sleep_index, 1], data[curr_position, 1]), units = "secs") - data[curr_position, 3]) <= 900){
    if((data[curr_position, 3] >= 7200 && data[curr_position, 4] < 2)){
      backward_SLNW <- c(curr_position, backward_SLNW)
    }else if(data[curr_position, 3] <= 1800 && data[curr_position, 4] < 2 && (data[curr_position, 5] - data[curr_position - 1, 5]) <= 20){
      backward_SLNW <- c(curr_position, backward_SLNW)
    }else if(curr_position != 1 && data[curr_position, 5] == data[curr_position - 1, 5]){
      backward_SLNW <- c(curr_position, backward_SLNW)
    }
    curr_position <- curr_position - 1
  }
  return(backward_SLNW)
}

#Now we can iterate through each day and identify our sleep periods--this is version B of the SLNW alg parts 1-2
all_SLNW <- c()
for(i in 1:(length(noon_days) - 1)){
  sleep_indices = c()
  longest_bout = 0
  for(j in noon_days[i]:(noon_days[i + 1] - 1)){
    if(data[j, 3] >= 18000 && data[j, 4] < 2){
      sleep_indices <- c(sleep_indices, j)
    } else if(data[j, 3] >= 7200 && data[j, 4] < 2){
      longest_bout = j
    }
  }
  if(length(sleep_indices) == 0){
    sleep_indices <- c(sleep_indices, longest_bout)
  }
  for(j in 1:length(sleep_indices)){
    all_SLNW <- c(all_SLNW, sleep_indices[j], search_forwards(sleep_indices[j], i, noon_days, data))
  }
}
all_SLNW <- all_SLNW[!duplicated(all_SLNW)]
print(data[all_SLNW,])

#MOVE SLNW BOUTS TO NEW DATAFRAME----
#IDENTIFY OTHER BOUTS----
#SUMMARY STATISTICS PER DAY----
#SUMMARY STATISTICS OVERALL----
