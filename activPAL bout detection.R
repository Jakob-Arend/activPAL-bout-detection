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
#first we need to go ahead and read in our dataset
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
#MOVE SLNW BOUTS TO NEW DATAFRAME----
#IDENTIFY OTHER BOUTS----
#SUMMARY STATISTICS PER DAY----
#SUMMARY STATISTICS OVERALL----