rm(list = ls())
library(activpalProcessing)
library(lubridate)
library(ggplot2)



get_summary_statistics <- function(file, data) {
  write.csv(data, file=file, row.names = FALSE)
}



working_directory <- "C:/Users/User/Desktop/PNC_Lab/activPAL-bout-detection"
setwd(working_directory)
if(!(file.exists("OUTPUT"))){
  stop("OUTPUT folder does not exist.")
}



output <- paste(working_directory, "/OUTPUT", sep="")
folders <- Sys.glob(paste(output, "/valid_data/*", sep=""))
dir.create(file.path(output, "/summary_statistics"), showWarnings = FALSE)
summary_statistics <- paste(output, "/summary_statistics", sep="")



for(folder in folders){
  dir.create(file.path(summary_statistics, basename(folder)), showWarnings = FALSE)
  group_data <- data.frame(time=as.Date(character()),
                           datacount=integer(),
                           interval=double(),
                           activity=integer(),
                           cumulativesteps=integer(),
                           methrs=double(),
                           steps=integer())
  files <- Sys.glob(paste(folder, "/*", sep=""))
  output_folder <- paste(summary_statistics, "/", basename(folder), sep="")
  for(file in files){
    rm(list = setdiff(ls(), c("get_summary_statistics", "working_directory", "output", "folders", "summary_statistics", "folder", "group_data", "files", "output_folder", "file")))
    data <- read.csv(file)
    group_data <- rbind(group_data, data)
    output_file <- paste(output_folder, "/", basename(file), sep="")
    get_summary_statistics(output_file, data)
  }
  get_summary_statistics(paste(output_folder, "/GROUP STATISTICS.csv", sep=""), group_data)
}

