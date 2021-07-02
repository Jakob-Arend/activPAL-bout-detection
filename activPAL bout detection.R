#Jakob Arend
library(sjmisc)
library(NLP)
library(rlist)
setwd("C:/Users/User/Desktop/PNC Lab/activPAL SLNW detection algorithm")


test <- activpalProcessing::activpal.file.reader("data/SA008-SA008-AP840031 9Apr19 12-00am for 13d 16h 23m-VANE-PB08090417-Events.csv")
test <- test[-c(1,2),]
test

test2 <- activpalProcessing::activpal.file.reader("data/SA008-SA008-AP840031 9Apr19 12-00am for 13d 16h 23m-VANE-PB08090417-Events.csv")
test2$time <- factor(test2$time)
test2 <- activpalProcessing::second.by.second(test2)
test2 <- test2[-c(1:86400),]
test2

head(test2)
head(test)

test2[61343:61352,]
