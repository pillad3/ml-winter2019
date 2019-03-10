library(dplyr)
library(tidyverse)
source("R/taxiData.R")
data1m <- taxiData(100000)

data1mDF <- data.frame(data1m$rawData$fare,data1m$rawData$company)
colnames(data1mDF) <- c('fare','company')

data1mOrder <- data1mDF[order(data1mDF$fare,decreasing=TRUE),]