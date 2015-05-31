#############################################
# Load requirements
#############################################

library(data.table)

setwd("~/GitHub/predict-west-nile-virus")
source("functions.R") # load functions

#############################################
# Read data
#############################################

temp <- unzip(paste(getwd(), "/data/train.csv.zip", sep=""))
train.dt <- read.table(temp, header=TRUE, sep=",")

temp <- unzip(paste(getwd(), "/data/test.csv.zip", sep=""))
test.dt <- read.table(temp, header=TRUE, sep=",")

temp <- unzip(paste(getwd(), "/data/spray.csv.zip", sep=""))
spray.dt <- read.table(temp, header=TRUE, sep=",")

temp <- unzip(paste(getwd(), "/data/weather.csv.zip", sep=""))
weather.dt <- read.table(temp, header=TRUE, sep=",")

rm(temp) # Remove temp object from memory

mapdata <- readRDS(paste(getwd(), "/data/mapdata_copyright_openstreetmap_contributors.rds", sep=""))

#############################################
# Clean and transform data
#############################################

keep <- c(1,3,6,8,9,10,11,12)
train.dt <- train.dt[,keep]

keep <- c(1,2,4,5,7,9,10,11)
test.dt <- test.dt[,keep]


#############################################
# Explore data
#############################################




#############################################
# Models
#############################################

