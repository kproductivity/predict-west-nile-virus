#############################################
# Load requirements
#############################################

library(data.table)
library(MASS)
library(caret)
library(pls); library(klaR)
library(rpart)
library(e1071)

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
train.dt$Date <- as.Date(train.dt$Date)
train.dt$Month <- month(train.dt$Date)
train.dt$Week <- week(train.dt$Date)

# M is missing data
weather.dt[weather.dt == "M"] <- NA

# T is trace; we round to 0 (nil)
weather.dt[weather.dt == "T"] <- 0

weather.dt$Date <- as.Date(weather.dt$Date)
weather.dt$Tavg <- as.numeric(weather.dt$Tavg)
weather.dt$Depart <- as.numeric(weather.dt$Depart)
weather.dt$DewPoint <- as.numeric(weather.dt$DewPoint)
weather.dt$WetBulb <- as.numeric(weather.dt$WetBulb)
weather.dt$Heat <- as.numeric(weather.dt$Heat)
weather.dt$Cool <- as.numeric(weather.dt$Cool)
weather.dt$Water1 <- as.numeric(weather.dt$Water1)
weather.dt$SnowFall <- as.numeric(weather.dt$SnowFall)
weather.dt$PrecipTotal <- as.numeric(weather.dt$PrecipTotal)
weather.dt$StnPressure <- as.numeric(weather.dt$StnPressure)
weather.dt$SeaLevel <- as.numeric(weather.dt$SeaLevel)
weather.dt$AvgSpeed <- as.numeric(weather.dt$AvgSpeed)

weather.1 <- weather.dt[which(weather.dt$Station == 1), ]
weather.1 <- weather.1[ , -1]

fulltrain.dt <- merge(train.dt, weather.1, all.x = TRUE)
fulltrain.dt <- fulltrain.dt[ , -c(2, 3, 7)]

# Transform factor Species into dummy variables
species.dt <- model.matrix(~ Species - 1, data=train.dt)
fulltrain.dt <- cbind(fulltrain.dt, species.dt)

keep <- c(1,2,4,5,7,9,10,11)
test.dt <- test.dt[,keep]
test.dt$Date <- as.Date(test.dt$Date)
test.dt$Month <- month(test.dt$Date)
test.dt$Week <- week(test.dt$Date)
fulltest.dt <- merge(test.dt, weather.1, all.x = TRUE)

# Transform factor Species into dummy variables
species.dt <- model.matrix(~ Species - 1, data=test.dt)
fulltest.dt <- cbind(fulltest.dt, species.dt)
fulltest.dt <- fulltest.dt[ , -c(3, 5, 39)]

#############################################
# Explore data
#############################################




#############################################
# Models
#############################################

# Model 1 - Linear Discriminant Analysis
# 0.50 :(
fit.lda1 <- lda(WnvPresent ~ Latitude+Longitude+Month, data = train.dt)
virus <- predict(fit.lda1, test.dt)$class
sub.dt <- cbind.data.frame(Id = test.dt$Id, WnvPresent = virus)
write.csv(sub.dt, file = "submission.csv", row.names = FALSE, quote = FALSE)

# Model 2 - Linear Discriminant Analysis
# 0.50 :(
fit.lda2 <- lda(WnvPresent ~ Latitude+Longitude+Month+Species, data = train.dt)
virus <- predict(fit.lda2, test.dt)$class

# Model 3 - LDA
fit.lda3 <- lda(WnvPresent ~ Latitude+Longitude+Month+Species+Tmax, data = fulltrain.dt)
virus <- predict(fit.lda3, fulltest.dt)$class

# Model 4 - ML
set.seed(123)

# dt <- na.omit(fulltrain.dt[ , -20])
dt <-  fulltrain.dt[ , -20]

y <- dt[ , 5]
x <- dt[ , -c(1, 5, 16, 17, 18, 19)]

preProc <- preProcess(x, method = c("center", "scale"), thresh = 0.95,
                      k = 5, knnSummary = mean,
                      fudge = .2, numUnique = 3)

x <- predict(preProc, x)

fit.ml <- train(x, y, method = "glmboost",
                preProcess=c("center","scale"))

virus <- predict(fit.ml, fulltest.dt)
#virus.t <- function(t) ifelse(virus > t , 1,0)
#virus <- virus.t(0)

# Model 5
fit.