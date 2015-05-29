#############################################
# Load requirements
#############################################

pkg <- c("data.table")

for (i in pkg) {
  if(!require(i, character.only = TRUE)) install.packages(i, character.only = TRUE)
  library(i, character.only = TRUE)
}

source("functions.R") # load functions

#############################################
# Read data
#############################################



#############################################
# Clean and transform data
#############################################



#############################################
# Explore data
#############################################
