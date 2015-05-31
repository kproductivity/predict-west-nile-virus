###############################################
# Functions for the West Nile Virus challenge #
###############################################

# Generate predictions and submission file
gensubmission <- function(whichmodel, newdata, type = "response"){
  # which model is the model to use for the prediction
  # newdata is the test data (variable names have to coincide with train names)
  # type is the prediction type (response, terms)
  virus <- predict(whichmodel, newdata, type)
  sub.dt <- cbind.data.frame(Id = newdata$Id, WnvPresent = virus)
  if (identifyNA(sub.dt) == 0) {
    write.csv(sub.dt, file = "submission.csv", row.names = FALSE, quote = FALSE)
    message("Submission file created. Expected RMSE is:")
    #testRMSLE(sub.dt, count, whichmodel)
  } else {
    message("There are NAs and/or negative values in the predicted set!")
    sub.dt[!complete.cases(sub.dt),]
    sub.dt[sub.dt$count < 0, ]
  }  
}

# Test RMSLE
testRMSLE <- function(dt, y, whichmodel){
  # dt is the data table
  # y is the variable to test against the predicted one
  # which model is the model to use for the prediction
  require(Metrics)
  
  # ...with own data (not recommended)
  myrmsle <- rmsle(dt[, y], fitted(whichmodel))
  
  # todo: split data frame to do in-sample testing
  
  return(myrmsle)
  
}

# Identify NAs and Negative values
identifyNA <- function(dt){
  # dt is the data table
  nas <- nrow(!complete.cases(dt))
  neg <- nrow(dt[dt$count < 0, ])
  unfit <- ifelse(length(nas) == 0, 0, nas) +
    ifelse(length(neg) == 0, 0, neg)
  return(unfit)
}

