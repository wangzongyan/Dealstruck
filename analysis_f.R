###############################
# analysis.R
##This file provide the functions for building models:
## transf_loan(data = "lc", level = "Charged_Off")
#' @return data.frame with y = 1 if data$loan_status = level, 0 otherwise
#
## log_loan(data = "lc", level = "Charged_Off") 
#' @return logistic regression: log(Odds(P(loan_status = level))) ~ . 
#
## model(glm.fun, level = "Charged_Off")
#' @param glm.fun a logistic function of loan_status
#' @return an expression of model
#' 
## glm_list(data, status = status)
#' @return include a list of the glm functions related to status
#' 
## predict_glm(list.l, newdata, status = status)
#' @param list.l a list of all the glm functions included in our model
#' @newdata newdata waiting to be classified
#' @return a vector contains the estimated class
#' 
## cv_k(data, k)
#' @param data is the data want to use glm and apply cross-validation
#' @param number of fold of cross-validation
#' @return cross validation error (random value)
#' 
#' 
#' 
#' 
##############################
#require(plyr)
#require(dplyr)
#require(ggplot2)
#require("XML")
#require("reshape2")
###############################
#Logistic Regression
###############################

# Transform loan_status from multi-level to binary and create data.frame
# data$y = 1 if level, data$y = 0 otherwise
transf_loan <- function(data, level){
  test_that("level is not a level of loan_status",{
    expect_equal(level%in%levels(data$loan_status), T)
  })
  lc_y <- data %>% 
    mutate(y = as.numeric(.$loan_status == level)) %>%
    dplyr::select(-loan_status)
  return(lc_y)
}

# logistic regression
logistic <- function(data){
  if(length(data$weight) >0){
    l<-glm(y~., data, weights = weight, family=binomial(link = "logit"))
  }else{
    l<-glm(y~., data, family=binomial(link = "logit"))
  }
  
  return(l)
}

# Logistic regression for one particular level of loan_status
log.loan <- function(data, level){
  transform_data <- transf_loan(data, level)
  return(logistic(transform_data))
}

# model
model <- function(glm.fun, level){
  glm.fun$coefficients[is.na(glm.fun$coefficients)] <- 0
  paste0("odds(p(", level, ")) = ", "exp(", 
         paste0("(", glm.fun$coefficients, ")", "*", 
                names(glm.fun$coefficients), collapse = " + "), ")")
}

# Create a list of the glm regressions
glm_list <- function(data, status = status){
  l.List <- list()
  for(i in 1:length(status)){
    l.List[[i]] <- log.loan(data,status[i])
  }
  return(l.List)
  
}



###################
## Aggregate logistic prediction model
######################

# predict new data on multiple level
predict_glm <- function(list.l, newdata, status = status){
  p.dataframe <- data.frame(index = 1:dim(newdata)[1])
  for(i in 1: length(status)){
    level <- status[i]
    p.dataframe[,level] <- predict(list.l[[i]], newdata, type = "response")
  }  
  p.dataframe <- p.dataframe %>% select(-index)
  pred <- p.dataframe %>% 
    mutate(max = apply(., 1, function(x) names(p.dataframe)[which.max(x)]))
  pred <- as.data.frame(pred)
  return(pred$max)
}

## Cross-Validation resample
cv_k <- function(data, k = 10){
  flds <- createFolds(1:length(data[,1]), k = k, list = TRUE, returnTrain = FALSE)
  error <- c()
  cv.error <- c()
  cv.weight <- 1
  cv_error <- 1
    for(i in 1:10){
      test <- data[flds[[i]],]
      train <- data[-flds[[i]],]
      l.list <- glm_list(train, status)
      cv_pred <- predict_glm(l.list, test, status)
      y <- as.character(test$loan_status)
      error[i] <- 1 - sum(cv_pred == y)/length(y)
    }
  cv.error <- mean(error)
  return(cv.error)
}

## misclassification plot
# misclassification
mis <- function(true, pred){
  level <- as.character(levels(true))
  true <- as.character(true)
  pred <- as.character(pred)
  data <- data.frame(true = true, pred = pred)
  true <- as.factor(true)
  l <- list()
  for (i in level){
    data1 <- data %>% subset(true == i)
    for (j in level[level %in% pred]){
      l[[i]] <- table(data1$pred)/length(data1$pred)
    }
  }
  return(l)
}


####################
# SVM
######################
prep_svm <- function(data){
  lc_svm <- data
  lc_svm <- lc_svm %>% mutate(term_36 = as.numeric(lc_svm$term == "_36_months")) %>%
    mutate(verification_source = as.numeric(.$verification_status == "Source_Verified")) %>% 
    mutate(verification_Verified = as.numeric(lc_svm$verification_status == "Verified"))
  lc_svm$term <- 0
  lc_svm$verification_status <- 0
  return(lc_svm)
}


svm <- function(lc_svm, one){
  # an example of R code, goodLoan = Fully_paid or current loan status
  # change multi-level response to binary response
  lc_svm$goodLoan <- 0
  lc_svm$goodLoan[lc_svm$loan_status == one] <- 1
  lc_svm <- lc_svm %>% na.omit()  %>%  select(-loan_status)
    # SVM
  SVM_X <- as.matrix(lc_svm %>% select(-goodLoan))
  SVM_Y <- as.matrix(lc_svm %>% select(goodLoan))

  SVM_GaussianKernel <- ksvm(SVM_X, SVM_Y, kernel = "rbfdot", type = "C-svc", prob.model=TRUE)
 
  return(SVM_GaussianKernel)
}

### Predict with kernel SVM
kern_list <- function(data){
  l.List <- list()
  status <- levels(data$loan_status)
  for(i in status){
      l.List[[i]] <-svm(data,i)
  }
  return(l.List)
  
}



pred.svm <- function(kern.list, newdata, status = status){
  newdata <- as.matrix(newdata)
  p.dataframe <- data.frame(index = 1:dim(newdata)[1])
  for(i in 1: length(status)){
    level <- status[i]
    p.dataframe[,level] <- predict(kern.list[[i]], newdata, type="probabilities")[,2]
  }  
  p.dataframe <- p.dataframe %>% select(-index)
  pred <- p.dataframe %>% 
    mutate(max = apply(., 1, function(x) names(p.dataframe)[which.max(x)]))
  pred <- as.data.frame(pred)
  return(pred$max)
 
}
