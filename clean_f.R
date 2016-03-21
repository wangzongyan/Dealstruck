# Data clean
## Change all the percentage to numbers
perToN <- function(data){
  var1 <- which(lapply(data[1,], function(x) grep(pattern = "%", x)) == 1)
  lc.clean <- data
  lc.clean[,var1] <- lapply(LC[,var1], function(x) sub(pattern = "%", replacement = "", x))
  lc.clean[,var1] <- lapply(lc.clean[,var1], function(x) as.numeric(x)/100)
  return(lc.clean)
}


## find truncated variables, and change truncated variables to numbers
truncateToN <- function(LC){
  lc.clean <- LC
  var2 <- lapply(LC, function(x) grep(pattern = "\\+", x))
  LC$emp_title[5060] #Engineer+
  str(LC$annual_inc) #grep "+" because of scientific notation
  str(LC$emp_length) #Truncated variables, sorted categrical variable
  lc.clean$emp_length_truncate <- rep(0, length(lc.clean[,1]))
  truncate.order <- grep(pattern = "\\+", LC$emp_length)
  lc.clean$emp_length_truncate[truncate.order] <- 1
  lc.clean$emp_length <- lapply(LC$emp_length, 
                                function(x) sub(pattern = "(\\w+)(\\+)*(\\ )(.*)", replacement = "\\1", x))
  lc.clean$emp_length[lc.clean$emp_length == "< 1"] <- 0
  lc.clean$emp_length <- as.numeric(lc.clean$emp_length)
  all(which(is.na(lc.clean$emp_length)) == which(LC$emp_length == "n/a"))
  return(lc.clean)
}

## replace blank space to "_"
replaceBlank <- function(var){
  var <- as.character(var)
  var <- gsub(pattern = " ", replacement = "_", var)
  var <- as.factor(var)
  return(var)
}

## replace blank missing data to "NA"


blankNA <- function(entry){
  if (entry == ""){
    entry <- NA
  }
  return(entry)
}

blankNA_row <- function(var){
  if(class(var) == "factor"){
    var <- as.character(var)
  }
  var <- lapply(var, function(x) blankNA(x))
  return(unlist(var))
}


# replace all the factor variable's blank space to "_", and all the "" to "NA"
replaceBlank_all <- function(data){
  type <- lapply(data, function(x) class(x))
  data[type == "character"] <- lapply(data[type == "character"], function(x) replaceBlank(x)) 
  data[type == "factor"] <- lapply(data[type == "factor"], function(x) replaceBlank(x))   
  return(data)
}




# Check how many NA in a variable
hasNA_row <- function(var){
    return(sum(is.na(var)))
}

# Check how many NA in variables among whole data
hasNA_all <- function(data){
  k <- sapply(data, function(x) hasNA_row(x))
  return(k[k>0])
}

# number of levels in factor variable
n.factor <- function(var){
  return(length(levels(var)))
}

# number of levels in factor variables among the data
n.factor_all <- function(data){
  x <- data %>% lapply(., function(x) n.factor(x))
  return(as.data.frame(x[x>0]))
}

# change the variable earliest_cr_line from date to numeric, begin at jan, 1960
dateToNum <- function(data){
  test_that("data should include variable earliest_cr_line", { # should include addr_state
    expect_equal(is.null(data$earliest_cr_line), F)
  })
  data$earliest_cr_line <- as.character(data$earliest_cr_line)
  ## form1 is (x)x-mmm
  form1 <- grep(pattern = "(\\d+)-(\\w+)", x = data$earliest_cr_line, value = T)
  form1.index <- grep(pattern = "(\\d+)-(\\w+)", x = data$earliest_cr_line)
  year1 <- as.numeric(sub(pattern = "(\\d+)-(\\w+)", x = form1, replacement = "\\1"))+2000
  month1 <- sub(pattern = "(\\d+)-(\\w+)", x = form1, replacement = "\\2")
  month1 <- as.numeric(match(month1,month.abb))
  m1 <-  (year1 - 1960)*12 + month1 
  ## form 2 is mmm-xx
  form2 <- grep(pattern = "(\\w+)-(\\d+)", x = data$earliest_cr_line, value = T)
  form2.index <- grep(pattern = "(\\w+)-(\\d+)", x = data$earliest_cr_line)
  year2 <- as.numeric(sub(pattern = "(\\w+)-(\\d+)", x = form2, replacement = "\\2"))+1900
  month2 <- sub(pattern = "(\\w+)-(\\d+)", x = form2, replacement = "\\1")
  month2 <- as.numeric(match(month2, month.abb))
  m2 <- (year2 - 1960)*12 + month2
  ## return back m1, m2
  data$earliest_cr_line[form1.index] <- m1
  data$earliest_cr_line[form2.index] <- m2
  return(data)
}


