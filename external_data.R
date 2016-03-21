# External data
# jobs median salary
URL <- "http://www.payscale.com/index/US/Job"
tables = readHTMLTable(URL, stringsAsFactors = FALSE)
job <- tables[[1]][1]

# string match
source("clean_f.R")
job <- replaceBlank_all(job)
job1 <- apply(job, 2, function(x) as.character(x))
index <- grep(pattern = "(.*)/(.*)", job1)
job2 <- job1
job2[index] <- sub(pattern = "(.*)(/)(.*)", replacement = "\\1%2f\\3", job1[index])


# job and salary match
function.link<-function(job){
  url <- paste0("http://www.payscale.com/research/US/Job=", job, "/Salary")
  lines<-readLines(url)
  salary.lines<-grep(pattern=".*MEDIAN.*\\d+",x=lines,value=T)
  salary.lines<-grep(pattern=".*,.*", x = salary.lines, value = T)
  salary <- gsub(pattern = ".+nbsp;(.+)", replacement = "\\1", salary.lines)
  names(salary) <- job
  return(salary)
}
salary <- sapply(job2[1:5], function(x) function.link(x))
