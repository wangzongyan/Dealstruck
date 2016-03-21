# GUIDE description file
source("clean_f.R") # LC <- LC_biz_all.csv
lc.description <- perToN(LC)
lc.description <- replaceBlank_all(lc.description)
lc.description$goodLoan <- 0
lc.description$goodLoan[lc.description$loan_status %in% c("Fully_Paid", "Current")] <- 1
lc.description <- dateToNum(lc.description)
data.name <- "LC_guide.data"
#exclude loan_status, id, last payment day, next payment day, zip_code, issue_d
lc.description <- lc.description %>% select(-id, -loan_status, -emp_title,
                                            -last_pymnt_d, -next_pymnt_d, -zip_code,
                                            -issue_d, -last_credit_pull_d)


description<-function(input, output, respond = "goodLoan", data_name = data.name){
  write.table(data_name, file = output, append = F, quote = F, row.names = F, col.names = F)
  write.table("NA", file = output, append = T, quote = T, row.names = F, col.names = F)
  c2 <- names(input)
  c1 <- 1:length(c2)
  c3 <- lapply(input, function(x) class(x))
  c3 <- unlist(c3)
  c3[c3 == "integer"] <- "N"
  c3[c3 == "factor"] <- "C"
  c3[c3 == "character"] <- "C"
  c3[c3 == "numeric"] <- "N"
  c3[c2 == respond] <- "D"
  if(length(which(c2 == "emp_length_truncate"))>0){ #change truncation data to numeric data
    c3[c2 == "emp_length_truncate"] = "c"
  }
  c <- data.frame(c1 = c1, c2 = c2, c3 = c3)
  write.table(c, file = output, append = T, quote = F, row.names = F, col.names = T)
}
description(lc.description,"lc.txt")
write.table(lc.description, file = data.name, row.names = F, col.names = F, quote = T)

