getwd()
setwd("~/Downloads")

library(readr)
loan <- read.csv("loan.csv", header = TRUE, stringsAsFactors = TRUE, na = "empty")

loan1<-subset(loan, loan_status !="Current")
loan2 <- subset(loan1, loan_status != "Issued")
names(loan2)
loan2$member_id <- NULL
loan2$emp_title <- NULL
loan2$url <- NULL
loan2$desc <- NULL
loan2$title <- NULL
loan2$annual_inc_joint <- NULL
loan2$dti_joint <- NULL
loan2$verification_status_joint <- NULL
loan2$open_acc_6m <- NULL
loan2$open_il_6m <- NULL
loan2$open_il_12m <- NULL
loan2$open_il_24m <- NULL
loan2$mths_since_rcnt_il <- NULL
loan2$total_bal_il <- NULL
loan2$il_util <- NULL
loan2$open_rv_12m <- NULL
loan2$open_rv_24m <- NULL
loan2$max_bal_bc <- NULL
loan2$all_util <- NULL
loan2$total_cu_tl <- NULL
loan2$inq_last_12m <- NULL
loan2$inq_fi <- NULL
loan2$mths_since_last_major_derog <- NULL
loan2$mths_since_last_record <- NULL
loan2$next_pymnt_d <- NULL
colSums(is.na(loan2))

summary(loan2)

library(dataQualityR)
checkDataQuality(data = loan2, 
                 out.file.num ="dq_num.csv", 
                 out.file.cat= "dq_cat.csv")
dq_num<-read.csv("~/Downloads/dq_num.csv")
dq_cat<-read.csv("~/Downloads/dq_cat.csv")
View(dq_num)
View(dq_cat)

library(caret)

total_rev_hi_lim.median <- preProcess(loan2, method = c("medianImpute"))
loan.impute <- predict(total_rev_hi_lim.median, loan2)
sum(is.na(loan.impute)) 
summary(loan.impute$total_rev_hi_lim)
table(loan.impute$total_rev_hi_lim)

loan.impute$default<-as.logical(0)
for(i in 1:nrow(loan.impute)){
  if (loan.impute$loan_status[i]=="Fully Paid")
    loan.impute$default[i]<-as.logical(0)
  else if (loan.impute$loan_status[i]=="Does not meet the credit policy. Status:Fully Paid")
    loan.impute$default[i]<-as.logical(0)
  else
    loan.impute$defaul[i]<-as.logical(1)
}
loan.impute$defaul<-as.integer(ifelse(loan.impute$defaul=="TRUE","1","0"))
table(loan.impute$defaul)
prop.table(table(loan.impute$defaul))

loan.dates <- loan.impute
loan.dates$issue_d <- as.character(loan.dates$issue_d)
loan.dates$last_credit_pull_d <- as.character(loan.dates$last_credit_pull_d)
loan.dates$last_pymnt_d <- as.character(loan.dates$last_pymnt_d)
loan.dates$earliest_cr_line <- as.character(loan.dates$earliest_cr_line)

str(loan.dates)

library(zoo)
loan.dates$issue_d <- as.yearmon(loan.dates$issue_d, "%b-%Y")
loan.dates$last_pymnt_d <- as.yearmon(loan.dates$last_pymnt_d, "%b-%Y")
loan.dates$pmnt_months <- as.numeric((loan.dates$last_pymnt_d - loan.dates$issue_d) * 12)
loan.dates$term <- as.character(loan.dates$term)
sub.term <- substr(loan.dates$term, start = 1, stop = 3)
new_T <- as.character(sub.term)
new_T <- as.integer(sub.term)
loan.dates$new_Term <- new_T
loan.dates$pmnt_diff <- as.numeric(loan.dates$new_Term - loan.dates$pmnt_months)
loan.dates$term <- as.factor(loan.dates$term)
