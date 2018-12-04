library(readr)
loan<- read.csv("~/Desktop/lending-club-loan-data/loan.csv",header = TRUE,stringsAsFactors = TRUE)
str(loan)
summary(loan)

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
loan2$inq_fi<-NULL
loan2$mths_since_last_major_derog<-NULL
loan2$mths_since_last_record<-NULL
loan2$next_pymnt_d<-NULL
sum(is.na(loan2))

library(dataQualityR)
checkDataQuality(data = loan2, 
                 out.file.num ="dq_num.csv", 
                 out.file.cat= "dq_cat.csv")
dq_num<-read.csv("~/Downloads/dq_num.csv")
dq_cat<-read.csv("~/Downloads/dq_cat.csv")
View(dq_num)   # "age" and " date_first_booking" with high perc missing!!
View(dq_cat)   

library(caret)
total_rev_hi_lim.median <- preProcess(loan2, method = c("medianImpute"))
loan.impute <- predict(total_rev_hi_lim.median, loan2)
sum(is.na(loan.impute)) 
summary(loan.impute$total_rev_hi_lim)
table(loan.impute$total_rev_hi_lim)

loan2$default<-as.logical(0)
for(i in 1:nrow(loan2)){
  if (loan2$loan_status[i]=="Fully Paid")
    loan2$default[i]<-as.logical(0)
  else if (loan2$loan_status[i]=="Does not meet the credit policy. Status:Fully Paid")
    loan2$default[i]<-as.logical(0)
  else
    loan2$defaul[i]<-as.logical(1)
}
loan2$defaul<-as.integer(ifelse(loan2$defaul=="TRUE","1","0"))
table(loan2$defaul)
prop.table(table(loan2$defaul))
