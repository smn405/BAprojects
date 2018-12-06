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
loan2$delinq_2yrs[is.na(loan2$delinq_2yrs)] <- mean(loan2$delinq_2yrs, na.rm = TRUE)
loan2$inq_last_6mths[is.na(loan2$inq_last_6mths)] <- mean(loan2$inq_last_6mths, na.rm = TRUE)
loan2$pub_rec[is.na(loan2$pub_rec)] <- mean(loan2$pub_rec, na.rm = TRUE)
loan2$acc_now_delinq[is.na(loan2$acc_now_delinq)] <- mean(loan2$acc_now_delinq, na.rm = TRUE)
loan2$tot_coll_amt[is.na(loan2$tot_coll_amt)] <- mean(loan2$tot_coll_amt, na.rm = TRUE)

loan_median <- preProcess(loan2, method = c("medianImpute"))
loan.impute <- predict(loan_median, loan2)
sum(is.na(loan.impute)) 

loan.impute$default<-as.logical(0)
for(i in 1:nrow(loan.impute)){
  if (loan.impute$loan_status[i]=="Fully Paid")
    loan.impute$default[i]<-as.logical(0)
  else if (loan.impute$loan_status[i]=="Does not meet the credit policy. Status:Fully Paid")
    loan.impute$default[i]<-as.logical(0)
  else
    loan.impute$default[i]<-as.logical(1)
}
loan.impute$default<-as.integer(ifelse(loan.impute$default=="TRUE","1","0"))
table(loan.impute$default)
prop.table(table(loan.impute$default))

loan.dates <- loan.impute
loan.dates$issue_d <- as.character(loan.dates$issue_d)
loan.dates$last_credit_pull_d <- as.character(loan.dates$last_credit_pull_d)
loan.dates$last_pymnt_d <- as.character(loan.dates$last_pymnt_d)
loan.dates$earliest_cr_line <- as.character(loan.dates$earliest_cr_line)
loan.dates$earliest_cr_line <- ifelse(is.na(loan.dates$earliest_cr_line), loan.dates$issue_d, loan.dates$earliest_cr_line)

str(loan.dates)

library(zoo)
loan.dates$issue_d <- as.yearmon(loan.dates$issue_d, "%b-%Y")
loan.dates$last_pymnt_d <- as.yearmon(loan.dates$last_pymnt_d, "%b-%Y")
loan.dates$earliest_cr_line <- as.yearmon(loan.dates$earliest_cr_line, "%b-%Y")
loan.dates$last_credit_pull_d <- as.yearmon(loan.dates$last_credit_pull_d, "%b-%Y")
loan.dates$pmnt_months <- as.numeric((loan.dates$last_pymnt_d - loan.dates$issue_d) * 12)
loan.dates$pmnt_months <- as.integer(loan.dates$pmnt_months)
loan.dates$pmnt_months <- ifelse(is.na(loan.dates$pmnt_months), -1, loan.dates$pmnt_months)
loan.dates$term <- as.character(loan.dates$term)
sub.term <- substr(loan.dates$term, start = 1, stop = 3)
new_T <- as.character(sub.term)
new_T <- as.integer(sub.term)
loan.dates$new_Term <- new_T
loan.dates$pmnt_diff <- as.numeric(loan.dates$new_Term - loan.dates$pmnt_months)
loan.dates$term <- as.factor(loan.dates$term)
loan.dates$new_Term <- NULL
str(loan.dates)

#Creating Bins
loan_bins <- loan.dates
loan_bins$loan_amnt_band <- cut(loan_bins$loan_amnt, c(0,5000,1000,15000,20000,25000,30000,35000))
summary(loan_bins$loan_amnt_band)

loan_bins$funded_amnt_band <- cut(loan_bins$funded_amnt, c(0,5000,1000,15000,20000,25000,30000,35000))
summary(loan_bins$funded_amnt_band)

loan_bins$funded_amnt_inv_band <- cut(loan_bins$funded_amnt_inv, c(-1,0,5000,1000,15000,20000,25000,30000,35000))
summary(loan_bins$funded_amnt_inv_band)

loan_bins$int_rate_band <- cut(loan_bins$int_rate, c(5,10,15,20,25,30))
summary(loan_bins$int_rate_band)

loan_bins$installment_band <- cut(loan_bins$installment, c(0,100,200,300,500,700,1000,1200,1500))
summary(loan_bins$installment_band)

loan_bins$annual_inc_band <- cut(loan_bins$annual_inc, c(0,2000,5000,10000,25000,50000,80000,100000,
                                                         200000,400000,800000,1000000,5000000,10000000))
summary(loan_bins$annual_inc_band)

loan_bins$dti_band <- cut(loan_bins$dti, c(-1,0,10,20,30,40,50,60))
summary(loan_bins$dti_band)

loan_bins$delinq_2yrs_band <- cut(loan_bins$delinq_2yrs, c(-1,0,5,10,15,20,25,30))
summary(loan_bins$delinq_2yrs_band)

loan_bins$inq_last_6mths_band <- cut(loan_bins$inq_last_6mths, c(-1,0,5,10,15,20,25,30,35))
summary(loan_bins$inq_last_6mths_band)

loan_bins$mths_since_last_delinq_band <- cut(loan_bins$mths_since_last_delinq, c(-1,0,10,20,30,50,70,100,130,150,170))
summary(loan_bins$mths_since_last_delinq_band)

loan_bins$open_acc_band <- cut(loan_bins$open_acc, c(-1,0,10,20,30,40,50,60,70,80))
summary(loan_bins$open_acc_band)

loan_bins$pub_rec_band <- cut(loan_bins$pub_rec, c(-1,0,5,10,15,20))
summary(loan_bins$pub_rec_band)

loan_bins$revol_bal_band <- cut(loan_bins$revol_bal, c(-1,0,2000,5000,10000,25000,50000,80000,100000,
                                                       200000,400000,800000,1000000,2000000))
summary(loan_bins$revol_bal_band)

loan_bins$revol_util_band <- cut(loan_bins$revol_util, c(-1,0,10,20,30,40,50,60,70,80,100,300,500,700,1000))
summary(loan_bins$revol_util_band)

loan_bins$total_acc_band<- cut(loan_bins$total_acc, c(0,10,20,30,40,50,60,70,80,100,150))
summary(loan_bins$total_acc_band)

loan_bins$out_prncp_band <- cut(loan_bins$out_prncp, c(-1,0,1000,5000,10000,20000,30000,40000))
summary(loan_bins$out_prncp_band)

loan_bins$out_prncp_inv_band <- cut(loan_bins$out_prncp_inv, c(-1,0,1000,5000,10000,20000,30000,40000))
summary(loan_bins$out_prncp_inv_band)

loan_bins$total_pymnt_band <- cut(loan_bins$total_pymnt, c(-1,0,1000,5000,10000,20000,30000,40000,50000,60000))
summary(loan_bins$total_pymnt_band)

loan_bins$total_pymnt_inv_band <- cut(loan_bins$total_pymnt_inv, c(-1,0,1000,5000,10000,20000,30000,40000,50000,60000))
summary(loan_bins$total_pymnt_inv_band)

loan_bins$total_rec_prncp_band <- cut(loan_bins$total_rec_prncp, c(-1,0,1000,5000,10000,20000,30000,40000))
summary(loan_bins$total_rec_prncp_band)

loan_bins$total_rec_int_band <- cut(loan_bins$total_rec_int, c(-1,0,1000,2000,5000,10000,15000,25000))
summary(loan_bins$total_rec_int_band)

loan_bins$total_rec_late_fee_band <- cut(loan_bins$total_rec_late_fee, c(-1,0,10,20,40,50,100,200,400))
summary(loan_bins$total_rec_late_fee_band)

loan_bins$recoveries_band <- cut(loan_bins$recoveries, c(-1,0,1000,2000,5000,10000,15000,20000,40000))
summary(loan_bins$recoveries_band)

loan_bins$collection_recovery_fee_band <- cut(loan_bins$collection_recovery_fee, c(-1,0,1000,2000,3000,4000,5000,8000))
summary(loan_bins$collection_recovery_fee_band)

loan_bins$last_pymnt_amnt_band <- cut(loan_bins$last_pymnt_amnt, c(-1,0,1000,2000,5000,10000,15000,20000,40000))
summary(loan_bins$last_pymnt_amnt_band)

loan_bins$collections_12_mths_ex_med_band <- cut(loan_bins$collections_12_mths_ex_med, c(-1,0,1,2,3,4,6))
summary(loan_bins$collections_12_mths_ex_med_band)

loan_bins$acc_now_delinq_band <- cut(loan_bins$acc_now_delinq, c(-1,0,1,2,3,4,5))
summary(loan_bins$acc_now_delinq_band)

loan_bins$tot_coll_amt_band <- cut(loan_bins$tot_coll_amt, c(-1,0,1000,5000,10000,20000,50000,100000,500000,10000000))
summary(loan_bins$tot_coll_amt_band)

loan_bins$tot_cur_bal_band <- cut(loan_bins$tot_cur_bal, c(-1,0,5000,10000,20000,50000,100000,500000,10000000))
summary(loan_bins$tot_cur_bal_band)

loan_bins$total_rev_hi_lim_band <- cut(loan_bins$total_rev_hi_lim, c(-1,0,1000,5000,10000,20000,50000,100000,500000,1000000,5000000))
summary(loan_bins$total_rev_hi_lim_band)

loan_bins$pmnt_months_band <- cut(loan_bins$pmnt_months, c(-2,-1,0,10,20,30,40,50,60,70))
summary(loan_bins$pmnt_months_band)

loan_bins$pmnt_diff_band <- cut(loan_bins$pmnt_diff, c(-40,-20,0,10,20,30,40,50,61))
summary(loan_bins$pmnt_diff_band)

loan_bins$loan_amnt <- NULL
loan_bins$funded_amnt <- NULL
loan_bins$funded_amnt_inv <- NULL
loan_bins$int_rate <- NULL
loan_bins$installment <- NULL
loan_bins$annual_inc <- NULL
loan_bins$dti <- NULL
loan_bins$delinq_2yrs <- NULL
loan_bins$inq_last_6mths <- NULL
loan_bins$mths_since_last_delinq <- NULL
loan_bins$open_acc <- NULL
loan_bins$pub_rec <- NULL
loan_bins$revol_bal <- NULL
loan_bins$revol_util <- NULL
loan_bins$total_acc <- NULL
loan_bins$out_prncp <- NULL
loan_bins$out_prncp_inv <- NULL
loan_bins$total_pymnt <- NULL
loan_bins$total_pymnt_inv <- NULL
loan_bins$total_rec_prncp <- NULL
loan_bins$total_rec_int <- NULL
loan_bins$total_rec_late_fee <- NULL
loan_bins$recoveries <- NULL
loan_bins$collection_recovery_fee <- NULL
loan_bins$last_pymnt_amnt <- NULL
loan_bins$collections_12_mths_ex_med <- NULL
loan_bins$acc_now_delinq <- NULL
loan_bins$tot_coll_amt <- NULL
loan_bins$tot_cur_bal <- NULL
loan_bins$total_rev_hi_lim <- NULL
loan_bins$pmnt_months <- NULL
loan_bins$pmnt_diff <- NULL
loan_bins$issue_d <- NULL
loan_bins$last_pymnt_d <- NULL
loan_bins$last_credit_pull_d <- NULL
loan_bins$earliest_cr_line <- NULL
loan_bins$policy_code <- NULL

loan.clean <- loan_bins
str(loan.clean)

library(dplyr)
names(loan.clean)
loan.df <- select(loan.clean,
                 id,
                 term,
                 grade,
                 emp_length,
                 home_ownership,
                 verification_status,
                 purpose,
                 addr_state,
                 initial_list_status,
                 application_type,
                 loan_amnt_band,
                 funded_amnt_band,
                 funded_amnt_inv_band,
                 int_rate_band,
                 installment_band,
                 annual_inc_band,
                 dti_band,
                 delinq_2yrs_band,
                 inq_last_6mths_band,
                 mths_since_last_delinq_band,
                 open_acc_band,
                 pub_rec_band,
                 revol_bal_band,
                 revol_util_band,
                 total_acc_band,
                 out_prncp_band,
                 out_prncp_inv_band,
                 total_pymnt_band,
                 total_pymnt_inv_band,
                 total_rec_prncp_band,
                 total_rec_int_band,
                 total_rec_late_fee_band,
                 recoveries_band,
                 collection_recovery_fee_band,
                 last_pymnt_amnt_band,
                 collections_12_mths_ex_med_band,
                 acc_now_delinq_band,
                 tot_coll_amt_band,
                 tot_cur_bal_band,
                 total_rev_hi_lim_band,
                 pmnt_months_band,
                 pmnt_diff_band,
                 default)

str(loan.df)
dataDummy <- dummyVars("~.", data = loan.df, fullRank = F)
data.dummified <- as.data.frame(predict(dataDummy, loan.df))
data.dummified$default <- as.factor(data.dummified$default)

index <- sample(1:nrow(data.dummified),(.05)*nrow(data.dummified))
part.dummified <- data.dummified [index, ]
prop.table(table(part.dummified$default))

outcomeName <- 'default'
predictorNames <- names(part.dummified)[names(part.dummified) != outcomeName]

set.seed(1234)
split <- (.8)
index <- createDataPartition(part.dummified$default, p=split, list=FALSE)

train.df <- part.dummified[index, ]
test.df <- part.dummified[-index, ]

fitControl.rf <- trainControl(method = "none")
rf <- train(train.df[,predictorNames], train.df[,outcomeName],
            method = 'rf',
            trControl = fitControl.rf)

rfImp <- varImp(rf, scale = TRUE)
rfImp
plot(rfImp, top = 20)

rfImp.T <- as.data.frame(rfImp$importance)
write.csv(rfImp.T, "factors.csv")

rf.predict <- predict(rf, test.df[,predictorNames], type = "raw")
confusionMatrix(rf.predict, test.df[,outcomeName], positive = "1")

library(pROC)
rf.probs <- predict(rf, test.df[,predictorNames], type = "prob")
rf.plot <- plot(roc(test.df$default, rf.probs[,2]))

band <- as.numeric(rf.probs$`1`)
scoreband <- cut(band,10)
barplot(prop.table(table(scoreband)))

save(rf, file = "RFmodel.Rdata1")
