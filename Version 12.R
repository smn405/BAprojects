

library(readr)
loan <- read.csv("loan.csv", header = TRUE, stringsAsFactors = TRUE, na = "empty")

loan1<-subset(loan, loan_status !="Current")
loan2 <- subset(loan1, loan_status != "Issued")
loan_current <- subset(loan, loan_status == "Current" | loan_status == "Issued")

#Indexing current loans 10%
index <- sample(1:nrow(loan_current),(.1)*nrow(loan_current))
loan_current1 <- loan_current [index, ]
prop.table(table(loan_current1$loan_status))

loan_new <- rbind(loan2, loan_current1)



library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(moments)
install.packages("glmnet")
library(glmnet)
install.packages("elasticnet")
library(elasticnet)
library(knitr)

names(loan_new)
loan_new$member_id <- NULL
loan_new$emp_title <- NULL
loan_new$url <- NULL
loan_new$desc <- NULL
loan_new$title <- NULL
loan_new$annual_inc_joint <- NULL
loan_new$dti_joint <- NULL
loan_new$verification_status_joint <- NULL
loan_new$open_acc_6m <- NULL
loan_new$open_il_6m <- NULL
loan_new$open_il_12m <- NULL
loan_new$open_il_24m <- NULL
loan_new$mths_since_rcnt_il <- NULL
loan_new$total_bal_il <- NULL
loan_new$il_util <- NULL
loan_new$open_rv_12m <- NULL
loan_new$open_rv_24m <- NULL
loan_new$max_bal_bc <- NULL
loan_new$all_util <- NULL
loan_new$total_cu_tl <- NULL
loan_new$inq_last_12m <- NULL
loan_new$inq_fi <- NULL
loan_new$mths_since_last_major_derog <- NULL
loan_new$mths_since_last_record <- NULL
loan_new$next_pymnt_d <- NULL
colSums(is.na(loan_new))

summary(loan_new)

library(caret)
loan_new$delinq_2yrs[is.na(loan_new$delinq_2yrs)] <- mean(loan_new$delinq_2yrs, na.rm = TRUE)
loan_new$inq_last_6mths[is.na(loan_new$inq_last_6mths)] <- mean(loan_new$inq_last_6mths, na.rm = TRUE)
loan_new$pub_rec[is.na(loan_new$pub_rec)] <- mean(loan_new$pub_rec, na.rm = TRUE)
loan_new$acc_now_delinq[is.na(loan_new$acc_now_delinq)] <- mean(loan_new$acc_now_delinq, na.rm = TRUE)
loan_new$tot_coll_amt[is.na(loan_new$tot_coll_amt)] <- mean(loan_new$tot_coll_amt, na.rm = TRUE)

loan_median <- preProcess(loan_new, method = c("medianImpute"))
loan.impute <- predict(loan_median, loan_new)
sum(is.na(loan.impute)) 

loan.impute$default<-as.logical(0)
for(i in 1:nrow(loan.impute)){
  if (loan.impute$loan_status[i]=="Fully Paid")
    loan.impute$default[i]<-as.logical(0)
  else if (loan.impute$loan_status[i]=="Does not meet the credit policy. Status:Fully Paid")
    loan.impute$default[i]<-as.logical(0)
  else if (loan.impute$loan_status[i]=="Current")
    loan.impute$default[i]<-as.logical(0)
  else if (loan.impute$loan_status[i]=="Issued")
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
loan.dates$earliest_cr_line <- ifelse(loan.dates$earliest_cr_line == "", loan.dates$issue_d, loan.dates$earliest_cr_line)

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
loan.dates$pmnt_months <- NULL
str(loan.dates)

loan.dates$issue_d <- as.character(loan.dates$issue_d)
loan.dates$last_credit_pull_d <- as.character(loan.dates$last_credit_pull_d)
loan.dates$last_pymnt_d <- as.character(loan.dates$last_pymnt_d)
loan.dates$earliest_cr_line <- as.character(loan.dates$earliest_cr_line)



loan.dates <- loan.dates[ , -which(names(loan.dates) %in% c("id","member_id","verification_status_joint" , "funded_amnt", "funded_amnt_inv", "sub_grade", "issue_d"
                                                            , "url", "desc", "title", "zip_code", "emp_title" , "addr_state", "initial_list_status"
                                                            ,"out_prncp", "out_prncp_inv", "total_pymnt_inv", "total_rec_prncp"
                                                            ,"total_rec_int", "total_rec_late_fee", "collection_recovery_fee", "last_pymnt_d"
                                                            ,"last_pymnt_amnt", "next_pymnt_d", "last_credit_pull_d", "pymnt_plan", "policy_code","application_type"))]


unique(loan.dates$term)
loan.dates$term <- as.integer(gsub("months", "", loan.dates$term))
loan.dates$term[loan.dates$term == 36] <-  1
loan.dates$term[loan.dates$term != 1] <- 0


unique(loan.dates$grade)
loan.dates$grade <- as.character(loan.dates$grade)
loan.dates$grade[loan.dates$grade == "A"] <- 7 
loan.dates$grade[loan.dates$grade == "B"] <- 6
loan.dates$grade[loan.dates$grade == "C"] <- 5
loan.dates$grade[loan.dates$grade == "D"] <- 4
loan.dates$grade[loan.dates$grade == "E"] <- 3
loan.dates$grade[loan.dates$grade == "F"] <- 2
loan.dates$grade[loan.dates$grade == "G"] <- 1
loan.dates$grade <- as.integer(loan.dates$grade)


unique(loan.dates$emp_length)
loan.dates$emp_length <- gsub("<", "", loan.dates$emp_length)
loan.dates$emp_length <- gsub("years", "", loan.dates$emp_length)
loan.dates$emp_length <- gsub("year", "", loan.dates$emp_length)
loan.dates$emp_length <- gsub("n/a", "", loan.dates$emp_length)
loan.dates$emp_length <- gsub(" ", "", loan.dates$emp_length)
loan.dates$emp_length <- gsub("\\+", "", loan.dates$emp_length)
loan.dates$emp_length <- ifelse(loan.dates$emp_length =="", 10, loan.dates$emp_length)
loan.dates$emp_length <- as.integer(loan.dates$emp_length)
unique(loan.dates$emp_length)

unique(loan.dates$home_ownership)
loan.dates$home_ownership <- as.character(loan.dates$home_ownership)
loan.dates$home_ownership[loan.dates$home_ownership=="OWN" | loan.dates$home_ownership=="MORTGAGE"  ] <- 1       
loan.dates$home_ownership[loan.dates$home_ownership!=1] <- 0
loan.dates$home_ownership <- as.numeric(loan.dates$home_ownership)


## Binarization of purpose

loan.dates$purpose <- as.character(loan.dates$purpose)
loan.dates$purpose[loan.dates$purpose == "home_improvement" | loan.dates$purpose == "other" | loan.dates$purpose == "moving" | loan.dates$purpose == "vacation" | 
                     loan.dates$purpose == "major_purchase"| loan.dates$purpose == "small_business"| loan.dates$purpose == "car" | loan.dates$purpose == "medical"|
                     loan.dates$purpose == "house" | loan.dates$purpose == "renewable_energy" | loan.dates$purpose == "wedding"] <- 1
loan.dates$purpose[loan.dates$purpose != 1] <- 0
loan.dates$purpose <- as.numeric(loan.dates$purpose)

## Earliest_Cr_line
head(loan.dates$earliest_cr_line)
loan.dates$earliest_cr_line <- as.character(loan.dates$earliest_cr_line)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
loan.dates$earliest_cr_line <- substrRight(loan.dates$earliest_cr_line, 4)
loan.dates$earliest_cr_line <- as.numeric(loan.dates$earliest_cr_line)


str(loan.dates)

loan.dates$verification_status <- NULL
loan.dates$loan_status <- NULL
loan.dates$earliest_cr_line <- NULL
loan.dates$pub_rec <- NULL
Corr_ <- cor(loan.dates)
loan.dates$term <- as.factor(loan.dates$term)
loan.dates$purpose <- as.factor(loan.dates$purpose)
loan.dates$home_ownership <- as.factor(loan.dates$home_ownership)
loan.dates$default <- as.factor(loan.dates$default)
loan.dates$grade <- as.factor(loan.dates$grade)
loan.dates$emp_length <- as.factor(loan.dates$emp_length)
install.packages("corrplot")
library(corrplot)
corrplot(Corr_, method = "square", type = "upper")

#CREATING BINS
loan.bins <- loan.dates
loan.bins$loan_amnt_band <- cut(loan.bins$loan_amnt, c(0,5000,1000,15000,20000,25000,30000,35000))
summary(loan.bins$loan_amnt_band)
loan.bins$loan_amnt <- NULL

loan.bins$int_rate_band <- cut(loan.bins$int_rate, c(5,10,15,20,25,30))
summary(loan.bins$int_rate_band)
loan.bins$int_rate <- NULL

loan.bins$installment_band <- cut(loan.bins$installment, c(0,100,200,300,500,700,1000,1200,1500))
summary(loan.bins$installment_band)
loan.bins$installment <- NULL

loan.bins$annual_inc_band <- cut(loan.bins$annual_inc, c(0,2000,5000,10000,25000,50000,80000,100000,
                                                         200000,400000,800000,1000000,10000000))
summary(loan.bins$annual_inc_band)
loan.bins$annual_inc <- NULL

loan.bins$dti_band <- cut(loan.bins$dti, c(-1,0,10,20,30,40,50,60,140))
summary(loan.bins$dti_band)
loan.bins$dti <- NULL

loan.bins$delinq_2yrs_band <- cut(loan.bins$delinq_2yrs, c(-1,0,5,10,15,20,25,30))
summary(loan.bins$delinq_2yrs_band)
loan.bins$delinq_2yrs <- NULL

loan.bins$inq_last_6mths_band <- cut(loan.bins$inq_last_6mths, c(-1,0,5,10,15,20,25,30,35))
summary(loan.bins$inq_last_6mths_band)
loan.bins$inq_last_6mths <- NULL

loan.bins$mths_since_last_delinq_band <- cut(loan.bins$mths_since_last_delinq, c(-1,0,10,20,30,50,70,100,130,150,170))
summary(loan.bins$mths_since_last_delinq_band)
loan.bins$mths_since_last_delinq <- NULL

loan.bins$open_acc_band <- cut(loan.bins$open_acc, c(-1,0,10,20,30,40,50,60,70,80))
summary(loan.bins$open_acc_band)
loan.bins$open_acc <- NULL

loan.bins$revol_bal_band <- cut(loan.bins$revol_bal, c(-1,0,2000,5000,10000,25000,50000,80000,100000,
                                                       200000,400000,800000,1000000,3000000))
summary(loan.bins$revol_bal_band)
loan.bins$revol_bal <- NULL

loan.bins$revol_util_band <- cut(loan.bins$revol_util, c(-1,0,10,20,30,40,50,60,70,80,100,300,500,700,1000))
summary(loan.bins$revol_util_band)
loan.bins$revol_util <- NULL

loan.bins$total_acc_band <- cut(loan.bins$total_acc, c(0,10,20,30,40,50,60,70,80,100,150))
summary(loan.bins$total_acc_band)
loan.bins$total_acc <- NULL

loan.bins$total_pymnt_band <- cut(loan.bins$total_pymnt, c(-1,0,1000,5000,10000,20000,30000,40000,50000,60000))
summary(loan.bins$total_pymnt_band)
loan.bins$total_pymnt <- NULL

loan.bins$recoveries_band <- cut(loan.bins$recoveries, c(-1,0,1000,2000,5000,10000,15000,20000,40000))
summary(loan.bins$recoveries_band)
loan.bins$recoveries <- NULL

loan.bins$collections_12_mths_ex_med_band <- cut(loan.bins$collections_12_mths_ex_med, c(-1,0,1,2,3,4,6))
summary(loan.bins$collections_12_mths_ex_med_band)
loan.bins$collections_12_mths_ex_med <- NULL

loan.bins$acc_now_delinq_band <- cut(loan.bins$acc_now_delinq, c(-1,0,1,2,3,4,5))
summary(loan.bins$acc_now_delinq_band)
loan.bins$acc_now_delinq <- NULL

loan.bins$tot_coll_amt_band <- cut(loan.bins$tot_coll_amt, c(-1,0,1000,5000,10000,20000,50000,100000,500000,10000000))
summary(loan.bins$tot_coll_amt_band)
loan.bins$tot_coll_amt <- NULL

loan.bins$tot_cur_bal_band <- cut(loan.bins$tot_cur_bal, c(-1,0,5000,10000,20000,50000,100000,500000,10000000))
summary(loan.bins$tot_cur_bal_band)
loan.bins$tot_cur_bal <- NULL

loan.bins$total_rev_hi_lim_band <- cut(loan.bins$total_rev_hi_lim, c(-1,0,1000,5000,10000,20000,50000,100000,500000,1000000,10000000))
summary(loan.bins$total_rev_hi_lim_band)
loan.bins$total_rev_hi_lim <- NULL

loan.bins$pmnt_diff_band <- cut(loan.bins$pmnt_diff, c(-40,-20,0,10,20,30,40,50,61))
summary(loan.bins$pmnt_diff_band)
loan.bins$pmnt_diff <- NULL

dataDummy <- dummyVars("~.", data = loan.bins, fullRank = F)
data.dummified <- as.data.frame(predict(dataDummy, loan.bins))
data.dummified$default <- as.factor(data.dummified$default)


################################################################################################
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

fitControl.gbm <- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 4)
gbm <- train(train.df[,predictorNames], train.df[,outcomeName],
            method = 'gbm',
            trControl = fitControl.gbm)

library(gbm)
gbmImp <- varImp(gbm, scale = TRUE)
gbmImp
plot(gbmImp, top = 20)

gbmImp.T <- as.data.frame(gbmImp$importance)
write.csv(gbmImp.T, "factors.csv")

gbm.predict <- predict(gbm, test.df[,predictorNames], type = "raw")
confusionMatrix(gbm.predict, test.df[,outcomeName], positive = "1")


####################################################################################################
RF

fitControl.rf <- trainControl(method = "repeatedcv",
                              number = 2,
                              repeats = 2)
rf <- train(train.df[,predictorNames], train.df[,outcomeName],
             method = 'rf',
             trControl = fitControl.rf)

library(rf)
rfImp <- varImp(rf, scale = TRUE)
rfImp
plot(rfImp, top = 20)

rfImp.T <- as.data.frame(rfImp$importance)
write.csv(rfImp.T, "factors.csv")

rf.predict <- predict(rf, test.df[,predictorNames], type = "raw")
confusionMatrix(rf.predict, test.df[,outcomeName], positive = "1")

library(pROC)
library(dplyr)
rf.probs <- predict(rf, test.df[,predictorNames], type = "prob")
rf.plot <- plot(roc(test.df$default, rf.probs[,2]))

band <- as.numeric(rf.probs$`1`)
scoreband <- cut(band,10)
barplot(prop.table(table(scoreband)))
##################################################################################################

set.seed(1234)
split <- (.8)
index <- createDataPartition(loan.dates$default, p=split, list=FALSE)

train.df <- loan.dates[index, ]
test.df <- loan.dates[-index, ]


model_LR <- glm(default ~., data = train.df, family = binomial)
summary(model_LR)

glm.probs <- predict(model_LR, 
                     newdata = test.df, 
                     type = "response")


test.df$prob <- glm.probs
na.omit(test.df, cols="prob")

test.df$preds <- ifelse(test.df$prob <= 0.5 , 0, 1)
test.df$preds <- as.factor(test.df$preds)
test.df$default <- as.factor(test.df$default)

confusionMatrix(test.df$preds, test.df$default)
table(test.df$preds)

table(test.df$default)
table(test.df$preds)