getwd()
setwd("~/Downloads")

library(readr)
loan <- read.csv("loan.csv", header = TRUE, stringsAsFactors = TRUE, na = "empty")

loan1<-subset(loan, loan_status !="Current")
loan2 <- subset(loan1, loan_status != "Issued")
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(moments)
library(glmnet)
library(elasticnet)
library(knitr)

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



loan.dates <- loan.dates[ , -which(names(loan.dates) %in% c("id","member_id","verification_status_joint" , "funded_amnt", "funded_amnt_inv", "sub_grade", "issue_d"
                                                            , "url", "desc", "title", "zip_code", "emp_title" , "addr_state", "initial_list_status"
                                                            ,"out_prncp", "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_prncp"
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
Corr_ <- cor(loan.dates)
corrplot(Corr_, method = "square", type = "upper")

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
