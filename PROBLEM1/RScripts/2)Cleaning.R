#Choose the credit_card_defaulters file
x=file.choose(new = FALSE)

#read the file and store it in a dataframe
credit_card_defaulters<-read.csv(x, header = TRUE)

#Convert all columns to character to assign the first row as the new Column names
credit_card_defaulters[] <- lapply(credit_card_defaulters, as.character)

#Use the first row as the column names
colnames(credit_card_defaulters) = credit_card_defaulters[1, ]

#Delete the first row
credit_card_defaulters = credit_card_defaulters[-1, ]

library(plyr)

#Renaming the column names
credit_card_defaulters <- rename(credit_card_defaulters, c("PAY_0"="repay_status_Sept", "PAY_2"="repay_status_Aug", "PAY_3"="repay_status_July", "PAY_4"="repay_status_June", "PAY_5"="repay_status_May", "PAY_6"="repay_status_April"))

#Renaming the column names
credit_card_defaulters <- rename(credit_card_defaulters, c("BILL_AMT1"="bill_amt_Sept", "BILL_AMT2"="bill_amt_Aug", "BILL_AMT3"="bill_amt_July", "BILL_AMT4"="bill_amt_June", "BILL_AMT5"="bill_amt_May", "BILL_AMT6"="bill_amt_April"))

#Renaming the column names
credit_card_defaulters <- rename(credit_card_defaulters, c("PAY_AMT1"="PAY_Amt_Sept", "PAY_AMT2"="PAY_Amt_Aug", "PAY_AMT3"="PAY_Amt_July", "PAY_AMT4"="PAY_Amt_June", "PAY_AMT5"="PAY_Amt_May", "PAY_AMT6"="PAY_Amt_April"))

#Renaming the column names
credit_card_defaulters <- rename(credit_card_defaulters, c("default payment next month"="Default_Payment"))

#Renumbering the rows as removing rows changes the sequence
rownames(credit_card_defaulters) <- NULL

#Convert the columns data-types to perform regression
credit_card_defaulters[] <- lapply(credit_card_defaulters, as.numeric)

#Converting the following values to factor as these values are categorical
#credit_card_defaulters$SEX <- as.factor(credit_card_defaulters$SEX)
#credit_card_defaulters$EDUCATION <- as.factor(credit_card_defaulters$EDUCATION)
#credit_card_defaulters$MARRIAGE <- as.factor(credit_card_defaulters$MARRIAGE)
#credit_card_defaulters$repay_status_Sept <- as.factor(credit_card_defaulters$repay_status_Sept)
#credit_card_defaulters$repay_status_Aug <- as.factor(credit_card_defaulters$repay_status_Aug)
#credit_card_defaulters$repay_status_July <- as.factor(credit_card_defaulters$repay_status_July)
#credit_card_defaulters$repay_status_June <- as.factor(credit_card_defaulters$repay_status_June)
#credit_card_defaulters$repay_status_May <- as.factor(credit_card_defaulters$repay_status_May)
#credit_card_defaulters$repay_status_April <- as.factor(credit_card_defaulters$repay_status_April)

#credit_card_defaulters$Default_Payment <- as.factor(credit_card_defaulters$Default_Payment)


#Storing all the zero bill payments and pay amounts in a data frame. 
zero_check <- credit_card_defaulters[(credit_card_defaulters$bill_amt_April==0  & credit_card_defaulters$bill_amt_May==0 & credit_card_defaulters$bill_amt_June==0 & credit_card_defaulters$bill_amt_July==0 & credit_card_defaulters$bill_amt_Aug==0 & credit_card_defaulters$bill_amt_Sept==0 & credit_card_defaulters$PAY_Amt_April==0 & credit_card_defaulters$PAY_Amt_May==0 & credit_card_defaulters$PAY_Amt_June==0 & credit_card_defaulters$PAY_Amt_July==0 & credit_card_defaulters$PAY_Amt_Aug==0 & credit_card_defaulters$PAY_Amt_Sept==0),]

#Removing the values with bill amounts and pay amounts 0 which coincidentally have the repayment statuses as -2 as well.
credit_card_defaulters <- credit_card_defaulters[!(credit_card_defaulters$bill_amt_April==0  & credit_card_defaulters$bill_amt_May==0 & credit_card_defaulters$bill_amt_June==0 & credit_card_defaulters$bill_amt_July==0 & credit_card_defaulters$bill_amt_Aug==0 & credit_card_defaulters$bill_amt_Sept==0 & credit_card_defaulters$PAY_Amt_April==0 & credit_card_defaulters$PAY_Amt_May==0 & credit_card_defaulters$PAY_Amt_June==0 & credit_card_defaulters$PAY_Amt_July==0 & credit_card_defaulters$PAY_Amt_Aug==0 & credit_card_defaulters$PAY_Amt_Sept==0),]

