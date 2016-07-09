credit_card_defaulters$Default_Payment <- as.factor(credit_card_defaulters$Default_Payment)
library(caret)
set.seed(123)

#Considering 75% of the sample size
smp_size <- floor(0.70 * nrow(credit_card_defaulters))

#Set the seed
set.seed(123)
sample_ccd <- sample(seq_len(nrow(credit_card_defaulters)),size=smp_size)

#Split the credit_card_defaulters dataset into train and test
train_ccd <- credit_card_defaulters[sample_ccd,]
test_ccd <- credit_card_defaulters[-sample_ccd,]

#Classification using neural network
library(nnet)

# Fit a Single Hidden Layer Neural Network using Least Squares, you can maximize the accuracy by changing the size.
train.nnet<-nnet(Default_Payment~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+repay_status_Sept+repay_status_Aug+repay_status_July+bill_amt_Sept+PAY_Amt_Sept+PAY_Amt_Aug,train_ccd,size=10, rang=0.03, HESS=FALSE,decay=15e-4,maxit=250)

#Use TEST data for testing the trained model
test.nnet <- predict(train.nnet,test_ccd,type=("class"))

#MisClassification Confusion Matrix
table(test_ccd$Default_Payment,test.nnet)

#installing and importing the NeuralNetTools package
#install.packages("NeuralNetTools")
#library(NeuralNetTools)
## Plot the trained Neural Network
plotnet(train.nnet)

#Create a Lift curve
library(caret)
library(lift)
test_ccd$probs=test.nnet
test_ccd$prob=sort(test_ccd$probs, decreasing=T)
lift <- lift(Default_Payment ~ prob, data=test_ccd)
lift
xyplot(lift,plot="gain")

#Create the ROC curve
test.nnet <- as.numeric(test.nnet)
library(ROCR)
prediction <- prediction(test.nnet, test_ccd$Default_Payment)
performance <- performance(prediction, measure="tpr", x.measure="fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

