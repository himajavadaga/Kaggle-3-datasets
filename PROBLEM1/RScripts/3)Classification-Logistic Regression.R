#Considering 75% of the sample size
smp_size <- floor(0.75 * nrow(credit_card_defaulters))

#Set the seed
set.seed(123)
sample_ccd <- sample(seq_len(nrow(credit_card_defaulters)),size=smp_size)

#Split the credit_card_defaulters dataset into train and test
train_ccd <- credit_card_defaulters[sample_ccd,]
test_ccd <- credit_card_defaulters[-sample_ccd,]

#Fitting a logistic Regression Model
fit <- glm(Default_Payment~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+repay_status_Sept+repay_status_Aug+repay_status_July+bill_amt_Sept+PAY_Amt_Sept+PAY_Amt_Aug, data=train_ccd, family=binomial(link=logit))

#Run the model on the test dataset
test.prob <- predict(fit,test_ccd, type='response')
pred <- rep("0", length(test.prob))

#set the cutoff value to 0.5
pred[test.prob>=0.5] <- "1"

#Build the classification matrix
library(caret)
confusionMatrix(test_ccd$Default_Payment,pred)

#Create the ROC curve
library(ROCR)
prediction <- prediction(test.prob, test_ccd$Default_Payment)
performance <- performance(prediction, measure="tpr", x.measure="fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#Create a Lift curve
test_ccd$Default_Payment <- as.factor(test_ccd$Default_Payment)
test_ccd$probs=test.prob
test_ccd$prob=sort(test_ccd$probs, decreasing=T)
lift <- lift(Default_Payment ~ prob, data=test_ccd)
lift
xyplot(lift,plot="gain")
