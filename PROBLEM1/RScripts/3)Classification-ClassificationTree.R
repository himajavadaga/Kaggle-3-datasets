Default_Payment = credit_card_defaulters$Default_Payment
credit_card_defaulters$Default_Payment <- as.factor(credit_card_defaulters$Default_Payment)
#Fit the Classification tree
library(tree)
tree=tree(Default_Payment~ repay_status_Sept+repay_status_Aug, credit_card_defaulters)
summary(tree)

#displaying the tree structure
plot(tree)
text(tree,pretty=0)

#Split the dataset into a training set and a test set
set.seed (2)
train.ccd = sample (1:nrow(credit_card_defaulters), nrow(credit_card_defaulters)/2)

ccd.test = credit_card_defaulters[-train.ccd,]
Default_Payment.test = Default_Payment[-train.ccd]

#Building the tree on the train data
tree.train.ccd = tree(Default_Payment~ repay_status_Sept, credit_card_defaulters, subset=train.ccd)
summary(tree.train.ccd)

#Apply the tree.train.ccd on the test data to evaluate.
tree.pred = predict(tree.train.ccd, ccd.test, type="class")

#create the confusion matrix
table(tree.pred, Default_Payment.test)

#Create the ROC curve
library(ROCR)
tree.pred <- as.numeric(tree.pred)
prediction <- prediction(tree.pred, ccd.test$Default_Payment)
performance <- performance(prediction, measure="tpr", x.measure="fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#Create a Lift curve
library(caret)
library(lift)
ccd.test$probs=tree.pred
ccd.test$prob=sort(ccd.test$probs, decreasing=T)
lift <- lift(Default_Payment ~ prob, data=ccd.test)
lift
xyplot(lift,plot="gain")
