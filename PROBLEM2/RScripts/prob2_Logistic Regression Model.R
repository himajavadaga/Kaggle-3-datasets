
#ad_data2$Ad_Class=as.character(ad_data2$Ad_Class)
#ad_data2$Ad_Class = with(ad_data2, ifelse(Ad_Class == "ad.", 1, 0)) 
#ad_data2$Ad_Class=as.numeric(ad_data2$Ad_Class)


#Considering 75% of the sample size
smp_size <- floor(0.75 * nrow(ad_data2))

#Set the seed
set.seed(123)
sample_ad_data2 <- sample(seq_len(nrow(ad_data2)),size=smp_size)

#Split the ad_data2 dataset into train and test
train_ad_data2 <- ad_data2[sample_ad_data2,]
test_ad_data2 <- ad_data2[-sample_ad_data2,]

#Fitting a logistic Regression Model
fit <- glm(Ad_Class~., data=train_ad_data2, family=binomial(link=logit))

#Run the model on the test dataset
test.prob <- predict(fit,test_ad_data2, type='response')
pred <- rep("nonad.", length(test.prob))

#set the cutoff value to 0.5
pred[test.prob>=0.5] <- "ad."

#Build the confusion matrix
library(caret)
confusionMatrix(test_ad_data2$Ad_Class,pred)

#Create the ROC curve
library(ROCR)
prediction <- prediction(test.prob, test_ad_data2$Ad_Class)
performance <- performance(prediction, measure="tpr", x.measure="fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#Create a Lift curve
test_ad_data2$probs=test.prob
test_ad_data2$prob=sort(test_ad_data2$probs, decreasing=T)
lift <- lift(Ad_Class ~ prob, data=test_ad_data2)
lift
xyplot(lift,plot="gain")
