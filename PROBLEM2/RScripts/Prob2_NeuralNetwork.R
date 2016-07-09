#credit_card_defaulters$Default_Payment <- as.factor(credit_card_defaulters$Default_Payment)
library(caret)
set.seed(123)

#Considering 75% of the sample size
smp_size <- floor(0.70 * nrow(ad_data3))

#Set the seed
set.seed(123)
sample_ad_data3 <- sample(seq_len(nrow(ad_data3)),size=smp_size)

#Split the credit_card_defaulters dataset into train and test
train_ad_data3 <- ad_data3[sample_ad_data3,]
test_ad_data3 <- ad_data3[-sample_ad_data3,]

#Classification using neural network
library(nnet)

# Fit a Single Hidden Layer Neural Network using Least Squares, you can maximize the accuracy by changing the size.
train.nnet<-nnet(Ad_Class~.,train_ad_data3,size=5, rang=0.03, HESS=FALSE,decay=15e-4,maxit=250,MaxNWts = 2501)

#Use TEST data for testing the trained model
test.nnet <- predict(train.nnet,test_ad_data3,type=("class"))

#MisClassification Confusion Matrix
ConfusionMatrix_Neural_Network=table(test_ad_data3$Ad_Class,test.nnet)
ConfusionMatrix_Neural_Network

#install.packages("NeuralNetTools")
library(NeuralNetTools)
## Plot the trained Neural Network
plotnet(train.nnet)

#Create a Lift chart
library(caret)
library(lift)
test_ad_data3$probs=test.nnet
test_ad_data3$prob=sort(test_ad_data3$probs, decreasing=T)
lift <- lift(Ad_Class ~ prob, data=test_ad_data3)
lift
xyplot(lift,plot="gain")



#Create the ROC curve
#credit_card_defaulters$Default_Payment <- as.numeric(credit_card_defaulters$Default_Payment)
library(ROCR)
for(i in 1:length(test.nnet))
{
  if(test.nnet[i]=="ad.")
  {
    test.nnet[i]=1
  }
  else{
    test.nnet[i]=0
  }
}
test.nnet=as.numeric(test.nnet)
test_ad_data3$Ad_Class=as.character(test_ad_data3$Ad_Class)
for(i in 1:length(test_ad_data3$Ad_Class))
{
  if(test_ad_data3$Ad_Class[i]=="ad.")
  {
    test_ad_data3$Ad_Class[i]=1
  }
  else{
    test_ad_data3$Ad_Class[i]=0
  }
}
test_ad_data3$Ad_Class=as.numeric(test_ad_data3$Ad_Class)
prediction <- prediction(test.nnet, test_ad_data3$Ad_Class)
performance <- performance(prediction, measure="tpr", x.measure="fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

