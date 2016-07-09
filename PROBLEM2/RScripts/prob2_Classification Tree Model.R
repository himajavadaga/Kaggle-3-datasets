
Ad_Class=PC$Ad_Class

#CLASSIFICATION MODEL FOR CLASSIFICATION TREE
#install.packages('lift')
library(tree)
library(ISLR)
tree = tree(Ad_Class ~.,ad_data1)
summary(tree)

#Display the tree structure and node labels
plot(tree)
text(tree, pretty =0) #Pretty=0 includes the category names

#Split the dataset into a training set and a test set
set.seed (2)
train = sample (1:nrow(ad_data1), nrow(ad_data1)/2)

ad_data1.test =ad_data1 [-train,]
Ad_Class.test = Ad_Class[-train]
#Build the tree based on the training set
tree.train = tree(Ad_Class ~ .   , ad_data1, subset = train)
#Evaluate its performance on the test data
tree.pred = predict(tree.train, ad_data1.test, type = "class")
ConfusionMatrix_Tree_NotPruined=table(tree.pred, Ad_Class.test)
ConfusionMatrix_Tree_NotPruined

#Determine the optimal level
set.seed (3)
#FUN = prune.misclass indicate that classification error rate is used to 
#guide the cross-validation and pruning process
cv.ad_data1 = cv.tree(tree, FUN = prune.misclass)
names(cv.ad_data1)
cv.ad_data1

#Prune the tree
prune.ad_data1 = prune.misclass(tree, best =12)
plot(prune.ad_data1)
text(prune.ad_data1, pretty =0)

#The pruned tree performance
prune.pred = predict(prune.ad_data1, ad_data1.test,type = "class")
ConfusionMatrix_TreePruined=table(prune.pred, Ad_Class.test)
ConfusionMatrix_TreePruined

#Exporting Confusion Matrix
write(capture.output(table(prune.pred, Ad_Class.test)),"classification trees Confusion matrix.csv")

#Building ROC Curve
prune.pred=as.numeric(prune.pred)

library(ROCR)
prediction <- prediction(prune.pred, ad_data1.test$Ad_Class)
performance <- performance(prediction, measure="tpr", x.measure="fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")




#Building Lift Chart
library(caret)
library(lift)
ad_data1.test$probs=prune.pred
ad_data1.test$prob=sort(ad_data1.test$probs, decreasing=T)
lift <- lift(Ad_Class ~ prob, data=ad_data1.test)
lift
xyplot(lift,plot="gain")