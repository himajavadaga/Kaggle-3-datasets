

#helps the user to pick the file
x=file.choose(new = FALSE)

#importing the file
wf1<-read.csv(x, header = TRUE)

wf1$agg_value <- rep(1:(length(wf1$date)/4), each=4)

aggdata <- aggregate(wf1, by=list(wf1$agg_value), FUN=mean)


#converting date to character
aggdata$date <- as.character(aggdata$date)

#separating the year, month, day and hour from the date column
aggdata$hour <- substr(aggdata$date,9,10)
aggdata$date <- substr(aggdata$date,1,8)

#filling the hours from 0 to 23
aggdata$hour <- rep(0:23, times=(length(aggdata$hour))/24)

#replacing 00 instead of 0 and so on till 9
aggdata$hour <- as.character(aggdata$hour)
aggdata$hour <- with(aggdata, ifelse(hour=="0", "00",ifelse(hour=="1", "01",ifelse(hour=="2", "02", ifelse(hour=="3", "03", ifelse(hour=="4", "04", ifelse(hour=="5", "05", ifelse(hour=="6", "06",ifelse(hour=="7", "07",ifelse(hour=="8", "08",ifelse(hour=="9", "09",hour)))))))))))

#concatenating the columns date and hour
aggdata$date <- do.call(paste, c(aggdata[c("date", "hour")], sep = "")) 

#removing the unecessary columns
aggdata$Group.1=NULL
aggdata$hors=NULL
aggdata$agg_value=NULL
aggdata$hour=NULL

#windforecast file
#Choose the train.csv file
x=file.choose(new = FALSE)

#read the file and store it in a dataframe named windforcast
windforcast<-read.csv(x, header = TRUE)


#adding wp1 values from weatherforecast files corresponding to the dates
windforcast_wp1 <- windforcast[, c(1,2)]
wf1_wp1 =merge(x = aggdata, y = windforcast_wp1, by = "date", all.x = TRUE)

summary(wf1_wp1)
####filling the NA's in all the columns#####
library(zoo)

#Using Zoo package and filling NA's with na.fill
###u###
wf1_wp1$u <- zoo(wf1_wp1$u)
wf1_wp1$u=na.fill(wf1_wp1$u, "extend")
wf1_wp1$u= format(round(wf1_wp1$u, 3), nsmall = 3)


###v###
wf1_wp1$v <- zoo(wf1_wp1$v)
wf1_wp1$v=na.fill(wf1_wp1$v, "extend")
wf1_wp1$v= format(round(wf1_wp1$v, 3), nsmall = 3)


###ws###
wf1_wp1$ws <- zoo(wf1_wp1$ws)
wf1_wp1$ws=na.fill(wf1_wp1$ws, "extend")
wf1_wp1$ws= format(round(wf1_wp1$ws, 3), nsmall = 3)


###wd###
wf1_wp1$wd <- zoo(wf1_wp1$wd)
wf1_wp1$wd=na.fill(wf1_wp1$wd, "extend")
wf1_wp1$wd= format(round(wf1_wp1$wd, 3), nsmall = 3)


###wp1###

wf1_wp1$wp1 <- zoo(wf1_wp1$wp1)
wf1_wp1$wp1=na.fill(wf1_wp1$wp1, "extend")
wf1_wp1$wp1= format(round(wf1_wp1$wp1, 3), nsmall = 3)

for(i in 1:length(wf1_wp1$wp1))
{
  if(wf1_wp1$wp1[i] == 0)
  {
    wf1_wp1$wp1[i]=NA
  }
}

wf1_wp1$wp1 <- zoo(wf1_wp1$wp1)
wf1_wp1$wp1=na.fill(wf1_wp1$wp1, "extend")
wf1_wp1$wp1= format(round(wf1_wp1$wp1, 3), nsmall = 3)



str(wf1_wp1$wp1)
summary(wf1_wp1)

### conversion into numeric format
wf1_wp1$date <- as.integer(wf1_wp1$date)
wf1_wp1$u <- as.numeric(wf1_wp1$u)
wf1_wp1$v <- as.numeric(wf1_wp1$v)
wf1_wp1$ws <- as.numeric(wf1_wp1$ws)
wf1_wp1$wd <- as.numeric(wf1_wp1$wd)
wf1_wp1$wp1 <- as.numeric(wf1_wp1$wp1)

str(wf1_wp1)
summary(wf1_wp1)

#converting date to character
wf1_wp1$date <- as.character(wf1_wp1$date)

#separating the year, month, day and hour from the date column
wf1_wp1$hour <- substr(wf1_wp1$date,9,10)
wf1_wp1$date <- substr(wf1_wp1$date,1,8)

wf1_wp1$date <- as.Date(wf1_wp1$date, format="%Y%m%d")

wf1_wp1$year <- format(wf1_wp1$date, format="%Y")
wf1_wp1$month <- format(wf1_wp1$date, format="%m")
wf1_wp1$day <- format(wf1_wp1$date, format="%d")

wf1_wp1$year <- as.numeric(wf1_wp1$year)
wf1_wp1$month <- as.numeric(wf1_wp1$month)
wf1_wp1$day <- as.numeric(wf1_wp1$day)
wf1_wp1$hour <- as.numeric(wf1_wp1$hour)

#removing the date column as the date has been split
wf1_wp1$date <- NULL

#Rearranging the columns
wf1_wp1 = wf1_wp1[,c(6,7,8,9,1,2,3,4,5)]

#Removing 0 values 
for(i in 1:length(wf1_wp1$wp1))
{
  if(wf1_wp1$wp1[i] == 0)
  {
    wf1_wp1$wp1[i]=NA
  }
}

wf1_wp1$wp1 <- zoo(wf1_wp1$wp1)
wf1_wp1$wp1=na.fill(wf1_wp1$wp1, "extend")
wf1_wp1$wp1= format(round(wf1_wp1$wp1, 3), nsmall = 3)


#splitting to training and test data
wf1_wp1_training <- wf1_wp1[((wf1_wp1$year=="2009") | (wf1_wp1$year=="2010")),]
wf1_wp1_test <- wf1_wp1[((wf1_wp1$year=="2011") | (wf1_wp1$year=="2012")),]



#Renumbering the rows for test data frame
rownames(wf1_wp1_test) <- NULL



wf1_wp1_test$wp1= format(round(wf1_wp1_test$wp1, 3), nsmall = 3)
wf1_wp1_training$wp1= format(round(wf1_wp1_training$wp1, 3), nsmall = 3)

#numeric conversion
wf1_wp1_test$wp1 <- as.numeric(wf1_wp1_test$wp1)
wf1_wp1_training$wp1 <- as.numeric(wf1_wp1_training$wp1)
wf1_wp1$wp1 <- as.numeric(wf1_wp1$wp1)




########################################################################################################
########################################BUILDING MODELS#################################################
########################################################################################################

##Building models for Windfarms 1 using the wf1_wp1_training datasets##

#####wf1_wp1_training#####

####REGRESSION###

summary(wf1_wp1)
summary(wf1_wp1_training)
summary(wf1_wp1_test)


#Start Regression
#install.packages('forecast')

str(wf1_wp1_training$wp1)

library(MASS)
library(ISLR)

set.seed(123)


lm.fit= lm(wp1~.-day , data = wf1_wp1_training)
summary(lm.fit)

library(forecast)
 pred = predict(lm.fit, wf1_wp1_test)



#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,wf1_wp1_test$wp1)
a

write.csv(a, "PerformanceMatrics_wp1.csv")

summary(wf1_wp1_training)

benchmark <- aggdata[, c(1,2,3,4)]
benchmark$wp1 <- (predict(lm.fit,wf1_wp1))
benchmark$wp1= format(round(benchmark$wp1, 3), nsmall = 3)

benchmark$u<- NULL
benchmark$v<- NULL
benchmark$ws <- NULL


####REGRESSION TREES####
library (tree)
library (MASS)
library (ISLR)
set.seed (1)


train = sample (1:nrow(wf1_wp1_training), nrow(wf1_wp1_training)/2)
tree.wf = tree(wp1~.,wf1_wp1_training,subset=train)
summary (tree.wf)
plot (tree.wf)
text (tree.wf, pretty = 0)
cv.wf = cv.tree (tree.wf)
plot (cv.wf$size, cv.wf$dev, type='b')

prune.wf =prune.tree(tree.wf, best = 4)
plot(prune.wf)
text(prune.wf, pretty = 0)

yhat=predict (tree.wf, newdata =wf1_wp1_training [-train,])
wf.test=wf1_wp1_training [-train,"wp1"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

yhat=predict (prune.wf, newdata =wf1_wp1_training [-train,])
wf.test=wf1_wp1_training [-train,"wp1"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

regtree=accuracy(yhat,wf.test)
regtree
write.csv(regtree,"PerformanceMetrics_Regression_Tree_wp1.csv",row.names = FALSE)


#### NEURAL NETWORKS ####

set.seed(500)
library(MASS)


# Train-test random splitting for linear model
index <- sample(1:nrow(wf1_wp1),round(0.75*nrow(wf1_wp1)))
#train <- wf1_wp1[index,]
#test <- wf1_wp1[-index,]


# Fitting linear model


lm.fit <- glm(wp1~.-day , data=wf1_wp1_training)

summary(lm.fit)

# Predicted data from lm

pr.lm <- predict(lm.fit,wf1_wp1_test)

# Test MSE
MSE.lm <- sum((pr.lm - wf1_wp1_test$wp1)^2)/nrow(wf1_wp1_test)

# Neural net fitting


# Scaling data for the NN

maxs <- apply(wf1_wp1, 2, max) 
mins <- apply(wf1_wp1, 2, min)
maxs
mins
scaled <- as.data.frame(scale(wf1_wp1, center = mins, scale = maxs - mins))


# Train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]
#View(train_)
# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("wp1 ~", paste(n[!n %in% "wp1"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,5), threshold= 0.5, linear.output=F)
print(nn)
# Visual plot of the model
plot(nn)

summary(nn)



# Predict and the covariate value should be same as nn's covariate number
pr.nn <- compute(nn,test_[,1:8])



# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(wf1_wp1$wp1)-min(wf1_wp1$wp1))+min(wf1_wp1$wp1)
test.r <- (test_$wp1)*(max(wf1_wp1$wp1)-min(wf1_wp1$wp1))+min(wf1_wp1$wp1)

# Calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# Compare the two MSEs
print(paste(MSE.lm,MSE.nn))

write.csv(MSE.nn,"PerformanceMetrics_Regression_Tree_wp2.csv",row.names = FALSE)




