

#helps the user to pick the file
x=file.choose(new = FALSE)

#importing the file
wf5<-read.csv(x, header = TRUE)


wf5$agg_value <- rep(1:(length(wf5$date)/4), each=4)

aggdata <- aggregate(wf5, by=list(wf5$agg_value), FUN=mean)


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


#adding wp5 values from weatherforecast files corresponding to the dates
windforcast_wp5 <- windforcast[, c(1,6)]
wf5_wp5 =merge(x = aggdata, y = windforcast_wp5, by = "date", all.x = TRUE)

summary(wf5_wp5)
####filling the NA's in all the columns#####
library(zoo)

#Using Zoo package and filling NA's with na.fill
###u###
wf5_wp5$u <- zoo(wf5_wp5$u)
wf5_wp5$u=na.fill(wf5_wp5$u, "extend")
wf5_wp5$u= format(round(wf5_wp5$u, 3), nsmall = 3)


###v###
wf5_wp5$v <- zoo(wf5_wp5$v)
wf5_wp5$v=na.fill(wf5_wp5$v, "extend")
wf5_wp5$v= format(round(wf5_wp5$v, 3), nsmall = 3)


###ws###
wf5_wp5$ws <- zoo(wf5_wp5$ws)
wf5_wp5$ws=na.fill(wf5_wp5$ws, "extend")
wf5_wp5$ws= format(round(wf5_wp5$ws, 3), nsmall = 3)


###wd###
wf5_wp5$wd <- zoo(wf5_wp5$wd)
wf5_wp5$wd=na.fill(wf5_wp5$wd, "extend")
wf5_wp5$wd= format(round(wf5_wp5$wd, 3), nsmall = 3)


###wp5###

wf5_wp5$wp5 <- zoo(wf5_wp5$wp5)
wf5_wp5$wp5=na.fill(wf5_wp5$wp5, "extend")
wf5_wp5$wp5= format(round(wf5_wp5$wp5, 3), nsmall = 3)

for(i in 1:length(wf5_wp5$wp5))
{
  if(wf5_wp5$wp5[i] == 0)
  {
    wf5_wp5$wp5[i]=NA
  }
}

wf5_wp5$wp5 <- zoo(wf5_wp5$wp5)
wf5_wp5$wp5=na.fill(wf5_wp5$wp5, "extend")
#wf5_wp5$wp5= format(round(wf5_wp5$wp5, 3), nsmall = 3)



str(wf5_wp5$wp5)
summary(wf5_wp5)

### conversion into numeric format
wf5_wp5$date <- as.integer(wf5_wp5$date)
wf5_wp5$u <- as.numeric(wf5_wp5$u)
wf5_wp5$v <- as.numeric(wf5_wp5$v)
wf5_wp5$ws <- as.numeric(wf5_wp5$ws)
wf5_wp5$wd <- as.numeric(wf5_wp5$wd)
wf5_wp5$wp5 <- as.numeric(wf5_wp5$wp5)

str(wf5_wp5)
summary(wf5_wp5)

#converting date to character
wf5_wp5$date <- as.character(wf5_wp5$date)

#separating the year, month, day and hour from the date column
wf5_wp5$hour <- substr(wf5_wp5$date,9,10)
wf5_wp5$date <- substr(wf5_wp5$date,1,8)

wf5_wp5$date <- as.Date(wf5_wp5$date, format="%Y%m%d")

wf5_wp5$year <- format(wf5_wp5$date, format="%Y")
wf5_wp5$month <- format(wf5_wp5$date, format="%m")
wf5_wp5$day <- format(wf5_wp5$date, format="%d")

wf5_wp5$year <- as.numeric(wf5_wp5$year)
wf5_wp5$month <- as.numeric(wf5_wp5$month)
wf5_wp5$day <- as.numeric(wf5_wp5$day)
wf5_wp5$hour <- as.numeric(wf5_wp5$hour)

#removing the date column as the date has been split
wf5_wp5$date <- NULL

#Rearranging the columns
wf5_wp5 = wf5_wp5[,c(6,7,8,9,1,2,3,4,5)]

#Removing 0 values 
for(i in 1:length(wf5_wp5$wp5))
{
  if(wf5_wp5$wp5[i] == 0)
  {
    wf5_wp5$wp5[i]=NA
  }
}

wf5_wp5$wp5 <- zoo(wf5_wp5$wp5)
wf5_wp5$wp5=na.fill(wf5_wp5$wp5, "extend")
wf5_wp5$wp5= format(round(wf5_wp5$wp5, 3), nsmall = 3)

wf5_wp5$wp5 <- as.numeric(wf5_wp5$wp5)

summary(wf5_wp5)


#splitting to training and test data
wf5_wp5_training <- wf5_wp5[((wf5_wp5$year=="2009") | (wf5_wp5$year=="2010")),]
wf5_wp5_test <- wf5_wp5[((wf5_wp5$year=="2011") | (wf5_wp5$year=="2012")),]

wf5_wp5_test$wp5= format(round(wf5_wp5_test$wp5, 3), nsmall = 3)
wf5_wp5_training$wp5= format(round(wf5_wp5_training$wp5, 3), nsmall = 3)


#Renumbering the rows for test data frame
rownames(wf5_wp5_test) <- NULL

#numeric conversion
wf5_wp5_test$wp5 <- as.numeric(wf5_wp5_test$wp5)
wf5_wp5_training$wp5 <- as.numeric(wf5_wp5_training$wp5)
wf5_wp5$wp5 <- as.numeric(wf5_wp5$wp5)



########################################################################################################
########################################BUILDING MODELS#################################################
########################################################################################################

##Building models for Windfarms 1 using the wf5_wp5_training datasets##

#####wf5_wp5_training#####

####REGRESSION###

summary(wf5_wp5)
summary(wf5_wp5_training)
summary(wf5_wp5_test)


#Start Regression
#install.packages('forecast')

str(wf5_wp5_training$wp5)

library(MASS)
library(ISLR)

set.seed(123)


lm.fit5= lm(wp5~.-day -wd -hour  , data = wf5_wp5_training)
summary(lm.fit5)

library(forecast)
pred = predict(lm.fit5, wf5_wp5_test)



#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,wf5_wp5_test$wp5)
a

write.csv(a, "PerformanceMatrics_wp5.csv")

summary(wf5_wp5_training)


benchmark$wp5 <- (predict(lm.fit5,wf5_wp5))
benchmark$wp5= format(round(benchmark$wp5, 3), nsmall = 3)

####REGRESSION TREES####
library (tree)
library (MASS)
library (ISLR)
set.seed (1)


train = sample (1:nrow(wf5_wp5_training), nrow(wf5_wp5_training)/2)
tree.wf = tree(wp5~.,wf5_wp5_training,subset=train)
summary (tree.wf)
plot (tree.wf)
text (tree.wf, pretty = 0)
cv.wf = cv.tree (tree.wf)
plot (cv.wf$size, cv.wf$dev, type='b')

prune.wf =prune.tree(tree.wf, best = 4)
plot(prune.wf)
text(prune.wf, pretty = 0)

yhat=predict (tree.wf, newdata =wf5_wp5_training [-train,])
wf.test=wf5_wp5_training [-train,"wp5"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

yhat=predict (prune.wf, newdata =wf5_wp5_training [-train,])
wf.test=wf5_wp5_training [-train,"wp5"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

regtree=accuracy(yhat,wf.test)
regtree
write.csv(regtree,"PerformanceMetrics_Regression_Tree_wp5.csv",row.names = FALSE)


#### NEURAL NETWORKS ####

set.seed(500)
library(MASS)


# Train-test random splitting for linear model
index <- sample(1:nrow(wf5_wp5),round(0.75*nrow(wf5_wp5)))
#train <- wf5_wp5[index,]
#test <- wf5_wp5[-index,]


# Fitting linear model


lm.fit5 <- glm(wp5~.-day -wd -hour, data=wf5_wp5_training)

summary(lm.fit5)

# Predicted data from lm

pr.lm <- predict(lm.fit5,wf5_wp5_test)

# Test MSE
MSE.lm <- sum((pr.lm - wf5_wp5_test$wp5)^2)/nrow(wf5_wp5_test)

# Neural net fitting


# Scaling data for the NN

maxs <- apply(wf5_wp5, 2, max) 
mins <- apply(wf5_wp5, 2, min)
maxs
mins
scaled <- as.data.frame(scale(wf5_wp5, center = mins, scale = maxs - mins))


# Train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]
#View(train_)
# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("wp5 ~", paste(n[!n %in% "wp5"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,5), threshold= 0.5, linear.output=F)
print(nn)
# Visual plot of the model
plot(nn)

summary(nn)



# Predict and the covariate value should be same as nn's covariate number
pr.nn <- compute(nn,test_[,1:8])



# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(wf5_wp5$wp5)-min(wf5_wp5$wp5))+min(wf5_wp5$wp5)
test.r <- (test_$wp5)*(max(wf5_wp5$wp5)-min(wf5_wp5$wp5))+min(wf5_wp5$wp5)

# Calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# Compare the two MSEs
print(paste(MSE.lm,MSE.nn))

write.csv(MSE.nn,"PerformanceMetrics_Regression_Tree_wp5.csv",row.names = FALSE)




