

#helps the user to pick the file
x=file.choose(new = FALSE)

#importing the file
wf7<-read.csv(x, header = TRUE)


wf7$agg_value <- rep(1:(length(wf7$date)/4), each=4)

aggdata <- aggregate(wf7, by=list(wf7$agg_value), FUN=mean)


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


#adding wp7 values from weatherforecast files corresponding to the dates
windforcast_wp7 <- windforcast[, c(1,8)]
wf7_wp7 =merge(x = aggdata, y = windforcast_wp7, by = "date", all.x = TRUE)

summary(wf7_wp7)
####filling the NA's in all the columns#####
library(zoo)

#Using Zoo package and filling NA's with na.fill
###u###



wf7_wp7$u <- zoo(wf7_wp7$u)
wf7_wp7$u=na.fill(wf7_wp7$u, "extend")
wf7_wp7$u= format(round(wf7_wp7$u, 3), nsmall = 3)


###v###
wf7_wp7$v <- zoo(wf7_wp7$v)
wf7_wp7$v=na.fill(wf7_wp7$v, "extend")
wf7_wp7$v= format(round(wf7_wp7$v, 3), nsmall = 3)


###ws###
wf7_wp7$ws <- zoo(wf7_wp7$ws)
wf7_wp7$ws=na.fill(wf7_wp7$ws, "extend")
wf7_wp7$ws= format(round(wf7_wp7$ws, 3), nsmall = 3)


###wd###
wf7_wp7$wd <- zoo(wf7_wp7$wd)
wf7_wp7$wd=na.fill(wf7_wp7$wd, "extend")
wf7_wp7$wd= format(round(wf7_wp7$wd, 3), nsmall = 3)


###wp7###

wf7_wp7$wp7 <- zoo(wf7_wp7$wp7)
wf7_wp7$wp7=na.fill(wf7_wp7$wp7, "extend")
wf7_wp7$wp7= format(round(wf7_wp7$wp7, 3), nsmall = 3)

for(i in 1:length(wf7_wp7$wp7))
{
  if(wf7_wp7$wp7[i] == 0)
  {
    wf7_wp7$wp7[i]=NA
  }
}

wf7_wp7$wp7 <- zoo(wf7_wp7$wp7)
wf7_wp7$wp7=na.fill(wf7_wp7$wp7, "extend")
#wf7_wp7$wp7= format(round(wf7_wp7$wp7, 3), nsmall = 3)



str(wf7_wp7$wp7)
summary(wf7_wp7)

### conversion into numeric format
wf7_wp7$date <- as.integer(wf7_wp7$date)
wf7_wp7$u <- as.numeric(wf7_wp7$u)
wf7_wp7$v <- as.numeric(wf7_wp7$v)
wf7_wp7$ws <- as.numeric(wf7_wp7$ws)
wf7_wp7$wd <- as.numeric(wf7_wp7$wd)
wf7_wp7$wp7 <- as.numeric(wf7_wp7$wp7)

str(wf7_wp7)
summary(wf7_wp7)

#converting date to character
wf7_wp7$date <- as.character(wf7_wp7$date)

#separating the year, month, day and hour from the date column
wf7_wp7$hour <- substr(wf7_wp7$date,9,10)
wf7_wp7$date <- substr(wf7_wp7$date,1,8)

wf7_wp7$date <- as.Date(wf7_wp7$date, format="%Y%m%d")

wf7_wp7$year <- format(wf7_wp7$date, format="%Y")
wf7_wp7$month <- format(wf7_wp7$date, format="%m")
wf7_wp7$day <- format(wf7_wp7$date, format="%d")

wf7_wp7$year <- as.numeric(wf7_wp7$year)
wf7_wp7$month <- as.numeric(wf7_wp7$month)
wf7_wp7$day <- as.numeric(wf7_wp7$day)
wf7_wp7$hour <- as.numeric(wf7_wp7$hour)

#removing the date column as the date has been split
wf7_wp7$date <- NULL

#Rearranging the columns
wf7_wp7 = wf7_wp7[,c(6,7,8,9,1,2,3,4,5)]

#Removing 0 values 
for(i in 1:length(wf7_wp7$wp7))
{
  if(wf7_wp7$wp7[i] == 0)
  {
    wf7_wp7$wp7[i]=NA
  }
}

wf7_wp7$wp7 <- zoo(wf7_wp7$wp7)
wf7_wp7$wp7=na.fill(wf7_wp7$wp7, "extend")
wf7_wp7$wp7= format(round(wf7_wp7$wp7, 3), nsmall = 3)

wf7_wp7$wp7 <- as.numeric(wf7_wp7$wp7)

summary(wf7_wp7)


#splitting to training and test data
wf7_wp7_training <- wf7_wp7[((wf7_wp7$year=="2009") | (wf7_wp7$year=="2010")),]
wf7_wp7_test <- wf7_wp7[((wf7_wp7$year=="2011") | (wf7_wp7$year=="2012")),]

wf7_wp7_test$wp7= format(round(wf7_wp7_test$wp7, 3), nsmall = 3)
wf7_wp7_training$wp7= format(round(wf7_wp7_training$wp7, 3), nsmall = 3)


#Renumbering the rows for test data frame
rownames(wf7_wp7_test) <- NULL

#numeric conversion
wf7_wp7_test$wp7 <- as.numeric(wf7_wp7_test$wp7)
wf7_wp7_training$wp7 <- as.numeric(wf7_wp7_training$wp7)
wf7_wp7$wp7 <- as.numeric(wf7_wp7$wp7)



########################################################################################################
########################################BUILDING MODELS#################################################
########################################################################################################

##Building models for Windfarms 1 using the wf7_wp7_training datasets##

#####wf7_wp7_training#####

####REGRESSION###

summary(wf7_wp7)
summary(wf7_wp7_training)
summary(wf7_wp7_test)


#Start Regression
#install.packages('forecast')

str(wf7_wp7_training$wp7)

library(MASS)
library(ISLR)

set.seed(123)


lm.fit7= lm(wp7~.-hour -day -v , data = wf7_wp7_training)
summary(lm.fit7)

library(forecast)
pred = predict(lm.fit7, wf7_wp7_test)



#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,wf7_wp7_test$wp7)
a

write.csv(a, "PerformanceMatrics_wp7.csv")

summary(wf7_wp7_training)

benchmark$wp7 <- (predict(lm.fit7,wf7_wp7))
benchmark$wp7= format(round(benchmark$wp7, 3), nsmall = 3)

####REGRESSION TREES####
library (tree)
library (MASS)
library (ISLR)
set.seed (1)


train = sample (1:nrow(wf7_wp7_training), nrow(wf7_wp7_training)/2)
tree.wf = tree(wp7~.,wf7_wp7_training,subset=train)
summary (tree.wf)
plot (tree.wf)
text (tree.wf, pretty = 0)
cv.wf = cv.tree (tree.wf)
plot (cv.wf$size, cv.wf$dev, type='b')

prune.wf =prune.tree(tree.wf, best = 4)
plot(prune.wf)
text(prune.wf, pretty = 0)

yhat=predict (tree.wf, newdata =wf7_wp7_training [-train,])
wf.test=wf7_wp7_training [-train,"wp7"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

yhat=predict (prune.wf, newdata =wf7_wp7_training [-train,])
wf.test=wf7_wp7_training [-train,"wp7"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

regtree=accuracy(yhat,wf.test)
regtree
write.csv(regtree,"PerformanceMetrics_Regression_Tree_wp7.csv",row.names = FALSE)


#### NEURAL NETWORKS ####

set.seed(500)
library(MASS)


# Train-test random splitting for linear model
index <- sample(1:nrow(wf7_wp7),round(0.75*nrow(wf7_wp7)))
#train <- wf7_wp7[index,]
#test <- wf7_wp7[-index,]


# Fitting linear model


lm.fit7 <- glm(wp7~.-hour -day -v, data=wf7_wp7_training)

summary(lm.fit7)

# Predicted data from lm

pr.lm <- predict(lm.fit7,wf7_wp7_test)

# Test MSE
MSE.lm <- sum((pr.lm - wf7_wp7_test$wp7)^2)/nrow(wf7_wp7_test)

# Neural net fitting


# Scaling data for the NN

maxs <- apply(wf7_wp7, 2, max) 
mins <- apply(wf7_wp7, 2, min)
maxs
mins
scaled <- as.data.frame(scale(wf7_wp7, center = mins, scale = maxs - mins))


# Train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]
#View(train_)
# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("wp7 ~", paste(n[!n %in% "wp7"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,5), threshold= 0.5, linear.output=F)
print(nn)
# Visual plot of the model
plot(nn)

summary(nn)



# Predict and the covariate value should be same as nn's covariate number
pr.nn <- compute(nn,test_[,1:8])



# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(wf7_wp7$wp7)-min(wf7_wp7$wp7))+min(wf7_wp7$wp7)
test.r <- (test_$wp7)*(max(wf7_wp7$wp7)-min(wf7_wp7$wp7))+min(wf7_wp7$wp7)

# Calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# Compare the two MSEs
print(paste(MSE.lm,MSE.nn))

write.csv(MSE.nn,"PerformanceMetrics_Regression_Tree_wp7.csv",row.names = FALSE)




