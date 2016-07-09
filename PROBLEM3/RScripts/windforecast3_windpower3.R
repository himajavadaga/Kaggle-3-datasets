

#helps the user to pick the file
x=file.choose(new = FALSE)

#importing the file
wf3<-read.csv(x, header = TRUE)

wf3$agg_value <- rep(1:(length(wf3$date)/4), each=4)

aggdata <- aggregate(wf3, by=list(wf3$agg_value), FUN=mean)


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


#adding wp3 values from weatherforecast files corresponding to the dates
windforcast_wp3 <- windforcast[, c(1,4)]
wf3_wp3 =merge(x = aggdata, y = windforcast_wp3, by = "date", all.x = TRUE)

summary(wf3_wp3)
####filling the NA's in all the columns#####
library(zoo)

#Using Zoo package and filling NA's with na.fill
###u###
wf3_wp3$u <- zoo(wf3_wp3$u)
wf3_wp3$u=na.fill(wf3_wp3$u, "extend")
wf3_wp3$u= format(round(wf3_wp3$u, 3), nsmall = 3)


###v###
wf3_wp3$v <- zoo(wf3_wp3$v)
wf3_wp3$v=na.fill(wf3_wp3$v, "extend")
wf3_wp3$v= format(round(wf3_wp3$v, 3), nsmall = 3)


###ws###
wf3_wp3$ws <- zoo(wf3_wp3$ws)
wf3_wp3$ws=na.fill(wf3_wp3$ws, "extend")
wf3_wp3$ws= format(round(wf3_wp3$ws, 3), nsmall = 3)


###wd###
wf3_wp3$wd <- zoo(wf3_wp3$wd)
wf3_wp3$wd=na.fill(wf3_wp3$wd, "extend")
wf3_wp3$wd= format(round(wf3_wp3$wd, 3), nsmall = 3)


###wp3###

wf3_wp3$wp3 <- zoo(wf3_wp3$wp3)
wf3_wp3$wp3=na.fill(wf3_wp3$wp3, "extend")
wf3_wp3$wp3= format(round(wf3_wp3$wp3, 3), nsmall = 3)

for(i in 1:length(wf3_wp3$wp3))
{
  if(wf3_wp3$wp3[i] == 0)
  {
    wf3_wp3$wp3[i]=NA
  }
}

wf3_wp3$wp3 <- zoo(wf3_wp3$wp3)
wf3_wp3$wp3=na.fill(wf3_wp3$wp3, "extend")
#wf3_wp3$wp3= format(round(wf3_wp3$wp3, 3), nsmall = 3)



str(wf3_wp3$wp3)
summary(wf3_wp3)

### conversion into numeric format
wf3_wp3$date <- as.integer(wf3_wp3$date)
wf3_wp3$u <- as.numeric(wf3_wp3$u)
wf3_wp3$v <- as.numeric(wf3_wp3$v)
wf3_wp3$ws <- as.numeric(wf3_wp3$ws)
wf3_wp3$wd <- as.numeric(wf3_wp3$wd)
wf3_wp3$wp3 <- as.numeric(wf3_wp3$wp3)

str(wf3_wp3)
summary(wf3_wp3)

#converting date to character
wf3_wp3$date <- as.character(wf3_wp3$date)

#separating the year, month, day and hour from the date column
wf3_wp3$hour <- substr(wf3_wp3$date,9,10)
wf3_wp3$date <- substr(wf3_wp3$date,1,8)

wf3_wp3$date <- as.Date(wf3_wp3$date, format="%Y%m%d")

wf3_wp3$year <- format(wf3_wp3$date, format="%Y")
wf3_wp3$month <- format(wf3_wp3$date, format="%m")
wf3_wp3$day <- format(wf3_wp3$date, format="%d")

wf3_wp3$year <- as.numeric(wf3_wp3$year)
wf3_wp3$month <- as.numeric(wf3_wp3$month)
wf3_wp3$day <- as.numeric(wf3_wp3$day)
wf3_wp3$hour <- as.numeric(wf3_wp3$hour)

#removing the date column as the date has been split
wf3_wp3$date <- NULL

#Rearranging the columns
wf3_wp3 = wf3_wp3[,c(6,7,8,9,1,2,3,4,5)]

#Removing 0 values 
for(i in 1:length(wf3_wp3$wp3))
{
  if(wf3_wp3$wp3[i] == 0)
  {
    wf3_wp3$wp3[i]=NA
  }
}

wf3_wp3$wp3 <- zoo(wf3_wp3$wp3)
wf3_wp3$wp3=na.fill(wf3_wp3$wp3, "extend")
wf3_wp3$wp3= format(round(wf3_wp3$wp3, 3), nsmall = 3)

wf3_wp3$wp3 <- as.numeric(wf3_wp3$wp3)

summary(wf3_wp3)

#splitting to training and test data
wf3_wp3_training <- wf3_wp3[((wf3_wp3$year=="2009") | (wf3_wp3$year=="2010")),]
wf3_wp3_test <- wf3_wp3[((wf3_wp3$year=="2011") | (wf3_wp3$year=="2012")),]

wf3_wp3_test$wp3= format(round(wf3_wp3_test$wp3, 3), nsmall = 3)
wf3_wp3_training$wp3= format(round(wf3_wp3_training$wp3, 3), nsmall = 3)


#Renumbering the rows for test data frame
rownames(wf3_wp3_test) <- NULL

#numeric conversion
wf3_wp3_test$wp3 <- as.numeric(wf3_wp3_test$wp3)
wf3_wp3_training$wp3 <- as.numeric(wf3_wp3_training$wp3)
wf3_wp3$wp3 <- as.numeric(wf3_wp3$wp3)



########################################################################################################
########################################BUILDING MODELS#################################################
########################################################################################################

##Building models for Windfarms 1 using the wf3_wp3_training datasets##

#####wf3_wp3_training#####

####REGRESSION###

summary(wf3_wp3)
summary(wf3_wp3_training)
summary(wf3_wp3_test)


#Start Regression
#install.packages('forecast')

str(wf3_wp3_training$wp3)

library(MASS)
library(ISLR)

set.seed(123)


lm.fit3= lm(wp3~.-day , data = wf3_wp3_training)
summary(lm.fit3)

library(forecast)
pred = predict(lm.fit3, wf3_wp3_test)



#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,wf3_wp3_test$wp3)
a

write.csv(a, "PerformanceMatrics_wp3.csv")

summary(wf3_wp3_training)

benchmark$wp3 <- (predict(lm.fit3,wf3_wp3))
benchmark$wp3= format(round(benchmark$wp3, 3), nsmall = 3)



####REGRESSION TREES####
library (tree)
library (MASS)
library (ISLR)
set.seed (1)


train = sample (1:nrow(wf3_wp3_training), nrow(wf3_wp3_training)/2)
tree.wf = tree(wp3~.,wf3_wp3_training,subset=train)
summary (tree.wf)
plot (tree.wf)
text (tree.wf, pretty = 0)
cv.wf = cv.tree (tree.wf)
plot (cv.wf$size, cv.wf$dev, type='b')

prune.wf =prune.tree(tree.wf, best = 4)
plot(prune.wf)
text(prune.wf, pretty = 0)

yhat=predict (tree.wf, newdata =wf3_wp3_training [-train,])
wf.test=wf3_wp3_training [-train,"wp3"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

yhat=predict (prune.wf, newdata =wf3_wp3_training [-train,])
wf.test=wf3_wp3_training [-train,"wp3"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

regtree=accuracy(yhat,wf.test)
regtree
write.csv(regtree,"PerformanceMetrics_Regression_Tree_wp3.csv",row.names = FALSE)


#### NEURAL NETWORKS ####

set.seed(500)
library(MASS)


# Train-test random splitting for linear model
index <- sample(1:nrow(wf3_wp3),round(0.75*nrow(wf3_wp3)))
#train <- wf3_wp3[index,]
#test <- wf3_wp3[-index,]


# Fitting linear model


lm.fit3 <- glm(wp3~.-day , data=wf3_wp3_training)

summary(lm.fit3)

# Predicted data from lm

pr.lm <- predict(lm.fit3,wf3_wp3_test)

# Test MSE
MSE.lm <- sum((pr.lm - wf3_wp3_test$wp3)^2)/nrow(wf3_wp3_test)

# Neural net fitting


# Scaling data for the NN

maxs <- apply(wf3_wp3, 2, max) 
mins <- apply(wf3_wp3, 2, min)
maxs
mins
scaled <- as.data.frame(scale(wf3_wp3, center = mins, scale = maxs - mins))


# Train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]
#View(train_)
# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("wp3 ~", paste(n[!n %in% "wp3"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,5), threshold= 0.5, linear.output=F)
print(nn)
# Visual plot of the model
plot(nn)

summary(nn)



# Predict and the covariate value should be same as nn's covariate number
pr.nn <- compute(nn,test_[,1:8])



# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(wf3_wp3$wp3)-min(wf3_wp3$wp3))+min(wf3_wp3$wp3)
test.r <- (test_$wp3)*(max(wf3_wp3$wp3)-min(wf3_wp3$wp3))+min(wf3_wp3$wp3)

# Calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# Compare the two MSEs
print(paste(MSE.lm,MSE.nn))

write.csv(MSE.nn,"PerformanceMetrics_Regression_Tree_wp2.csv",row.names = FALSE)




