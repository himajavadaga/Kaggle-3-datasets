

#helps the user to pick the file
x=file.choose(new = FALSE)

#importing the file
wf6<-read.csv(x, header = TRUE)


wf6$agg_value <- rep(1:(length(wf6$date)/4), each=4)

aggdata <- aggregate(wf6, by=list(wf6$agg_value), FUN=mean)


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

#adding wp6 values from weatherforecast files corresponding to the dates
windforcast_wp6 <- windforcast[, c(1,7)]
wf6_wp6 =merge(x = aggdata, y = windforcast_wp6, by = "date", all.x = TRUE)

summary(wf6_wp6)
####filling the NA's in all the columns#####
library(zoo)

#Using Zoo package and filling NA's with na.fill
###u###



wf6_wp6$u <- zoo(wf6_wp6$u)
wf6_wp6$u=na.fill(wf6_wp6$u, "extend")
wf6_wp6$u= format(round(wf6_wp6$u, 3), nsmall = 3)


###v###
wf6_wp6$v <- zoo(wf6_wp6$v)
wf6_wp6$v=na.fill(wf6_wp6$v, "extend")
wf6_wp6$v= format(round(wf6_wp6$v, 3), nsmall = 3)


###ws###
wf6_wp6$ws <- zoo(wf6_wp6$ws)
wf6_wp6$ws=na.fill(wf6_wp6$ws, "extend")
wf6_wp6$ws= format(round(wf6_wp6$ws, 3), nsmall = 3)


###wd###
wf6_wp6$wd <- zoo(wf6_wp6$wd)
wf6_wp6$wd=na.fill(wf6_wp6$wd, "extend")
wf6_wp6$wd= format(round(wf6_wp6$wd, 3), nsmall = 3)


###wp6###

wf6_wp6$wp6 <- zoo(wf6_wp6$wp6)
wf6_wp6$wp6=na.fill(wf6_wp6$wp6, "extend")
wf6_wp6$wp6= format(round(wf6_wp6$wp6, 3), nsmall = 3)

for(i in 1:length(wf6_wp6$wp6))
{
  if(wf6_wp6$wp6[i] == 0)
  {
    wf6_wp6$wp6[i]=NA
  }
}

wf6_wp6$wp6 <- zoo(wf6_wp6$wp6)
wf6_wp6$wp6=na.fill(wf6_wp6$wp6, "extend")
#wf6_wp6$wp6= format(round(wf6_wp6$wp6, 3), nsmall = 3)



str(wf6_wp6$wp6)
summary(wf6_wp6)

### conversion into numeric format
wf6_wp6$date <- as.integer(wf6_wp6$date)
wf6_wp6$u <- as.numeric(wf6_wp6$u)
wf6_wp6$v <- as.numeric(wf6_wp6$v)
wf6_wp6$ws <- as.numeric(wf6_wp6$ws)
wf6_wp6$wd <- as.numeric(wf6_wp6$wd)
wf6_wp6$wp6 <- as.numeric(wf6_wp6$wp6)

str(wf6_wp6)
summary(wf6_wp6)

#converting date to character
wf6_wp6$date <- as.character(wf6_wp6$date)

#separating the year, month, day and hour from the date column
wf6_wp6$hour <- substr(wf6_wp6$date,9,10)
wf6_wp6$date <- substr(wf6_wp6$date,1,8)

wf6_wp6$date <- as.Date(wf6_wp6$date, format="%Y%m%d")

wf6_wp6$year <- format(wf6_wp6$date, format="%Y")
wf6_wp6$month <- format(wf6_wp6$date, format="%m")
wf6_wp6$day <- format(wf6_wp6$date, format="%d")

wf6_wp6$year <- as.numeric(wf6_wp6$year)
wf6_wp6$month <- as.numeric(wf6_wp6$month)
wf6_wp6$day <- as.numeric(wf6_wp6$day)
wf6_wp6$hour <- as.numeric(wf6_wp6$hour)

#removing the date column as the date has been split
wf6_wp6$date <- NULL

#Rearranging the columns
wf6_wp6 = wf6_wp6[,c(6,7,8,9,1,2,3,4,5)]

#Removing 0 values 
for(i in 1:length(wf6_wp6$wp6))
{
  if(wf6_wp6$wp6[i] == 0)
  {
    wf6_wp6$wp6[i]=NA
  }
}

wf6_wp6$wp6 <- zoo(wf6_wp6$wp6)
wf6_wp6$wp6=na.fill(wf6_wp6$wp6, "extend")
wf6_wp6$wp6= format(round(wf6_wp6$wp6, 3), nsmall = 3)

wf6_wp6$wp6 <- as.numeric(wf6_wp6$wp6)

summary(wf6_wp6)


#splitting to training and test data
wf6_wp6_training <- wf6_wp6[((wf6_wp6$year=="2009") | (wf6_wp6$year=="2010")),]
wf6_wp6_test <- wf6_wp6[((wf6_wp6$year=="2011") | (wf6_wp6$year=="2012")),]

wf6_wp6_test$wp6= format(round(wf6_wp6_test$wp6, 3), nsmall = 3)
wf6_wp6_training$wp6= format(round(wf6_wp6_training$wp6, 3), nsmall = 3)


#Renumbering the rows for test data frame
rownames(wf6_wp6_test) <- NULL

#numeric conversion
wf6_wp6_test$wp6 <- as.numeric(wf6_wp6_test$wp6)
wf6_wp6_training$wp6 <- as.numeric(wf6_wp6_training$wp6)
wf6_wp6$wp6 <- as.numeric(wf6_wp6$wp6)



########################################################################################################
########################################BUILDING MODELS#################################################
########################################################################################################

##Building models for Windfarms 1 using the wf6_wp6_training datasets##

#####wf6_wp6_training#####

####REGRESSION###

summary(wf6_wp6)
summary(wf6_wp6_training)
summary(wf6_wp6_test)


#Start Regression
#install.packages('forecast')

str(wf6_wp6_training$wp6)

library(MASS)
library(ISLR)

set.seed(123)


lm.fit6= lm(wp6~.-day -wd -hour  , data = wf6_wp6_training)
summary(lm.fit6)

library(forecast)
pred = predict(lm.fit6, wf6_wp6_test)



#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,wf6_wp6_test$wp6)
a

write.csv(a, "PerformanceMatrics_wp6.csv")

summary(wf6_wp6_training)

benchmark$wp6 <- (predict(lm.fit6,wf6_wp6))
benchmark$wp6= format(round(benchmark$wp6, 3), nsmall = 3)

####REGRESSION TREES####
library (tree)
library (MASS)
library (ISLR)
set.seed (1)


train = sample (1:nrow(wf6_wp6_training), nrow(wf6_wp6_training)/2)
tree.wf = tree(wp6~.,wf6_wp6_training,subset=train)
summary (tree.wf)
plot (tree.wf)
text (tree.wf, pretty = 0)
cv.wf = cv.tree (tree.wf)
plot (cv.wf$size, cv.wf$dev, type='b')

prune.wf =prune.tree(tree.wf, best = 4)
plot(prune.wf)
text(prune.wf, pretty = 0)

yhat=predict (tree.wf, newdata =wf6_wp6_training [-train,])
wf.test=wf6_wp6_training [-train,"wp6"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

yhat=predict (prune.wf, newdata =wf6_wp6_training [-train,])
wf.test=wf6_wp6_training [-train,"wp6"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

regtree=accuracy(yhat,wf.test)
regtree
write.csv(regtree,"PerformanceMetrics_Regression_Tree_wp6.csv",row.names = FALSE)


#### NEURAL NETWORKS ####

set.seed(500)
library(MASS)


# Train-test random splitting for linear model
index <- sample(1:nrow(wf6_wp6),round(0.75*nrow(wf6_wp6)))
#train <- wf6_wp6[index,]
#test <- wf6_wp6[-index,]


# Fitting linear model


lm.fit6 <- glm(wp6~.-day -wd -hour, data=wf6_wp6_training)

summary(lm.fit6)

# Predicted data from lm

pr.lm <- predict(lm.fit6,wf6_wp6_test)

# Test MSE
MSE.lm <- sum((pr.lm - wf6_wp6_test$wp6)^2)/nrow(wf6_wp6_test)

# Neural net fitting


# Scaling data for the NN

maxs <- apply(wf6_wp6, 2, max) 
mins <- apply(wf6_wp6, 2, min)
maxs
mins
scaled <- as.data.frame(scale(wf6_wp6, center = mins, scale = maxs - mins))


# Train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]
#View(train_)
# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("wp6 ~", paste(n[!n %in% "wp6"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,5), threshold= 0.5, linear.output=F)
print(nn)
# Visual plot of the model
plot(nn)

summary(nn)



# Predict and the covariate value should be same as nn's covariate number
pr.nn <- compute(nn,test_[,1:8])



# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(wf6_wp6$wp6)-min(wf6_wp6$wp6))+min(wf6_wp6$wp6)
test.r <- (test_$wp6)*(max(wf6_wp6$wp6)-min(wf6_wp6$wp6))+min(wf6_wp6$wp6)

# Calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# Compare the two MSEs
print(paste(MSE.lm,MSE.nn))

write.csv(MSE.nn,"PerformanceMetrics_Regression_Tree_wp6.csv",row.names = FALSE)




