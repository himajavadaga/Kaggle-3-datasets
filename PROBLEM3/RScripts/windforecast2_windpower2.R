

#helps the user to pick the file
x=file.choose(new = FALSE)

#importing the file
wf2<-read.csv(x, header = TRUE)


wf2$agg_value <- rep(1:(length(wf2$date)/4), each=4)

aggdata <- aggregate(wf2, by=list(wf2$agg_value), FUN=mean)


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
 
summary(aggdata)

#windforecast file
#Choose the train.csv file
x=file.choose(new = FALSE)

#read the file and store it in a dataframe named windforcast
windforcast<-read.csv(x, header = TRUE)


#adding wp2 values from weatherforecast files corresponding to the dates
windforcast_wp2 <- windforcast[, c(1,3)]
wf2_wp2 =merge(x = aggdata, y = windforcast_wp2, by = "date", all.x = TRUE)

summary(wf2_wp2)
####filling the NA's in all the columns#####
library(zoo)

#Using Zoo package and filling NA's with na.fill
###u###
wf2_wp2$u <- zoo(wf2_wp2$u)
wf2_wp2$u=na.fill(wf2_wp2$u, "extend")
wf2_wp2$u= format(round(wf2_wp2$u, 3), nsmall = 3)

summary(wf2_wp2)

###v###
wf2_wp2$v <- zoo(wf2_wp2$v)
wf2_wp2$v=na.fill(wf2_wp2$v, "extend")
wf2_wp2$v= format(round(wf2_wp2$v, 3), nsmall = 3)


###ws###
wf2_wp2$ws <- zoo(wf2_wp2$ws)
wf2_wp2$ws=na.fill(wf2_wp2$ws, "extend")
wf2_wp2$ws= format(round(wf2_wp2$ws, 3), nsmall = 3)


###wd###
wf2_wp2$wd <- zoo(wf2_wp2$wd)
wf2_wp2$wd=na.fill(wf2_wp2$wd, "extend")
wf2_wp2$wd= format(round(wf2_wp2$wd, 3), nsmall = 3)


summary(wf2_wp2)

###wp2###


for(i in 1:length(wf2_wp2$wp2))
{
  if(wf2_wp2$wp2[i] == 0)
  {
    wf2_wp2$wp2[i]=NA
  }
}

wf2_wp2$wp2 <- zoo(wf2_wp2$wp2)
wf2_wp2$wp2=na.fill(wf2_wp2$wp2, "extend")

for(i in 1:length(wf2_wp2$wp2))
{
  if(wf2_wp2$wp2[i] == 0)
  {
    wf2_wp2$wp2[i]=NA
  }
}

wf2_wp2$wp2 <- zoo(wf2_wp2$wp2)
wf2_wp2$wp2=na.fill(wf2_wp2$wp2, "extend")

str(wf2_wp2$wp2)
summary(wf2_wp2)

### conversion into numeric format
wf2_wp2$date <- as.integer(wf2_wp2$date)
wf2_wp2$u <- as.numeric(wf2_wp2$u)
wf2_wp2$v <- as.numeric(wf2_wp2$v)
wf2_wp2$ws <- as.numeric(wf2_wp2$ws)
wf2_wp2$wd <- as.numeric(wf2_wp2$wd)
wf2_wp2$wp2 <- as.numeric(wf2_wp2$wp2)

str(wf2_wp2)
summary(wf2_wp2)

#converting date to character
wf2_wp2$date <- as.character(wf2_wp2$date)

#separating the year, month, day and hour from the date column
wf2_wp2$hour <- substr(wf2_wp2$date,9,10)
wf2_wp2$date <- substr(wf2_wp2$date,1,8)

wf2_wp2$date <- as.Date(wf2_wp2$date, format="%Y%m%d")

wf2_wp2$year <- format(wf2_wp2$date, format="%Y")
wf2_wp2$month <- format(wf2_wp2$date, format="%m")
wf2_wp2$day <- format(wf2_wp2$date, format="%d")

wf2_wp2$year <- as.numeric(wf2_wp2$year)
wf2_wp2$month <- as.numeric(wf2_wp2$month)
wf2_wp2$day <- as.numeric(wf2_wp2$day)
wf2_wp2$hour <- as.numeric(wf2_wp2$hour)

#removing the date column as the date has been split
wf2_wp2$date <- NULL

#Rearranging the columns
wf2_wp2 = wf2_wp2[,c(6,7,8,9,1,2,3,4,5)]


#splitting to training and test data
wf2_wp2_training <- wf2_wp2[((wf2_wp2$year=="2009") | (wf2_wp2$year=="2010")),]
wf2_wp2_test <- wf2_wp2[((wf2_wp2$year=="2011") | (wf2_wp2$year=="2012")),]

wf2_wp2_test$wp2 <- as.numeric(wf2_wp2_test$wp2)
wf2_wp2_training$wp2 <- as.numeric(wf2_wp2_training$wp2)

wf2_wp2_test$wp2= format(round(wf2_wp2_test$wp2, 3), nsmall = 3)
wf2_wp2_training$wp2= format(round(wf2_wp2_training$wp2, 3), nsmall = 3)

summary(wf2_wp2_training)
summary(wf2_wp2_test)


#Renumbering the rows for test data frame
rownames(wf2_wp2_test) <- NULL

#numeric conversion
wf2_wp2_test$wp2 <- as.numeric(wf2_wp2_test$wp2)
wf2_wp2_training$wp2 <- as.numeric(wf2_wp2_training$wp2)

for(i in 1:length(wf2_wp2_test$wp2))
{
  if(wf2_wp2_test$wp2[i] == 0)
  {
    wf2_wp2_test$wp2[i]=NA
  }
}

wf2_wp2_test$wp2 <- zoo(wf2_wp2_test$wp2)
wf2_wp2_test$wp2=na.fill(wf2_wp2_test$wp2, "extend")

#numeric conversion
wf2_wp2_test$wp2 <- as.numeric(wf2_wp2_test$wp2)
wf2_wp2_training$wp2 <- as.numeric(wf2_wp2_training$wp2)
wf2_wp2$wp2 <- as.numeric(wf2_wp2$wp2)



########################################################################################################
########################################BUILDING MODELS#################################################
########################################################################################################

##Building models for Windfarms 1 using the wf2_wp2_training datasets##

#####wf2_wp2_training#####

####REGRESSION###

summary(wf2_wp2)

summary(wf2_wp2_training)
summary(wf2_wp2_test)


#Start Regression
install.packages('forecast')

str(wf2_wp2_training$wp2)

library(MASS)
library(ISLR)



set.seed(123)


lm.fit2= lm(wp2~.-day , data = wf2_wp2_training)
summary(lm.fit2)

library(forecast)
pred = predict(lm.fit2, wf2_wp2_test)



#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,wf2_wp2_test$wp2)
a

write.csv(a, "PerformanceMatrics_wp2.csv")

summary(wf2_wp2_training)

benchmark$wp2 <- (predict(lm.fit2,wf2_wp2))
benchmark$wp2= format(round(benchmark$wp2, 3), nsmall = 3)



####REGRESSION TREES####
library (tree)
library (MASS)
library (ISLR)
set.seed (1)


train = sample (1:nrow(wf2_wp2_training), nrow(wf2_wp2_training)/2)
tree.wf = tree(wp2~.,wf2_wp2_training,subset=train)
summary (tree.wf)
plot (tree.wf)
text (tree.wf, pretty = 0)
cv.wf = cv.tree (tree.wf)
plot (cv.wf$size, cv.wf$dev, type='b')

prune.wf =prune.tree(tree.wf, best = 4)
plot(prune.wf)
text(prune.wf, pretty = 0)

yhat=predict (tree.wf, newdata =wf2_wp2_training [-train,])
wf.test=wf2_wp2_training [-train,"wp2"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

yhat=predict (prune.wf, newdata =wf2_wp2_training [-train,])
wf.test=wf2_wp2_training [-train,"wp2"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

regtree=accuracy(yhat,wf.test)
regtree
write.csv(regtree,"PerformanceMetrics_Regression_Tree_wp2.csv",row.names = FALSE)


#### NEURAL NETWORKS ####

set.seed(500)
library(MASS)


# Train-test random splitting for linear model
index <- sample(1:nrow(wf2_wp2),round(0.75*nrow(wf2_wp2)))
#train <- wf2_wp2[index,]
#test <- wf2_wp2[-index,]


# Fitting linear model


lm.fit2 <- glm(wp2~.-day , data=wf2_wp2_training)

summary(lm.fit2)

# Predicted data from lm

pr.lm <- predict(lm.fit2,wf2_wp2_test)

# Test MSE
MSE.lm <- sum((pr.lm - wf2_wp2_test$wp2)^2)/nrow(wf2_wp2_test)

# Neural net fitting


# Scaling data for the NN

maxs <- apply(wf2_wp2, 2, max) 
mins <- apply(wf2_wp2, 2, min)
maxs
mins
scaled <- as.data.frame(scale(wf2_wp2, center = mins, scale = maxs - mins))


# Train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]
#View(train_)
# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("wp2 ~", paste(n[!n %in% "wp2"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,5), threshold= 0.5, linear.output=F)
print(nn)
# Visual plot of the model
plot(nn)

summary(nn)



# Predict and the covariate value should be same as nn's covariate number
pr.nn <- compute(nn,test_[,1:8])



# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(wf2_wp2$wp2)-min(wf2_wp2$wp2))+min(wf2_wp2$wp2)
test.r <- (test_$wp2)*(max(wf2_wp2$wp2)-min(wf2_wp2$wp2))+min(wf2_wp2$wp2)

# Calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# Compare the two MSEs
print(paste(MSE.lm,MSE.nn))

write.csv(MSE.nn,"PerformanceMetrics_wp2.csv",row.names = FALSE)




