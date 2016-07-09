

#helps the user to pick the file
x=file.choose(new = FALSE)

#importing the file
wf4<-read.csv(x, header = TRUE)

wf4$agg_value <- rep(1:(length(wf4$date)/4), each=4)

aggdata <- aggregate(wf4, by=list(wf4$agg_value), FUN=mean)


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


#adding wp4 values from weatherforecast files corresponding to the dates
windforcast_wp4 <- windforcast[, c(1,5)]
wf4_wp4 =merge(x = aggdata, y = windforcast_wp4, by = "date", all.x = TRUE)

summary(wf4_wp4)
####filling the NA's in all the columns#####
library(zoo)

#Using Zoo package and filling NA's with na.fill
###u###
wf4_wp4$u <- zoo(wf4_wp4$u)
wf4_wp4$u=na.fill(wf4_wp4$u, "extend")
wf4_wp4$u= format(round(wf4_wp4$u, 3), nsmall = 3)


###v###
wf4_wp4$v <- zoo(wf4_wp4$v)
wf4_wp4$v=na.fill(wf4_wp4$v, "extend")
wf4_wp4$v= format(round(wf4_wp4$v, 3), nsmall = 3)


###ws###
wf4_wp4$ws <- zoo(wf4_wp4$ws)
wf4_wp4$ws=na.fill(wf4_wp4$ws, "extend")
wf4_wp4$ws= format(round(wf4_wp4$ws, 3), nsmall = 3)


###wd###
wf4_wp4$wd <- zoo(wf4_wp4$wd)
wf4_wp4$wd=na.fill(wf4_wp4$wd, "extend")
wf4_wp4$wd= format(round(wf4_wp4$wd, 3), nsmall = 3)


###wp4###

wf4_wp4$wp4 <- zoo(wf4_wp4$wp4)
wf4_wp4$wp4=na.fill(wf4_wp4$wp4, "extend")
wf4_wp4$wp4= format(round(wf4_wp4$wp4, 3), nsmall = 3)

for(i in 1:length(wf4_wp4$wp4))
{
  if(wf4_wp4$wp4[i] == 0)
  {
    wf4_wp4$wp4[i]=NA
  }
}

wf4_wp4$wp4 <- zoo(wf4_wp4$wp4)
wf4_wp4$wp4=na.fill(wf4_wp4$wp4, "extend")
#wf4_wp4$wp4= format(round(wf4_wp4$wp4, 3), nsmall = 3)



str(wf4_wp4$wp4)
summary(wf4_wp4)

### conversion into numeric format
wf4_wp4$date <- as.integer(wf4_wp4$date)
wf4_wp4$u <- as.numeric(wf4_wp4$u)
wf4_wp4$v <- as.numeric(wf4_wp4$v)
wf4_wp4$ws <- as.numeric(wf4_wp4$ws)
wf4_wp4$wd <- as.numeric(wf4_wp4$wd)
wf4_wp4$wp4 <- as.numeric(wf4_wp4$wp4)

str(wf4_wp4)
summary(wf4_wp4)

#converting date to character
wf4_wp4$date <- as.character(wf4_wp4$date)

#separating the year, month, day and hour from the date column
wf4_wp4$hour <- substr(wf4_wp4$date,9,10)
wf4_wp4$date <- substr(wf4_wp4$date,1,8)

wf4_wp4$date <- as.Date(wf4_wp4$date, format="%Y%m%d")

wf4_wp4$year <- format(wf4_wp4$date, format="%Y")
wf4_wp4$month <- format(wf4_wp4$date, format="%m")
wf4_wp4$day <- format(wf4_wp4$date, format="%d")

wf4_wp4$year <- as.numeric(wf4_wp4$year)
wf4_wp4$month <- as.numeric(wf4_wp4$month)
wf4_wp4$day <- as.numeric(wf4_wp4$day)
wf4_wp4$hour <- as.numeric(wf4_wp4$hour)

#removing the date column as the date has been split
wf4_wp4$date <- NULL

#Rearranging the columns
wf4_wp4 = wf4_wp4[,c(6,7,8,9,1,2,3,4,5)]

#Removing 0 values 
for(i in 1:length(wf4_wp4$wp4))
{
  if(wf4_wp4$wp4[i] == 0)
  {
    wf4_wp4$wp4[i]=NA
  }
}

wf4_wp4$wp4 <- zoo(wf4_wp4$wp4)
wf4_wp4$wp4=na.fill(wf4_wp4$wp4, "extend")
wf4_wp4$wp4= format(round(wf4_wp4$wp4, 3), nsmall = 3)

wf4_wp4$wp4 <- as.numeric(wf4_wp4$wp4)

summary(wf4_wp4)


#splitting to training and test data
wf4_wp4_training <- wf4_wp4[((wf4_wp4$year=="2009") | (wf4_wp4$year=="2010")),]
wf4_wp4_test <- wf4_wp4[((wf4_wp4$year=="2011") | (wf4_wp4$year=="2012")),]

wf4_wp4_test$wp4= format(round(wf4_wp4_test$wp4, 3), nsmall = 3)
wf4_wp4_training$wp4= format(round(wf4_wp4_training$wp4, 3), nsmall = 3)


#Renumbering the rows for test data frame
rownames(wf4_wp4_test) <- NULL

#numeric conversion
wf4_wp4_test$wp4 <- as.numeric(wf4_wp4_test$wp4)
wf4_wp4_training$wp4 <- as.numeric(wf4_wp4_training$wp4)
wf4_wp4$wp4 <- as.numeric(wf4_wp4$wp4)



########################################################################################################
########################################BUILDING MODELS#################################################
########################################################################################################

##Building models for Windfarms 1 using the wf4_wp4_training datasets##

#####wf4_wp4_training#####

####REGRESSION###

summary(wf4_wp4)
summary(wf4_wp4_training)
summary(wf4_wp4_test)


#Start Regression
#install.packages('forecast')

str(wf4_wp4_training$wp4)

library(MASS)
library(ISLR)

set.seed(123)


lm.fit4= lm(wp4~.-day -wd , data = wf4_wp4_training)
summary(lm.fit4)

library(forecast)
pred = predict(lm.fit4, wf4_wp4_test)



#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,wf4_wp4_test$wp4)
a

write.csv(a, "PerformanceMatrics_wp4.csv")

summary(wf4_wp4_training)

benchmark$wp4 <- (predict(lm.fit4,wf4_wp4))
benchmark$wp4= format(round(benchmark$wp4, 3), nsmall = 3)


####REGRESSION TREES####
library (tree)
library (MASS)
library (ISLR)
set.seed (1)


train = sample (1:nrow(wf4_wp4_training), nrow(wf4_wp4_training)/2)
tree.wf = tree(wp4~.,wf4_wp4_training,subset=train)
summary (tree.wf)
plot (tree.wf)
text (tree.wf, pretty = 0)
cv.wf = cv.tree (tree.wf)
plot (cv.wf$size, cv.wf$dev, type='b')

prune.wf =prune.tree(tree.wf, best = 4)
plot(prune.wf)
text(prune.wf, pretty = 0)

yhat=predict (tree.wf, newdata =wf4_wp4_training [-train,])
wf.test=wf4_wp4_training [-train,"wp4"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

yhat=predict (prune.wf, newdata =wf4_wp4_training [-train,])
wf.test=wf4_wp4_training [-train,"wp4"]
plot(yhat,wf.test)
abline (0,1)
mean((yhat -wf.test)^2)

regtree=accuracy(yhat,wf.test)
regtree
write.csv(regtree,"PerformanceMetrics_Regression_Tree_wp4.csv",row.names = FALSE)


#### NEURAL NETWORKS ####

set.seed(500)
library(MASS)


# Train-test random splitting for linear model
index <- sample(1:nrow(wf4_wp4),round(0.75*nrow(wf4_wp4)))
#train <- wf4_wp4[index,]
#test <- wf4_wp4[-index,]


# Fitting linear model


lm.fit4 <- glm(wp4~.-day -wd, data=wf4_wp4_training)

summary(lm.fit4)

# Predicted data from lm

pr.lm <- predict(lm.fit4,wf4_wp4_test)

# Test MSE
MSE.lm <- sum((pr.lm - wf4_wp4_test$wp4)^2)/nrow(wf4_wp4_test)

# Neural net fitting


# Scaling data for the NN

maxs <- apply(wf4_wp4, 2, max) 
mins <- apply(wf4_wp4, 2, min)
maxs
mins
scaled <- as.data.frame(scale(wf4_wp4, center = mins, scale = maxs - mins))


# Train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]
#View(train_)
# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("wp4 ~", paste(n[!n %in% "wp4"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,5), threshold= 0.5, linear.output=F)
print(nn)
# Visual plot of the model
plot(nn)

summary(nn)



# Predict and the covariate value should be same as nn's covariate number
pr.nn <- compute(nn,test_[,1:8])



# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(wf4_wp4$wp4)-min(wf4_wp4$wp4))+min(wf4_wp4$wp4)
test.r <- (test_$wp4)*(max(wf4_wp4$wp4)-min(wf4_wp4$wp4))+min(wf4_wp4$wp4)

# Calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# Compare the two MSEs
print(paste(MSE.lm,MSE.nn))

write.csv(MSE.nn,"PerformanceMetrics_Regression_Tree_wp4.csv",row.names = FALSE)




