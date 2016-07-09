library(stringr)
#install.packages('weatherData')
library(weatherData)
#install.packages('outliers')
#install.packages('zoo')
library(outliers)
library(zoo)
library(ISLR)
library(caret)


x=file.choose(new = FALSE)
ad_data<-read.csv(x, header = FALSE)


#Replacing '?' with NA's
ad_data$V1=as.character(ad_data$V1)
ad_data$V1=as.numeric(ad_data$V1)

ad_data$V2=as.character(ad_data$V2)
ad_data$V2=as.numeric(ad_data$V2)

ad_data$V3=as.character(ad_data$V3)
ad_data$V3=as.numeric(ad_data$V3)

ad_data$V4=as.character(ad_data$V4)
ad_data$V4=as.numeric(ad_data$V4)

#Detecting outliers and Replacing them with NA in Height, Width and Aspect Ratio attributes
ad_data$V1 <- with(ad_data, ifelse(is.na(V1)==TRUE,NA,ifelse(V1<30, NA,ifelse(V1>600,NA,V1))))
ad_data$V2 <- with(ad_data, ifelse(is.na(V2)==TRUE,NA,ifelse(V2<88, NA,ifelse(V2>728,NA,V2))))
ad_data$V3 <- with(ad_data, ifelse(is.na(V3)==TRUE,NA,ifelse(V3<0.14, NA,ifelse(V3>24.26,NA,V3))))

#Building a function to calculate Mode
#This function is built in such a way that it works for data types of numeric,character and factor
#This function ignores NA's and calculates Mode for rest of the values in the data
Mode <- function(x) {
  ux <- na.omit(unique(x) )
  ux[which.max(tabulate(match(x, ux)))]
}


#Replacing the NA's in Height, width, Aspect Ratio and local variables by Mode of Height, Width, Aspect Ratio and local respectively
for(i in 1:length(ad_data$V1))
{
  if(is.na(ad_data$V1[i])==TRUE)
  {
    ad_data$V1[i]=Mode(x=ad_data$V1)
  }
}

for(i in 1:length(ad_data$V2))
{
  if(is.na(ad_data$V2[i])==TRUE)
  {
    ad_data$V2[i]=Mode(x=ad_data$V2)
  }
}

for(i in 1:length(ad_data$V3))
{
  if(is.na(ad_data$V3[i])==TRUE)
  {
    ad_data$V3[i]=Mode(x=ad_data$V3)
  }
}

for(i in 1:length(ad_data$V4))
{
  if(is.na(ad_data$V4[i])==TRUE)
  {
    ad_data$V4[i]=Mode(x=ad_data$V4)
  }
}

#Dimensionality Reduction by using PCA in Clusters of features
require(caret)

#Corresponds to Height, Width, Aspect Ratio and Local features
trans = preProcess(ad_data[,1:3], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
#Corresponds to URL Features
trans1 = preProcess(ad_data[,4:461], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
#Corresponds to origurl Features
trans2 = preProcess(ad_data[,462:956], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
#Corresponds to ancurl Features
trans3 = preProcess(ad_data[,957:1428], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
#Corresponds to alt Features
trans4 = preProcess(ad_data[,1429:1539], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
#Corresponds to caption Features
trans5 = preProcess(ad_data[,1540:1558], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))


#require(caret)
#trans = preProcess(ad_data[,1:1558], 
                   #method=c("BoxCox", "center", 
                            #"scale", "pca"))
PC = predict(trans, ad_data[,1:3])
PC1 = predict(trans1, ad_data[,4:461])
PC2 = predict(trans2, ad_data[,462:956])
PC3 = predict(trans3, ad_data[,957:1428])
PC4 = predict(trans4, ad_data[,1429:1539])
PC5 = predict(trans5, ad_data[,1540:1558])


#Combining the attributes from resultant PCA Clusters
PC=cbind(PC,PC1,PC2,PC3,PC4,PC5)

#Renaming the Column names of final PCA
colnames(PC)=c(paste("PC_",1:498,sep=""))

#Adding the AD_class Variable
PC$Ad_Class=ad_data$V1559

#Making duplicate copies of the cleansed and processed dataset for future uses while building the models
ad_data1=PC
ad_data2=PC
ad_data3=PC

write.csv(PC,"Data after cleaning and pre-processing.csv",row.names = FALSE)






