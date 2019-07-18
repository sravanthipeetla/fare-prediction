setwd("E:/K")
rm(list=ls(all=T))

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

install.packages(x)

lapply(x, require, character.only = TRUE)
####loading train and data
train=read.csv("train_cab.csv",header = T, na.strings = c(" ", "", "NA"))
testt=read.csv("test.csv",header = T,na.strings = c(" ","","NA"))
####calculating missing values in train and test data
missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))
missing_val1 = data.frame(apply(testt,2,function(x){sum(is.na(x))}))

####deleting na's in train data
train1=na.omit(train)

sapply(train, class)
####converting fareamount to numeric
train1$fare_amount <- as.numeric(as.character(train1$fare_amount))
train=na.omit(train)
####converting pickup_datatime to datetime format
library(lubridate)
train1$date <- as.Date( train1$pickup_datetime)
testt$date=as.Date(testt$pickup_datetime)
train1$year <- year(train1$date)
testt$year <- year(testt$date)
train1$month <- month(train1$date)
testt$month <- month(testt$date)
train1$day <- day(train1$date)
testt$day=day(testt$date)


class(train1$pickup_datetime) 
class(testt$pickup_datetime)
library(stringi)
##### deleting rows which is not in format of date
c=stri_length(train1[1,2])
c
 train1=train1[-1280,]

for (i in 1:nrow(train1))
{
   if(stri_length(train1[i,2])!=c)
   {
      print(train1[i,2])
   }
   
}


train1 <- train1[,-2]
testt=testt[,-1]

train1[1,4]
train1$abs_lon_diff = abs(train1$pickup_longitude - train1$dropoff_longitude)
train1$abs_lat_diff = abs(train1$pickup_latitude - train1$dropoff_latitude)

testt$abs_lon_diff = abs(testt$pickup_longitude - testt$dropoff_longitude)
testt$abs_lat_diff = abs(testt$pickup_latitude - testt$dropoff_latitude)
####function to convert degree to radians

deg2rad <- function(deg) {(deg * pi) / (180)}


R = 6371000 # Approximate mean radius of earth (in m)
# lons and lats must be in radians
lon1=deg2rad(train1$pickup_longitude)
lat1=deg2rad(train1$pickup_latitude)

lont1=deg2rad(testt$pickup_longitude)
latt1=deg2rad(testt$pickup_latitude)

lon2=deg2rad(train1$dropoff_longitude)
lat2 = deg2rad(train1$dropoff_latitude)

lont2=deg2rad(testt$dropoff_longitude)
latt2=deg2rad(testt$dropoff_latitude)
####calculating euclidian and absolute distance

train1$pickup_x = R*cos(lon1)*cos(lat1)
train1$pickup_y = R*sin(lon1)*cos(lat1)
train1$dropoff_x= R*cos(lon2)*cos(lat2)
train1$dropoff_y = R*sin(lon2)*cos(lat2)
train1$euclidean_distance = sqrt((  train1$pickup_x - train1$dropoff_x )**2 + (train1$pickup_y  -train1$dropoff_y )**2)/1000 
train1$absolute_distance=(abs(train1$pickup_x - train1$dropoff_x) + abs(train1$pickup_y  -train1$dropoff_y))/1000 # in km



testt$pickup_x = R*cos(lont1)*cos(latt1)
testt$pickup_y = R*sin(lont1)*cos(latt1)
testt$dropoff_x= R*cos(lont2)*cos(latt2)
testt$dropoff_y = R*sin(lont2)*cos(latt2)
testt$euclidean_distance = sqrt((  testt$pickup_x - testt$dropoff_x )**2 + (testt$pickup_y  -testt$dropoff_y )**2)/1000 
testt$absolute_distance=(abs(testt$pickup_x - testt$dropoff_x) + abs(testt$pickup_y  -testt$dropoff_y))/1000 # in km

for(i in 1:ncol(train1)){
  
  if(class(train1[,i]) == 'factor'){
    print(class(train1[,i]))
    train1[,i] = factor(train1[,i], labels=(1:length(levels(factor(train1[,i])))))
    
  }
}

numeric_index = sapply(train1,is.numeric) #selecting only numeric

numeric_data = train1[,numeric_index]
cnames = colnames(numeric_data)

numeric_indext = sapply(testt,is.numeric) #selecting only numeric

numeric_datat = testt[,numeric_indext]
cnamest = colnames(numeric_datat)


 for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "fare_amount"), data = subset(train1))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="fare_amount")+
            ggtitle(paste("Box plot of responded for",cnames[i])))
 }
# 
# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
 gridExtra::grid.arrange(gn17,gn15,gn16,ncol=3)
 
 gridExtra::grid.arrange(gn8,gn9,ncol=2)
# 
# # #Remove outliers using boxplot method
 train2 = train1
 train1= train2

# # #loop to remove from all variables
 for(i in cnames){
   print(i)
   val = train1[,i][train1[,i] %in% boxplot.stats(train1[,i])$out]
   print(length(val))
   train1 = train1[which(!train1[,i] %in% val),]
 }
 
 for(i in cnamest){
    print(i)
    val = testt[,i][testt[,i] %in% boxplot.stats(testt[,i])$out]
    print(length(val))
    testt = testt[which(!testt[,i] %in% val),]
 }
 
 
 ## Correlation Plot 
 corrgram(train1[,numeric_index], order = F,
          upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
 
 train2=train1
 train1$pickup_datetime=NULL
 testt$date=NULL
 train1$date=NULL
 
 train1$Time=NULL
 train1$pickup_longitude=NULL
 testt$pickup_longitude=NULL
 train1$pickup_latitude=NULL
 testt$pickup_longitude=NULL

 train1=na.omit(train1)
 testt=na.omit(testt)
 
 #Divide data into train and test 
 set.seed(1234)
 train1.index = createDataPartition(train1$fare_amount, p = .80, list = FALSE)
 traink = train1[ train1.index,]
 testk  = train1[-train1.index,]
 
 
 
 #####Decision tree
 # ##rpart for regression
 fit = rpart(fare_amount ~ ., data = train1, method = "anova")
 
 #Predict for new test cases
 predictions_DT1 = predict(fit, testk[,-1])
 
 #MAPE
 #calculate MAPE
 MAPE = function(y, yhat){
    mean(abs((y - yhat)/y)*100)
 }
 
 MAPE(testk[,1], predictions_DT1)
 
 
 #run regression model
 lm_model = lm(fare_amount ~., data = traink)
 
 #Summary of the model
 summary(lm_model)
 
 #Predict
 predictions_LR = predict(lm_model, testk[,-1])
 
 #Calculate MAPE
 MAPE(testk[,1], predictions_LR)
 
 
 
 ###########Random Forest
 RF_model = randomForest(fare_amount ~ ., traink, importance = TRUE, ntree = 500)
 RF_Predictions = predict(RF_model, testk[,-1])
 
 summary(RF_Predictions)
 

 MAPE = function(y, yhat){
   mean(abs((y - yhat)/y*100))
 }

 MAPE(testk[,1], RF_Predictions)
 
 
 RMSE = function(m, o){
    sqrt(mean((m - o)^2))
 }
 RMSE(testk[,1],RF_Predictions)
 
 
 
 #############pedicting fareamount for test data using Random forest
 RF_Predictions = predict(RF_model, testt)
 
 
 
 write.csv(RF_Predictions, file = "MyData.csv") 
 
 
 
 
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 