library(caret)
library(dplyr)
library(e1071)
library(NLP)
library(tm)
library(data.table)

#ds = read.csv('C:/Users/hugos/Desktop/FS Dataset/Health/data_cancer.csv', 
#              header = TRUE)

ds = read.csv('C:/Users/hugos/Desktop/FS Dataset/Health/data_cancer.csv', header = TRUE, stringsAsFactors = TRUE)
str(ds)

setDT(ds)[, (mycols) := lapply(.SD, as.numeric), .SDcols = mycols]

mycols <- c("id","diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean",              
             "smoothness_mean","compactness_mean","concavity_mean",         
             "concave.points_mean","symmetry_mean","fractal_dimension_mean", 
             "radius_se","texture_se","perimeter_se",           
             "area_se","smoothness_se","compactness_se",         
             "concavity_se","concave.points_se","symmetry_se",            
             "fractal_dimension_se","radius_worst","texture_worst",          
             "perimeter_worst","area_worst","smoothness_worst",       
             "compactness_worst","concavity_worst","concave.points_worst",   
             "symmetry_worst","fractal_dimension_worst")

#Convert to numeric
#setDT(ds)[, (mycols) := lapply(.SD, as.numeric), .SDcols = mycols]

#Convert classification to logical
data <- ds[,.(id,radius_mean,texture_mean,perimeter_mean,area_mean,smoothness_mean,compactness_mean,concavity_mean,concave.points_mean,symmetry_mean,fractal_dimension_mean,radius_se,texture_se,perimeter_se,area_se,smoothness_se,compactness_se,concavity_se,concave.points_se,symmetry_se,fractal_dimension_se,radius_worst,texture_worst,perimeter_worst,area_worst,smoothness_worst,compactness_worst,concavity_worst,concave.points_worst,symmetry_worst,fractal_dimension_worst,diagnosis = diagnosis == 1)]

dataclean <- na.omit(data)

#Separating train and test
inTrain<-createDataPartition(1:nrow(dataclean),p=0.7,list=FALSE)
train<- dataclean[inTrain]
test <- dataclean[-inTrain]


svm.model<-svm(diagnosis ~ id+radius_mean+texture_mean+perimeter_mean+area_mean+smoothness_mean+compactness_mean+concavity_mean+concave.points_mean+symmetry_mean+fractal_dimension_mean+radius_se+texture_se+perimeter_se+area_se+smoothness_se+compactness_se+concavity_se+concave.points_se+symmetry_se+fractal_dimension_se+radius_worst+texture_worst+perimeter_worst+area_worst+smoothness_worst+compactness_worst+concavity_worst+concave.points_worst+symmetry_worst+fractal_dimension_worst, data = train,
               type='one-classification',
               trControl = fitControl,
               nu=0.10,
               scale=TRUE,
               kernel="radial",
               metric = "ROC")

#Perform predictions 
svm.predtrain<-predict(svm.model,train)
svm.predtest<-predict(svm.model,test)

confTrain <- table(Predicted=svm.predtrain,
                   Reference=train$diagnosis[as.integer(names(svm.predtrain))])
confTest <- table(Predicted=svm.predtest,
                  Reference=test$diagnosis[as.integer(names(svm.predtest))])

confusionMatrix(confTest,positive='TRUE')

print(confTrain)
print(confTest)


