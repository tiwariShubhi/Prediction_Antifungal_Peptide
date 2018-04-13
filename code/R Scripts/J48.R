#######################################################################################################
# J48
#
#######################################################################################################
# Shubhi Tiwari 13 April, 2018
# 
########################################################################################################

library(matrixStats)
library('caret')
library('pROC')
library('RWeka')
inFile<- readline(prompt="Enter data set file name : ")
data <- read.csv(file=inFile,header=TRUE,sep=",")

# splitting data into 80:20 for internal validation
set.seed(2005)
intrain <- createDataPartition(y = data$Label, p= 0.8, list = FALSE)
trainData <- data[intrain,]
testData <- data[-intrain,]
outcomeName<- 'Label'


predictors<-colnames(trainData)[!colnames(trainData) %in% outcomeName]
train_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
set.seed(3233)
model_j48 <-train(trainData[,predictors],trainData[,outcomeName],method='J48',trControl = train_ctrl,metric = 'Accuracy')
pred_j48 <-predict.train(object=model_j48,testData[,predictors],type="raw",metric='Accuracy')
table(pred_j48)
j48_cnfMat <- confusionMatrix(pred_j48,testData[,outcomeName])
acc_j48 <- j48_cnfMat$overall["Accuracy"]
sen_j48 <- j48_cnfMat$byClass["Sensitivity"]
spe_j48 <- j48_cnfMat$byClass["Specificity"]
ROC_j48 <- roc(predictor=as.numeric(pred_j48),
              response=testData$Label,
              levels=rev(levels(testData$Label)))


#external validation
inExFile<- readline(prompt="Enter external data set file name : ")
Exdata <- read.csv(file=inExFile,header=TRUE,sep=",")
pred_j48_ex<-predict.train(object=model_j48,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_j48_ex)
j48_cnfMat_ex <- confusionMatrix(pred_j48_ex,Exdata[,outcomeName])
acc_j48_ex <- j48_cnfMat_ex$overall["Accuracy"]
sen_j48_ex <- j48_cnfMat_ex$byClass["Sensitivity"]
spe_j48_ex <- j48_cnfMat_ex$byClass["Specificity"]
ROC_j48_ex <- roc(predictor=as.numeric(pred_j48_ex),
                 response=Exdata$Label,
                 levels=rev(levels(Exdata$Label)))

