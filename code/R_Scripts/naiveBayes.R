#######################################################################################################
# naiveBayes.R
#
#######################################################################################################
# Shubhi Tiwari 12 April, 2018
# 
########################################################################################################

library(matrixStats)
library('caret')
library('pROC')
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
model_nb<-train(trainData[,predictors],trainData[,outcomeName],method='nb',trControl = train_ctrl,metric = 'Accuracy')
pred_nb<-predict.train(object=model_nb,testData[,predictors],type="raw",metric='Accuracy')
table(pred_nb)
nb_cnfMat <- confusionMatrix(pred_nb,testData[,outcomeName])
acc_nb <- nb_cnfMat$overall["Accuracy"]
sen_nb <- nb_cnfMat$byClass["Sensitivity"]
spe_nb <- nb_cnfMat$byClass["Specificity"]
ROC_nb <- roc(predictor=as.numeric(pred_nb),
              response=testData$Label,
              levels=rev(levels(testData$Label)))


#external validation
inExFile<- readline(prompt="Enter external data set file name : ")
Exdata <- read.csv(file=inExFile,header=TRUE,sep=",")
pred_nb_ex<-predict.train(object=model_nb,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_nb_ex)
nb_cnfMat_ex <- confusionMatrix(pred_nb_ex,Exdata[,outcomeName])
acc_nb_ex <- nb_cnfMat_ex$overall["Accuracy"]
sen_nb_ex <- nb_cnfMat_ex$byClass["Sensitivity"]
spe_nb_ex <- nb_cnfMat_ex$byClass["Specificity"]
ROC_nb_ex <- roc(predictor=as.numeric(pred_nb_ex),
              response=Exdata$Label,
              levels=rev(levels(Exdata$Label)))

