
library(matrixStats)
library('caret')
library('pROC')
library('RWeka')
library(klaR)
library(elmNN)
library(adabag)
library(plyr)


inFile<- readline(prompt="Enter data set file name : ")
trainData <- read.csv(file=inFile,header=TRUE,sep=",",row.names = 1)
inExFile<- readline(prompt="Enter external data set file name : ")
Exdata <- read.csv(file=inExFile,header=TRUE,sep=",",row.names = 1)

outcomeName<- 'Label'


predictors<-colnames(trainData)[!colnames(trainData) %in% outcomeName]
train_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,classProbs = TRUE,savePredictions = TRUE)

set.seed(3233)
grid_nnet <- expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
model_nnet<-train(trainData[,predictors],trainData[,outcomeName],method='nnet',trControl = train_ctrl,metric = 'Accuracy',tuneLength = 10,tuneGrid = grid_nnet)
pred_nnet<-predict.train(object=model_nnet,trainData[,predictors],type="raw",metric='Accuracy')
table(pred_nnet)
nnet_cnfMat <- confusionMatrix(pred_nnet,trainData[,outcomeName])
acc_nnet <- nnet_cnfMat$overall["Accuracy"]
sen_nnet <- nnet_cnfMat$byClass["Sensitivity"]
spe_nnet <- nnet_cnfMat$byClass["Specificity"]
ROC_nnet <- roc(predictor=as.numeric(pred_nnet),
                response=trainData$Label,
                levels=rev(levels(trainData$Label)))


#external validation
pred_nnet_ex<-predict.train(object=model_nnet,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_nnet_ex)
nnet_cnfMat_ex <- confusionMatrix(pred_nnet_ex,Exdata[,outcomeName])
acc_nnet_ex <- nnet_cnfMat_ex$overall["Accuracy"]
sen_nnet_ex <- nnet_cnfMat_ex$byClass["Sensitivity"]
spe_nnet_ex <- nnet_cnfMat_ex$byClass["Specificity"]
ROC_nnet_ex <- roc(predictor=as.numeric(pred_nnet_ex),
                   response=Exdata$Label,
                   levels=rev(levels(Exdata$Label)))
