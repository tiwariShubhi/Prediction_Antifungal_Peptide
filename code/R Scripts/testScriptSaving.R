library(matrixStats)
library('caret')
library('pROC')
library('RWeka')


args <- commandArgs(TRUE)
#args
inFile <- args[1]
inExFile <- args[2]
outputCsv <- args[3]
# inFile
# inExFile


#inFile<- readline(prompt="Enter data set file name : ")
trainData <- read.csv(file=inFile,header=TRUE,sep=",",row.names = 1)
#inExFile<- readline(prompt="Enter external data set file name : ")
Exdata <- read.csv(file=inExFile,header=TRUE,sep=",",row.names = 1)


outcomeName<- 'Label'


predictors<-colnames(trainData)[!colnames(trainData) %in% outcomeName]
train_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,classProbs = TRUE)
set.seed(3233)
library(klaR)

#Naive bayes
model_nb<-train(trainData[,predictors],trainData[,outcomeName],method='nb',trControl = train_ctrl,metric = 'Accuracy')
pred_nb<-predict.train(object=model_nb,trainData[,predictors],type="raw",metric='Accuracy')
table(pred_nb)
nb_cnfMat <- confusionMatrix(pred_nb,trainData[,outcomeName])
acc_nb <- nb_cnfMat$overall["Accuracy"]
sen_nb <- nb_cnfMat$byClass["Sensitivity"]
spe_nb <- nb_cnfMat$byClass["Specificity"]
ROC_nb <- roc(predictor=as.numeric(pred_nb),
              response=trainData$Label,
              levels=rev(levels(trainData$Label)))
res <- "Model Name, Accuracy_train,Sensitivity_train,Specificity_train,ROC_AUC_train, ,Accuracy_external,Sensitivity_external,Specificity_external,ROC_AUC_external"
s <- paste(" Naive Bayes",acc_nb,sep=',')
s <- paste(s,sen_nb,sep=',')
s <- paste(s,spe_nb,sep=',')
s <- paste(s,ROC_nb$auc[1],sep=',')
res <- paste(res,s,sep='\n')
#external validation
res
pred_nb_ex<-predict.train(object=model_nb,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_nb_ex)
nb_cnfMat_ex <- confusionMatrix(pred_nb_ex,Exdata[,outcomeName])
acc_nb_ex <- nb_cnfMat_ex$overall["Accuracy"]
sen_nb_ex <- nb_cnfMat_ex$byClass["Sensitivity"]
spe_nb_ex <- nb_cnfMat_ex$byClass["Specificity"]
ROC_nb_ex <- roc(predictor=as.numeric(pred_nb_ex),
                 response=Exdata$Label,
                 levels=rev(levels(Exdata$Label)))


s <- paste("",acc_nb_ex,sep=',')
s <- paste(s,sen_nb_ex,sep=',')
s <- paste(s,spe_nb_ex,sep=',')
s <- paste(s,ROC_nb_ex$auc[1],sep=',')
res <- paste(res,s,sep=',')
res
#Random forest-------------------------------------------------------------------------------
model_svm<-train(trainData[,predictors],trainData[,outcomeName],method='svmLinear2',trControl = train_ctrl,metric = 'ROC')
pred_svm<-predict.train(object=model_svm,trainData[,predictors],type="raw",metric='Accuracy')
table(pred_svm)
svm_cnfMat <- confusionMatrix(pred_svm,trainData[,outcomeName])
acc_svm <- svm_cnfMat$overall["Accuracy"]
sen_svm <- svm_cnfMat$byClass["Sensitivity"]
spe_svm <- svm_cnfMat$byClass["Specificity"]
ROC_svm <- roc(predictor=as.numeric(pred_svm),
               response=trainData$Label,
               levels=rev(levels(trainData$Label)))
s <- paste(" SVM",acc_svm,sep=',')
s <- paste(s,sen_svm,sep=',')
s <- paste(s,spe_svm,sep=',')
s <- paste(s,ROC_svm$auc[1],sep=',')
res <- paste(res,s,sep='\n')
res
#external validation
pred_svm_ex<-predict.train(object=model_svm,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_svm_ex)
svm_cnfMat_ex <- confusionMatrix(pred_svm_ex,Exdata[,outcomeName])
acc_svm_ex <- svm_cnfMat_ex$overall["Accuracy"]
sen_svm_ex <- svm_cnfMat_ex$byClass["Sensitivity"]
spe_svm_ex <- svm_cnfMat_ex$byClass["Specificity"]
ROC_svm_ex <- roc(predictor=as.numeric(pred_svm_ex),
                  response=Exdata$Label,
                  levels=rev(levels(Exdata$Label)))
s <- paste("",acc_svm_ex,sep=',')
s <- paste(s,sen_svm_ex,sep=',')
s <- paste(s,spe_svm_ex,sep=',')
s <- paste(s,ROC_svm_ex$auc[1],sep=',')
res <- paste(res,s,sep=',')
res



fp = file(outputCsv)
write(res,fp)
close(fp)
