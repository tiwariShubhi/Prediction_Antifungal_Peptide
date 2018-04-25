#######################################################################################################
#Antifungal peptides prediction
#######################################################################################################
#Models used : 
# 1.Naive Bayes, 
# 2.Random Forest, 
# 3.SVM Linear, 
# 4.SVM Radial, 
# 5.J48, 
# 6.ELM, 
# 7.Adaboost, 
# 8.KNN
#######################################################################################################
# Meghal Dani 12 April, 2018
# Shubhi Tiwari 22 April, 2018
########################################################################################################

library(matrixStats)
library('caret')
library('pROC')
library('RWeka')

# read all files as command line args
# args <- commandArgs(TRUE)
# #args
# inFile <- args[1]
# inExFile <- args[2]
# outputCsv <- args[3]
outputCsv <- "/media/adalove/WorkDrive/M.Tech/Sem2/BDMH/Project/Prediction_Antifungal_Peptide/results/res_main_n15_c15_bin2.csv"


inFile<- readline(prompt="Enter data set file name : ")
trainData <- read.csv(file=inFile,header=TRUE,sep=",",row.names = 1)
inExFile<- readline(prompt="Enter external data set file name : ")
Exdata <- read.csv(file=inExFile,header=TRUE,sep=",",row.names = 1)

# splitting data into 80:20 for internal validation
#set.seed(2005)
#intrain <- createDataPartition(y = data$Label, p= 0.8, list = FALSE)
#trainData <- data[intrain,]
#testData <- data[-intrain,]
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
tunegrid <- expand.grid(.mtry=c(1:15))#,.ntree = c(50,100,150,200,250,300,350,400,450,500,550,700,1000))
modellist <- list()
for (ntree in c(50,350)) {
  set.seed(7)
  fit <- train(trainData[,predictors],trainData[,outcomeName],method='rf',trControl = train_ctrl,metric = 'Accuracy',tuneGrid = tunegrid,tuneLength = 10, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
#dotplot(results)
model_rf<-train(trainData[,predictors],trainData[,outcomeName],method='rf',trControl = train_ctrl,metric = 'Accuracy',tuneGrid = tunegrid,tuneLength = 10,ntree= 350)
pred_rf<-predict.train(object=model_rf,trainData[,predictors],type="raw",metric='Accuracy')
table(pred_rf)
rf_cnfMat <- confusionMatrix(pred_rf,trainData[,outcomeName])
acc_rf <- rf_cnfMat$overall["Accuracy"]
sen_rf <- rf_cnfMat$byClass["Sensitivity"]
spe_rf <- rf_cnfMat$byClass["Specificity"]
ROC_rf <- roc(predictor=as.numeric(pred_rf),
              response=trainData$Label,
              levels=rev(levels(trainData$Label)))
s <- paste(" Random Forest",acc_rf,sep=',')
s <- paste(s,sen_rf,sep=',')
s <- paste(s,spe_rf,sep=',')
s <- paste(s,ROC_rf$auc[1],sep=',')
res <- paste(res,s,sep='\n')
res

#external validation
pred_rf_ex<-predict.train(object=model_rf,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_rf_ex)
rf_cnfMat_ex <- confusionMatrix(pred_rf_ex,Exdata[,outcomeName])
acc_rf_ex <- rf_cnfMat_ex$overall["Accuracy"]
sen_rf_ex <- rf_cnfMat_ex$byClass["Sensitivity"]
spe_rf_ex <- rf_cnfMat_ex$byClass["Specificity"]
ROC_rf_ex <- roc(predictor=as.numeric(pred_rf_ex),
                 response=Exdata$Label,
                 levels=rev(levels(Exdata$Label)))
s <- paste("",acc_rf_ex,sep=',')
s <- paste(s,sen_rf_ex,sep=',')
s <- paste(s,spe_rf_ex,sep=',')
s <- paste(s,ROC_rf_ex$auc[1],sep=',')
res <- paste(res,s,sep=',')
res



#SVM Linear-------------------------------------------------------------------------------
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

#SVM Radial--------------------------------------------------------------------------------------
grid_radial <- expand.grid(sigma = c(0.0005,0.001,0.01,0.05, 0.1, 0.5),
                           C = c(0.01, 0.05, 0.1, 0.25,
                                 1, 2,4,8))
set.seed(3233)
model_svm_rbf<-train(trainData[,predictors],trainData[,outcomeName],method='svmRadial',trControl = train_ctrl,metric = 'Accuracy',tuneGrid = grid_radial,tuneLength = 10)
pred_svm_rbf<-predict.train(object=model_svm_rbf,trainData[,predictors],type="raw",metric='Accuracy')
table(pred_svm_rbf)
svm_rbf_cnfMat <- confusionMatrix(pred_svm_rbf,trainData[,outcomeName])
acc_svm_rbf <- svm_rbf_cnfMat$overall["Accuracy"]
sen_svm_rbf <- svm_rbf_cnfMat$byClass["Sensitivity"]
spe_svm_rbf <- svm_rbf_cnfMat$byClass["Specificity"]
ROC_svm_rbf <- roc(predictor=as.numeric(pred_svm_rbf),
               response=trainData$Label,
               levels=rev(levels(trainData$Label)))
s <- paste(" SVM Radial",acc_svm_rbf,sep=',')
s <- paste(s,sen_svm_rbf,sep=',')
s <- paste(s,spe_svm_rbf,sep=',')
s <- paste(s,ROC_svm_rbf$auc[1],sep=',')
res <- paste(res,s,sep='\n')
res

#external validation
pred_svm_rbf_ex<-predict.train(object=model_svm_rbf,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_svm_rbf_ex)
svm_rbf_cnfMat_ex <- confusionMatrix(pred_svm_rbf_ex,Exdata[,outcomeName])
acc_svm_rbf_ex <- svm_rbf_cnfMat_ex$overall["Accuracy"]
sen_svm_rbf_ex <- svm_rbf_cnfMat_ex$byClass["Sensitivity"]
spe_svm_rbf_ex <- svm_rbf_cnfMat_ex$byClass["Specificity"]
ROC_svm_rbf_ex <- roc(predictor=as.numeric(pred_svm_rbf_ex),
                  response=Exdata$Label,
                  levels=rev(levels(Exdata$Label)))


s <- paste("",acc_svm_rbf_ex,sep=',')
s <- paste(s,sen_svm_rbf_ex,sep=',')
s <- paste(s,spe_svm_rbf_ex,sep=',')
s <- paste(s,ROC_svm_rbf_ex$auc[1],sep=',')
res <- paste(res,s,sep=',')
res

#J48--------------------------------------------------------------------------------------------------------------
require('Rweka')
grid_j48 <- expand.grid(C= c(0.001,0.01,0.05,0.1,0.25,0.5,1),M=c(0.01,0.1,1,3,6,9))
model_j48 <-train(trainData[,predictors],trainData[,outcomeName],method='J48',trControl = train_ctrl,tuneGrid = grid_j48,metric = 'Accuracy')
pred_j48 <-predict.train(object=model_j48,trainData[,predictors],type="raw",metric='Accuracy')
table(pred_j48)
j48_cnfMat <- confusionMatrix(pred_j48,trainData[,outcomeName])
acc_j48 <- j48_cnfMat$overall["Accuracy"]
sen_j48 <- j48_cnfMat$byClass["Sensitivity"]
spe_j48 <- j48_cnfMat$byClass["Specificity"]
ROC_j48 <- roc(predictor=as.numeric(pred_j48),
               response=trainData$Label,
               levels=rev(levels(trainData$Label)))

s <- paste(" J48",acc_j48,sep=',')
s <- paste(s,sen_j48,sep=',')
s <- paste(s,spe_j48,sep=',')
s <- paste(s,ROC_j48$auc[1],sep=',')
res <- paste(res,s,sep='\n')
res
#external validation
pred_j48_ex<-predict.train(object=model_j48,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_j48_ex)
j48_cnfMat_ex <- confusionMatrix(pred_j48_ex,Exdata[,outcomeName])
acc_j48_ex <- j48_cnfMat_ex$overall["Accuracy"]
sen_j48_ex <- j48_cnfMat_ex$byClass["Sensitivity"]
spe_j48_ex <- j48_cnfMat_ex$byClass["Specificity"]
ROC_j48_ex <- roc(predictor=as.numeric(pred_j48_ex),
                  response=Exdata$Label,
                  levels=rev(levels(Exdata$Label)))

s <- paste("",acc_j48_ex,sep=',')
s <- paste(s,sen_j48_ex,sep=',')
s <- paste(s,spe_j48_ex,sep=',')
s <- paste(s,ROC_j48_ex$auc[1],sep=',')
res <- paste(res,s,sep=',')
res

#SMO-------------------------------------------------------------------------------------------------------------
#model_smo<- SMO(trainData[,predictors],trainData[,outcomeName],control = Weka_control(K =list("weka.classifiers.functions.supportVector.RBFKernel", G =2)))
# model_smo <- SMO(Label~A+C+D+E+F+G+H+I+K+L+M+N+P+Q+R+S+T+V+W+Y,data=trainData,control = Weka_control(K ="weka.classifiers.functions.supportVector.RBFKernel"))
# pred_smo <-predict.train(object=model_smo,trainData[,predictors],type="raw",metric='Accuracy')
# table(pred_smo)
# smo_cnfMat <- confusionMatrix(pred_smo,trainData[,outcomeName])
# acc_smo <- smo_cnfMat$overall["Accuracy"]
# sen_smo <- smo_cnfMat$byClass["Sensitivity"]
# spe_smo <- smo_cnfMat$byClass["Specificity"]
# ROC_smo <- roc(predictor=as.numeric(pred_smo),
#                response=trainData$Label,
#                levels=rev(levels(trainData$Label)))



#ELM-Neural Network-----------------------------------------------------------------------------------------------------------------------------
library(elmNN)
grid_elm <- expand.grid(nhid = c(20,200,1000,5000,10000), actfun = c("sig","radbas","tansig"))
model_elm <-train(trainData[,predictors],trainData[,outcomeName],method='elm',trControl = train_ctrl,tuneGrid =grid_elm ,metric = 'Accuracy')
pred_elm <-predict.train(object=model_elm,trainData[,predictors],type="raw",metric='Accuracy')
table(pred_elm)
elm_cnfMat <- confusionMatrix(pred_elm,trainData[,outcomeName])
acc_elm <- elm_cnfMat$overall["Accuracy"]
sen_elm <- elm_cnfMat$byClass["Sensitivity"]
spe_elm <- elm_cnfMat$byClass["Specificity"]
ROC_elm <- roc(predictor=as.numeric(pred_elm),
               response=trainData$Label,
               levels=rev(levels(trainData$Label)))

s <- paste(" ELM",acc_elm,sep=',')
s <- paste(s,sen_elm,sep=',')
s <- paste(s,spe_elm,sep=',')
s <- paste(s,ROC_elm$auc[1],sep=',')
res <- paste(res,s,sep='\n')
res
#external validation
pred_elm_ex<-predict.train(object=model_elm,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_elm_ex)
elm_cnfMat_ex <- confusionMatrix(pred_elm_ex,Exdata[,outcomeName])
acc_elm_ex <- elm_cnfMat_ex$overall["Accuracy"]
sen_elm_ex <- elm_cnfMat_ex$byClass["Sensitivity"]
spe_elm_ex <- elm_cnfMat_ex$byClass["Specificity"]
ROC_elm_ex <- roc(predictor=as.numeric(pred_elm_ex),
                  response=Exdata$Label,
                  levels=rev(levels(Exdata$Label)))

s <- paste("",acc_elm_ex,sep=',')
s <- paste(s,sen_elm_ex,sep=',')
s <- paste(s,spe_elm_ex,sep=',')
s <- paste(s,ROC_elm_ex$auc[1],sep=',')
res <- paste(res,s,sep=',')
res
#ENSEMBL-Tree based method- ADABOOST-----------------------------------------------------------------------------------------------------
library(adabag)
library(plyr)
grid_ada <- expand.grid(mfinal = (1:3)*3, maxdepth = c(1, 3,10,20,100),
                    coeflearn = c("Breiman", "Freund", "Zhu"))

#grid_ada <- expand.grid(nhid = c(20,200,1000,5000,10000), actfun = c("sig","radbas","tansig"))
model_ada <-train(trainData[,predictors],trainData[,outcomeName],method='AdaBoost.M1',trControl = train_ctrl,tuneGrid = grid_ada ,metric = 'Accuracy')
pred_ada <-predict.train(object=model_ada,trainData[,predictors],type="raw",metric='Accuracy')
table(pred_ada)
ada_cnfMat <- confusionMatrix(pred_ada,trainData[,outcomeName])
acc_ada <- ada_cnfMat$overall["Accuracy"]
sen_ada <- ada_cnfMat$byClass["Sensitivity"]
spe_ada <- ada_cnfMat$byClass["Specificity"]
ROC_ada <- roc(predictor=as.numeric(pred_ada),
               response=trainData$Label,
               levels=rev(levels(trainData$Label)))

s <- paste(" Adaboost",acc_ada,sep=',')
s <- paste(s,sen_ada,sep=',')
s <- paste(s,spe_ada,sep=',')
s <- paste(s,ROC_ada$auc[1],sep=',')
res <- paste(res,s,sep='\n')
res
#external validation
pred_ada_ex<-predict.train(object=model_ada,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_elm_ex)
ada_cnfMat_ex <- confusionMatrix(pred_ada_ex,Exdata[,outcomeName])
acc_ada_ex <- ada_cnfMat_ex$overall["Accuracy"]
sen_ada_ex <- ada_cnfMat_ex$byClass["Sensitivity"]
spe_ada_ex <- ada_cnfMat_ex$byClass["Specificity"]
ROC_ada_ex <- roc(predictor=as.numeric(pred_ada_ex),
                  response=Exdata$Label,
                  levels=rev(levels(Exdata$Label)))

s <- paste("",acc_ada_ex,sep=',')
s <- paste(s,sen_ada_ex,sep=',')
s <- paste(s,spe_ada_ex,sep=',')
s <- paste(s,ROC_ada_ex$auc[1],sep=',')
res <- paste(res,s,sep=',')
res

#KNN------------------------------------------------------------------------------------------------------------------
#SVM Radial--------------------------------------------------------------------------------------
#grid_radial <- expand.grid(sigma = c(0.0005,0.001,0.01,0.05, 0.1, 0.5),
#                           C = c(0.01, 0.05, 0.1, 0.25,
#                                 1, 2,4,8))
set.seed(3233)
model_knn<-train(trainData[,predictors],trainData[,outcomeName],method='knn',trControl = train_ctrl,metric = 'Accuracy',tuneLength = 10)
pred_knn<-predict.train(object=model_knn,trainData[,predictors],type="raw",metric='Accuracy')
table(pred_knn)
knn_cnfMat <- confusionMatrix(pred_knn,trainData[,outcomeName])
acc_knn <- knn_cnfMat$overall["Accuracy"]
sen_knn <- knn_cnfMat$byClass["Sensitivity"]
spe_knn <- knn_cnfMat$byClass["Specificity"]
ROC_knn <- roc(predictor=as.numeric(pred_knn),
                   response=trainData$Label,
                   levels=rev(levels(trainData$Label)))
s <- paste(" KNN",acc_knn,sep=',')
s <- paste(s,sen_knn,sep=',')
s <- paste(s,spe_knn,sep=',')
s <- paste(s,ROC_knn$auc[1],sep=',')
res <- paste(res,s,sep='\n')
res

#external validation
pred_knn_ex<-predict.train(object=model_knn,Exdata[,predictors],type="raw",metric='Accuracy')
table(pred_knn_ex)
knn_cnfMat_ex <- confusionMatrix(pred_knn_ex,Exdata[,outcomeName])
acc_knn_ex <- knn_cnfMat_ex$overall["Accuracy"]
sen_knn_ex <- knn_cnfMat_ex$byClass["Sensitivity"]
spe_knn_ex <- knn_cnfMat_ex$byClass["Specificity"]
ROC_knn_ex <- roc(predictor=as.numeric(pred_knn_ex),
                      response=Exdata$Label,
                      levels=rev(levels(Exdata$Label)))
s <- paste("",acc_knn_ex,sep=',')
s <- paste(s,sen_knn_ex,sep=',')
s <- paste(s,spe_knn_ex,sep=',')
s <- paste(s,ROC_knn_ex$auc[1],sep=',')
res <- paste(res,s,sep=',')
res

#write results to csv

fp = file(outputCsv)

write(res,fp)
close(fp)
