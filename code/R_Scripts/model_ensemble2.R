

library(matrixStats)
library('caret')
library('pROC')
library('RWeka')
library(klaR)
library(elmNN)
library(adabag)
library(plyr)


library(mlbench)
library(caret)
library(caretEnsemble)

inFile<- readline(prompt="Enter data set file name : ")
trainData <- read.csv(file=inFile,header=TRUE,sep=",",row.names = 1)
inExFile<- readline(prompt="Enter external data set file name : ")
Exdata <- read.csv(file=inExFile,header=TRUE,sep=",",row.names = 1)

outcomeName<- 'Label'


predictors<-colnames(trainData)[!colnames(trainData) %in% outcomeName]
control <- trainControl(method = "repeatedcv", number = 5, repeats = 5,classProbs = TRUE,savePredictions = TRUE)




#control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('svmRadial', 'J48')
set.seed(3233)
models <- caretList(trainData[,predictors], trainData[,outcomeName], trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)


# correlation between results
modelCor(results)
splom(results)

#individual prediction of each method on data
p <- as.data.frame(predict(models, newdata=head(Exdata[,predictors])))
print(p)


# stack using random forest
set.seed(3233)
stack.rf <- caretStack(models, method="rf", metric="ROC", trControl=control,type="prob")
print(stack.rf)

# validation
library("caTools")
model_preds <- lapply(models, predict, newdata=Exdata[,predictors], type="prob")
#model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)

model_preds$ensemble <- predict(stack.rf,newdata=Exdata[,predictors],type="prob")
#CF <- coef(stack.rf$ens_model$finalModel)[,outcomeName]
CF <- coef(stack.rf$models$svmRadial)[,-1]
CF
colAUC(model_preds, Exdata[,outcomeName])
CF




colAUC(model_preds$ensemble,Exdata[,outcomeName])


#my prediction trial
p_ens <- as.data.frame(predict(stack.rf, newdata=head(Exdata[,predictors])))
print(p_ens)


#########
###trying caretEnsemble

