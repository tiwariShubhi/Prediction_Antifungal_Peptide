#BDMH Assignment R scripts on Data Mining
#Meghal Dani, MT17144, M.Tech CB

#-------------------------------------------------------------------------------------------------------------------------------
#Q1: Calculate mean and standard deviation (sd) of each gene in Early and Late stage samples. 
#Report the genes which have significant mean difference in early and late stage using a T test. 
#Report the genes which have p-value less than 0.05 along with their mean and sd in early and late stage 
#in a file names significant_genes.csv.
#------------------------------------------------------------------------------------------------------------------------------

#asking for user input for negative and positive file:
pos_file <- readline("Enter a positive file name with path: ")
neg_file <- readline("Enter a negative file name with path: ")
#reading xae.pos.csv file : Early stage samples 
positive <- read.csv(pos_file,header =TRUE)
#reading xae.neg.csv : Late Stage samples
negative <- read.csv(neg_file,header =TRUE)
#find standard deviation and mean of positive dataset
s_dev_pos <- apply(as.matrix(positive),2,sd)
mean_pos <- apply(as.matrix(positive),2,mean)
#find standar deviation and mean of negative dataset
s_dev_neg <- apply(as.matrix(negative),2,sd)
mean_neg <- apply(as.matrix(negative),2,mean)

#t-test
t_test= stack(mapply(function(positive, negative) t.test(positive,negative)$p.value, positive, negative))
#obtain p values of each gene
t_pval = as.matrix(t_test[,-2])
rownames(t_pval) = t_test[,2]
#save all genes with p value < 0.05 in S_genes and save all data in significant_genes.csv 
data = cbind(t_pval,as.matrix(mean_pos),as.matrix(s_dev_pos),as.matrix(mean_neg),as.matrix(s_dev_neg))
colnames(data) = c("pval","pos_mean","pos_sd","neg_mean","neg_sd")
p_val = which(data[,1] < 0.05)
s_genes = data[p_val,]
write.table(s_genes,file="significant_genes.csv",sep=",",col.names=T,row.names=T)


#---------------------------------------------------------------------------------------------------------------------------------------
#Q2Find the enrichment of various Gene Ontology terms for the genes 
#which have significant difference in their p-value in 1 Question and 
#report in it enrichment.csv along with p-value and score. 
#---------------------------------------------------------------------------------------------------------------------------------------------

#Go term analysis package used is enrichR
install.packages("enrichR")
library(enrichR)
dbs <- listEnrichrDbs()
#we did analysis on molecular_function , cellular component and biological process
dbs <- c("GO_Molecular_Function_2015", "GO_Cellular_Component_2015", "GO_Biological_Process_2015")
enriched <- enrichr(as.character(rownames(s_genes)), dbs)
#combine p values and scores of each analysis row-wise and save in .csv file
enrich_data <- rbind(as.matrix(enriched$GO_Molecular_Function_2015),as.matrix(enriched$GO_Cellular_Component_2015),as.matrix(enriched$GO_Biological_Process_2015))
write.table(enrich_data,file="enrichment.csv",sep=",",col.names=NA)

#----------------------------------------------------------------------------------------------------------------------------------------
#Q3 Divide the Dataset into 80:20 for training and validation for developing machine learning models 
#and Pre-process the data to covert training data into Z-Scores or unit normalization. 
#Select the best features using any feature selection method and save the features in selected_features.csv
#---------------------------------------------------------------------------------------------------------------------------------------

#add labels to positive data =1 and negative data =0
positive$label <- rep(1,nrow(positive))
negative$label <- rep(0,nrow(negative))
#combine both positive and negative data
sample_data <- rbind(positive,negative)
final_data <- sample_data[sample(nrow(sample_data)),]
#install and load caret and all other dependent libraries
install.packages("caret", dependencies = c("Depends", "Suggests"))
library(munsell)
library(ggplot2) 
library(ModelMetrics) 
library(recipes) 
library(assertthat) 
library(bindrcpp) 
library(glue) 
library(pkgconfig)
library(DEoptimR) 
library(caret) 
library("caret")
#check final structure of data and for na values
str(final_data)
sum(is.na(final_data)) #no na values were found

#Spliting training set into two parts based on outcome: 80% and 20%
index <- createDataPartition(final_data$label, p=0.80, list=FALSE)
trainSet <- final_data[ index,]
testSet <- final_data[-index,]
#removing columns with almost zero variance 
nzv <- nearZeroVar(trainSet)
filtered_train <- trainSet[,-nzv]
filtered_test <- testSet[,-nzv]
#normalise training set data 
procValues <- preProcess(filtered_train[,-466],method = c("center", "scale"))
scaledTrainData <- predict(procValues,filtered_train[,-466])
scaledTrainData <- cbind(scaledTrainData,trainSet[,501])
#adding name to column having labels for classification
colnames(scaledTrainData)[466]="label"
#nomralise testing data on training z score parameters using procvalues
scaledTestData <- predict(procValues,filtered_test[,-466])
scaledTestData <- cbind(scaledTestData, testSet[,501])
#changing integer labels to string 
scaledTrainData$label[scaledTrainData$label == 0] <- "neg"
scaledTrainData$label[scaledTrainData$label == 1] <- "pos"
scaledTestData$label[scaledTestData$label == 0] <- "neg"
scaledTestData$label[scaledTestData$label == 1] <- "pos"
colnames(scaledTestData)[466]="label"

#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
outcomeName <- "label"
predictors<-names(scaledTrainData)[!names(scaledTrainData) %in% outcomeName]
cancer_st <- rfe(scaledTrainData[,predictors], scaledTrainData[,outcomeName],
                         rfeControl = control)
#selecting top 5 features and saving in .csv file
predictors  <- predictors(cancer_st)[1:5]
write.table(predictors,file="selected_features.csv",sep=",")

#---------------------------------------------------------------------------------------------------------------------------------------
#Q4 Using features Question3, develop machine learning models using at least three techniques (e.g. SVM, Logistic Regression, Random Forest) 
#along with fivefold cross-validation.
#---------------------------------------------------------------------------------------------------------------------------------------

#factor labels
scaledTrainData$label <- as.factor(scaledTrainData$label)
scaledTestData$label <- as.factor(scaledTestData$label)
outcomeName <- 'label'
#5 fold cross validation
fitControl <- trainControl(method = "repeatedcv",number = 5,repeats = 5,classProbs = TRUE, summaryFunction = twoClassSummary)
#creation of learning models (random forest, neural network, glm and svm)
model_rf<-train(scaledTrainData[,predictors],scaledTrainData[,outcomeName],method='rf',trControl = fitControl,metric = "ROC")
model_nnet<-train(scaledTrainData[,predictors],scaledTrainData[,outcomeName],method='nnet',trControl = fitControl,metric = "ROC")
model_glm<-train(scaledTrainData[,predictors],scaledTrainData[,outcomeName],method='glm',trControl = fitControl,metric = "ROC")
model_svm<-train(scaledTrainData[,predictors],scaledTrainData[,outcomeName],method='svmLinear2',trControl = fitControl,metric = "ROC")

#-------------------------------------------------------------------------------------------------------------------------------------------------
#Q5 Using grid search find optimal parameters for your classifier and develop a model on 80% training data and validate on 20% data. 
#(Keep in mind to apply same prepossessing on validation data as you applied on Training data.)
#-------------------------------------------------------------------------------------------------------------------------------------------------

#grid search
tune_svm <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:5))
mod_svm <- train(scaledTrainData[,predictors], scaledTrainData[,outcomeName], 
             method = "svmRadial",
             preProc = c("center", "scale"),
             tuneGrid = tune_svm,
             metric = "ROC",
             trControl = fitControl)
#obtain roc of the model
svm_train_roc <- max(mod_svm$results[,"ROC"])
#find parameters of model
modelLookup(model="rf")
tune_rf <- expand.grid(mtry=1)
mod_rf <- train(scaledTrainData[,predictors], scaledTrainData[,outcomeName], 
             method = "rf",
             preProc = c("center", "scale"),
             tuneGrid = tune_rf,
             metric = "ROC",
             trControl = fitControl)
rf_train_roc <- max(mod_rf$results[,"ROC"])

modelLookup(model="nnet")
tune_nnet <- expand.grid(size=2,decay=1)
mod_nnet <- train(scaledTrainData[,predictors], scaledTrainData[,outcomeName], 
             method = "nnet",
             preProc = c("center", "scale"),
             tuneGrid = tune_nnet,
             metric = "ROC",
             trControl = fitControl)
nnet_train_roc <- max(mod_nnet$results[,"ROC"])

#prediction on training data and confusion matrix
library(ROCR)
svm1 = predictions<-predict.train(object=model_svm,scaledTrainData[,predictors],type="raw")
table(svm1)
perf_ROC=performance(predictions,"tpr","fpr")
rf1= predictions<-predict.train(object=model_rf,scaledTrainData[,predictors],type="raw")
table(rf1)
nnet1= predictions <- predict.train(object = model_nnet,scaledTrainData[,predictors],type="raw")
table(nnet1)

svm_train = confusionMatrix(svm1,scaledTrainData[,outcomeName])
#save all relevant parameters
svm_train_ac<-svm_train$overall["Accuracy"]
svm_train_sn <- svm_train$byClass["Sensitivity"]
svm_train_sp <- svm_train$byClass["Specificity"]

rf_train = confusionMatrix(rf1,scaledTrainData[,outcomeName])
##save all relevant parameters
rf_train_ac<-rf_train$overall["Accuracy"]
rf_train_sn <- rf_train$byClass["Sensitivity"]
rf_train_sp <- rf_train$byClass["Specificity"]

nnet_train = confusionMatrix(nnet1,scaledTrainData[,outcomeName])
#save all relevant parameters
nnet_train_ac<-nnet_train$overall["Accuracy"]
nnet_train_sn <- nnet_train$byClass["Sensitivity"]
nnet_train_sp <- nnet_train$byClass["Specificity"]

#prediction on testing data and confusion matrix
svm2 = predictions<-predict.train(object=model_svm,scaledTestData[,predictors],type="raw")
table(svm2)
rf2= predictions<-predict.train(object=model_rf,scaledTestData[,predictors],type="raw")
table(rf2)
nnet2= predictions <- predict.train(object = model_nnet,scaledTestData[,predictors],type="raw")
table(nnet2)

#prediction using grid search model
svm_grid = predictions<-predict.train(object=mod_svm,scaledTestData[,predictors],type="raw")
table(svm_grid)
rf_grid= predictions<-predict.train(object=mod_rf,scaledTestData[,predictors],type="raw")
table(rf_grid)
nnet_grid= predictions <- predict.train(object = mod_nnet,scaledTestData[,predictors],type="raw")
table(nnet_grid)
svm_grid_test = confusionMatrix(svm_grid,scaledTestData[,outcomeName])
rf_grid_test = confusionMatrix(rf_grid,scaledTestData[,outcomeName])
nnet_grid_test = confusionMatrix(nnet_grid,scaledTestData[,outcomeName])

#confusion matrix of models
svm_test = confusionMatrix(svm2,scaledTestData[,outcomeName])
svm_test_ac<-svm_test$overall["Accuracy"]
svm_test_sn <- svm_test$byClass["Sensitivity"]
svm_test_sp <- svm_test$byClass["Specificity"]

rf_test = confusionMatrix(rf2,scaledTestData[,outcomeName])
rf_test_ac<-rf_test$overall["Accuracy"]
rf_test_sn <- rf_test$byClass["Sensitivity"]
rf_test_sp <- rf_test$byClass["Specificity"]

nnet_test = confusionMatrix(nnet2,scaledTestData[,outcomeName])
nnet_test_ac<-nnet_test$overall["Accuracy"]
nnet_test_sn <- nnet_test$byClass["Sensitivity"]
nnet_test_sp <- nnet_test$byClass["Specificity"]

#ROC calculation for test dataset
predictions_svm <- predict(model_svm, scaledTestData )
svm_test_ROC <- roc(predictor=as.numeric(predictions_svm),
                    response=scaledTestData$label,
                    levels=rev(levels(scaledTestData$label)))

predictions_rf <- predict(model_rf, scaledTestData )
rf_test_ROC <- roc(predictor=as.numeric(predictions_rf),
                    response=scaledTestData$label,
                    levels=rev(levels(scaledTestData$label)))

predictions_nnet <- predict(model_nnet, scaledTestData )
nnet_test_ROC <- roc(predictor=as.numeric(predictions_nnet),
                    re8sponse=scaledTestData$label,
                    levels=rev(levels(scaledTestData$label)))


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q6 Report accuracy, sensitivity, specificity, and ROC on training data and validation data in
# training_result.csv and validation_result.csv.
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------


#training data
data.a <- c(svm_train_ac,svm_train_sn,svm_train_sp,svm_train_roc,rf_train_ac,rf_train_sn,rf_train_sp,rf_train_roc,nnet_train_ac,nnet_train_sn,nnet_train_sp,nnet_train_roc)
matrix.a <- matrix(data.a, nrow = 3, ncol = 4, byrow = TRUE)
colnames(matrix.a) = c("Sensitivity","Specificity","Accuracy","ROC")
rownames(matrix.a) = c("svm","rf","nnet")
write.table(matrix.a,file="training_result.csv",sep=",",col.names=NA)

#testing data
data.b <- c(svm_test_ac,svm_test_sn,svm_test_sp,0.3962,rf_test_ac,rf_test_sn,rf_test_sp,0.7034,nnet_test_ac,nnet_test_sn,nnet_test_sp,0.6078)
matrix.b <- matrix(data.b, nrow = 3, ncol = 4, byrow = TRUE)
colnames(matrix.b) = c("Sensitivity","Specificity","Accuracy","ROC")
rownames(matrix.b) = c("svm","rf","nnet")
write.table(matrix.b,file="validation_result.csv",sep=",",col.names=NA)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q7 Cluster your data using K-Means clustering and find the number of optimal clusters using Silhouette distance. 
#Save the Clustering plot obtained on the best number of clusters in pdf format. 
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#kmeans package loaded
pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
library(factoextra)
library(NbClust)
require(cluster)
library(cluster)
#a reference start by taking 3 clusters and silhoutte method is run
km.res <- kmeans(final_data, 3, nstart = 1)
sil <- silhouette(km.res$cluster, dist(final_data))
fviz_silhouette(sil) 

#we found 2 as best parameter size reported was largest = 303 
#Optimal no. of clusters are 2
pc = princomp(final_data[,1:500])
new_df = cbind(pc$loadings[,1],pc$loading[,2])
clusters <- kmeans(new_df, 2)

#output clusters in a pdf file 
pdf("kmeans_output.pdf")
plot(new_df, col =(clusters$cluster +1) , main="K-Means result with 2 clusters", pch=2, cex=2)
dev.off()

#-------------------------------------------------FINISH--------------------------------------------------------------------
