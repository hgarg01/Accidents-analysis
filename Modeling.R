library(UBL)
library(compareGroups)
library(caret)
library(neuralnet)
library(utils)
library(pROC)
library(naivebayes)
library(dplyr)

#Read the data frame and set factor variables
df=readRDS("C:/Users/harsh/Documents/R/Project1/final_df.rds")
df = read.csv("c:/users/harsh/documents/R/Project1/final_df_without_norm.csv")
cat_vars <- c("Accident_Severity", "Day_of_Week", "X1st_Road_Class", "Road_Type", "Junction_Detail", "Pedestrian_Crossing.Physical_Facilities","Urban_or_Rural_Area", "Journey_Purpose_of_Driver","Sex_of_Driver", "Age_Band_of_Driver","Vehicle_Manoeuvre" )
df[,cat_vars] <-data.frame(apply(df[cat_vars],2,as.factor))
 

#under.sample <- RandUnderClassif(Accident_Severity~., df, C.perc = list(fatal = 1, serious= 0.6, mild = 0.2))
c <- compareGroups(Accident_Severity~., data = df, method = 1)
createTable(c)


table(df$Accident_Severity)

#Create training set - 60% of data, Test set - 20% of data and validation set - 20% of data
set.seed(123)
trainIndex <- createDataPartition(df$Accident_Severity, p = .60,list=FALSE)
train_set <- df[trainIndex,]
temp <- df[-trainIndex,]
valIndex <- createDataPartition(temp$Accident_Severity, p = .50,list=FALSE)

validation_set <- temp[valIndex,]
test_set <- temp[-valIndex,]

#Create a smoted training set by oversampling minority class and undersampling majority classes
smoted_train_4_0.8_0.4 <- SmoteClassif(Accident_Severity~., train_set, C.perc = list('1' = 4,'2'= 0.8,'3'= 0.4), k = 4, dist = "HEOM")

table(train_set$Accident_Severity)
table(smoted_train$Accident_Severity)
train_y <-smoted_train[,"Accident_Severity"]
train_x <- smoted_train[,-3] 

#Naive Bayes with cross validation
library(e1071)
NB_with_CV <- naive_bayes(Accident_Severity~.,data = smoted_train,trControl = trainControl(method = 'cv', number = 5), laplace = TRUE)
library(naivebayes)
NB_model <- naive_bayes(Accident_Severity~.,data = train_set, laplace = T)
plot(NB_model)
pred.NB_CV<- predict(NB_model,validation_set,type = "class")
(tab1 <- table(pred.NB_CV,test_set$Accident_Severity))
##misclassification error
1-sum(diag(tab1))/sum(tab1)

cf <- confusionMatrix(pred.NB_CV, validation_set$Accident_Severity,positive = '1')
cf$byClass
t <- table(validation_set$Accident_Severity)
wtd_recall <- (cf$byClass[1,6]*t[1]+cf$byClass[2,6]*t[2]+cf$byClass[3,6]*t[3])/(nrow(test_set))
wtd_recall
predicted <- predict(NB_model, test_set, probability = TRUE)
multiclass.roc(response= test_set$Accident_Severity, predictor = factor(predicted, ordered = TRUE), plot = TRUE)

table(train_set$Accident_Severity)


rf.cutoff <- randomForest(Accident_Severity~., data = train_set,cutoff = c(0.1,0.15,0.75))
rf.strata <- randomForest(Accident_Severity~., data = smoted_train)
rf.classwt <- randomForest(Accident_Severity~., data = train_set, classwt = c(0.01,0.2,0.79))

plot(multiclass.roc(predictor = factor(rf.cutoff$votes, ordered = TRUE),response =train_set$Accident_Severity),main="black default, red stata, green classwt")
#plot(roc(rf.strata$votes[,2],train.data$y),col=2,add=T)
plot(roc(rf.classwt$votes[,2],train_set$Accident_Severity),col=3,add=T)

rf.100tree.cutoff <- randomForest(Accident_Severity~.,
                                 data = smoted_train, ntrees = 100, 
                                 mtry = 5,
                                 trControl = trainControl(method = 'cv', number = 5), 
                                 importance = TRUE,cutoff = c(0.1,0.15,0.75),classwt = c(3,1,0.2))

print(rf.100tree.cutoff)
plot(rf.100tree.cutoff)
pred_rf <- predict(rf.100tree.cutoff, newdata = test_set)

cf <- confusionMatrix(pred_rf, test_set$Accident_Severity)
cf$byClass
t <- table(test_set$Accident_Severity)
wtd_recall <- (cf$byClass[1,6]*t[1]+cf$byClass[2,6]*t[2]+cf$byClass[3,6]*t[3])/(nrow(test_set))
wtd_recall
mean(pred_rf == test_set$Accident_Severity) #0.5762256 With no var selection
importance(rf.100tree.cutoff)
varImpPlot(rf.100tree.cutoff)

predicted <- predict(rf.100tree.cutoff, test_set, probability = TRUE)
multiclass.roc(response= test_set$Accident_Severity, predictor = factor(predicted, ordered = TRUE), plot = TRUE)

costMatrix <- matrix(c(0,50,80,15,0,40,10,8,0),3)
mod_rf <- randomForest(Accident_Severity~., data = train_set, ntree = 50, params = list(loss = costMatrix))

########################################
# baseline multinomial regression model
#######################################
set.seed(500)
# Fit the model
model_MNom <- nnet::multinom(Accident_Severity~., data = smoted_train)
# Summarize the model
summary(model_MNom)
predicted.classes <- model_MNom %>% predict(test_set)

(tab2 <- table(predicted.classes,test_set$Accident_Severity))
#misclassification error
1-sum(diag(tab2))/sum(tab2)

head(predicted.classes)
mean(predicted.classes == test_set$Accident_Severity) # test Set Accuracy = 0.6624158974
saveRDS(model_MNom,'model_MNom_Accident_Severity.rds')

df_multinom_Predictions<-as.data.frame(cbind(predicted.classes,test$Accident_Severity))
write.csv(df_multinom_Predictions, 'multinom_Predictions_model_MNom.csv')


###Area under the curve

predicted <- predict(model_MNom, test_set, probability = TRUE)
multiclass.roc(response= test_set$Accident_Severity, predictor = factor(predicted, ordered = TRUE), plot = TRUE)

cf <- confusionMatrix(predicted.classes, test_set$Accident_Severity)
cf$byClass
t <- table(test_set$Accident_Severity)
wtd_recall <- (cf$byClass[1,6]*t[1]+cf$byClass[2,6]*t[2]+cf$byClass[3,6]*t[3])/(nrow(test_set))
wtd_recall

###########Create same models with 2 classes
levels(df$Accident_Severity) <- c(1,1,3)
table(df$Accident_Severity)

#df_2cls_smoted <- SmoteClassif(Accident_Severity~., df, C.perc = list('1' = 1, '3' = 0.5), k = 4, dist = "HEOM")
trainIndex <- createDataPartition(df_2cls_smoted$Accident_Severity, p = .70,list=FALSE)
train <- df_2cls_smoted[trainIndex,]
test <- df_2cls_smoted[-trainIndex,]
smoted_train <- SmoteClassif(Accident_Severity~., df, C.perc = list('1' = 1, '3' = 0.5), k = 4, dist = "HEOM")
table(train$Accident_Severity)

wtrf.2cls <- randomForest(Accident_Severity~.,
                                  data = train, ntrees = 50, 
                                  mtry = 5,
                                  importance = TRUE,classwt = c(3,0.2))

wtrf.2cls.cutoff <- randomForest(Accident_Severity~.,
                                 data = smoted_train, ntrees = 50, 
                                 mtry = 5,
                                 importance = TRUE,cutoff = c(0.3,0.7))

print(wtrf.2cls)
plot(wtrf.2cls)
pred_Severity_wtrf<- predict(wtrf.2cls, newdata = test)
cf <- confusionMatrix(pred_Severity_wtrf,test$Accident_Severity)
mean(pred_Severity_wtrf == test$Accident_Severity) #0.5762256 With no var selection
importance(wtrf.2cls)
varImpPlot(wtrf.2cls)

#NB 2 class
NB_2cls <- naive_bayes(Accident_Severity~., data = df_2cls_smoted, laplace = T)
plot(NB_2cls)
p<- predict(NB_2cls,test)
(tab1 <- table(p,test$Accident_Severity))
##misclassification error
1-sum(diag(tab1))/sum(tab1)
cf <- confusionMatrix(p,test$Accident_Severity)
cf$byClass


table(p, test$Accident_Severity)
predicted <- predict(NB_2cls, test, probability = TRUE)
multiclass.roc(response= test$Accident_Severity, predictor = factor(predicted, ordered = TRUE), plot = TRUE)


#ensemble
library(caret)
predDF <- data.frame(pred_rf, p, Accident_Severity = test_set$Accident_Severity, stringsAsFactors = F)
modelStack <- train(Accident_Severity ~., data = predDF, method = "gbm" )
summary(modelStack)

ensemble_pred <- predict(modelStack, test_set)
cf <- confusionMatrix( ensemble_pred,test_set$Accident_Severity)
cf$byClass
t <- table(validation_set$Accident_Severity)
wtd_recall <- (cf$byClass[1,6]*t[1]+cf$byClass[2,6]*t[2]+cf$byClass[3,6]*t[3])/(nrow(validation_set))
mean(pred_rf == validation_set$Accident_Severity)
mean(ensemble_pred == validation_set$Accident_Severity)

plot(modelStack)
trellis.par.set(caretTheme())
plot(modelStack, plotType = "level")

# Generate predictions on the test set
testPredNB <- predict(NB_model, newdata = test_set)
testPredRF <- predict(rf.under.100tree, newdata = test_set)


# Using the base learner test set predictions, 
# create the level-one dataset to feed to the ensemble
testPredLevelOne <- data.frame(testPredRF, testPredNB, Accident_Severity = test_set$Accident_Severity, stringsAsFactors = F)
combPred <- predict(modelStack, testPredLevelOne)

# Evaluate ensemble test performance
cf <- confusionMatrix(combPred, test_set$Accident_Severity)

# Evaluate base learner test performance 
confusionMatrix(testPredRF, testing$diagnosis)$overall[1]
confusionMatrix(testPredGBM, testing$diagnosis)$overall[1]
confusionMatrix(testPredLDA, testing$diagnosis)$overall[1]


##ensemble using caretEnsemble
algorithmList <- c('nb', 'glm', 'svmRadial')
set.seed(123)
library(caretEnsemble)
models <- caretList(Accident_Severity~., data= train_set, methodList=algorithmList)

results <- resamples(models)

