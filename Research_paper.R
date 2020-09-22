#Import dataframe
library(readxl)
variables_classif <- read_excel("variables.xlsx", sheet = "Sheet1")
DF_classif <- subset(DF_paper_light,select=variables_classif$VARIABLES)
DF_ss_consentement_continu <- subset(DF_paper_light, select = c(K_MT_DMDR,K_MT_OFFR,K_MT_RC,REV_SALR_MT_EPSE,R_MT_DMDR,NUM_ECH_MT_OFFR,NUM_ECH_MT_RC,R_MT_OFFR,RT_MT_K_TOT_OFFR, NUM_ECH_NBV_DMDR,NUM_ECH_MT_DMDR,PC_MT_CAPITAL))

#Train/test split
sample_size = floor(0.75*nrow(DF_paper_light))
set.seed(100)
train_ind = sample(seq_len(nrow(DF_paper_light)), size = sample_size)
train_regres = DF_ss_consentement_continu[train_ind, ]
test_regres = DF_ss_consentement_continu[train_ind, ]
train_classif = DF_classif[train_ind,]
test_classif = DF_classif[train_ind,]

#Build classification model
library(randomForest)
model_classif <- randomForest(PC_FIXE ~ .,data = train_classif, ntree=200)
y_pred_classif <- predict(model_classif, test_classif[,-ncol(test_classif)],type = 'class')
#Confusion matrix
mc_classif <- table(test_classif$PC_FIXE,y_pred_classif)
#Accuracy Rate
print(sum(diag(mc_classif))/sum(mc_classif))
#50 arbres: 0.9946921
#100 arbres: 0.99787
#200 arbres: 0.9989384
#300 arbres: 0.9978769

#################################### Linear regression model
#Build regression
model_regress <- lm(PC_MT_CAPITAL~K_MT_DMDR + K_MT_OFFR + K_MT_RC + 
                     REV_SALR_MT_EPSE + R_MT_DMDR + NUM_ECH_MT_OFFR + NUM_ECH_MT_RC + 
                     R_MT_OFFR + RT_MT_K_TOT_OFFR + NUM_ECH_NBV_DMDR + NUM_ECH_MT_DMDR,train_regres)
y_pred_regress <- predict(model_regress,test_regres)
summary(model_regress) #0.6619

#Prediction accuracy
actuals_preds_step <- data.frame(cbind(actuals=test_regres$PC_MT_CAPITAL, predicteds=y_pred_regress))  # make actuals_predicteds dataframe.
cor(actuals_preds_step)^2 #Rsq 0.6618669
mean(actuals_preds_step$actuals) #33653.89
mean(actuals_preds_step$predicteds) #33653.89
actuals_preds_step$error <- abs(actuals_preds_step$actuals - actuals_preds_step$predicteds)
max(actuals_preds_step$error) #377397.6
min(actuals_preds_step$error) #18.74151
mean(actuals_preds_step$error)#21469.38
median(actuals_preds_step$error) #10645.81
sd(actuals_preds_step$error) #35932.17

#Final Result: Classif x regression
actuals_preds_step <- data.frame(cbind(actuals=test_regres$PC_MT_CAPITAL,y_random_forest=as.numeric(y_random_forest)-1,y_pred_regress=y_pred_regress, adjusted_predicteds=y_pred_regress*(as.numeric(y_random_forest)-1)))  # make actuals_predicteds dataframe.
cor(actuals_preds_step)^2# 0.70795420  
mean(actuals_preds_step$actuals) #33653.89
mean(actuals_preds_step$adjusted_predicteds) #28826.45
actuals_preds_step$error <- abs(actuals_preds_step$actuals - actuals_preds_step$adjusted_predicted)
max(actuals_preds_step$error) #377397.6
min(actuals_preds_step$error) #0
mean(actuals_preds_step$error)#16466.27
median(actuals_preds_step$error) #3952.877
sd(actuals_preds_step$error) #35597.2


#################################### Regularized regression model: Elastic Net
library(caret)
library(glmnet) # for ridge regression
library(dplyr)

# Center y, X will be standardized in the modelling function
y_train_ridge <- train_regres %>% select(PC_MT_CAPITAL) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
y_test_ridge <- test_regres %>% select(PC_MT_CAPITAL) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
X_train_ridge <- train_regres %>% select(-PC_MT_CAPITAL) %>% as.matrix()
X_test_ridge <- test_regres %>% select(-PC_MT_CAPITAL) %>% as.matrix()

# Set training control
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random",
                              verboseIter = TRUE)

# Train the model
elastic_net_model <- train(PC_MT_CAPITAL ~ .,
                           data = cbind(y_train_ridge, X_train_ridge),
                           method = "glmnet",
                           #preProcess = c("center", "scale"),
                           tuneLength = 25,
                           trControl = train_control)
print(elastic_net_model)

# Check multiple R-squared
y_hat_enet <- predict(elastic_net_model, X_test_ridge)*(as.numeric(y_pred_classif)-1)
ssr_cv <- t(y_test_ridge  - y_hat_enet) %*% (y_test_ridge  - y_hat_enet)
mse_cv <- ssr_cv / nrow(y_test_ridge ) #1 707 516 602
rsq_enet <- cor(y_test_ridge, y_hat_enet)^2 #0.6681109
error_enet <- abs(y_test_ridge  - y_hat_enet)
max(error_enet) #417556.1
min(error_enet) #24.85192
mean(error_enet)#26142
median(error_enet) #23417.88
sd(error_enet) #32018.75


#################################### #Quantile regression
library(quantreg)
rqmodel <- rq(formula = PC_MT_CAPITAL ~ K_MT_DMDR + K_MT_OFFR + K_MT_RC + 
                REV_SALR_MT_EPSE + R_MT_DMDR + NUM_ECH_MT_OFFR + NUM_ECH_MT_RC + 
                R_MT_OFFR + RT_MT_K_TOT_OFFR + NUM_ECH_NBV_DMDR + NUM_ECH_MT_DMDR,data=train_regres)
print(rqmodel)
y_pred_rq <- predict(rqmodel,test_regres)*(as.numeric(y_random_forest)-1)

#prediction accuracy
actuals_preds_step <- data.frame(cbind(actuals=test_regres$PC_MT_CAPITAL, predicteds=y_pred_rq))  # make actuals_predicteds dataframe.
cor(actuals_preds_step)^2# 0.6599093
mean(actuals_preds_step$actuals) #33653.89
mean(actuals_preds_step$predicteds) #24142.24
actuals_preds_step$error <- abs(actuals_preds_step$actuals - actuals_preds_step$predicteds)
max(actuals_preds_step$error) #624696.5
min(actuals_preds_step$error) #0
mean(actuals_preds_step$error)#15950.93
median(actuals_preds_step$error) #3432.981
sd(actuals_preds_step$error) #41142.31

################################################################################################
#NOM_TGI Effect
DF_classif_TGI <-cbind(DF_paper_TGI$NOM_TGI,DF_classif)
names(DF_classif_TGI)[names(DF_classif_TGI)=='DF_paper_TGI$NOM_TGI'] <- 'NOM_TGI'

#Train/test split
sample_size = floor(0.75*nrow(DF_classif_TGI))
set.seed(100)
train_ind = sample(seq_len(nrow(DF_classif_TGI)), size = sample_size)
train_classif_TGI = DF_classif_TGI[train_ind,]
test_classif_TGI = DF_classif_TGI[train_ind,]

#Decision Tree without NOM_TGI
library(rpart)
modelAD <- rpart(PC_FIXE ~ .,data = train_classif,method = 'class',control = rpart.control(cp = 0, minsplit = 10,maxdepth=4))
y_probAD <- predict(modelAD, test_classif[,-ncol(test_classif)],type = 'prob')
y_predAD <- ifelse(y_probAD[,2]>0.5,1,0)
#Confusion Matrix
mcAD <- table(test_classif$PC_FIXE,y_predAD)
#Accuracy Rate
print(sum(diag(mcAD))/sum(mcAD)) #0.8046709
#F1-score
library(MLmetrics)
print(F1_Score(test_classif$PC_FIXE,y_predAD)) #0.6275304
#AUC
require(ROSE)
roc.curve(test_classif$PC_FIXE,y_predAD) #0.735

#Visualiser l'arbre
library(rpart.plot)
rpart.plot(modelAD, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)



#Decision Tree with NOM_TGI
modelAD_TGI <- rpart(PC_FIXE ~ .,data = train_classif_TGI,method = 'class')
y_probAD_TGI <- predict(modelAD_TGI, test_classif_TGI[,-ncol(test_classif_TGI)],type = 'prob')
y_predAD_TGI <- ifelse(y_probAD_TGI[,2]>0.5,1,0)
#Confusion Matrix
mcAD_TGI <- table(test_classif_TGI$PC_FIXE,y_predAD_TGI)
#Accuracy Rate
print(sum(diag(mcAD_TGI))/sum(mcAD_TGI)) #0.8789809
#F1-score
print(F1_Score(test_classif_TGI$PC_FIXE,y_predAD_TGI)) #0.7807692
#AUC
roc.curve(test_classif_TGI$PC_FIXE,y_predAD_TGI) #0.840

###########################################################################################
library(caret)
random_forest <- train(PC_FIXE ~ .,data = train_classif, method = "rf")
y_random_forest <- predict(random_forest, test_classif[,-ncol(test_classif)])
#Confusion Matrix
mcAD <- table(test_classif$PC_FIXE,y_random_forest)
#Accuracy Rate
print(sum(diag(mcAD))/sum(mcAD)) #0.9989384
#F1-score
library(MLmetrics)
print(F1_Score(test_classif$PC_FIXE,y_random_forest)) #0.9981584
#AUC
require(ROSE)
roc.curve(test_classif$PC_FIXE,y_random_forest) #0.998

#With TGI
random_forest_TGI <- train(PC_FIXE ~ .,data = train_classif_TGI, method = "rf")
y_random_forest_TGI <- predict(random_forest, test_classif_TGI[,-ncol(test_classif_TGI)])
#Confusion Matrix
mcAD <- table(test_classif_TGI$PC_FIXE,y_random_forest_TGI)
#Accuracy Rate
print(sum(diag(mcAD))/sum(mcAD)) #0.9989384
#F1-score
library(MLmetrics)
print(F1_Score(test_classif_TGI$PC_FIXE,y_random_forest_TGI)) #0.9981584
#AUC
require(ROSE)
roc.curve(test_classif_TGI$PC_FIXE,y_random_forest_TGI) #0.998
View(random_forest_TGI$finalModel$importance)

#####################ACP
acp.data  <- princomp(DF_ss_consentement_continu, cor = T, scores = T)
acp <- data.frame()
acp <- data.frame(cbind(acp.data$scores[,1],acp.data$scores[,2], DF_ss_consentement_continu$PC_MT_CAPITAL))
colnames(acp) <- c('comp1','comp2','actual_alimony')
train_regres_acp = acp[train_ind, ]
test_regres_acp = acp[train_ind, ]
rqmodel <- rq(formula = actual_alimony~.,data=train_regres_acp)
y_pred_rq_acp <- predict(rqmodel,test_regres_acp)*(as.numeric(y_random_forest)-1)

#prediction accuracy
actuals_preds_step <- data.frame(cbind(actuals=test_regres_acp$actual_alimony, predicteds=y_pred_rq_acp))  # make actuals_predicteds dataframe.
cor(actuals_preds_step)^2# 0.698
mean(actuals_preds_step$actuals) #33653.89
mean(actuals_preds_step$predicteds) #24090.11
actuals_preds_step$error <- abs(actuals_preds_step$actuals - actuals_preds_step$predicteds)
max(actuals_preds_step$error) #624696.5
min(actuals_preds_step$error) #0
mean(actuals_preds_step$error)#17679.79
median(actuals_preds_step$error) #6113.498
sd(actuals_preds_step$error) #37667.5

#####################GRAPH
library(ggplot2)
test_regres_acp$y_pred <- predict(rqmodel,test_regres_acp)
acp_zoom <- test_regres_acp[test_regres_acp$y_pred<0.0000000001,]
ggplot(acp_zoom, aes(x = comp1, y = y_pred)) + geom_point() + geom_abline(color = "blue")
test_regres_acp <- test_regres_acp[,-ncol(test_regres_acp)]

#########ACP and graphe
test_regres_light <- test_regres[test_regres$PC_MT_CAPITAL<600000,]
test_regres_light <- test_regres
test_regres_acp <- data.frame(cbind(princomp(test_regres_light[,-ncol(test_regres_light)],cor=T,scores=T)$scores[,1:2],test_regres_light$PC_MT_CAPITAL))
colnames(test_regres_acp) <- c('comp1','comp1','actuals_alimonies')
View(test_regres_acp)
plot(test_regres_acp[,1],test_regres_acp[,2],xlab="comp.1-20.0%",ylab="comp.2-16.7%")
abline(lm(formula = actuals_alimonies~comp1,data=test_regres_acp),col='red')
legend(0,500000,c('Predicted alimonies','Actual alimonies'),lty=c(1,NA),pch=c(NA,'o'),,col=c("red","black"),bg='white')

#Get the variances associated with the axes i.e. eigenvalues
val.propres <- princomp(test_regres_light[,-ncol(test_regres_light)],cor=T,scores=T)$sdev^2
print(val.propres/sum(val.propres))
