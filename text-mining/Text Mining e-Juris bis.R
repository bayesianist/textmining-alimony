library(tm)
library(stringr)
library(readxl)
library(dplyr)
library(qdap)
library(topicmodels)
library(tidyverse)
library(tidytext)
library(caret)
library(randomForest)
library(MLmetrics)
library(ROSE)
library(rpart)
library(doParallel)
library(caret)
library(quantreg)
library(ggplot2)
library(pscl)

############################################################IMPORTER LES DONNEES
############################################################

#Path to folder that holds multiple .docx files
folder_lotA <- "C:/Users/ThinkPad/Documents/Decisions justices/ocr/OCR_Version2/OCR_lotA_V2/DOCX_OCR_lotA_V2"
folder_lotB <- "C:/Users/ThinkPad/Documents/Decisions justices/ocr/OCR_Version2/OCR_lotB_V2/DOCX_OCR_lotB_V2"
folder_lotX <- "C:/Users/ThinkPad/Documents/Decisions justices/ocr/OCR_Version2/OCR_lotX/OCR_lotX_DOCX"

#create list of all .docx files in 3 folders (finalement pas necessaire)
list_lotA <- list.files(path=folder_lotA, pattern="*.docx")
list_lotB <- list.files(path=folder_lotB, pattern="*.docx")
list_lotX <- list.files(path=folder_lotX, pattern="*.docx")

#Create dataframe to stock justice decisions'text
#3 options:

#lotA  <-Corpus(DirSource(folder_lotA)) : ne marche pas
#lotA <- textreadr::read_dir(folder_lotA) : marche bien mais decoupe les texts par ligne ce qui n'est pas derangeant.
lotAbis <- readtext::readtext(paste0(folder_lotA, "/*.docx")) # marche parfaitement mais trop lourde #1269 decisions
lotAbis[,1] <- str_remove(lotAbis[,1], ".docx") #Supprimer les ".docx" dans le nom des fichiers.
lotAbis[,2] <- str_replace_all(lotAbis[,2], "[^[:alnum:]]", " ") #Supprimer les caracteres non alphanumerique dans les textes.

lotB <- readtext::readtext(paste0(folder_lotB, "/*.docx"))
lotB[,1] <- str_remove(lotB[,1], ".docx")
lotB[,2] <- str_replace_all(lotB[,2], "[^[:alnum:]]", " ") 

lotX <- readtext::readtext(paste0(folder_lotX, "/*.docx"))
lotX[,1] <- str_remove(lotX[,1], ".docx")
lotX[,2] <- str_replace_all(lotX[,2], "[^[:alnum:]]", " ") 

lotall <- rbind(lotAbis,lotB,lotX)


############################################################
############################################################PREPARER LES DF
############################################################
#Extraire le numero de decision
lotall$doc_id <- sub(".*_", "", lotall$doc_id) #extract num after the "_"
lotall$doc_id <- gsub("-", "/", lotall$doc_id) #changer "-" en "/" comme dans le fichier COMPRES

#Uniformer l'expression des montants en euro
lotall$text <- gsub("€","euro",lotall$text)
lotall$text <- gsub("Euros","euro",lotall$text)
lotall$text <- gsub("euros","euro",lotall$text)
lotall$text <- gsub("EUROS","euro",lotall$text)
lotall$text <- gsub("EUR","euro",lotall$text)
lotall$text <- gsub("eur","euro",lotall$text)
lotall$text <- gsub("EURO","euro",lotall$text)

#Importer le fichier COMPRES
COMPRES <- read_excel("C:/Users/ThinkPad/kDrive/LONG/Stage e-Juris/Phase 1/fichier_COMPRES_variables_de_base_pour_Fabrice.xlsx")
#Creer la variables predictives pour rÃ©gression: 
#Montant total du capital verse en une seule fois + Montant total du capital a versement echelonne
COMPRES$PC_MT_CAPITAL <- COMPRES$K_MT_FIXE + COMPRES$NUM_ECH_MT_FIXE
COMPRES_PC_FIXE <- COMPRES[,c('NUM_AFF','PC','PC_FIXE','PC_MT_CAPITAL')] #nrow 5453
colnames(COMPRES_PC_FIXE) <- c('doc_id','PC','PC_FIXE','PC_MT_CAPITAL') #harmoniser le nom des colonnes

#Joindre les dataframes par numero de decision
df <- lotall %>% inner_join(COMPRES_PC_FIXE,by="doc_id") #nrow 3707
#Enlever les cas de non-demande de PC
df <- subset(df,PC=="1") #nrow 3250
#Enlever les modalites "2" de PC fixe
df <- subset(df,PC_FIXE != 2) #nrow = 3246
df <- subset(df, select=-c(PC)) #Enlever la colonne "PC" 
# Pour eviter le probleme de noms des colonnes
colnames(df) <- make.names(colnames(df))
#To make sure of type factor
df$PC_FIXE <- as.factor(df$PC_FIXE)
df_nonum <- df[,-1] #Enlever le numero de decision pour l'apprentissage
#Supprimer les observations avec PC >500k et negatif
df_nonum <- df_nonum[df_nonum$PC_MT_CAPITAL<100000,]
df_nonum <- df_nonum[df_nonum$PC_MT_CAPITAL>=0,]


############################################################
############################################################FONCTION NETTOYAGE & PREPARER MDT NUMERIQUE
############################################################
nettoyage <- function(texttoclean){
  corpusA <- Corpus(VectorSource(texttoclean)) #transformation en corpus
  corpusA <- tm_map(corpusA, content_transformer(tolower))
  corpusA <- tm_map(corpusA, removePunctuation)
  corpusA <- tm_map(corpusA, removeNumbers) #A voir si il faut enlever pour la regression
  corpusA <- tm_map(corpusA, stripWhitespace)
  corpusA$content <- stemDocument(corpusA$content, language = 'french')
  # remove french accents
  corpusA$content = stringi::stri_trans_general(corpusA$content,"Latin-ASCII")
  #Creer nos stopwords
  stpw <- c("republique","francaisecour","tribunal", "grande", "instance", "decision","'","a","madame","monsieur","divorce","code","civil","euros","juge","parties",
            " 'article ","ete","date","entre","familiales","droit","mois","jugement","dun","etre","rue","sous","cas","ainsi","dit","larticle","dont","jour","fait",
            "peut","vie","articles","prononce","conformement","situation","numero","barreau","conditions","avoir","non","apres","procedure","droits","fax","ca",
            'prise','celuici','celles','pouvant','ledit','vertu','lequel','alors','actuellement','suivante','toutefois','pu','faite','na','hyperlink','conclusions',
            'alinea','celui','peuvent','pierre','rg','section','lien','annexe','ciapres','annexee','x','encore','autre','sis','letat','expressement','pourra',
            'dispositif','f','resulte','temps','attendu','quils','jean','suivantes','page','v','tel','h','suit','p','personne','eme','e','donc','prendre','fin',
            'avant','lun','loi','b','r','pris'," 'acte",'sauf','tant','faits','comme','o','faits','elles','u','concerne','suite','suivants','partie','ni',
            'toutes','lors','cidessus','greffier','saint','toute','consequences','consequence','sousigne','ci','eur','etant','devant','article','vu','dune',
            'tout','quil','mme','francaise','ans','declarent','er','i','nationalite','tous','presentes','si','plus','selon','christophe')
  corpusA <- tm_map(corpusA, removeWords, c(stopwords(kind = "french"), stpw))
  return(corpusA)
}

#Controle: Verifier les termes les plus utilises
freq_terms(nettoyage(lotall$text),100) #ajouter les termes non significatives a notre liste de stopword et recommence

get_mdtnumbis <- function(texttoclean){
  #tofind <- paste(c("[[:digit:][:blank:]]*[[:blank:]]EUR","[[:digit:][:blank:]]*[[:blank:]]euros","[[:digit:][:blank:]]*[[:blank:]]€"), collapse="|")
  #toreplace <- paste(c("[[:blank:]][[:digit:]][[:digit:]][[:blank:]]EUR","[[:blank:]][[:digit:]][[:digit:]][[:blank:]]euros","[[:blank:]][[:digit:]][[:digit:]][[:blank:]]€"),collapse="|")
  
  cash1<- str_extract_all(texttoclean, "[[:digit:][:blank:]]*[[:blank:]]euro")
  num <- gsub("[[:blank:]][[:digit:]][[:digit:]][[:blank:]]euro", "", c(cash1))
  num <- gsub("[^0-9,-]", " ", c(num))
  corpusnum <- Corpus(VectorSource(num)) #transformation en corpus
  corpusnum <- tm_map(corpusnum, stripWhitespace)
  corpusnum <- tm_map(corpusnum, removePunctuation)
  mdtnum <- DocumentTermMatrix(corpusnum)
  mdtnum <- as.data.frame(as.matrix(mdtnum)) #%>%  select_if(is.numeric)
  colnames(mdtnum) <- as.numeric(colnames(mdtnum))
  return(mdtnum)
}

get_mdtnum <- function(texttoclean){
  #Extraire des paragraphes contenant le mot prestation compensatoire: "prestation compensatoire" + XXX caractère
  para_PC <- str_extract_all(texttoclean,".{100}prestation compensatoire.{100}")
  para_PC_num <- gsub("[^0-9,]", " ", c(para_PC))
  #Extraire les montants en euros à partir de ces paragraphes.
  para_num <- str_extract_all(para_PC_num,"[:digit:]*[:digit:]*[:digit:].[:digit:]{3}")
  para_num <- gsub(" ", "", c(para_num))
  para_num <- str_extract_all(para_num,"[:digit:]*[:digit:]*[:digit:][:digit:]{3}")
  para_num <- gsub("[^0-9]", " ", c(para_num))
  corpus_para <- Corpus(VectorSource(para_num))
  #corpus_para <- tm_map(corpus_para, stripWhitespace)
  #corpus_para <- tm_map(corpus_para, removePunctuation)
  mdtpara <- DocumentTermMatrix(corpus_para)
  mdtpara <- as.data.frame(as.matrix(mdtpara))
  colnames(mdtpara) <- as.numeric(colnames(mdtpara))
  return(mdtpara)
}

#Controle:
View(get_mdtnum(test$text))



############################################################
############################################################PREPARER TRAIN/TEST POUR CLASSIFICATION
############################################################
#Repartir en donnees d'apprentissage et de test pour classification
sample_size = floor(0.75*nrow(df_nonum))
set.seed(100)
train_ind = sample(seq_len(nrow(df_nonum)), size = sample_size)
train = df_nonum[train_ind, ]
test = df_nonum[-train_ind, ]
table(train$PC_FIXE)/nrow(train) #Repartition PC dans train 0: 0.1550936 / 1: 0.8449064
table(test$PC_FIXE)/nrow(test) #Repartition PC dans test 0: 0.1571072 / 1: 0.8428928

#DocumentTermMatrix for train
mdt_train <- DocumentTermMatrix(nettoyage(train$text))
#Supprimer les termes peu frequentes
mdt_train_s <- removeSparseTerms(mdt_train, sparse=0.9)
#ui <- unique(mdt_s$i) #remove les lignes qui n'ont aucune terme
#mdt_s_new <- mdt_s[ui,]
mdt_train_df <- cbind(as.data.frame(as.matrix(mdt_train_s)),PC_FIXE =train$PC_FIXE)
#Sparse=1: 95218 sans les chiffres
#Sparse=0.995: #9468 sans les chiffres/#5270 apres steaming
#Sparse=0.99: #6584 sans les chiffres /#3607 apres steaming
#Sparse=0.9: #1229 apres steaming
# Pour eviter le probleme de noms des colonnes
colnames(mdt_train_df) <- make.names(colnames(mdt_train_df))

#DocumentTermMatrix for test
mdt_test <- DocumentTermMatrix(nettoyage(test$text))
mdt_test_df <- cbind(as.data.frame(as.matrix(mdt_test)),PC_FIXE =test$PC_FIXE)

# Match train & test column
w = colnames(mdt_test_df) %in% colnames(mdt_train_df) #identify test's columns in train
# remove columns which is not in train
mdt_test_df = mdt_test_df[,colnames(mdt_test_df)[w]]
w_i = colnames(mdt_train_df) %in% colnames(mdt_test_df) #identify train's columns in test
# set to 0 tous les colonnes qui ne sont pas dans test mais existent en train
mdt_test_df[,names(mdt_train_df)[!w_i]] = 0
# tri comme dans train
mdt_test_df <- mdt_test_df[, names(mdt_train_df)]
# Pour eviter le probleme de noms des colonnes
colnames(mdt_test_df) <- make.names(colnames(mdt_test_df))



############################################################
############################################################FONCTIONS POUR PREPARER REGRESSION
############################################################

options(scipen=999) #remove scientific notation

#Fonction pour recuperer la valeur maximales des classes de la discretisation une df numerique train
disval <- function(train,k){  #k = nombre de classes
  #Methodes tenant compte de la dispersion des classes
  #Algorithm ascendente hierarchique
  d <- dist(as.numeric(colnames(train)))
  dendro <- hclust(d, method="ward.D2")
  #Classer selon le nombre de classe k
  p <- cutree(dendro,k)
  result <- data.frame(cbind(p,as.numeric(colnames(train))))
  #valeur maximale de chaque classe
  maxval <- sort(tapply(as.numeric(colnames(train)),p,max))
  return(maxval)
}  

#Fonction pour transformer une df numerique en df discretisee
classval <- function(df, classmax) {
  classcol <- c()
  for (i in as.numeric(colnames(df))) {
    if (i > classmax[length(classmax)-1]){
      classcol <- append(classcol,length(classmax))
    } else {  
      for (j in classmax) {
        if (i<=j) {
          classcol <- append(classcol,match(j,classmax))
          break
        }
      }  
    }    
  }
  #mydf <- t(apply(df,1,function(x){x*as.numeric(colnames(df))} ))
  #mydf <- rbind(classcol=classcol, mydf)
  
  mydf <- rbind(classcol=classcol, df)
  #Moyenne de chaque classe
  #dffinal <- t(apply(mydf,1,function(x) {tapply(x,classcol,mean)}))
  dffinal <- t(apply(mydf,1,function(x) {tapply(x,classcol,sum)}))
  dffinal <- dffinal[-1,]
  
  return(dffinal)
}


############################################################
############################################################PREPARER TRAIN/TEST POUR REGRESSION
############################################################
#Create MatrixDocumentTerm for train with only number
mdt_train_reg <- get_mdtnum(train$text)
mdt_train_reg <- mdt_train_reg[,as.numeric(colnames(mdt_train_reg))<201000]
#Recuper la valeur maximales de la discretisation de train
maxclasstrain <- disval(mdt_train_reg,k=20)

#Transformer mdt_train_reg en mdt_train_reg_discretise
mdt_train_reg_dis <- classval(mdt_train_reg,maxclasstrain)
#Transformer mdt_test_reg en mdt_test_reg_discretise
mdt_test_reg_dis <- classval(get_mdtnum(test$text),maxclasstrain)

#Ajouter la variable dependante
mdt_train_reg_dis <- data.frame(cbind(mdt_train_reg_dis,PC_MT_CAPITAL=train$PC_MT_CAPITAL))
mdt_test_reg_dis <- data.frame(cbind(mdt_test_reg_dis,PC_MT_CAPITAL=test$PC_MT_CAPITAL))

# Match train & test column
w_i = colnames(mdt_train_reg_dis) %in% colnames(mdt_test_reg_dis) #identify train's columns in test
# set to 0 tous les colonnes qui ne sont pas dans test mais existent en train
mdt_test_reg_dis[,names(mdt_train_reg_dis)[!w_i]] = 0
# tri comme dans train
mdt_test_reg_dis <- mdt_test_reg_dis[, names(mdt_train_reg_dis)]

#Scenario 2: meme mdt que classif (pas mieux)
train_reg <- subset(mdt_train_df, select=-c(PC_FIXE))
train_reg <- data.frame(cbind(train_reg,PC_MT_CAPITAL = train$PC_MT_CAPITAL))
test_reg <- subset(mdt_test_df, select=-c(PC_FIXE))
test_reg <- data.frame(cbind(test_reg,PC_MT_CAPITAL = test$PC_MT_CAPITAL))


############################################################
############################################################Topic modelling (optionnel)
############################################################
mdt_lda <- LDA(mdt_train_df,k=10,method = 'Gibbs', control = list(seed=1111))
gammas <- tidy(mdt_lda, matrix = "gamma")
#Les top topics
top_topics <- gammas %>%
  group_by(document) %>%
  top_n(10, gamma) %>%
  #ungroup() %>%
  arrange(document, -gamma)
#Now that we now which topic best corresponds to document, let's find top words per topic

#Les top words
betas <- tidy(mdt_lda,matrix="beta")
top_terms <- betas %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#Graphique
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()




############################################################
############################################################CLASSIFICATION
############################################################
#Regression logistique
rl <- glm(PC_FIXE ~.,data = mdt_train_df, family = "binomial")
y_rl <-predict(rl, newdata = mdt_test_df[,-ncol(mdt_test_df)])
y_rl <- ifelse(y_rl>0.5,1,0)
mc = table(y_rl, mdt_test_df$PC_FIXE)
sum(diag(mc))/sum(mc) #0.8455 / #0.7770936: avec steaming (sparse=0.9)

#SVM en caret
svm <- train(PC_FIXE ~.,data = train, method = 'svmLinear3')
y_svm <- predict(svm, newdata = test[,-ncol(test)])
y_svm <- ifelse(y_svm>0.5,1,0)
mc = table(y_svm, test$PC_FIXE)
sum(diag(mc))/sum(mc) #0.8483 / 0.7893258: avec steaming (sparse=0.8)
#F1-score
print(F1_Score(test$PC_FIXE,y_svm)) #0.5801 #0.6029
#AUC
roc.curve(test$PC_FIXE,y_svm) #0.787 #0.812

#Un arbre de decision
modelAD <- rpart(PC_FIXE ~.,data = mdt_train_df)
y_probAD <- predict(modelAD, mdt_test_df[,-ncol(mdt_test_df)])
y_predAD <- ifelse(y_probAD[,2]>0.8,1,0)
#Matrice de confusion
mcAD <- table(mdt_test_df$PC_FIXE,y_predAD)
#AC
print(sum(diag(mcAD))/sum(mcAD)) #0.8651685(lotA) #0.8756158(seuil=0.8) #0.8743842(seuil=0.5) #0.8189655(seuil=0.9) / Sparse 0.9:#0.8830049 (seuil=0.8)
#F1-score
print(F1_Score(mdt_test_df$PC_FIXE,y_predAD)) #0.5932203(lotA) #0.6599327(seuil=0.8) #0.575(seuil=0.5) #0.6352357(seuil=0.9)  / Sparse 0.9:#0.6303502 (seuil=0.8)
#AUC

roc.curve(mdt_test_df$PC_FIXE,y_predAD) #0.776(lotA) #0.810 (seuil=0.8) #0.726(seuil=0.5) #0.862(seuil=0.9) / Sparse 0.9:#0.765 (seuil=0.8)

#Random forest
modelRF <- randomForest(PC_FIXE ~ .,data = mdt_train_df, ntree=500)
y_probRF <- predict(modelRF, mdt_test_df[,-ncol(mdt_test_df)],type = "prob")
y_predRF <- ifelse(y_probRF[,2]>0.5,1,0)
#Matrice de confusion
mcRF <- table(mdt_test_df$PC_FIXE,y_predRF)
#AC
print(sum(diag(mcRF))/sum(mcRF)) #Sparse 0.9 #0.7512315(seuil=0.8) #0.8522167(seuil=0.5) #0.703202(seuil=0.9) 
#F1-score
print(F1_Score(mdt_test_df$PC_FIXE,y_predAD)) #Sparse 0.9 #0.6303502(seuil=0.8) #0.6303502(seuil=0.5) #0.6303502(seuil=0.9) 
#AUC
roc.curve(mdt_test_df$PC_FIXE,y_predAD) #Sparse 0.9 #0.765(seuil=0.8) #0.765(seuil=0.5) #0.765(seuil=0.9) 

#Randomforest tuning + doParallel
cl <- makePSOCKcluster(5) #or makeCluster(nb_coeurs) 
registerDoParallel(cl)

## All subsequent models are then run in parallel
modelRF <- train(PC_FIXE ~ .,data = mdt_train_df, method = "rf")

## When you are done:
stopCluster(cl)

#ZIP Regression (pas adapté)
mdt_train_df$PC_FIXE <- as.numeric(mdt_train_df$PC_FIXE)
mdt_train_df$PC_FIXE <- mdt_train_df$PC_FIXE-1
modelZIP <- zeroinfl(PC_FIXE ~ . |. ,data = mdt_train_df)
############################################################
############################################################REGRESSION
############################################################
#Regression + discretisation
#Linear
lmmodel <- lm(formula = PC_MT_CAPITAL ~ .,data=mdt_train_reg_dis)
y_pred_lm <- predict(lmmodel,mdt_test_reg_dis) *as.numeric(y_predAD)
#Arbre de decision
ADmodel <- rpart(PC_MT_CAPITAL ~ .,data=mdt_train_reg_dis)
y_pred_AD <- predict(ADmodel, mdt_test_reg_dis[,-ncol(mdt_test_reg_dis)]) *as.numeric(y_predAD)
#Quantile regression
rqmodel <- rq(formula = PC_MT_CAPITAL ~ .,data=mdt_train_reg_dis)
y_pred_rq <- predict(rqmodel,mdt_test_reg_dis) *as.numeric(y_predAD)

#prediction accuracy
actuals_preds_step <- data.frame(cbind(actuals=mdt_test_reg_dis$PC_MT_CAPITAL, predicteds=y_pred_lm))  # make actuals_predicteds dataframe.
cor(actuals_preds_step)^2# 0.04935686 / lm: 0.05192809 / rq: 0.03087807
mean(actuals_preds_step$actuals) #38486.03
mean(actuals_preds_step$predicteds) #34704.4 / 34990.63 / 18581.11
actuals_preds_step$error <- abs(actuals_preds_step$actuals - actuals_preds_step$predicteds)
max(actuals_preds_step$error) #340466.8 / 337961.4 / 356862.4
min(actuals_preds_step$error) #0
mean(actuals_preds_step$error)#32793.31 / 33212.21 / 30946.74
median(actuals_preds_step$error) #24633.21 / 25881.36 / 16780
sd(actuals_preds_step$error) #41550.31 / 41232.57 / 47571.62

max(actuals_preds_step$actuals)
max(actuals_preds_step$predicteds)

#####################GRAPH
ggplot(mdt_test_reg_dis, aes(x = mdt_test_reg_dis$PC_MT_CAPITA, y = y_pred_lm)) + geom_point() + geom_abline(color = "blue")








############################################################
############################################################BROUILLON
############################################################


#Regression sur meme df que classif
#Linear
lmmodel <- lm(formula = PC_MT_CAPITAL ~ .,data=train_reg)
y_pred_lm <- predict(lmmodel,test_reg) *as.numeric(y_predAD)
#Arbre de decision
ADmodel <- rpart(PC_MT_CAPITAL ~ .,data=train_reg)
y_pred_AD <- predict(ADmodel, test_reg[,-ncol(test_reg)]) *as.numeric(y_predAD)

#prediction accuracy
actuals_preds_step <- data.frame(cbind(actuals=test_reg$PC_MT_CAPITAL, predicteds=y_pred_AD))  # make actuals_predicteds dataframe.
cor(actuals_preds_step)^2# 0.01471141
mean(actuals_preds_step$actuals) #40824.7
mean(actuals_preds_step$predicteds) #44444.16
actuals_preds_step$error <- abs(actuals_preds_step$actuals - actuals_preds_step$predicteds)
max(actuals_preds_step$error) #674589.8
min(actuals_preds_step$error) #0
mean(actuals_preds_step$error)#42395.72
median(actuals_preds_step$error) #30829.17
sd(actuals_preds_step$error) #63689.01


#Test Regex
mytext <- c('Je paie 4000 euros et 500 â¬, de 700 Ã  300')
mytextbis <- gsub("[[:digit:]]*[[:blank:]]euros","bingo",x=mytext)
regmatches(mytext,regexpr("[[:digit:]]*[[:blank:]]euros",mytext))

tofind <- paste(c("[[:digit:][:blank:]]*[[:blank:]]EUR","[[:digit:][:blank:]]*[[:blank:]]euros","[[:digit:][:blank:]]*[[:blank:]]â¬"), collapse="|")
toreplace <- paste(c("[[:blank:]][[:digit:]][[:digit:]][[:blank:]]EUR","[[:blank:]][[:digit:]][[:digit:]][[:blank:]]euros","[[:blank:]][[:digit:]][[:digit:]][[:blank:]]â¬"),collapse="|")

cash1<- str_extract_all(train$text, "[[:digit:][:blank:]]*[[:blank:]]EUR")
cash2<- str_extract_all(train$text, "[[:digit:]]*[[:blank:]]â¬")
cash3<- str_extract_all(train$text, "de[[:blank:]][[:digit:]]*")
cash4<- str_extract_all(train$text, "Ã [[:blank:]][[:digit:]]*")

num <- gsub(toreplace, "", c(cash1))
num <- gsub("[^0-9,-]", " ", c(num))

corpusnum <- Corpus(VectorSource(num)) #transformation en corpus
corpusnum <- tm_map(corpusnum, stripWhitespace)
corpusnum <- tm_map(corpusnum, removePunctuation)
#corpusnum <- tm_map(corpusnum, removeWords, c('euros','â¬','de','Ã '))
mdtnum <- DocumentTermMatrix(corpusnum)
mdtnum <- as.data.frame(as.matrix(mdtnum))
#colnames(mdtnum) <- gsub("[^0-9,-]", " ", colnames(mdtnum))
colnames(mdtnum) <- as.numeric(colnames(mdtnum))
View(mdtnum)

#Extraction des montants de prestation compensatoire
#Extraire des paragraphes contenant le mot prestation compensatoire: "prestation compensatoire" + XXX caractère
para_PC <- str_extract_all(train$text,".{100}prestation compensatoire.{100}")
para_PC_num <- gsub("[^0-9,]", " ", c(para_PC))
#Extraire les montants en euros à partir de ces paragraphes.
para_num <- str_extract_all(para_PC_num,"[:digit:]*[:digit:]*[:digit:].[:digit:]{3}")
para_num <- gsub(" ", "", c(para_num))
para_num <- str_extract_all(para_num,"[:digit:]*[:digit:]*[:digit:][:digit:]{3}")
para_num <- gsub("[^0-9]", " ", c(para_num))
corpus_para <- Corpus(VectorSource(para_num))
#corpus_para <- tm_map(corpus_para, stripWhitespace)
#corpus_para <- tm_map(corpus_para, removePunctuation)
mdtpara <- DocumentTermMatrix(corpus_para)
mdtpara <- as.data.frame(as.matrix(mdtpara))
colnames(mdtpara) <- as.numeric(colnames(mdtpara))
View(mdtpara)





############################################################