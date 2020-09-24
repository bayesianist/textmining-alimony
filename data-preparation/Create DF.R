#Importer les donnees
library(readxl)
library(lubridate)
COMPRES <- read_excel("fichier_COMPRES_variables_de_base_pour_Fabrice.xlsx")
#Calculer l'age des ex-epoux
COMPRES$DA_NAISS_EPX <- as.Date(as.numeric(COMPRES$DA_NAISS_EPX), origin = "1899-12-30")
COMPRES$DA_NAISS_EPSE <- as.Date(as.numeric(COMPRES$DA_NAISS_EPSE), origin = "1899-12-30")
COMPRES$DA_RC <- as.Date(as.numeric(COMPRES$DA_RC), origin = "1899-12-30")
COMPRES$AGE_EPX <- year(COMPRES$DA_RC) - year(COMPRES$DA_NAISS_EPX)
COMPRES$AGE_EPSE <- year(COMPRES$DA_RC) - year(COMPRES$DA_NAISS_EPSE)
#Enlever les cas de non-demande de PC
COMPRES_PC <- subset(COMPRES,PC=="1")
#Enlever les modalites "2" de PC fixe
COMPRES_PC <- subset(COMPRES_PC,PC_FIXE != 2)
dim(COMPRES_PC)
table(COMPRES_PC$PC_FIXE)/nrow(COMPRES_PC)
#Les classes sont déséquilibrés

#Stats de l'age des ex-epoux
barplot(table(COMPRES_PC$AGE_EPX))
barplot(table(COMPRES_PC$AGE_EPSE))

#Import les dataframes vides pour avoirs la liste des variable
folder <- "C:/Users/ThinkPad/Documents/Stage eJuris/Dataframe/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(folder, file_list[i], sep=''))
  )}

library(stringr)  
#Creer les df pour chaques groupe de variables
for (i in file_list){
  assign(str_remove(i, ".csv"), 
         COMPRES_PC[,colnames(get(i))])
}
#List des nouvelles dataframe
DF_list <- sapply(file_list,function(x){str_remove(x,".csv")})

#TYPE OF VARIABLES
#Creer une fonction pour affecter le type de chaque variable d'une df
set_type <- function(df, listoftype) {
  for (j in 1:length(colnames(df))){
    if (is.null(listoftype[j]) || listoftype[j] == '' || is.na(listoftype[j])){
    } else if (listoftype[j]=='Nominale'){
      df[[j]] <- as.factor(df[[j]])
    } else {
      df[[j]] <- as.numeric(df[[j]])
    }
  }
  return(df)
}

#Importer le fichier de type de variable de chaque groupe
Typeofvariable <- read.csv("~/Stage eJuris/Typeofvariable.csv", header=FALSE)

#Creer un boucle pour affecter le type a chaque variable
for (i in DF_list){
  DF_temp <- set_type(get(i),Typeofvariable[[which(DF_list == i)]])
  assign(i,DF_temp)
}

#Controler les NA
dfNA <- c()
for (i in DF_list){
  #Creer une df de nombre de NA par groupe de variable
  assign(paste(i,NA,sep=''),t(as.data.frame(colSums(is.na(get(i))))))
  #Creer une df de nombre de NA global
  dfNA <- rbind(dfNA,as.data.frame(colSums(is.na(get(i)))))
}
dfNA <- dfNA / nrow(COMPRES_PC)
colnames(dfNA) <- "Nb_NA"
View(dfNA)
#Certaines variables ont que des NA et certaines ont plus de NA qu'observation.

#Traitement NA
#library(mice)
#GFilled <- mice(G,m=5,method='pmm',maxit=50, seed=500)
#Des erreurs: system is computationally singular: reciprocal condition number = 1.42293e-18

library(missForest)
#GFilled <- missForest(as.data.frame(G))[["ximp"]]
#Toujours le seul qui marche

#library(mi)
#GFilled <- mi(G)

#Creer un boucle pour remplir les NA de chaque groupe de variable
for (i in DF_list){
  assign(paste(i,'Filled',sep=''),missForest(as.data.frame(get(i)))[["ximp"]])
}

#Impossible de remplir pour les groupes J, M, N

#Combiner les groupes de variable pour creer le jeu de donnes pour prediction
DF <- cbind(AFilled,BFilled,CFilled,DFilled,GFilled,HFilled,IFilled,M$PC_FIXE)
dim(DF)
#Renommer la variable a predire
names(DF)[names(DF)=='M$PC_FIXE'] <- 'PC_FIXE'
#Repartir en donnees d'apprentissage et de test
sample_size = floor(0.75*nrow(DF))
set.seed(100)
train_ind = sample(seq_len(nrow(DF)), size = sample_size)
train = DF[train_ind, ]
test = DF[-train_ind, ]
#Répartition des modalités de la variable à prédire
table(train$PC_FIXE)
table(test$PC_FIXE)

#Supprimer les variables avec trop de NA
dfNA$vars <- rownames(dfNA)
empty_vars <- dfNA[dfNA$Nb_NA>0.4,]$vars
DFLight <- DF[,!(names(DF) %in% empty_vars)]

#Split train/test on DFLight
sample_size = floor(0.75*nrow(DF))
set.seed(100)
train_ind = sample(seq_len(nrow(DFLight)), size = sample_size)
trainlight = DFLight[train_ind, ]
testlight = DFLight[-train_ind, ]

#ROSE Sampling
library(ROSE)
train_rose <- ROSE(PC_FIXE~.,data=trainlight,seed=1)$data
table(train_rose$PC_FIXE)
table(trainlight$PC_FIXE)

#Ajouter les variables extra COMPRES
#Mettre le nom des communes en CAPITAL
Niveauvie$`Nom Commune` <- Niveauvie$`Nom Commune` %>% str_to_upper()
stat_divorces$SIEGE_TGI <- stat_divorces$SIEGE_TGI %>% str_to_upper()
pop2017$`Nom de la commune`<- pop2017$`Nom de la commune` %>% str_to_upper()
#Supprimer les colonnes non utilisables
NiveauvieC <- Niveauvie[,!(names(Niveauvie) %in% c('Code Commune'))]
ificom2018C <- ificom2018[,!(names(ificom2018) %in% c('Region','Departements','Code','imp_moyen'))]
#Renommer les colonnes
colnames(stat_divorces) <- c('NOM_TGI',"NBR_DIV")
colnames(NiveauvieC) <- c('NOM_TGI',"NIV_VIE_COMM","NIV_VIE_DEPT")
colnames(ificom2018C) <- c('NOM_TGI',"NBR_RED","PAT_MOYEN")
colnames(pop2017) <- c('NOM_TGI',"POP2017")

# remove french accents
stat_divorces$NOM_TGI <- stringi::stri_trans_general(stat_divorces$NOM_TGI,"Latin-ASCII")
NiveauvieC$NOM_TGI <- stringi::stri_trans_general(NiveauvieC$NOM_TGI,"Latin-ASCII")
ificom2018C$NOM_TGI <- stringi::stri_trans_general(ificom2018C$NOM_TGI,"Latin-ASCII")
pop2017$NOM_TGI <- stringi::stri_trans_general(pop2017$NOM_TGI,"Latin-ASCII")
NiveauvieC$NOM_TGI <- gsub("-"," ",NiveauvieC$NOM_TGI)
stat_divorces$NOM_TGI <- gsub("-"," ",stat_divorces$NOM_TGI)
ificom2018C$NOM_TGI <- gsub("-"," ",ificom2018C$NOM_TGI)
pop2017$NOM_TGI <- gsub("-"," ",pop2017$NOM_TGI)

#Reecrire certaine Commune
stat_divorces$NOM_TGI <- gsub("LYON","LYON 3EME",stat_divorces$NOM_TGI)
NiveauvieC$NOM_TGI <- gsub("LYON 3E ARRONDISSEMENT","LYON 3EME",NiveauvieC$NOM_TGI)
pop2017$NOM_TGI <- gsub("LYON 3E ARRONDISSEMENT","LYON 3EME",pop2017$NOM_TGI)
ificom2018C$NOM_TGI <- gsub("LYON","LYON 3EME",ificom2018C$NOM_TGI)
stat_divorces$NOM_TGI <- gsub("MARSEILLE","MARSEILLE 6EME",stat_divorces$NOM_TGI)
NiveauvieC$NOM_TGI <- gsub("MARSEILLE 6E ARRONDISSEMENT","MARSEILLE 6EME",NiveauvieC$NOM_TGI)
pop2017$NOM_TGI <- gsub("MARSEILLE 6E ARRONDISSEMENT","MARSEILLE 6EME",pop2017$NOM_TGI)
ificom2018C$NOM_TGI <- gsub("MARSEILLE","MARSEILLE 6EME",ificom2018C$NOM_TGI)
stat_divorces$NOM_TGI <- gsub("PARIS","PARIS 1ER",stat_divorces$NOM_TGI)
NiveauvieC$NOM_TGI <- gsub("PARIS 1ER ARRONDISSEMENT","PARIS 1ER",NiveauvieC$NOM_TGI)
pop2017$NOM_TGI <- gsub("PARIS 1ER ARRONDISSEMENT","PARIS 1ER",pop2017$NOM_TGI)
ificom2018C$NOM_TGI <- gsub("VILLE DE PARIS","PARIS 1ER",ificom2018C$NOM_TGI)
stat_divorces$NOM_TGI <- gsub("SAINT","ST",stat_divorces$NOM_TGI)
NiveauvieC$NOM_TGI <- gsub("SAINT","ST",NiveauvieC$NOM_TGI)
pop2017$NOM_TGI <- gsub("SAINT","ST",pop2017$NOM_TGI)
ificom2018C$NOM_TGI <- gsub("SAINT","ST",ificom2018C$NOM_TGI)
stat_divorces$NOM_TGI <- gsub("ST DENIS DE LA REUNION","ST DENIS",stat_divorces$NOM_TGI)
NiveauvieC$NOM_TGI <- gsub("ST DENIS","	ST DENIS93",NiveauvieC$NOM_TGI)
ificom2018C$NOM_TGI <- gsub("ST DENIS","ST DENIS",ificom2018C$NOM_TGI)
stat_divorces$NOM_TGI <- gsub("STES","SAINTES",stat_divorces$NOM_TGI)
NiveauvieC$NOM_TGI <- gsub("STES","SAINTES",NiveauvieC$NOM_TGI)
pop2017$NOM_TGI <- gsub("STES","SAINTES",pop2017$NOM_TGI)
stat_divorces$NOM_TGI <- gsub("LE PUY","LE PUY EN VELAY",stat_divorces$NOM_TGI)
pop2017$NOM_TGI <- gsub("VAL DE BRIEY","BRIEY",pop2017$NOM_TGI)
#Ajouter des communes manquantes
pop2017 <- rbind(pop2017,c('CHERBOURG OCTEVILLE',17202))
pop2017 <- rbind(pop2017,c('MAMOUDZOU',71437))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="LA ROCHELLE"),]
pop2017 <- rbind(pop2017,c('LA ROCHELLE',77855))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="ST DENIS"),]
pop2017 <- rbind(pop2017,c('ST DENIS',149313))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="ST PIERRE"),]
pop2017 <- rbind(pop2017,c('ST PIERRE',85083))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="VALENCE"),]
pop2017 <- rbind(pop2017,c('VALENCE',65028))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="BONNEVILLE"),]
pop2017 <- rbind(pop2017,c('BONNEVILLE',13010))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="CASTRES"),]
pop2017 <- rbind(pop2017,c('CASTRES',43082))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="CHAUMONT"),]
pop2017 <- rbind(pop2017,c('CHAUMONT',23172))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="LAVAL"),]
pop2017 <- rbind(pop2017,c('LAVAL',52284))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="MOULINS"),]
pop2017 <- rbind(pop2017,c('MOULINS',20227))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="SENLIS"),]
pop2017 <- rbind(pop2017,c('SENLIS',14878))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="ST NAZAIRE"),]
pop2017 <- rbind(pop2017,c('ST NAZAIRE',71772))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="ST OMER"),]
pop2017 <- rbind(pop2017,c('ST OMER',15470))
pop2017 <- pop2017[!(pop2017$NOM_TGI=="VERDUN"),]
pop2017 <- rbind(pop2017,c('VERDUN',18449))

NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="VALENCE"),]
NiveauvieC <- rbind(NiveauvieC,c('VALENCE',18135,19301))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="ST DENIS"),]
NiveauvieC <- rbind(NiveauvieC,c('ST DENIS',9445,9445))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="ST PIERRE"),]
NiveauvieC <- rbind(NiveauvieC,c('ST PIERRE',9445,9445))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="BONNEVILLE"),]
NiveauvieC <- rbind(NiveauvieC,c('BONNEVILLE',20718,24708))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="CASTRES"),]
NiveauvieC <- rbind(NiveauvieC,c('CASTRES',18442,19095))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="CHAUMONT"),]
NiveauvieC <- rbind(NiveauvieC,c('CHAUMONT',19048,18612))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="EVRY"),]
NiveauvieC <- rbind(NiveauvieC,c('EVRY',16500,	23141))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="LA ROCHELLE"),]
NiveauvieC <- rbind(NiveauvieC,c('LA ROCHELLE',16500,	19676))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="LAVAL"),]
NiveauvieC <- rbind(NiveauvieC,c('LAVAL',19082, 19237))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="MOULINS"),]
NiveauvieC <- rbind(NiveauvieC,c('MOULINS',17680, 18828))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="SENLIS"),]
NiveauvieC <- rbind(NiveauvieC,c('SENLIS',23856.40,	20774.34))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="ST NAZAIRE"),]
NiveauvieC <- rbind(NiveauvieC,c('ST NAZAIRE',19208.50,20946.92))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="ST OMER"),]
NiveauvieC <- rbind(NiveauvieC,c('ST OMER',15384.50,	17738.49))
NiveauvieC <- NiveauvieC[!(NiveauvieC$NOM_TGI=="VERDUN"),]
NiveauvieC <- rbind(NiveauvieC,c('VERDUN',17911.00,	19025.08))

ificom2018C <- ificom2018C[!(ificom2018C$NOM_TGI=="PARIS 1ER"),]
ificom2018C <- rbind(ificom2018C,c('PARIS 1ER',482,	2692823))

#Join les nouvelles variables
DFLight_more1 <- merge(stat_divorces,DFLight,all.y=TRUE,by.x="NOM_TGI",by.y = "NOM_TGI")
DFLight_more2 <- merge(NiveauvieC,DFLight_more1,all.y=TRUE,by.x="NOM_TGI",by.y = "NOM_TGI")
DFLight_more3 <- merge(pop2017,DFLight_more2,all.y=TRUE,by.x="NOM_TGI",by.y = "NOM_TGI")
DFLight_more <- merge(ificom2018C,DFLight_more3,all.y=TRUE,by.x="NOM_TGI",by.y = "NOM_TGI")

#Remplir 0 a toutes les communes pas d'IFI
DFLight_more$NOM_TGI <- as.factor(DFLight_more$NOM_TGI)
DFLight_more$POP2017 <- as.numeric(DFLight_more$POP2017)
DFLight_more$NIV_VIE_COMM <- as.numeric(DFLight_more$NIV_VIE_COMM)
DFLight_more$NIV_VIE_DEPT <- as.numeric(DFLight_more$NIV_VIE_DEPT)
DFLight_more$NBR_DIV <- as.numeric(DFLight_more$NBR_DIV)
DFLight_more$PAT_MOYEN <- as.numeric(DFLight_more$PAT_MOYEN)
DFLight_more$NBR_RED <- as.numeric(DFLight_more$NBR_RED)
DFLight_more$NBR_RED <- ifelse(is.na(DFLight_more$NBR_RED),0,DFLight_more$NBR_RED)
DFLight_more$NIV_VIE_COMM <- ifelse(is.na(DFLight_more$NIV_VIE_COMM),DFLight_more$NIV_VIE_DEPT,DFLight_more$NIV_VIE_COMM)

#CALCULER LA PROPORTION IFI
DFLight_more$IFI_PRO <- (DFLight_more$NBR_RED / DFLight_more$POP2017)
DFLight_more <- DFLight_more[,c(1,172,2:171)]

#Traitement les NA
require(missForest)
DFLight_newvars <- missForest(as.data.frame(DFLight_more[,2:8]))[["ximp"]]
DFLight_more[,2:8] <-DFLight_newvars

colSums(is.na(DFLight_more[,1:8]))

#Repartir en donnees d'apprentissage et de test
sample_size = floor(0.75*nrow(DFLight_more))
set.seed(100)
train_ind = sample(seq_len(nrow(DFLight_more)), size = sample_size)
trainlight_more = DFLight_more[train_ind, ]
testlight_more = DFLight_more[-train_ind, ]
#Resampling
trainlight_more_rose <- ROSE(PC_FIXE~.,data=trainlight_more,seed=1)$data
table(trainlight_more_rose$PC_FIXE)
table(trainlight$PC_FIXE)
