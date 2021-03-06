DF_regres <- cbind(AFilled,BFilled,CFilled,DFilled,GFilled,HFilled,IFilled,J,M)

#Supprimer les variables avec trop de NA
dfNA$vars <- rownames(dfNA)
empty_vars <- dfNA[dfNA$Nb_NA>0.15,]$vars
DF_regres_Light <- DF_regres[,!(names(DF_regres) %in% empty_vars)]

#Supprimer les cas dont le groupe J est presque vide (408 cas)
DF_regres_Light_J <- DF_regres_Light[!is.na(DF_regres_Light$PATRI_GLOBAL_MT_CPLE),]

#Verifier les NA
View(colSums(is.na(DF_regres_Light_J))) #nickel chrome aucun cellule vide

#Comptabiliser les formes de rente
table(DF_regres$R_TYPE_FIXE)
#   0    1    2 
#2909  138  142 

#Creer la variables predictives pour r�gression
DF_regres_Light_J$PC_MT_CAPITAL <- DF_regres_Light_J$K_MT_FIXE + DF_regres_Light_J$NUM_ECH_MT_FIXE

#############################Enlever les cas avec consentement
DF_ss_consentement_full <- DF_regres_Light_J[DF_regres_Light_J$DIV_DMDE!=1,]

#Supprimer les variables du groupe M
DF_ss_consentement <- DF_ss_consentement_full[,!(names(DF_ss_consentement_full) %in% colnames(M))]

#Ajouter la variable � pr�dire PC_FIXE
DF_ss_consentement <- cbind(DF_ss_consentement,DF_ss_consentement_full$PC_FIXE)
#Renommer la variable a predire
names(DF_ss_consentement)[names(DF_ss_consentement)=='DF_ss_consentement_full$PC_FIXE'] <- 'PC_FIXE'

#Separer les variables continues et cat�gorielles
library(dplyr)
DF_ss_consentement_factor <- DF_ss_consentement %>%  select_if(is.factor)
DF_ss_consentement_continu <- DF_ss_consentement %>%  select_if(is.numeric)

#Supprimer les variables factorielles avec un seul modalit�
DF_ss_consentement_factor <- DF_ss_consentement_factor[,sapply(DF_ss_consentement_factor,nlevels)>1]

#Corriger les cas -9 (non renseigne) en 0 pour faciliter la regression
not_nine <- function(colonne){
  return(ifelse(colonne <0,0,colonne))
} 
DF_ss_consentement_continu <- data.frame(apply(DF_ss_consentement_continu,2,not_nine))

#############################Refaire la DF pour classifier
DF_classifier_ss_consentement <- cbind(DF_ss_consentement_continu[,-ncol(DF_ss_consentement_continu)],DF_ss_consentement_factor)

############################DF pour papier de recherche
variables <- read_excel("variables.xlsx", sheet = "Feuil4")
DF_paper <- cbind(DF_classifier_ss_consentement,DF_ss_consentement_continu$PC_MT_CAPITAL)
names(DF_paper)[names(DF_paper)=='DF_ss_consentement_continu$PC_MT_CAPITAL'] <- 'PC_MT_CAPITAL'
DF_paper_light <- subset(DF_paper,select=unlist(variables$VARS))
write.csv(DF_paper_light,"DF_paper")

###########################Analyse les montants de PC
montant_PC <- data.frame(table(DF_ss_consentement_continu$PC_MT_CAPITAL))
montant_PC <- montant_PC[order(-montant_PC$Freq),]
View(montant_PC)
barplot(montant_PC[2:30,2],names.arg=montant_PC[2:30,1],xlab = 'montant PC',ylab="Nombre de d�cisions",col="blue",las=2)
write.csv(montant_PC,'repartition_montant_PC.csv')
