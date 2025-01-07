
# TP 6 : analyse multivariée

#######################
# 1) Regression multiple
#######################

# 1.1) Préparation des données
#   - Charger à nouveau les données des matches de LOL,
#     Liens des données brutes : https://oracleselixir.com/tools/downloads
#                                https://drive.google.com/drive/u/1/folders/1gLSw0RLjBbtaNy0dgnGQDAZOHIgCe-HH
#                                https://drive.usercontent.google.com/download?id=1dlSIczXShnv1vIfGNvBjgk-thMKA5j7d&export=download&authuser=1&confirm=t&uuid=fcd5cae5-6d4f-43e3-89dd-c5065578e70d&at=AENtkXazWdVNyBL5N1En8vmZZhm9:1731572605501
#   - Filtrer les résultats d'équipe
#   - Selectionner quelques variables quantitatives dont par exemple
#      * result,gamelength : variables à expliquer
#      * dpm, totalgold, kills, deaths, elders, towers, minionkills, monsterkills, wardsplaced : variables explicatives
#   - Filtrer les NA
#   - Filtrer les duplicates

library(readr)
library(dplyr)

lol <- read_csv('2020_LoL_esports_match_data.csv')

variables_a_expliquer = c("result","gamelength")
variables_explicatives = c("dpm","totalgold","kills","deaths","elders","towers","minionkills","monsterkills","wardsplaced",
                           "goldat10","killsat10","goldat15","killsat15")

# filtrage et selection
d <- (lol %>% filter(position=="team") %>% select(c(variables_a_expliquer,variables_explicatives)))

# filtrage NAs
for(var in c(variables_a_expliquer,variables_explicatives)){
  d = d[!is.na(d[,var]),]
}

# duplicates
d = d[!duplicated(d),]

# verifier les donnees
summary(d)


# 1.2) Modèles linéaires multivariés

# Utiliser des modèles linéaires pour expliquer result,gamelength (fonction lm)

model1_result = lm(formula = result ~ dpm + totalgold, data = d)
summary(model1_result)
residuals(model1_result)
coefficients(model1_result)
summary(model1_result)$r.squared
summary(model1_result)$adj.r.squared


model2_result = lm(formula = result ~ dpm + totalgold + kills + deaths, data = d)
summary(model2_result)

modelfull_result = lm(formula = result ~ dpm + totalgold + kills + deaths + elders + towers + minionkills + monsterkills + wardsplaced, data = d)
summary(modelfull_result)


model1_gamelength = lm(formula = gamelength ~ dpm + totalgold, data = d)
summary(model1_gamelength)

modelfull_gamelength = lm(formula = gamelength ~ dpm + totalgold + kills + deaths + elders + towers + minionkills + monsterkills + wardsplaced, data = d)
summary(modelfull_gamelength)

# Comparer ces modèles en termes de R2, de AIC
summary(model1_gamelength)$adj.r.squared
summary(modelfull_gamelength)$adj.r.squared

AIC(model1_gamelength)
AIC(modelfull_gamelength)



# Idem en séparant victoires et défaites (pour gamelength)

# 2 modèles différents en filtrant les données

modelfull_gamelength_victoire = lm(formula = gamelength ~ dpm + totalgold + kills + deaths + elders + towers + minionkills + monsterkills + wardsplaced,
                          data = d[d$result==1,])
summary(modelfull_gamelength_victoire)

modelfull_gamelength_defaite = lm(formula = gamelength ~ dpm + totalgold + kills + deaths + elders + towers + minionkills + monsterkills + wardsplaced,
                                   data = d[d$result==0,])
summary(modelfull_gamelength_defaite)

coefsvic = modelfull_gamelength_victoire$coefficients
coefsdef = modelfull_gamelength_defaite$coefficients

abs(coefsvic)/coefsvic == abs(coefsdef)/coefsdef


# 1 modèle en controlant sur le résultat
modelfull_gamelength_result = lm(formula = gamelength ~ as.factor(result) + dpm + totalgold + kills + deaths + elders + towers + minionkills + monsterkills + wardsplaced,
                                  data = d)
summary(modelfull_gamelength_result)

AIC(modelfull_gamelength) - AIC(modelfull_gamelength_result)

coefs1 = modelfull_gamelength$coefficients
coefs2 = modelfull_gamelength_result$coefficients[-2]
abs(coefs1)/coefs1 == abs(coefs2)/coefs2



# Expliquer une variable composite qui vaut -gamelength en cas de défaite, gamelength en cas de victoire
d$lengthresult = ifelse(d$result==0,-d$gamelength,d$gamelength)
modelfull_lengthresult = lm(formula = lengthresult ~ dpm + totalgold + kills + deaths + elders + towers + minionkills + monsterkills + wardsplaced, data = d)
summary(modelfull_lengthresult)


# Trouver un modèle optimal en utilisant une "Stepwise Regression" (fonction step)

# A partir du modèle complet
step(modelfull_gamelength_result)

# A partir d'une constante
# TODO
#step(lm(data=d,gamelength~dpm),scope=modelfull_gamelength_result,direction = "both")



# 1.3) Régression logistique

# Expliquer la variable binaire "result" avec une régression logistique (fonction glm, en utilisant family = binomial(link = 'logit'))
#    - pour obtenir les effets marginaux du modèle logit, utiliser la fonction logitmfx du package mfx
#    - comparer le modèle logit avec un modèle linéaire, en utilisant le pseudo R2 de McFadden = 1 - logLik(logit)/logLik(model nul avec result~1)

logit1_result = glm(formula = result ~ dpm + totalgold, data = d, family = binomial(link = 'logit'))
summary(logit1_result)

logitfull_result = glm(formula = result ~  dpm + totalgold + kills + deaths + elders + towers + minionkills + monsterkills + wardsplaced,
                       data = d, family = binomial(link = 'logit'))
summary(logitfull_result)


library(mfx)

logitmfx(formula = result ~ dpm + totalgold, data = d)

# -> par comparaison avec les coefficients des modèles linéaires, le lm sous-estime les effets "réels"



# Performance du logit

# McFadden R2
1 - logLik(logitfull_result) / logLik(glm(formula = result~1 , data = d, family = binomial(link = 'logit')))

# https://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation -> pas le meilleur indicateur de la performance
# -> utiliser l'accuracy = (True positives + True negatives) / N

accuracy<- function(model){
  length(which(ifelse(fitted(model)>0.5,1,0)==d$result))/length(fitted(model))
}

accuracy(logitfull_result)

# in the case of unbalanced samples, accuracy is not the best measure -> look at the rest of the confusion matrix (precision, recall, F1-score)
# https://en.wikipedia.org/wiki/Precision_and_recall#Imbalanced_data -> balanced accuracy

balancedAccuracy<-function(model){
  prediction = ifelse(fitted(model)>0.5,1,0)
  true_positive_ratio = length(which(prediction==1&d$result==1)) / length(which(d$result==1))
  true_negative_ratio = length(which(prediction==0&d$result==0)) / length(which(d$result==0))
  (true_positive_ratio + true_negative_ratio) / 2
}
balancedAccuracy(logitfull_result)

# More advanced methods:
# https://stats.stackexchange.com/questions/235808/binary-classification-with-strongly-unbalanced-classes

# ROC curve to get the best threshold for logit model
library(pROC)
d$proba = predict(logitfull_result,type=c("response"))
roclogit <- roc(result~proba,data=d) 
plot(roclogit)
coords(roclogit,"best")


#######################
# 2) Séries temporelles
#######################


# Vérifier s'il existe une causalité de Granger entre les variables observées à étapes intermédiaires (kills, golddiff, etc at 10, 15) et le résultat

granger_full_result = lm(data=d, result ~ goldat10 + goldat15 + killsat10 + killsat15)
summary(granger_full_result)

granger_full_logit_result = glm(data=d, result ~ goldat10 + goldat15 + killsat10 + killsat15,
                                family = binomial(link = 'logit'))
summary(granger_full_logit_result)


d$resultat30 = ifelse(d$gamelength<=30*60,d$result,NA)
d$resultat40 = ifelse(d$gamelength<=40*60,d$result,NA)

granger_full_logit_result_at_30 = glm(data=d, resultat30 ~ goldat10 + goldat15 + killsat10 + killsat15,
                                family = binomial(link = 'logit'))
summary(granger_full_logit_result_at_30)

granger_full_logit_result_at_40 = glm(data=d, resultat40 ~ goldat10 + goldat15 + killsat10 + killsat15,
                                      family = binomial(link = 'logit'))
summary(granger_full_logit_result_at_40)




#######################
# 3) Reduction de dimensionalité
#######################

# Prendre en main les méthodes t-SNE (package Rtsne) et UMAP (package umap)
library(Rtsne)
library(umap)

library(palmerpenguins)
data("penguins")

dataPenguins = (penguins %>% na.omit() %>% dplyr::select(where(is.numeric)) %>% dplyr::select(-year))

tsnePenguins = Rtsne(dataPenguins)
tsnePenguinsCoords = data.frame(tsnePenguins$Y)
names(tsnePenguinsCoords) = c("X1","X2")

umapPenguins = umap(dataPenguins)
umapPenguinsCoords = data.frame(umapPenguins$layout)

pcaPenguins = prcomp(dataPenguins,scale. = T)
pcaPenguinsCoords = data.frame(pcaPenguins$x[,1:2])


# Comparer PCA, t-SNE, et UMAP avec leur projections dans l'espace 2D
ggplot(data = cbind(tsnePenguinsCoords,species = (penguins%>% na.omit())$species))+
  geom_point(aes(x= X1, y=X2, color=species))

ggplot(data = cbind(umapPenguinsCoords,species = (penguins%>% na.omit())$species))+
  geom_point(aes(x= X1, y=X2, color=species))

ggplot(data = cbind(pcaPenguinsCoords,species = (penguins%>% na.omit())$species))+
  geom_point(aes(x= PC1, y=PC2, color=species))



# Sur un ensemble de variables quantitatives avec les données LOL (joueurs et/ou équipes)

d_lol_reduc = d[,-(16:17)]

tsneLol = Rtsne(d_lol_reduc)
tsneLolCoords = data.frame(tsneLol$Y)
names(tsneLolCoords) = c("X1","X2")

umapLol = umap(d_lol_reduc)
umapLolCoords = data.frame(umapLol$layout)

pcaLol = prcomp(d_lol_reduc,scale. = T)
pcaLolCoords = data.frame(pcaLol$x[,1:2])

ggplot(data = cbind(tsneLolCoords,result = d_lol_reduc$result))+
  geom_point(aes(x= X1, y=X2, color=result))

ggplot(data = cbind(umapLolCoords,result = d_lol_reduc$result))+
  geom_point(aes(x= X1, y=X2, color=result))

ggplot(data = cbind(pcaLolCoords,result = d_lol_reduc$result))+
  geom_point(aes(x= PC1, y=PC2, color=result))












