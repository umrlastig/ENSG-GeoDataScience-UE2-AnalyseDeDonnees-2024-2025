
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



# Comparer PCA, t-SNE, et UMAP sur un ensemble de variables quantitatives (joueurs et/ou équipes)







