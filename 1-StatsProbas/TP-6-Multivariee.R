
# TP 6 : analyse multivariée

#######################
# 1) Regression multiple
#######################

# 1.1) Préparation des données
# Charger à nouveau les données des matches de LOL,
#   filtrer les résultats d'équipe,
#   selectionner quelques variables quantitatives (dont result,gamelength)






# 1.2) Modèles linéaires multivariés

# Utiliser des modèles linéaires pour expliquer dpm,result,gamelength (fonction lm)


# Comparer ces modèles en termes de R2, de AIC


# Idem en séparant victoires et défaites



# Expliquer une variable composite qui vaut -gamelength en cas de défaite, gamelength en cas de victoire



# Trouver un modèle optimal en utilisant une "Stepwise Regression" (fonction step)



# 1.3) Régression logistique

# Expliquer la variable binaire "result" avec une régression logistique (fonction glm, en utilisant family = binomial(link = 'logit'))
#    - pour obtenir les effets marginaux du modèle logit, utiliser la fonction logitmfx du package mfx
#    - comparer le modèle logit avec un modèle linéaire, en utilisant le pseudo R2 de McFadden = 1 - logLik(logit)/logLik(model nul avec result~1)





#######################
# 2) Séries temporelles
#######################



# Vérifier s'il existe une causalité de Granger entre les variables observées à étapes intermédiaires (kills, golddiff, etc at 10, 15) et le résultat








#######################
# 3) Reduction de dimensionalité
#######################

# Prendre en main les méthodes t-SNE (package Rtsne) et UMAP (package umap)



# Comparer PCA, t-SNE, et UMAP sur un ensemble de variables quantitatives (joueurs et/ou équipes)



