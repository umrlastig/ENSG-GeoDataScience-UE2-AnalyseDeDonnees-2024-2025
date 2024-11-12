
# TP 6 : analyse multivariée


# 1) Regression multiple

# Charger à nouveau les données des matches de LOL,
#   filtrer les résultats d'équipe,
#   selectionner quelques variables quantitatives (dont result,gamelength)



# Utiliser des modèles linéaires pour expliquer dpm,result,gamelength (fonction lm)



# Idem en séparant victoires et défaites



# Expliquer une variable composite qui vaut -gamelength en cas de défaite, gamelength en cas de victoire



# Trouver un modèle optimal en utilisant une "Stepwise Regression" (fonction step)




# Expliquer la variable binaire "result" avec une régression logistique (fonction glm)
#   (pour obtenir les effets marginaux du modèle logit, utiliser la fonction logitmfx du package mfx)




# 2) Séries temporelles

# Vérifier s'il existe une causalité de Granger entre les variables observées à étapes intermédiaires (kills, golddiff, etc at 10, 15) et le résultat









# 3) Reduction de dimensionalité


# Prendre en main les méthodes t-SNE (package Rtsne) et UMAP (package umap)



# Comparer PCA, t-SNE, et UMAP sur un ensemble de variables quantitatives (joueurs et/ou équipes)