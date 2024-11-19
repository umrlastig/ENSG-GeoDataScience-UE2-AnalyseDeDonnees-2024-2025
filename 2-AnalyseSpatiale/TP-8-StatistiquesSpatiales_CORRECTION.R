####
# TP : Statistiques spatiales


#########
#  1) Préparation des données


library(readr)
library(sf)
library(dplyr)
library(mapsf)

rawdvf2024 <- read_csv(file = 'https://files.data.gouv.fr/geo-dvf/latest/csv/2024/full.csv.gz')

# enlever super-outlier
toremove=unique(rawdvf2024[is.na(rawdvf2024$valeur_fonciere)|
                             rawdvf2024$valeur_fonciere>200000000,c("id_mutation")])

# agrégation au niveau départemental (via code_departement)
depdvf = rawdvf2024 %>%
  filter(type_local%in%c('Appartement','Maison')) %>%
  filter(!id_mutation%in%unlist(toremove)) %>%
  group_by(code_departement) %>% 
  summarise(
    prix = median(valeur_fonciere, na.rm=T),
    surface_bati = median(surface_reelle_bati, na.rm=T),
    surface_terrain = median(surface_terrain, na.rm=T)
  )
summary(depdvf$prix)

# autres données

deps = read_sf(dsn='data/departements/',layer='DEPARTEMENT')
popdeps = read_delim('data/insee/Departements.csv', delim=";")

# Insee: Filosofi 2020
insee_filosofi <- read_delim(file = 'data/filosofi/cc_filosofi_2020_DEP.csv', delim = ";")

# jointure
deps = left_join(deps,popdeps[,c("CODDEP","PTOT")],
                 by=c("CODE_DEPT"="CODDEP"))
deps = left_join(deps,insee_filosofi[,c("CODGEO","MED20","PPEN20","PPAT20")],
                 by=c("CODE_DEPT"="CODGEO"))
deps = left_join(deps,depdvf,by=c("CODE_DEPT"="code_departement"))
deps = na.exclude(deps)

d = st_drop_geometry(deps)


# exploration des données: cartes
mf_map(deps,var="prix",type="choro",nbreaks = 15)

# exploration des données: PCA / corrélations
cors = cor(d[,c("PTOT","MED20","PPEN20","PPAT20","prix","surface_bati","surface_terrain")])
library(corrplot)
corrplot(cors)

# modele lineaire non spatialise
simplemodel = lm(data=d, prix ~ PTOT+surface_bati)
linearmodel = lm(data=d, prix ~ PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain)
summary(simplemodel)
summary(linearmodel)
AIC(simplemodel) - AIC(linearmodel) # -> modele complet

# cartographier résidus du modèle linéaire
deps$residus = residuals(linearmodel)
mf_map(deps,var="residus",type="choro")


# tests autocorrelation spatiale
library(spdep)
depsnb = poly2nb(deps)
w = nb2listw(depsnb)

lm.morantest(linearmodel, w, alternative="two.sided")

lm.LMtests(linearmodel, w, test=c("LMerr","LMlag"))

# test en changeant la portée spatiale de l'autocorrelation : TODO



#########
#  2 ) Geographically weighted regression
#


# 2.1) Tester des modèles GWR à bandwidth fixe
# package GWmodel

library(GWmodel)

# GWR simple
gwbasic <- gwr.basic(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain,
                     data=deps,
                     bw=10,
                     kernel="bisquare", # noyau par default
                     adaptive=T # a mettre a F pour un voisinage de taille fixe (bw, en unités de mesure des geometry)
                     )
print(gwbasic)


# cartographier coefficients
coefs = gwbasic$SDF

deps$localR2=coefs$Local_R2
deps$residuals = coefs$residual
deps$alpha_population = coefs$PTOT
deps$alpha_surface = coefs$surface_bati

mf_map(deps,var='localR2',type='choro')
mf_map(deps,var='residuals',type='choro')
mf_map(deps,var='alpha_population',type='choro')
mf_map(deps,var='alpha_surface',type='choro')


# 2.2) Optimiser la bandwidth selon un critère d'AIC
# bandwidth adaptative en nombre de voisins

bwfullaic = bw.gwr(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain,
                   data=deps,
                   approach="AIC",
                   kernel="bisquare",
                   adaptive=T)

gwopt <- gwr.basic(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain,
                   data=deps,bw=bwfullaic,
                   adaptive=TRUE)
print(gwopt)
deps$opt_alpha_population = gwopt$SDF$PTOT
deps$opt_alpha_revenu = gwopt$SDF$MED20
mf_map(deps,var='opt_alpha_population',type='choro')
mf_map(deps,var='opt_alpha_revenu',type='choro')

# 2.3) Selection de modèle (méthode de "forward selection")
gwselec = gwr.model.selection(
  DeVar = "prix",
  InDeVars = c("PTOT","MED20","PPEN20","PPAT20","surface_bati","surface_terrain"),
  data=deps,bw = bwfullaic,
  approach="AIC",adaptive=T
)

print(gwselec)
aicc = gwselec[[2]][,3]
gwselec[[1]][which(aicc==min(aicc))]

gwoptmodel <- gwr.basic(prix~PTOT+MED20+PPEN20+PPAT20+surface_terrain,
                   data=deps,bw=bwfullaic,
                   adaptive=TRUE)
coefs = gwoptmodel$SDF

deps$alpha_population = coefs$PTOT
deps$alpha_revenu = coefs$MED20
mf_map(x = deps, var = "alpha_population", type = "choro")
mf_map(x = deps, var = "alpha_revenu", type = "choro")


#####
## 3 ) Auto-regression spatiales

library(spatialreg)

lm.LMtests(linearmodel, w, test=c("LMerr","LMlag"))

# modèle de lag spatial
# (! Durbin est un model plus compliqué avec un lag sur X et sur Y à la fois)
depsnb = poly2nb(deps)
w = nb2listw(depsnb)

spatiallag = lagsarlm(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain, data=d, w)
summary(spatiallag,Nagelkerke = TRUE)

# modèle avec erreur spatiale
spaterror = errorsarlm(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain, data=d, w)
summary(spaterror,Nagelkerke = TRUE)

AIC(spaterror) - AIC(spatiallag)


# TODO Durbin general


# changer la matrice de poids, les spécifications des modèles
weightMatrix<-function(layer,FUN){
  d = units::drop_units(st_distance(st_centroid(layer)))
  #w = exp(-d)/decay)
  w = FUN(d)
  diag(w)<-0
  return(w)
}

bws=seq(from=10,to=100,by=5)
aics = c()
for(bw in 1000*bws){
  show(bw)
  w=mat2listw(weightMatrix(deps, function(x){exp(-x/bw)}))
  spatiallag = lagsarlm(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain, data=d, w)
  aics = append(aics,AIC(spatiallag))
}
bws[aics==min(aics)]

w=mat2listw(weightMatrix(deps, function(x){exp(-x/26000)}))
spatiallag = lagsarlm(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain, data=d, w)
summary(spatiallag)


#####
##  4 ) Regressions multi-niveaux


library(lme4)

lmmulti = lm(prix~MED20+PPEN20+PPAT20+surface_bati+surface_terrain+NOM_REG,data=d)
summary(lmmulti)

# modèle simple avec intercepts variables
multiniv_intercept =
  lmer(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain + (1 | NOM_REG),
       data=d)
summary(multiniv_intercept)

# modèle simple avec coefficients variables
multiniv_slopes =
  lmer(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain +
         (PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain | NOM_REG),
       data=d)
summary(multiniv_slopes)

# comparer les modèles et en tester d'autres

AIC(lmmulti)
AIC(multiniv_intercept)
AIC(multiniv_slopes)

# comparaison à GWR et interprétation
#  ! AIC pas comparable entre multi-niveau et GWR -> utiliser R2
summary(lmmulti)$adj.r.squared
gwoptmodel$GW.diagnostic$gwR2.adj







