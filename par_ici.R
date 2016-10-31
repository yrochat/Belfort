rm(list=ls())

# Ici on définit l'espace de travail. MODIFIEZ-LE pour que cela corresponde 
# à l'emplacement sur votre propre ordinateur
setwd("~/Documents/Projets R/CaraNetwork")

# Ici on charge le package igraph qui sert à faire de l'analyse de réseaux
# Installation (si besoin) et chargement des packages requis
packages <- c("igraph","RColorBrewer","networkD3")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(igraph) 
library(RColorBrewer)
library(networkD3)

source("sources/create_graph.R")
source("sources/shapes.R")
source("sources/plotnework.R")

#############################
### LA CREATION DU RESEAU ###
#############################

# Placer le(s) fichier(s) dans le répertoire reprojetrseauxdepersonnages

plotnetwork(fichierindex = "1875.IleMysterieuse-adj.csv",
            fichierattr1 = "1875.IleMysterieuse-attr.csv", #si absent = NULL,
            fichierattr2 = "1875.IleMysterieuse-attr2.csv" , #si absent = NULL,
            seuil = 3)

################
### EN MASSE ###
################

# sans attributs secondaires
# session <- list.files (path ="reprojetrseauxdepersonnages/", pattern = "adj.csv") # Récupérer liste des fichiers .zip dans le répetoire de travail
# for (i in 1:length(session)){
#  plotnetwork(session[i],NULL,NULL,3)
#}

# avec 1 attribut secondaire (Science, Technique, Politique)
# session <- list.files (path ="reprojetrseauxdepersonnages/", pattern = "attr.csv") # Récupérer liste des fichiers .zip dans le répetoire de travail
# fichindex <- paste (substr(session, 1, nchar(session)-8),"adj.csv",sep="")
# for (i in 1:length(session)){
#  plotnetwork(fichindex[i],session[i],NULL,3)
#}
