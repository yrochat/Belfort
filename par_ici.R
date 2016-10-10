rm(list=ls())

# Ici on définit l'espace de travail. MODIFIEZ-LE pour que cela corresponde 
# à l'emplacement sur votre propre ordinateur
setwd("~/Documents/Projets R/CaraNetwork")

# Ici on charge le package igraph qui sert à faire de l'analyse de réseaux
# Installation (si besoin) et chargement des packages requis
packages <- c("igraph","RColorBrewer")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(igraph) 
library(RColorBrewer)

source("sources/create_graph.R")
source("sources/shapes.R")
source("sources/plotnework.R")

#############################
### LA CREATION DU RESEAU ###
#############################

# On donne à la fonction les noms de fichiers
# Qui doivent être dans le répetoire reprojetrseauxdepersonnages
# Si on n'a pas d'attributs secondaires, on indique = NULL à la fonction
# La fonction sort un pdf dans le répetoire principal

plotnetwork(fichierindex = "1990.Gunnm-adj.csv",
            fichierattr1 = "1990.Gunnm-attr.csv", #si absent = NULL,
            fichierattr2 = "1990.Gunnm-attr2.csv", #si absent = NULL,
            seuil = 10)
