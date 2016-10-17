rm(list=ls())

# Ici on définit l'espace de travail. MODIFIER-LE pour que cela corresponde 
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

plotnetwork(fichierindex = "1875.IleMysterieuse-adj.csv",
            fichierattr1 = "1875.IleMysterieuse-attr.csv", #si absent = NULL,
            fichierattr2 = "1875.IleMysterieuse-attr2.csv", #si absent = NULL,
            seuil = 3)

################
### EN MASSE ###
################

# session <- list.files (path ="reprojetrseauxdepersonnages/", pattern = "adj.csv") # Récupérer liste des fichiers .zip dans le répetoire de travail
# for (i in 1:length(session)){
#  plotnetwork(session[i],NULL,NULL,3)
# }
