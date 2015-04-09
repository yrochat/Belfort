rm(list=ls())

# Ici on définit l'espace de travail. MODIFIER-LE pour que cela corresponde 
# à l'emplacement sur votre propre ordinateur
setwd("~/Documents/Belfort")

# Ici on charge le package igraph qui sert à faire de l'analyse de réseaux
# si ça ne fonctionne pas, il faut d'abord le télécharger. Pour ça :
# install.packages("igraph")
library(igraph) 

source("sources/create_graph.R")

#############################
### LA CREATION DU RESEAU ###
#############################

g <- create_graph(fichier = "exemple/assomoir-adj.csv", seuil = 3, connexe = TRUE)

############
### PLOT ###
############

# Ici on crée le fichier où va arriver le visuel
# On lui donne les dimensions qu'on veut
# Cela crée une sorte de tunnel qui enregistrera tous nos ajouts
# Et qu'on ferme à la fin avec la fonction dev.off()
pdf("output.pdf", height = 15, width = 15)

# À ce stade et pour mémoire, le tunnel est ouvert
# Ce qu'on fait ici est de dire qu'il n'y a aucune marge 
# et que le graphe doit prendre toute la place
par(mar=c(0,0,0,0))

# Là on dessine le graphe, avec plein d'ajustements…
# Pour plus de renseignements, taper dans la console la commande suivant :
# ?igraph.plotting
plot(g,
		vertex.label = paste(substr(V(g)$name, 1, 6), ".", sep=""),
		vertex.size = log2(degree(g))-1,
		vertex.color = "black",
#		vertex.color = rainbow(length(unique(identif)))[V(g)$identif],
		vertex.label.color = "black",
		vertex.label.dist = (log2(degree(g))-.2)/20,
		vertex.label.family = "sans", 
		edge.width = log2(E(g)$weight) / 10,
		edge.color = "darkgrey",
		layout = layout.fruchterman.reingold(g, repulserad = vcount(g)^3.5)
	)
	
# legend("topright", legend = levels(identif), pch = 21, col = "black", pt.bg = rainbow(length(unique(identif))), cex = 3)

# Et on ferme le tunnel
dev.off()

# Le résultat est assez illisible, et devra être amélioré au cas par cas
# par exemple avec tkplot pour choisir un meilleur emplacement pour les noeuds
# ou alors en changeant tailles, couleurs, etc.

# À étudier : les différentes possibilités d'export en D3…







