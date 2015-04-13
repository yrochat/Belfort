rm(list=ls())

# Ici on définit l'espace de travail. MODIFIER-LE pour que cela corresponde 
# à l'emplacement sur votre propre ordinateur
setwd("~/Documents/Belfort")

# Ici on charge le package igraph qui sert à faire de l'analyse de réseaux
# si ça ne fonctionne pas, il faut d'abord le télécharger. Pour ça :
# install.packages("igraph")
library(igraph) 
library(RColorBrewer)

source("sources/create_graph.R")

#############################
### LA CREATION DU RESEAU ###
#############################

g <- create_graph(	fichier = "reprojetrseauxdepersonnages/Bradbury2.csv", 
					attr1 = "reprojetrseauxdepersonnages/Bradbury2-attr.csv",
					attr2 = "reprojetrseauxdepersonnages/Bradbury2-attr2.csv", 
					seuil = 3, 
					connexe = TRUE)

# g <- create_graph(	fichier = "reprojetrseauxdepersonnages/Bradbury2.csv",
#					seuil = 3, 
#					connexe = TRUE)

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

		vertex.label <- V(g)$name
if (max(sapply(V(g)$name,nchar)) > 15) {vertex.label <- paste(substr(V(g)$name, 1, 6), ".", sep="")}
		vertex.size <- log2(degree(g))-1
		vertex.color <- "firebrick2"
if (!is.null(V(g)$id1))	{vertex.color <- brewer.pal(length(unique((V(g)$id1))), "Set1")[factor(V(g)$id1)]}
		vertex.label.color <- "black"
		vertex.label.dist <- (log2(degree(g)+1)+.4)/25
		vertex.label.family <- "sans"
		edge.width <- ((E(g)$weight)-1)/2
		edge.color <- "darkgrey"

plot(g,
		vertex.label = vertex.label,
		vertex.size = vertex.size,
		vertex.color = vertex.color,
		vertex.label.color = vertex.label.color,
		vertex.label.dist = vertex.label.dist,
		vertex.label.family = vertex.label.family, 
		edge.width = edge.width,
		edge.color = edge.color
	)

if (!is.null(V(g)$id1)) {legend("bottomleft", legend = levels(factor(V(g)$id1)), pch = 21, col = "black", pt.bg = brewer.pal(length(unique((V(g)$id1))), "Set1"), cex = 3)}

# Et on ferme le tunnel
dev.off()

# Le résultat est assez illisible, et devra être amélioré au cas par cas
# par exemple avec tkplot pour choisir un meilleur emplacement pour les noeuds
# ou alors en changeant tailles, couleurs, etc.

# À étudier : les différentes possibilités d'export en D3…







