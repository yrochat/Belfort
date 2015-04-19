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
source("sources/shapes.R")

#############################
### LA CREATION DU RESEAU ###
#############################

g <- create_graph(	fichier = "reprojetrseauxdepersonnages/Bradbury2.csv", 
					attr1 = "reprojetrseauxdepersonnages/Bradbury2-attr.csv",
					attr2 = "reprojetrseauxdepersonnages/Bradbury2-attr2.csv", 
					seuil = 3, 
					connexe = TRUE)

# g <- create_graph(	fichier = "reprojetrseauxdepersonnages/Bradbury2.csv", 
					# attr1 = "reprojetrseauxdepersonnages/Bradbury2-attr.csv",
					# seuil = 3, 
					# connexe = TRUE)

# g <- create_graph(	fichier = "reprojetrseauxdepersonnages/Bradbury2.csv",
					# seuil = 3, 
					# connexe = TRUE)

############
### PLOT ###
############

# Ici on crée le fichier où va arriver le visuel
# On lui donne les dimensions qu'on veut
# Cela crée une sorte de tunnel qui enregistrera tous nos ajouts
# Et qu'on ferme à la fin avec la fonction dev.off()

pdf("output.pdf", height = 14 + g$windows, width = 14)

layout(matrix(1:(1+g$windows), nrow = 1+g$windows), widths = rep(1,1+g$windows), heights = c(10,rep(1,g$windows)))

# À ce stade et pour mémoire, le tunnel est ouvert
# Ce qu'on fait ici est de dire qu'il n'y a aucune marge sauf à droite
# et que le graphe doit prendre toute la place
par(mar=c(1,1,1,1))

# Là on dessine le graphe, avec plein d'ajustements…
# Pour plus de renseignements, taper dans la console la commande suivant :
# ?igraph.plotting

# On enregistre les noms
vertex.label <- V(g)$name

# Si un des noms est trop long, on les réduit tous à 20 caractères, suivis d'un point
if (max(sapply(V(g)$name,nchar)) > 20) {vertex.label <- paste(substr(V(g)$name, 1, 20), ".", sep="")}

# La taille des sommets
vertex.size <- log2(degree(g)+1)*2

# Une couleur par défaut
vertex.color <- "firebrick2"

# S'il existe un premier attribut (attr1), on distribue des couleurs aux sommets
if (!is.null(V(g)$id1))	{vertex.color <- brewer.pal(length(unique((V(g)$id1))), "Set1")[factor(V(g)$id1)]}

# Une forme par défaut
vertex.shape <- "circle"

# S'il existe un second attribut (attr2), on distribue des formes aux sommets
if (!is.null(V(g)$id2)) {vertex.shape <- c("circle", "square", "losange")[factor(V(g)$id2)]}

# La couleur du texte
vertex.label.color <- "black"

# La distance des labels dépend des tailles
vertex.label.dist <- (log2(degree(g)+5))/15

# L'angle où s'affiche le label
vertex.label.degree <- -pi/2

# Le choix de la police
vertex.label.family <- "mono"

# La taille de la police
vertex.label.cex <- 2

# La largeur des arêtes, normalisée entre 0 et 1
weight.normalised <- (E(g)$weight-min(E(g)$weight))/(max(E(g)$weight)-min(E(g)$weight))

# La largeur des arêtes, avec minimum égal à 0*8+2 = 2 et maximum égal à 1*8+2 = 10
edge.width <- weight.normalised*8+2

# On définit une palette de gris pour faire un dégradé sur les arêtes
gris <- grey(0:(1.2*max(E(g)$weight))/(1.2*max(E(g)$weight)))

# La couleur des arêtes
edge.color <- gris[max(E(g)$weight)-(E(g)$weight-min(E(g)$weight)+1)]

# Et ici on dessine le graphe ! L'instance appelle les paramètres sauvés ci-dessus.
plot(g,
		vertex.label = vertex.label,
		vertex.size = vertex.size,
		vertex.color = vertex.color,
		vertex.shape = vertex.shape,
		vertex.label.color = vertex.label.color,
		vertex.label.dist = vertex.label.dist,
		vertex.label.degree = vertex.label.degree,
		vertex.label.family = vertex.label.family, 
		vertex.label.cex = vertex.label.cex,
		edge.width = edge.width,
		edge.color = edge.color
	)

# la légende pour le premier attribut (la couleur)
if (g$windows > 0) {		plot.new()
							par(mar=c(0,0,0,0))
							legend(	x="center", 
									legend = levels(factor(V(g)$id1)), 
									pch = 21, 
									col = "black", 
									pt.bg = brewer.pal(length(unique((V(g)$id1))), "Set1"), 
									cex = g$windows + 1, 
									title = g$attr1, 
									bty = "n", 
									horiz = TRUE, 
									pt.cex = 4)}

# la légende pour le second attribut (la forme)
if (g$windows > 1) {	plot.new()
							par(mar=c(0,0,0,0))
							legend(	x="center", 	
									legend = levels(factor(V(g)$id2)), 
									pch = c(19,15,18)[1:length(levels(factor(V(g)$id2)))], 
									col = "black", 
									cex = 3, 
									title = g$attr2, 
									bty = "n", 
									horiz = TRUE, 
									pt.cex = 4)}

# Et on ferme le tunnel
dev.off()

# Le résultat est assez illisible, et devra être amélioré au cas par cas
# par exemple avec tkplot pour choisir un meilleur emplacement pour les noeuds
# ou alors en changeant tailles, couleurs, etc.

# À étudier : les différentes possibilités d'export en D3…







