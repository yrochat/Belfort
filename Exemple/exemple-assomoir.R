rm(list=ls())

library(igraph)

setwd("~/Documents/Belfort/Exemple")

# On importe le csv sous forme de matrice d'incidence
mat <- read.table("assomoir-adj.csv", header = TRUE, check.names = FALSE, sep=",")

# On donne comme noms de ligne les noms de la première colonne
rownames(mat) <- mat[,1]

# Puis on la supprime
mat <- mat[,-1]
# Maintenant la matrice n'est plus composée que de 0 et de 1

# On crée un graphe biparti à partir de cette matrice d'incidence
g0 <- graph.incidence(mat, multiple = TRUE)

# On extrait la liste des arêtes, et on la copie deux fois
tab <- get.edgelist(g0)
tab2 <- tab1 <- tab

# Ici, chaque page n se transforme en couple de pages (n-1, n)
tab1[,2] <- paste(as.numeric(tab[,2])-1, tab[,2], sep="")

# Ici, chaque page n se transforme en couple de pages (n, n+1)
tab2[,2] <- paste(tab[,2], as.numeric(tab[,2])+1, sep="")

# On réunit ces deux listes d'arêtes
tab0 <- rbind(tab1, tab2)

# On supprime les doublons créés par un nom sur des pages consécutives
tab0 <- tab0[!duplicated(tab0),]

# On crée le graphe, qui est déjà biparti
g0 <- graph.edgelist(tab0, directed = FALSE)

# Ceci est nécessaire pour pouvoir faire la projection et 
# obtenir le réseau de personnages (deux persos sont connectés 
# s'ils apparaissent sur les mêmes couples de pages)
V(g0)$type <- bipartite.mapping(g0)$type

# Voilà la projection…
g <- bipartite.projection(g0)$proj1

# Et voilà le produit final !
g <- g - E(g)[E(g)$weight < 3]









