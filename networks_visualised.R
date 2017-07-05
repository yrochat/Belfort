rm(list=ls())

library(igraph)
library(stringr)
library(ggraph)
library(readr)

set_graph_style(plot_margin = margin(1,1,1,1))

wd <- "~/Documents/GitHub/Belfort"
setwd(wd)
source("sources/create_graph.R")

setwd("reprojetrseauxdepersonnages")

list_of_sources <- list.files()
list_of_adjacency_sources <- list_of_sources[str_detect(list_of_sources, "adj.csv")]

g <- create_graph(list_of_adjacency_sources[1])

## TODO

## shiny app, voir https://github.com/dgrtwo/love-actually-network
## représenter distribution des degrés
## inclure occurrences
## fixer les personnages
## taille des arêtes : trois tailles possibles
## changement de police