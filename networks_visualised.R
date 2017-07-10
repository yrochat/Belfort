rm(list=ls())

library(igraph)
library(stringr)
library(ggraph)
library(readr)
library(tidygraph)

set_graph_style(plot_margin = margin(1,1,1,1))

wd <- "~/Documents/GitHub/Belfort"
setwd(wd)
source("sources/create_graph.R")

setwd("reprojetrseauxdepersonnages")

list_of_sources <- list.files()
list_of_adjacency_sources <- list_of_sources[str_detect(list_of_sources, "adj.csv")]

g <- lapply(list_of_adjacency_sources, create_graph)

g[[1]] <- as_tbl_graph(g[[1]])

V(g[[1]])$degree <- degree(g[[1]])

ggraph(g[[1]]) + 
  geom_node_point() + 
  geom_edge_link(aes(width = weight)) + 
  scale_edge_width_continuous(range = c(.1,2)) +
  geom_node_label(aes(label = name), size = 2, repel = TRUE, label.size = .1)

ggplot(as_tibble(g[[1]]), aes(degree)) + 
  stat_ecdf() +
  theme_bw()

  
## TODO

## shiny app, voir https://github.com/dgrtwo/love-actually-network
## représenter distribution des degrés
## inclure occurrences
## fixer les personnages
## taille des arêtes : trois tailles possibles
## changement de police