rm(list=ls())

library(igraph)
library(stringr)
library(ggraph)
library(readr)
library(tidygraph)
library(gridExtra)
library(extrafont)
loadfonts()

set_graph_style(plot_margin = margin(1,1,1,1))

wd <- "~/Documents/GitHub/Belfort"
setwd(wd)
source("sources/create_graph.R")

setwd("reprojetrseauxdepersonnages")

list_of_sources <- list.files()
list_of_adjacency_sources <- list_of_sources[str_detect(list_of_sources, "adj.csv")]

g <- lapply(list_of_adjacency_sources, create_graph)

titres <- str_replace_all(string = list_of_adjacency_sources, pattern = "-adj.csv", replacement = "")

setwd(wd)

for (i in 1:length(g)) {
  g[[i]] <- as_tbl_graph(g[[i]])

  V(g[[i]])$degree <- degree(g[[i]])
}

pdf("hello.pdf", width = 14, height = 7)

for (i in 1:length(g)) {
  
  plot1 <- ggraph(g[[i]]) + 
    geom_node_point() + 
    geom_edge_link(aes(width = weight)) + 
    scale_edge_width_continuous(range = c(.1,2)) +
    geom_node_label(aes(label = name), size = 2, repel = TRUE, label.size = .1, family = "Helvetica")
  
  plot2 <- ggplot(as_tibble(g[[i]]), aes(degree)) + 
    stat_ecdf() +
    theme_gray() +
    ggtitle(str_c("Distribution des degrés de ", titres[i]))
  
  grid.arrange(plot1, plot2, nrow=1, ncol=2)
}

dev.off()
  
## TODO

## shiny app, voir https://github.com/dgrtwo/love-actually-network
## représenter distribution des degrés
## inclure occurrences
## fixer les personnages
## taille des arêtes : trois tailles possibles
## changement de police