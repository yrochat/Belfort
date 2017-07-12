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

list_of_adjacency_sources <- 
  list_of_sources %>% 
  str_detect("adj.csv") %>% 
  list_of_sources[.]

g_3_unconnected <- lapply(list_of_adjacency_sources,
                          function(x) create_graph(x, 
                                                   connexe = FALSE,
                                                   seuil = 3))

g_10_unconnected <- lapply(list_of_adjacency_sources,
                          function(x) create_graph(x,
                                                   connexe = FALSE,
                                                   seuil = 10))

g_3_connected <- lapply(list_of_adjacency_sources,
                          function(x) create_graph(x,
                                                   connexe = TRUE,
                                                   seuil = 3))

g_10_connected <- lapply(list_of_adjacency_sources,
                          function(x) create_graph(x,
                                                   connexe = TRUE,
                                                   seuil = 10))

titles <- str_replace_all(string = list_of_adjacency_sources,
                          pattern = "-adj.csv",
                          replacement = "")

setwd(wd)

get_degree <- function(g) {
  
  res <- list(length(g))
  
  for (i in 1:length(g)) {
    res[[i]] <- as_tbl_graph(g[[i]])
    V(res[[i]])$degree <- degree(res[[i]])
  }
  
  return(res)
}

g_3_unconnected <- g_3_unconnected %>% get_degree
g_10_unconnected <- g_10_unconnected %>% get_degree
g_3_connected <- g_3_connected %>% get_degree
g_10_connected <- g_10_connected %>% get_degree

get_title <- function(g) {
  res <- g
  
  for (i in 1:length(g)) {
    res[[i]]$title <- titles[i]
  }
  
  return(res)
}

g_3_unconnected <- g_3_unconnected %>% get_title
g_10_unconnected <- g_10_unconnected %>% get_title
g_3_connected <- g_3_connected %>% get_title
g_10_connected <- g_10_connected %>% get_title

plot_networks <- function(g) {
  
  plot1 <- ggraph(g) + 
    geom_node_point() + 
    geom_edge_link(aes(width = weight)) + 
    scale_edge_width_continuous(range = c(.1,2)) +
    geom_node_label(aes(label = name), size = 2, repel = TRUE, label.size = .1, family = "Helvetica")
  
  plot2 <- ggplot(as_tibble(g), aes(degree)) + 
    stat_ecdf() +
    theme_gray() +
    ggtitle(str_c("Distribution des degrés de ", g$title))
  
  grid.arrange(plot1, plot2, nrow=1, ncol=2)
    
}

pdf("g_3_unconnected.pdf", width = 14, height = 7, onefile = TRUE)
  
  for(i in 1:length(g_3_unconnected)) {
    plot_networks(g_3_unconnected[[i]])
  }

dev.off()

pdf("g_10_unconnected.pdf", width = 14, height = 7, onefile = TRUE)

for(i in 1:length(g_10_unconnected)) {
  plot_networks(g_10_unconnected[[i]])
}

dev.off()

pdf("g_3_connected.pdf", width = 14, height = 7, onefile = TRUE)

for(i in 1:length(g_3_connected)) {
  plot_networks(g_3_connected[[i]])
}

dev.off()

pdf("g_10_connected.pdf", width = 14, height = 7, onefile = TRUE)

for(i in 1:length(g_10_connected)) {
  plot_networks(g_10_connected[[i]])
}

dev.off()









## TODO

## shiny app, voir https://github.com/dgrtwo/love-actually-network
## représenter distribution des degrés
## inclure occurrences
## fixer les personnages
## taille des arêtes : trois tailles possibles
## changement de police