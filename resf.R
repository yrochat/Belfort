### Script accompagnant l'article de 
### Mathieu Triclot et Yannick Rochat
### soumis à la revue ReS Futurae
### à l'été 2017


### RESTE A FAIRE 
### inclure occurrences
### fixer les personnages
### + voir les issues sur github


rm(list = ls())

library(igraph)
library(stringr)
library(ggraph)
library(readr)
library(tidygraph)
library(gridExtra)
library(extrafont)
loadfonts()

set_graph_style(plot_margin = margin(1, 1, 1, 1))


### working directory 

wd <- "~/Documents/GitHub/Belfort"
setwd(wd)
source("sources/create_graph.R")

setwd("reprojetrseauxdepersonnages")


### Identification de l'emplacement des données des réseaux

list_of_sources <- list.files()

list_of_adjacency_sources <-
  list_of_sources %>%
  str_detect("adj.csv") %>%
  list_of_sources[.]


### Chargement des données et creéation des réseaux

# Seuil à 3, tous les sommets
g_3_unconnected <- lapply(list_of_adjacency_sources,
                          function(x)
                            create_graph(x,
                                         connexe = FALSE,
                                         seuil = 3))

# Seuil à 10, tous les sommets
g_10_unconnected <- lapply(list_of_adjacency_sources,
                           function(x)
                             create_graph(x,
                                          connexe = FALSE,
                                          seuil = 10))

# Seuil à 3, composante géante
g_3_connected <- lapply(list_of_adjacency_sources,
                        function(x)
                          create_graph(x,
                                       connexe = TRUE,
                                       seuil = 3))

# Seuil à 10, composante géante
g_10_connected <- lapply(list_of_adjacency_sources,
                         function(x)
                           create_graph(x,
                                        connexe = TRUE,
                                        seuil = 10))

# Chargement des titres des réseaux
titles <- str_replace_all(string = list_of_adjacency_sources,
                          pattern = "-adj.csv",
                          replacement = "")

setwd(wd)


### Définition d'une fonction calculant le degré

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


### Définition d'une fonction insérant le titre 
### dans les données du réseau

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






plot1 <- ggraph(g) +
  geom_node_point() +
  geom_edge_link(aes(width = weight)) +
  scale_edge_width_continuous(range = c(.1, 2)) +
  geom_node_label(
    aes(label = name),
    size = 2,
    repel = TRUE,
    label.size = .1,
    family = "Helvetica"
  )











###########
### OLD ###
###########


### Cette fonction permettait de visualiser un 
### réseau et la distribution des degrés côte-à-côte

# plot_networks <- function(g) {
#   plot1 <- ggraph(g) +
#     geom_node_point() +
#     geom_edge_link(aes(width = weight)) +
#     scale_edge_width_continuous(range = c(.1, 2)) +
#     geom_node_label(
#       aes(label = name),
#       size = 2,
#       repel = TRUE,
#       label.size = .1,
#       family = "Helvetica"
#     )
#   
#   plot2 <- ggplot(as_tibble(g), aes(degree)) +
#     stat_ecdf() +
#     theme_gray() +
#     ggtitle(str_c("Distribution des degrés de ", g$title))
#   
#   grid.arrange(plot1, plot2, nrow = 1, ncol = 2)
#   
# }
# 
# pdf(
#   "g_3_unconnected.pdf",
#   width = 14,
#   height = 7,
#   onefile = TRUE
# )
# 
# for (i in 1:length(g_3_unconnected)) {
#   plot_networks(g_3_unconnected[[i]])
# }
# 
# dev.off()
# 
# pdf(
#   "g_10_unconnected.pdf",
#   width = 14,
#   height = 7,
#   onefile = TRUE
# )
# 
# for (i in 1:length(g_10_unconnected)) {
#   plot_networks(g_10_unconnected[[i]])
# }
# 
# dev.off()
# 
# pdf(
#   "g_3_connected.pdf",
#   width = 14,
#   height = 7,
#   onefile = TRUE
# )
# 
# for (i in 1:length(g_3_connected)) {
#   plot_networks(g_3_connected[[i]])
# }
# 
# dev.off()
# 
# pdf(
#   "g_10_connected.pdf",
#   width = 14,
#   height = 7,
#   onefile = TRUE
# )
# 
# for (i in 1:length(g_10_connected)) {
#   plot_networks(g_10_connected[[i]])
# }
# 
# dev.off()








