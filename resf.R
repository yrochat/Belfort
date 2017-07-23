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


### Chargement des données et création des réseaux

get_attr1 <- function(x) {
  attr1 <- x %>% 
    str_replace("-adj.csv", "") %>% 
    str_c("-attr.csv")
  if(attr1 %in% list_of_sources) {
    return(attr1)
  } else {
    return("")
  }
}

get_attr2 <- function(x) {
  attr2 <- x %>% 
    str_replace("-adj.csv", "") %>% 
    str_c("-attr2.csv")
  if(attr2 %in% list_of_sources) {
    return(attr2)
  } else {
    return("")
  }
}


# Seuil à 3, tous les sommets
g_3_unconnected <- lapply(list_of_adjacency_sources,
                          function(x)
                            create_graph(x,
                                         attr1 = get_attr1(x),
                                         attr2 = get_attr2(x),
                                         connexe = FALSE,
                                         seuil = 3))

# Seuil à 10, tous les sommets
g_10_unconnected <- lapply(list_of_adjacency_sources,
                           function(x)
                             create_graph(x,
                                          attr1 = get_attr1(x),
                                          attr2 = get_attr2(x),
                                          connexe = FALSE,
                                          seuil = 10))

# Seuil à 3, composante géante
g_3_connected <- lapply(list_of_adjacency_sources,
                        function(x)
                          create_graph(x,
                                       attr1 = get_attr1(x),
                                       attr2 = get_attr2(x),
                                       connexe = TRUE,
                                       seuil = 3))

# Seuil à 10, composante géante
g_10_connected <- lapply(list_of_adjacency_sources,
                         function(x)
                           create_graph(x,
                                        attr1 = get_attr1(x),
                                        attr2 = get_attr2(x),
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


### Dessiner les réseaux

draw <- function(g) {
  ggraph(g) +
  geom_node_point(aes(size = degree(g))) +
  geom_edge_link(aes(width = weight)) +
  scale_edge_width_continuous(range = c(.1, 2), "Poids") +
  geom_node_label(
    aes(label = name),
    size = 2,
    repel = TRUE,
    label.size = .1,
    family = "Helvetica",
    alpha = .8,
    segment.colour = "pink"
  ) +
  scale_size_area(max_size = 5, "Degré")
}


##################
###            ###
### CHAPITRE 2 ###
###            ###
##################


### METRO 2033

metro_2033 <- which(str_detect(titles, "Metro2033"))
metro_2033_plot <- draw(g_3_connected[[metro_2033]])

# graph.density(g_3_connected[[metro_2033]])

ggsave("viz/Image01.png", metro_2033_plot, width = 10, height = 7)


## ORANGE MECANIQUE

orange <- which(str_detect(titles, "OrangeMeca"))
g_3_connected[[orange]]$layout <- layout_with_fr(g_3_connected[[orange]], niter = 2500)
orange_plot <- draw(g_3_connected[[orange]])

ggsave("viz/Image02.png", orange_plot, width = 10, height = 7)


### La Cité des Permutants

cite_permutants <- which(str_detect(titles, "la_cite_des_permutants"))
g_3_connected[[cite_permutants]]$layout <- layout.auto(g_3_connected[[cite_permutants]])

cite_permutants_plot_3 <- draw(g_3_connected[[cite_permutants]])
cite_permutants_plot_10 <- draw(delete_edges(g_3_connected[[cite_permutants]], E(g_3_connected[[cite_permutants]])[weight < 10]))

cite_permutants_plot <- marrangeGrob(list(cite_permutants_plot_3, cite_permutants_plot_10), nrow = 1, ncol = 2, top = "")

ggsave("viz/Image03.png", cite_permutants_plot, width = 15, height = 7)


### LA CASTE DES META-BARONS

meta_barons <- which(str_detect(titles, "Meta-Baron"))
meta_barons_plot <- draw(g_3_connected[[meta_barons]])

ggsave("viz/Image04.png", meta_barons_plot, width = 10, height = 7)


### Seul sur Mars

seul_mars <- which(str_detect(titles, "Seul_sur_mars"))
g_3_connected[[seul_mars]]$layout <- layout_with_fr(g_3_connected[[seul_mars]], weights = NULL)
seul_mars_plot <- draw(g_3_connected[[seul_mars]])

# le graphe est hyper-connecté
# graph.density(g_3_connected[[seul_mars]])

ggsave("viz/Image05.png", seul_mars_plot, width = 10, height = 7)


### BLOODMONEY + CENTAURE

bloodmoney <- which(str_detect(titles, "Bloodmoney"))
bloodmoney_plot <- draw(g_3_connected[[bloodmoney]])

centaure <- which(str_detect(titles, "Ledieuvenuducentaure"))
centaure_plot <- draw(g_3_connected[[centaure]])

bloodmoney_centaure_plot <- marrangeGrob(list(bloodmoney_plot, centaure_plot), nrow = 1, ncol = 2, top = "")

ggsave("viz/Image06.png", bloodmoney_centaure_plot, width = 16, height = 7)


# ### ILE MYSTERIEUSE
# 
# ile_mysterieuse <- which(str_detect(titles, "IleMysterieuse"))
# ile_mysterieuse_plot <- draw(g_3_connected[[ile_mysterieuse]])
# 
# ggsave("viz/ile_mysterieuse.png", ile_mysterieuse_plot, width = 10, height = 7)


### BEGUM

begum <- which(str_detect(titles, "500millions_Begum"))
begum_plot <- draw(g_3_connected[[begum]])

ggsave("viz/Image07.png", begum_plot, width = 10, height = 7)


### SECONDE FONDATION

seconde_fondation <- which(str_detect(titles, "fondation3"))
seconde_fondation_plot <- draw(g_3_connected[[seconde_fondation]])

ggsave("viz/Image08.png", seconde_fondation_plot, width = 10, height = 7)



### COMPAGNIE DES GLACES

compagnie_glaces <- which(str_detect(titles, "CompagnieDesGlaces"))
compagnie_glaces_plot <- draw(g_3_connected[[compagnie_glaces]])

ggsave("viz/Image09.png", compagnie_glaces_plot, width = 10, height = 7)


##################
###            ###
### CHAPITRE 3 ###
###            ###
##################


### Dessiner les réseaux avec des attributs

draw2 <- function(g) {
  ggraph(g) +
    geom_edge_link(aes(width = weight)) +
    geom_node_point(aes(size = degree(g), color = id1, shape = id2)) +
    scale_edge_width_continuous(range = c(.1, 2), "Poids") +
    geom_node_label(
      aes(label = name),
      size = 2,
      repel = TRUE,
      label.size = .1,
      family = "Helvetica",
      alpha = .8,
      segment.colour = "pink"
    ) +
    scale_size_area(max_size = 5, "Degré") + 
    scale_shape_manual(values = c(15, 16, 17, 18, 4, 8), "Attribut secondaire") +
    scale_color_brewer(palette = "Set1", "Type")
}


### ILE MYSTERIEUSE AVEC ATTRIBUTS 2

ile_mysterieuse <- which(str_detect(titles, "IleMysterieuse"))
ile_mysterieuse_plot <- draw2(g_3_connected[[ile_mysterieuse]])

ggsave("viz/Image12.png", ile_mysterieuse_plot, width = 10, height = 7)


### ILE MYSTERIEUSE AVEC ATTRIBUTS 3

id3 <-
  read.csv(
    file = "reprojetrseauxdepersonnages/1875.IleMysterieuse-attr3.csv",
    header = TRUE,
    check.names = FALSE,
    sep = ",",
    stringsAsFactors = FALSE
  )

V(g_3_connected[[ile_mysterieuse]])$id3 <- 
  id3[, 2][match(V(g_3_connected[[ile_mysterieuse]])$name, id3[, 1])]

ile_mysterieuse_plot3 <- ggraph(g_3_connected[[ile_mysterieuse]]) +
  geom_edge_link(aes(width = weight)) +
  geom_node_point(aes(size = degree(g_3_connected[[ile_mysterieuse]]), color = id1, shape = id3)) +
  scale_edge_width_continuous(range = c(.1, 2), "Poids") +
  geom_node_label(
    aes(label = name),
    size = 2,
    repel = TRUE,
    label.size = .1,
    family = "Helvetica",
    alpha = .8,
    segment.colour = "pink"
  ) +
  scale_size_area(max_size = 5, "Degré") + 
  scale_shape_manual(values = c(15, 16, 17, 18, 4, 8), "Attribut secondaire") +
  scale_color_brewer(palette = "Set1", "Type")

ggsave("viz/Image13.png", ile_mysterieuse_plot3, width = 10, height = 7)


### L'ILE DU DOCTEUR MOREAU

docteur_moreau <- which(str_detect(titles, "Moreau"))
docteur_moreau_plot <- draw2(g_3_connected[[docteur_moreau]])

ggsave("viz/Image14.png", docteur_moreau_plot, width = 10, height = 7)


### JEKYLL & HYDE

jekyll_hyde <- which(str_detect(titles, "Jekyll_Hyde"))
jekyll_hyde_plot <- ggraph(g_3_connected[[jekyll_hyde]]) +
  geom_edge_link(aes(width = weight)) +
  geom_node_point(aes(size = degree(g_3_connected[[jekyll_hyde]]), color = id1)) +
  scale_edge_width_continuous(range = c(.1, 2), "Poids") +
  geom_node_label(
    aes(label = name),
    size = 2,
    repel = TRUE,
    label.size = .1,
    family = "Helvetica",
    alpha = .8,
    segment.colour = "pink"
  ) +
  scale_size_area(max_size = 5, "Degré") + 
  scale_color_brewer(palette = "Set1", "Type")

ggsave("viz/Image15.png", jekyll_hyde_plot, width = 10, height = 7)


### FAHRENHEIT 451

fahrenheit_451 <- which(str_detect(titles, "Fahrenheit451"))
fahrenheit_451_plot <- draw2(g_3_connected[[fahrenheit_451]])

ggsave("viz/Image16.png", fahrenheit_451_plot, width = 10, height = 7)


### 1984

eighty_four <- which(str_detect(titles, "1949.1984"))
eighty_four_plot <- ggraph(g_3_connected[[eighty_four]]) +
  geom_edge_link(aes(width = weight)) +
  geom_node_point(aes(size = degree(g_3_connected[[eighty_four]]), color = id1)) +
  scale_edge_width_continuous(range = c(.1, 2), "Poids") +
  geom_node_label(
    aes(label = name),
    size = 2,
    repel = TRUE,
    label.size = .1,
    family = "Helvetica",
    alpha = .8,
    segment.colour = "pink"
  ) +
  scale_size_area(max_size = 5, "Degré") + 
  scale_color_brewer(palette = "Set1", "Type")

ggsave("viz/Image17.png", eighty_four_plot, width = 10, height = 7)


### EQUILIBRIUM

equilibrium <- which(str_detect(titles, "Equilibrium"))
equilibrium_plot <- ggraph(g_3_connected[[equilibrium]]) +
  geom_edge_link(aes(width = weight)) +
  geom_node_point(aes(size = degree(g_3_connected[[equilibrium]]), color = id1)) +
  scale_edge_width_continuous(range = c(.1, 2), "Poids") +
  geom_node_label(
    aes(label = name),
    size = 2,
    repel = TRUE,
    label.size = .1,
    family = "Helvetica",
    alpha = .8,
    segment.colour = "pink"
  ) +
  scale_size_area(max_size = 5, "Degré") + 
  scale_color_brewer(palette = "Set1", "Type")

ggsave("viz/Image18.png", equilibrium_plot, width = 10, height = 7)


### GATTACA

gattaca <- which(str_detect(titles, "Bienvenue_a_Gattaca"))
gattaca_plot <- ggraph(g_3_connected[[gattaca]]) +
  geom_edge_link(aes(width = weight)) +
  geom_node_point(aes(size = degree(g_3_connected[[gattaca]]), color = id1)) +
  scale_edge_width_continuous(range = c(.1, 2), "Poids") +
  geom_node_label(
    aes(label = name),
    size = 2,
    repel = TRUE,
    label.size = .1,
    family = "Helvetica",
    alpha = .8,
    segment.colour = "pink"
  ) +
  scale_size_area(max_size = 5, "Degré") + 
  scale_color_brewer(palette = "Set1", "Type")

ggsave("viz/Image19.png", gattaca_plot, width = 10, height = 7)


### STALKER

stalker <- which(str_detect(titles, "Stalker"))
stalker_plot <- ggraph(g_3_connected[[stalker]]) +
  geom_edge_link(aes(width = weight)) +
  geom_node_point(aes(size = degree(g_3_connected[[stalker]]), color = id1)) +
  scale_edge_width_continuous(range = c(.1, 2), "Poids") +
  geom_node_label(
    aes(label = name),
    size = 2,
    repel = TRUE,
    label.size = .1,
    family = "Helvetica",
    alpha = .8,
    segment.colour = "pink"
  ) +
  scale_size_area(max_size = 5, "Degré") + 
  scale_color_brewer(palette = "Set1", "Type")

ggsave("viz/Image20.png", stalker_plot, width = 10, height = 7)


### NEUROMANCIEN

neuromancien <- which(str_detect(titles, "Neuromancien"))
neuromancien_plot <- ggraph(g_3_connected[[neuromancien]]) +
  geom_edge_link(aes(width = weight)) +
  geom_node_point(aes(size = degree(g_3_connected[[neuromancien]]), color = id1)) +
  scale_edge_width_continuous(range = c(.1, 2), "Poids") +
  geom_node_label(
    aes(label = name),
    size = 2,
    repel = TRUE,
    label.size = .1,
    family = "Helvetica",
    alpha = .8,
    segment.colour = "pink"
  ) +
  scale_size_area(max_size = 5, "Degré") + 
  scale_color_brewer(palette = "Set1", "Type")

ggsave("viz/Image21.png", neuromancien_plot, width = 10, height = 7)


### FINAL FANTASY VII

final_fantasy_vii <- which(str_detect(titles, "FF7"))
final_fantasy_vii_plot <- ggraph(g_3_connected[[final_fantasy_vii]]) +
  geom_edge_link(aes(width = weight)) +
  geom_node_point(aes(size = degree(g_3_connected[[final_fantasy_vii]]), color = id1)) +
  scale_edge_width_continuous(range = c(.1, 2), "Poids") +
  geom_node_label(
    aes(label = name),
    size = 2,
    repel = TRUE,
    label.size = .1,
    family = "Helvetica",
    alpha = .8,
    segment.colour = "pink"
  ) +
  scale_size_area(max_size = 5, "Degré") + 
  scale_color_brewer(palette = "Set1", "Type")

ggsave("viz/Image22.png", final_fantasy_vii_plot, width = 10, height = 7)


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








