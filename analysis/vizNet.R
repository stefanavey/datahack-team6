###################################################################################
## Visualize the Officer Network                                                 ##
###################################################################################

##############
## Packages ##
##############
library(tidyverse)
library(igraph)
## library(RCy3)
library(ggnet)

netFile <- file.path("..", "data", "co_occurence_complaints.csv")
edgelist <- read.csv(netFile)
g <- graph_from_edgelist(as.matrix(edgelist[,1:2]), directed = FALSE)
g <- set_edge_attr(graph = g, name = "weight",
                   index = E(g), edgelist$complaints)
g <- set_vertex_attr(graph = g, name = "label",
                     index = V(g), as.character(V(g)))
complaintNum <- edgelist %>%
    arrange(officer_id.1, officer_id.2) %>%
    filter(officer_id.1 == officer_id.2) %>%
    select(complaints) %>%
    unlist()
g <- set_vertex_attr(graph = g, name = "complaintNumber",
                     index = V(g), value = complaintNum)


## For testing on sub-network
gSub <- simplify(induced_subgraph(g, 1:4), remove.multiple = FALSE,
                 remove.loops = TRUE)

ggnet2(gSub, label.nodes = FALSE,
       alpha = 0.4, edge.size = "weight",
       edge.alpha = 0.1,
       size = "complaintNumber", max_size = 5)

## Full network
gSimple <- simplify(g)

## High-level overview
ggnet2(gSimple, label.nodes = FALSE, alpha = 0.25, size = 1)

## Only look at officers with more than 2 complaints
gSimple2 <- induced_subgraph(gSimple, which(V(gSimple)$complaintNumber > 2))
ggnet2(gSimple2, label.nodes = FALSE,
       alpha = 0.4, edge.size = "weight",
       edge.alpha = 0.1,
       size = "complaintNumber", max_size = 3)


