###################################################################################
## Visualize the Effects                                                         ##
###################################################################################

##############
## Packages ##
##############
library(tidyverse)
library(igraph)
library(ggnet)

effectFile <- file.path("..", "data", "Effect.Rdata")
load(effectFile)
g <- graph_from_adjacency_matrix(Effect, weighted = TRUE)
g <- simplify(g)

## For testing on sub-network
gSub <- induced_subgraph(g, 1:100)

ggnet2(gSub, label.nodes = FALSE, arrow.size = 5, size = 1)

## Full network
## High-level overview
ggnet2(g, label.nodes = FALSE, alpha = 0.25,
       edge.size = "weight",
       size = 1, arrow.size = 3)


