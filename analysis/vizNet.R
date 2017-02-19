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

officerFile <- file.path("..", "data", "toy.officer_data_cleaned.csv")
officers <- read.csv(officerFile, row.names = 1) %>%
    arrange(officer_id)
head(officers)

netFile <- file.path("..", "data", "co_occurence_complaints.csv")
edgelist <- read.csv(netFile)
g <- graph_from_edgelist(as.matrix(edgelist[,1:2]), directed = FALSE)
g <- set_edge_attr(graph = g, name = "weight",
                   index = E(g), edgelist$complaints)
g <- set_vertex_attr(graph = g, name = "label",
                     index = V(g), as.character(V(g)))
g <- set_vertex_attr(graph = g, name = "gender",
                     index = V(g), officers$gender)

complaintNum <- edgelist %>%
    arrange(officer_id.1, officer_id.2) %>%
    filter(officer_id.1 == officer_id.2) %>%
    select(complaints) %>%
    unlist()
g <- set_vertex_attr(graph = g, name = "complaintNumber",
                     index = V(g), value = complaintNum)


## For testing on sub-network
gSub <- simplify(induced_subgraph(g, 1:10), remove.multiple = FALSE,
                 remove.loops = TRUE)

ggnet2(gSub, label.nodes = FALSE,
       color = "gender",
       alpha = 0.4, edge.size = "weight",
       edge.alpha = 0.1,
       size = "complaintNumber", max_size = 5)

## Full network
gSimple <- simplify(g)

## High-level overview
ggnet2(gSimple, color = "gender",
       palette = c(`1` = "tomato", `2` = "steelblue"),
       label.nodes = FALSE,
       alpha = 0.25, size = 1) +
    theme(panel.background = element_rect(fill = "grey15"))


## Only look at officers with more than 1 co-occurence
gSimple2 <- subgraph.edges(gSimple, eids = which(E(gSimple)$weight > 1))
ggnet2(gSimple2, label.nodes = FALSE,
       alpha = 0.4, edge.color = "weight",
       edge.alpha = 0.1,
       size = "complaintNumber", max_size = 3)

res <- igraph::degree(graph = g) / V(g)$complaintNumber

vs <- neighbors(graph = g, v = which.max(res), mode = c("all"))




gSub <- simplify(induced_subgraph(g, vs))
ggnet2(gSub,
       label = as.character(vs),
       label.size = 10,
       alpha = 0.4, edge.size = "weight",
       edge.alpha = 0.1,
       size = "complaintNumber", max_size = 5)
