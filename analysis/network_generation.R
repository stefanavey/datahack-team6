## RCy3
library(RCy3)

library(tidyverse)
source("R/calc_network.R")

## Read in munged datasets
complaint <- read_csv("data/toy.complaint_data_cleaned.csv")[, -1] %>%
    arrange(officer_id)
officer <- read_csv("data/toy.officer_data_cleaned.csv")[, -1] %>%
    arrange(officer_id)

## Add total complaints per officer
officer <- complaint %>%
    group_by(officer_id) %>%
    count %>%
    rename(total_complaints = n) %>%
    inner_join(officer, ., by = "officer_id")
    

## -----------------------------------------------------------------
## Create the graph, initialize node names with officer ids
g <- new("graphNEL", edgemode = "undirected")
g@nodes <- officer$officer_id

## Initialize the Node Attributes from Officers ------
## Attribute Names
cols <- officer %>%
    select(-appointed_date) %>%
    colnames

## Attribute Types
types <- officer %>%
    select(-appointed_date) %>%
    map(class) %>% unlist %>% as.character
types <- ifelse(types == "character", "char", types)

## Assign attributes + defaults
for (i in seq_along(cols)) {
    g <- initNodeAttribute(graph = g,
                      attribute.name = cols[i],
                      attribute.type = types[i],
                      default.value = NA)
}

## Add node data
for (property in cols) {
    nodeData(g,
             n = as.character(officer$officer_id), # node names
             attr = property) <-
        officer %>% select_(property) %>% unlist 
}


## -----------------------------------------------------------------
## Calculate Edges
edgelist <- calc_network(complaint %>% head, group = "crid", indicator = "officer_id")
colnames(edgelist) <- c("officer_id.1", "officer_id.2", "complaints")

g <- addEdge(graph = g,
        from = as.character(edgelist$officer_id.1),
        to = as.character(edgelist$officer_id.2))
##        weights = rep(1, nrow(edgelist)))




## -----------------------------------------------------------------
## Send to Cytoscape

cw <- CytoscapeWindow("officers", overwrite=TRUE)
cw@graph <- g
displayGraph(cw) 

edgeNames(g)
