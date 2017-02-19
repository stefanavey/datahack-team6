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
types <- ifelse(types == "integer", "numeric", types)

## Assign attributes + defaults
for (i in seq_along(cols)) {
    g <- initNodeAttribute(graph = g,
                      attribute.name = cols[i],
                      attribute.type = types[i],
                      default.value = ifelse(types[i] == "char",
                                             "unspecified", 0))
}

## Add node data
for (i in seq_along(cols)) {

    dat <- officer %>% select_(cols[i]) %>% unlist

    if (types[i] == "char") {
        dat <- as.character(dat)
    } else {
        dat <- as.numeric(dat)
    }
    
    nodeData(g,
             n = as.character(officer$officer_id), 
             attr = cols[i]) <- dat

}


## -----------------------------------------------------------------
## Calculate Edges
edgelist <- calc_network(complaint, group = "crid", indicator = "officer_id") 
colnames(edgelist) <- c("officer_id.1", "officer_id.2", "complaints")

edgelist <- head(edgelist)

g <- initEdgeAttribute(graph = g, attribute.name = 'complaints',
                       attribute.type='numeric',
                       default.value=0)


for (i in 1:nrow(edgelist)) {
    from <- as.character(edgelist$officer_id.1[i])
    to <- as.character(edgelist$officer_id.2[i])                        
    g <- addEdge(graph = g, from = from, to = to)
    edgeData(g, from, to, "complaints") <- edgelist$complaints[i]
}

##     g <- addEdge(graph = g,
##         from = as.character(edgelist$officer_id.1),
##         to = as.character(edgelist$officer_id.2))
## ##        weights = rep(1, nrow(edgelist)))

## -----------------------------------------------------------------
## Send to Cytoscape

cw <- CytoscapeWindow("officers3",
                      graph = g,
                      overwrite=TRUE)
cw@graph <- g
displayGraph(cw) 

edgeNames(g)



g <- graph::addEdge ('A', 'B', g)
g <- graph::addEdge ('B', 'C', g)
g <- graph::addEdge ('C', 'A', g)

edgeData (g, 'A', 'B', 'edgeType') <- 'phosphorylates'
edgeData (g, 'B', 'C', 'edgeType') <- 'promotes'
edgeData (g, 'C', 'A', 'edgeType') <- 'indirectly activates'
    cw@graph <- g
    displayGraph (cw)





edgeData (g, 'A',
edgeData (g, 'B',
edgeData (g, 'C',
    cw@graph <- g
    displayGraph (cw)
