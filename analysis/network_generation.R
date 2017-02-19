## RCy3
library(RCy3)

library(tidyverse)
source("R/calc_network.R")

complaint <- read_csv("data/toy.complaint_data_cleaned.csv")[, -1]
officer <- read_csv("data/toy.officer_data_cleaned.csv")[, -1]

edgelist <- calc_network(complaint, group = "crid", indicator = "officer_id")
colnames(edgelist) <- c("officer_id.1", "officer_id.2", "complaints")


g <- new("graphNEL", edgemode = "directed")

initNodeAttribute
nodeData

initEdgeAttribute
edgeData
