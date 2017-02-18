## Calculate co-occurence of officers across complaints

library(tidyverse)
library(lineprof)
source("R/calc_network.R")

complaint <- read_csv("data/toy.complaint_data.csv")

net <- calc_network(complaint, group = "crid", indicator = "officer_id")

colnames(net) <- c("officer_id.1", "officer_id.2", "complaints")

write_csv(net, "co_occurence_complaints.csv")
