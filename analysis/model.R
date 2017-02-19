## Work on alternative model spec

library(tidyverse)
source("R/calc_network.R")

complaint <- read_csv("data/toy.complaint_data_cleaned.csv")[, -1]
officer <- read_csv("data/toy.officer_data_cleaned.csv")[, -1]

edgelist <- calc_network(complaint, group = "crid", indicator = "officer_id")
colnames(edgelist) <- c("officer_id.1", "officer_id.2", "complaints")


## Craft officer centric dataset

node <- officer %>%
    arrange(officer_id) %>%
    select(-first_name, -last_name, -birth_year,
           -primary, -secondary, -tertiary)

calc_effect_score <- function(edgelist) {


}

