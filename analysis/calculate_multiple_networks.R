###################################################################################
## Calculate multiple edgelists by year                                          ##
###################################################################################

library(tidyverse)
source("../R/calc_network.R")

## Read in munged datasets
complaint <- read_csv("../data/toy.complaint_data_cleaned.csv")[, -1] %>%
    arrange(officer_id) %>%
    separate(incident_date, into = c("Day", "Month", "Year"), sep = '-') %>%
    arrange(Year) %>%
    group_by(Year) %>%
    select(crid, officer_id) %>%
    nest %>%
    mutate(
        edgelist = map(data, calc_network,
                       group = "crid",
                       indicator = "officer_id")
    )

for(i in 1:nrow(complaint)) {
    write_csv(complaint[[i,"edgelist"]],
              path = paste0("../data/", complaint[i,"Year"], "_edgelist.csv"))
}

tmp <- complaint %>%
    select(Year, edgelist) %>%
    unnest %>%
    spread(Year, n)

## tmp2 <- tmp[rowSums(!is.na(tmp)) > 4, ]
tmp[is.na(tmp)] <- 0

for(i in 3:ncol(tmp)) {
    dat <- tmp
    dat$count <- rowSums(tmp[,3:i])
    write_csv(dat[,c("X1", "X2", "count")],
              path = paste0("../data/", colnames(tmp)[i], "_edgelist.csv"))
}



complaintFiltered <- complaint %>%
    filter(
edgelist <- calc_network(complaint %>% head, group = "crid", indicator = "officer_id")
colnames(edgelist) <- c("officer_id.1", "officer_id.2", "complaints")
