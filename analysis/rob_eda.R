## eda rob
library(tidyverse)
source("R/calc_network.R")

complaint <- read_csv("data/toy.complaint_data_cleaned.csv")[, -1]
officer <- read_csv("data/toy.officer_data_cleaned.csv")[, -1]

edgelist <- calc_network(complaint, group = "crid", indicator = "officer_id")
colnames(edgelist) <- c("officer_id.1", "officer_id.2", "complaints")


complaints_per_officer <- complaint %>%
    group_by(officer_id) %>%
    count(crid) %>%
    summarise(complaints_per_officer = sum(n))

num_unique_partners <- edgelist %>%
    group_by(officer_id.1) %>%
    summarise(num_unique_partners = n_distinct(officer_id.2)) %>%
    rename(officer_id = officer_id.1)

tmp <- inner_join(complaints_per_officer, num_unique_partners, by = "officer_id") %>%
    group_by(complaints_per_officer, num_unique_partners) %>%
    count


## Plot showing influencers
ggplot(arrange(tmp, n),
       aes(x = num_unique_partners,
           y = complaints_per_officer,
           size = log10(n), colour = log10(n))) +
    geom_point() +
    theme_bw() +
    xlab("Number of Unique Co-Complaintees") +
    ylab("Total Complaints per Officer") +
    scale_colour_distiller(palette = "RdBu", direction = -1,
                           name = "Frequency",
                           breaks = c(0, 1, 2),
                           labels = c(1, 10, 100)) +
    scale_size_continuous(name = "Frequency",
                          breaks = c(0, 1, 2),
                          labels = c(1, 10, 100))
    
ggsave("graphs/num-unique-complaintees_v_total-complaints-per-officer.pdf")
           
