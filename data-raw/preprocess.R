##############
## Packages ##
##############
library(tidyverse)
options(stringsAsFactors = FALSE)

######################
## Read in the data ##
######################
complaintFile <- "toy.complaint_data.csv"
officerFile <- "toy.officer_data.csv"

complaint <- read.csv(complaintFile) %>%
    filter(!is.na(complaintcategory)) %>%
    separate(col = incident_date,
             into = c("incident_date", "incident_time"), sep = '  ') %>%
    separate(col = complaintcategory,
             into = c("complaint_category", "complaint_name"),
             sep = '-', extra = "merge") %>%
    rename(final_finding = finalfinding) %>%
    select(crid, officer_id, incident_date, incident_time,
           beat_2012_geocoded, complaint_category, complaint_name,
           final_finding)
colnames(complaint) <- gsub('.', '_', colnames(complaint), fixed = TRUE)
fname <- file.path("..", "data", "toy.complaint_data_cleaned.csv")
write.csv(officers, file = fname)
head(complaint)

officers <- read.csv(officerFile) %>%
    select(-matches("^X")) %>%
    select(officer_id, first.name, last.name,
           appointed.date, race, gender, birth.year, age,
           rank, primary, secondary, tertiary)
colnames(officers) <- gsub('.', '_', colnames(officers), fixed = TRUE)
fname <- file.path("..", "data", "toy.officer_data_cleaned.csv")
write.csv(officers, file = fname)

