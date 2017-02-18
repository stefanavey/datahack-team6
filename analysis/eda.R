###################################################################################
## Explore the network!                                                          ##
###################################################################################

##############
## Packages ##
##############
library(tidyverse)
library(openxlsx)
options(stringsAsFactors = FALSE)

######################
## Read in the data ##
######################
dataDir <- file.path("..", "data")
complaintFile <- file.path(dataDir, "toy.complaint_data.csv")
officerFile <- file.path(dataDir, "toy.officer_data.csv")

complaints <- read.csv(complaintFile) %>%
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
colnames(complaints) <- gsub('.', '_', colnames(complaints), fixed = TRUE)
head(complaints)

officers <- read.csv(officerFile) %>%
    select(-matches("^X")) %>%
    select(officer_id, first.name, last.name,
           appointed.date, race, gender, birth.year, age,
           rank, primary, secondary, tertiary)
colnames(officers) <- gsub('.', '_', colnames(officers), fixed = TRUE)
fname <- file.path("..", "data-raw", "toy.officer_data_cleaned.csv")
write.csv(officers, file = fname)

## allegationsFile <- file.path(dataDir, "Master Database 12-7-15 vF_allegations.csv")
## allegations <- read.csv(allegationsFile) %>%
##     select(-matches("^X")) %>%
##     select(-value, -recc_finding, -recc_outcome,
##            -final_finding, -final_outcome) %>%
##     mutate(incident_date = ifelse(incident_date == "", " ", incident_date)) %>%
##     separate(col = incident_date, into = c("incident_date", "incident_time"),
##              sep = ' ')

##########################
## Explore the officers ##
##########################
## 3,884 officers
dim(officers)
head(officers)
summary(officers)

## There is 1 88-year old LT but most are between 30 and 60
ggplot(officers, aes(x = age)) +
    geom_histogram()

#############################
## Explore the complaints ##
#############################
## 13,840 complaints
dim(complaints)
summary(complaints)

## Distribution of complaints by officer
plotDat <- complaints %>%
    filter(!is.na(officer_id)) %>%
    group_by(officer_id) %>%
    summarize(NumberOfComplaints = length(unique(crid)))
ggplot(plotDat, aes(x = NumberOfComplaints)) +
    geom_histogram() +
    ylab("Number of Officers") +
    xlab("Number of Complaints")

## What is the difference between result and outcome_edit?
complaints %>%
    select(crid, officer_id, result, outcome_edit) %>%
    filter(result != outcome_edit) %>%
    tail(30)

## Distribution of complaints by date
