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
complaintFile <- file.path(dataDir, "example.complaint_data.csv")
officerFile <- file.path(dataDir, "example.officer_data.csv")

complaints <- read.csv(complaintFile, row.names = 1)
#%>%    separate(incident_date, into = c("incident_date", "incident_time"), sep = ' ')
head(complaints)

officers <- read.csv(officerFile, row.names = 1)

officerFile <- file.path(dataDir, "Master Database 12-7-15 vF_officers.csv")
allegationsFile <- file.path(dataDir, "Master Database 12-7-15 vF_allegations.csv")

officers <- read.csv(officerFile) %>%
    select(-matches("^X"))
allegations <- read.csv(allegationsFile) %>%
    select(-matches("^X"))

######################
## Explore the data ##
######################

## Distribution of complaints by officer
allegations %>%

