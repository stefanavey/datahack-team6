###################################################################################
## Explore the network!                                                          ##
###################################################################################

##############
## Packages ##
##############
library(tidyverse)
library(lubridate)
library(openxlsx)
library(aveytoolkit)
options(stringsAsFactors = FALSE)

######################
## Read in the data ##
######################
dataDir <- file.path("..", "data")
complaintFile <- file.path(dataDir, "toy.complaint_data_cleaned.csv")
officerFile <- file.path(dataDir, "toy.officer_data_cleaned.csv")

complaints <- read.csv(complaintFile, row.names = 1)
officers <- read.csv(officerFile)

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
    separate(incident_date, into = c("Day", "Month", "Year"), sep = '-') %>%
    group_by(officer_id, Year) %>%
    summarize(NumberOfComplaints = length(unique(crid))) %>%
    filter(Year == "2015") %>%
    full_join(officers[,"officer_id", drop =FALSE])
plotDat$NumberOfComplaints[is.na(plotDat$NumberOfComplaints)] <- 0

png(file.path("..", "graphs", "NumberOfComplaintsPerOfficer_year=2015.png"))
ggplot(plotDat, aes(x = NumberOfComplaints)) +
    geom_histogram() +
    scale_y_log10() +
    annotation_logticks(sides = "l") +
    ylab("Number of Officers") +
    xlab("Number of Complaints") +
    ggtitle("Number of Complaints per Officer in 2015") +
    getBaseTheme()
dev.off()

## How many officers have at least 1 complaint in each year
complaints <- complaints %>%
    mutate(incident_date = as.Date(incident_date, "%d-%B-%Y"))

## What is the date range
range(complaints$incident_date)
## "2009-01-01" to "2016-06-16"

## What is the distribution of number of complaints over the years?
plotDat <- complaints %>%
    separate(incident_date, into = c("Year", "Month", "Day"), sep = '-') %>%
    ## filter(incident_date < "2010-01-01") %>%
    group_by(Year, officer_id) %>%
    summarize(numComplaints = sum(length(unique(crid)))) %>%
    ## full_join(officers[,"officer_id",drop=FALSE]) %>%
    mutate(numComplaints = ifelse(is.na(numComplaints), 0, numComplaints))

ggplot(plotDat, aes(x = numComplaints)) +
    geom_histogram() +
    facet_grid(Year ~ .)

## What are the top 20 complaint categories?
plotDat <- complaints %>%
    group_by(complaint_name) %>%
    summarize(count = n()) %>%
    top_n(15, count) %>%
    arrange(count) %>%
    mutate(complaint_name = factor(complaint_name, levels = unique(complaint_name)))

png(file.path("..", "graphs", "ComplaintCategories_top=15.png"))
ggplot(plotDat, aes(x = complaint_name, y = count)) +
    xlab("Complaint Name") +
    ylab("Number of Complaints") +
    geom_bar(stat = "identity") +
    coord_flip()
dev.off()
