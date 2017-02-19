######################################################
################### read in data #####################
######################################################
setwd("E:/Yale/year3/semester2/datahack")
library(GGally)
library(pscl)
complaints <- read.csv("toy.complaint_data_cleaned.csv", header = TRUE)
officer <- read.csv("toy.officer_data.csv", header = TRUE)
officer_ID <- officer$officer_id[!duplicated(officer$officer_id)]
table(officer$primary)
table(officer$secondary)
table(officer$tertiary)


#######################################################
#### Defining the weighting metric of effect B -> A ###
#######################################################
# We have N = 3884 officers in total.
# Creating an N*N matrix Effect, (i,j) represent effect of i on j.
# We have Rob matrix. (off1, off2, occur)
N <- length(officer_ID)
Effect <- matrix(0, ncol = N, nrow = N)
colnames(Effect) <- officer_ID
rownames(Effect) <- officer_ID
#diag(Effect) <- 1
#Rob <- read.csv("co_occurence_complaints.csv", header = TRUE)
colnames(Rob) <- c("off1", "off2", "occur")
#Rob <- Rob[which(Rob$off1 != Rob$off2),]
for (j in 1:N){
  temp <- Rob[Rob$off2 == officer_ID[j],]
  m <- dim(temp)[1]
  Nj = sum(temp$occur, na.rm = TRUE)
  for(i in 1:m){
    index_i <- which(officer_ID == temp$off1[i])
    Nij <- temp$occur[i]
    Effect[index_i,j] = Nij/Nj
  }
}
officer_events <- function(X, IDlist){
  ret <- rep(0, length(IDlist))
  for(i in 1:length(IDlist)){
    ret[i] <- dim(X[X$officer_id == IDlist[i],])
  }
  return(ret)
}

hist(effect_score_0913)
a <- which(Effect!=0)
hist(Effect[which(Effect!=0 & Effect!=1)], breaks = 40)
#save(Effect, file = "Effect.Rdata")


#######################################################
############ check the effect of time on outcome ######
#######################################################
# add new variables: day, month, year, time
time <- strsplit(as.character(complaints$incident_date[1]), "-| ")
time_matrix <- matrix(nrow = dim(complaints)[1], ncol = 3)
colnames(time_matrix) <- c("day", "month", "year")
for(i in 1:dim(complaints)[1]){
  temp <- strsplit(as.character(complaints$incident_date[i]), "-| ")
  time_matrix[i,1] <- temp[[1]][1]
  time_matrix[i,2] <- temp[[1]][2]
  time_matrix[i,3] <- temp[[1]][3]
  #time_matrix[i,4] <- temp[[1]][5]
}
complaints <- cbind(complaints, time_matrix)
# overall pattern by day, year
# average 2.658471 officer for each case.

day <- complaints$day[!duplicated(complaints$day)]
month <-complaints$month[!duplicated(complaints$month)]
complaints$newmonth <- NULL
complaints$newmonth[complaints$month == "Jan"] <- "01"
complaints$newmonth[complaints$month == "Feb"] <- "02"
complaints$newmonth[complaints$month == "Mar"] <- "03"
complaints$newmonth[complaints$month == "Apr"] <- "04"
complaints$newmonth[complaints$month == "May"] <- "05"
complaints$newmonth[complaints$month == "Jun"] <- "06"
complaints$newmonth[complaints$month == "Jul"] <- "07"
complaints$newmonth[complaints$month == "Aug"] <- "08"
complaints$newmonth[complaints$month == "Sep"] <- "09"
complaints$newmonth[complaints$month == "Oct"] <- "10"
complaints$newmonth[complaints$month == "Nov"] <- "11"
complaints$newmonth[complaints$month == "Dec"] <- "12"
save(complaints, file = "complaints_cleaned_add_timecolumns.Rdata")
complaints$quarter <- NULL
complaints$quarter[complaints$newmonth == "01" | complaints$newmonth == "02" |
                     complaints$newmonth == "03"] <- "01"
complaints$quarter[complaints$newmonth == "04" | complaints$newmonth == "05" |
                     complaints$newmonth == "06"] <- "02"
complaints$quarter[complaints$newmonth == "07" | complaints$newmonth == "08" |
                     complaints$newmonth == "09"] <- "03"
complaints$quarter[complaints$newmonth == "10" | complaints$newmonth == "11" |
                     complaints$newmonth == "12"] <- "04"
year <- seq(2009, 2016, 1)
quarter <- c("01", "02", "03", "04")
#complaints$dmy <- paste0(complaints$year, complaints$newmonth, complaints$day)
#complaints$dm <- paste0(complaints$year, complaints$newmonth)
complaints_time <- complaints[!duplicated(complaints$crid),]
num_year <- rep(0, length(year))
for(i in 1:length(num_year)){
  num_year[i] <- dim(complaints_time[complaints_time$year == year[i],])[1]
}
plot(as.numeric(as.character(year)), num_year, type = "b", main = "complaints by year", xlab = "year", ylab = "# of complaints")
num_quarter <- matrix(nrow = length(year), ncol = 4)
rownames(num_quarter) <- year
colnames(num_quarter) <- paste0(rep("quarter", 4), seq(1,4,1))
for(i in 1:length(year)){
  for (j in 1:4){
    num_quarter[i,j] <- dim(complaints_time[which(complaints_time$year == year[i] & complaints_time$quarter == quarter[j]),])[1]
  }
}
matplot(t(num_quarter[1:7,]), type = "b", main = "complaints by quarter", xlab = "quarter", ylab = "# of complaints")
legend("topright", rownames(num_quarter)[1:7], pch = 1)

num_month <-num_quarter <- matrix(nrow = length(year), ncol = 12)
rownames(num_month) <- year
colnames(num_month) <- paste0(rep("month", 12), seq(1,12,1))
month <- paste0(c(rep("0", 9), rep("1",3)), c(seq(1,9,1),0,1,2))
for(i in 1:length(year)){
  for (j in 1:12){
    num_month[i,j] <- dim(complaints_time[which(complaints_time$year == year[i] & complaints_time$newmonth == month[j]),])[1]
  }
}
matplot(t(num_month[1:7,]), type = "b", main = "complaints by month", xlab = "month", ylab = "# of complaints")


########################################################################
########################################################################
## we don't need to use time series count data regression.
## because there is no pattern with respect to month and time.
## do poisson regression model: 09-13 calculate effect score.
## use 2014 to train model
## do prediction on 2015
complaints0913 <- complaints[which(complaints$year == "2009" |complaints$year == "2010"|
                                     complaints$year == "2011" | complaints$year == "2012"|
                                     complaints$year == "2012"),]
Rob <- calc_network(complaints0913, group = "crid", indicator = "officer_id")
officer_ID <- complaints0913$officer_id[!duplicated(complaints0913$officer_id)]
officer_events <- function(X, IDlist){
  ret <- rep(0, length(IDlist))
  for(i in 1:length(IDlist)){
    ret[i] <- dim(X[X$officer_id == IDlist[i],])[1]
  }
  return(ret)
}
events0913 <- officer_events(complaints0913, officer_ID)
N <- length(officer_ID)
Effect <- matrix(0, ncol = N, nrow = N)
colnames(Effect) <- officer_ID
rownames(Effect) <- officer_ID
#diag(Effect) <- 1
#Rob <- read.csv("co_occurence_complaints.csv", header = TRUE)
colnames(Rob) <- c("off1", "off2", "occur")
#Rob <- Rob[which(Rob$off1 != Rob$off2),]
for (j in 1:N){
  temp <- Rob[Rob$off2 == officer_ID[j],]
  m <- dim(temp)[1]
  Nj = sum(temp$occur, na.rm = TRUE)
  for(i in 1:m){
    index_i <- which(officer_ID == temp$off1[i])
    Nij <- temp$occur[i]
    Effect[index_i,j] = Nij/Nj
  }
}
which(Effect[,1]!=0)
effect_score1_0913 <- rowSums(Effect) # sum up weights of edges
effect_score2_0913 <- Effect%*%matrix(events0913, nrow = length(events0913), ncol = 1)
hist(effect_score2_0913)
a <- Effect[,1]*events0913
save(effect_score2_0913, effect_score1_0913, file = "effect_score0913.Rdata")
score <- matrix(ncol = 5, nrow = length(officer_ID))
colnames(score) <- c("officer_id", "effect_score1", "effect_score2", "num_cmplts", "event14")
score <- as.data.frame(score)
score$officer_id <- officer_ID
score$effect_score1 <- effect_score1_0913
score$effect_score2 <- effect_score2_0913
score$num_cmplts <- events0913
complaints14 <- complaints[which(complaints$year == "2014"),]
event14 <- officer_events(complaints14, officer_ID)
score$event14 <- event14
save(score, file = "score0913.Rdata")
sum(event14)
hist(event14)

officer$year <- substring(officer$appointed.date,1,4)
model0914 <- merge(officer, score, by = "officer_id")

#########################
#keep <- c("event14","year", "race", "gender", "age", "rank", "effect_score2", "num_cmplts") 
#model0914 <- na.omit(model0914[, keep])
model0914$year <- as.numeric(model0914$year)
m1 <- zeroinfl(event14 ~ race + gender + age + effect_score2 + num_cmplts,
               data = model0914, 
               dist = "negbin", link = "logit", EM = TRUE)
summary(m1)
m2 <- zeroinfl(event14 ~ gender + age + effect_score2 + num_cmplts,
               data = model0914, 
               dist = "negbin", link = "logit", EM = TRUE)
summary(m2)

m3 <- zeroinfl(event14 ~ age + effect_score2 + num_cmplts,
               data = model0914, 
               dist = "negbin", link = "logit", EM = TRUE)
summary(m3)
plot(m3$residuals)

cor(model0914$age, model0914$year) #-0.8098733
cor(model0914$effect_score2, model0914$num_cmplts) #0.8753921

library(nnet)
m1 <- multinom(event14 ~ year + gender + age + effect_score2 + num_cmplts,
               data = model0914, family  = "poisson")
summary(m1)


#############################################################
###################### prediction ###########################
complaints15 <- complaints[which(complaints$year == "2015"),]
event15 <- officer_events(complaints15, officer_ID)
hist(event15)
predict(m3, )
