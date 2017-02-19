######################################################
################### read in data #####################
######################################################
setwd("E:/Yale/year3/semester2/datahack")
library(GGally)
library(pscl)
#complaints <- read.csv("toy.complaint_data_cleaned.csv", header = TRUE)
load("E:/Yale/year3/semester2/datahack/complaints_cleaned_add_timecolumns.Rdata")
officer <- read.csv("toy.officer_data.csv", header = TRUE)
# officer_ID <- officer$officer_id[!duplicated(officer$officer_id)]
# table(officer$primary)
# table(officer$secondary)
# table(officer$tertiary)


#######################################################
#### Defining the weighting metric of effect B -> A ###
#######################################################
# We have N = 3884 officers in total.
# Creating an N*N matrix Effect, (i,j) represent effect of i on j.
# We have Rob matrix. (off1, off2, occur)
# effect_score: a function to calculate effect adjacency matrix from start(year) to end(year)
# X: the concurrence matrix given by Rob
effect <- function(X){
  temp_ID1 <- X$off1[!duplicated(X$off1)]
  temp_ID2 <- X$off2[!duplicated(X$off2)]
  temp_ID <- c(temp_ID1, temp_ID2)
  temp_ID <- temp_ID[!duplicated(temp_ID)]
  temp_N <- length(temp_ID)
  Effect <- matrix(0, ncol = temp_N, nrow = temp_N)
  colnames(Effect) <- temp_ID 
  rownames(Effect) <- temp_ID 
  for (j in 1:temp_N){
    temp <- X[X$off2 == temp_ID[j],]
    m <- dim(temp)[1]
    Nj = sum(temp$occur, na.rm = TRUE)
    for(i in 1:m){
      index_i <- which(temp_ID == temp$off1[i])
      Nij <- temp$occur[i]
      Effect[index_i,j] = Nij/Nj
    }
  }
  colnames(Effect) <- temp_ID
  rownames(Effect) <- temp_ID
  return(Effect)
}

#diag(Effect) <- 1
#Rob <- read.csv("co_occurence_complaints.csv", header = TRUE)
colnames(Rob) <- c("off1", "off2", "occur")
#Rob <- Rob[which(Rob$off1 != Rob$off2),]

# The total events
# X is complaints 
officer_events <- function(X, IDlist){
  ret <- rep(0, length(IDlist))
  for(i in 1:length(IDlist)){
    ret[i] <- dim(X[X$officer_id == IDlist[i],])[1]
  }
  return(ret)
}

hist(effect_score_0913)
a <- which(Effect!=0)
hist(Effect[which(Effect!=0 & Effect!=1)], breaks = 40)
#save(Effect, file = "Effect.Rdata")




########################################################################
########################################################################
## we don't need to use time series count data regression.
## because there is no pattern with respect to month and time.
## do poisson regression model: 09-13 calculate effect score.
## use 2014 to train model
## do prediction on 2015
complaints0913 <- complaints[which(complaints$year == "2009" |complaints$year == "2010"|
                                     complaints$year == "2011" | complaints$year == "2012"|
                                     complaints$year == "2013"),]
Rob0913 <- calc_network(complaints0913, group = "crid", indicator = "officer_id")
colnames(Rob0913) <- c("off1", "off2", "occur")
Effect0913 <- effect(Rob0913)
events0913 <- officer_events(complaints0913, colnames(Effect0913))

#N <- length(officer_ID)

effect_score1_0913 <- rowSums(Effect0913) # sum up weights of edges
effect_score2_0913 <- Effect0913%*%matrix(events0913, nrow = length(events0913), ncol = 1)
hist(effect_score2_0913)
#save(effect_score2_0913, effect_score1_0913, file = "effect_score0913.Rdata")
score <- matrix(ncol = 5, nrow = length(colnames(Effect0913)))
colnames(score) <- c("officer_id", "effect_score1", "effect_score2", "num_cmplts", "outcome")
score <- as.data.frame(score)
score$officer_id <- colnames(Effect0913)
score$effect_score1 <- effect_score1_0913
score$effect_score2 <- effect_score2_0913
score$num_cmplts <- events0913
complaints14 <- complaints[which(complaints$year == "2014"),]
Rob14 <- calc_network(complaints0913, group = "crid", indicator = "officer_id")
colnames(Rob14) <- c("off1", "off2", "occur")
event14 <- officer_events(complaints14, colnames(Effect0913))
score$outcome <- event14
# save(score, file = "score0913.Rdata")
# sum(event14)
# hist(event14)

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

m3 <- zeroinfl(outcome ~ age + effect_score2 + num_cmplts,
               data = model0914, 
               dist = "negbin", link = "logit", EM = TRUE)
summary(m3)
plot(m3$residuals)




cor(model0914$age, model0914$year) #-0.8098733
cor(model0914$effect_score2, model0914$num_cmplts) #0.8753921




#############################################################
###################### prediction ###########################
complaints15 <- complaints[which(complaints$year == "2015"),]
event15 <- officer_events(complaints15, colnames(Effect0913))
complaints1014 <- complaints[which(complaints$year == "2014" |complaints$year == "2010"|
                                     complaints$year == "2011" | complaints$year == "2012"|
                                     complaints$year == "2013"),]
Rob1014 <- calc_network(complaints1014, group = "crid", indicator = "officer_id")
colnames(Rob1014) <- c("off1", "off2", "occur")
Effect1014 <- effect(Rob1014)
events1014 <- officer_events(complaints1014, colnames(Effect1014))
effect_score1_1014 <- rowSums(Effect1014) # sum up weights of edges
effect_score2_1014 <- Effect1014%*%matrix(events1014, nrow = length(events1014), ncol = 1)
hist(effect_score2_0913)
#save(effect_score2_0913, effect_score1_0913, file = "effect_score0913.Rdata")
score <- matrix(ncol = 5, nrow = length(colnames(Effect0913)))
colnames(score) <- c("officer_id", "effect_score1", "effect_score2", "num_cmplts", "outcome")
score <- as.data.frame(score)
score$officer_id <- colnames(Effect0913)
score$effect_score1 <- effect_score1_0913
score$effect_score2 <- effect_score2_0913
score$num_cmplts <- events1014
score$outcome <- event15



officer$year <- substring(officer$appointed.date,1,4)
modelpred <- merge(officer, score, by = "officer_id")
modelpred$year <- as.numeric(modelpred$year)

pred15 <- predict(m3, modelpred, se.fit = FALSE, conf = 0.95,
              MC = 1000, type = "response",
              na.action = na.pass)
plot(modelpred$outcome, pred15)
a <- table(modelpred$outcome, round(pred15))
sum(diag(a))/sum(a)
IDlist <- colnames(Effect0913)
save(IDlist, pred15, event14, event15, modelpred, file = "prediction.Rdata")
## check which police man have no event on 14 but have event on 15
ID_noevent14 <- IDlist[which(event14 == 0)]
ID_event15 <- IDlist[which(event15 != 0)]
ID_interest <- intersect(ID_noevent14, ID_event15)
index <- which(IDlist%in%ID_interest)
pred15[index]
a <- round(pred15[index])
length(a[a!=0])/length(a)
ID_predict_correct <- ID_interest[which(a!=0)]
ID_not_predict <- ID_interest[which(a==0)]
length(ID_predict_correct)
length(ID_not_predict)
length(ID_interest)
save(ID_interest, ID_predict_correct, ID_not_predict, file = "ID.Rdata")
names(dat_predict)
hist(event15)
predict(m3, )
#821 1073
pred15[which(IDlist == "821")]
pred15[which(IDlist == "1073")]
modelpred$outcome[which(IDlist == "821")]
modelpred$outcome[which(IDlist == "1073")]


hist(event15, main = "The number of Complaints per Officer in 2015", xlab = "# of complaints")
