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
