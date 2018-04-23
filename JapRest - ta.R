library(tidyverse)
library(plyr)
#same day reservations
#next day reservations
# all other reservations
# x hours in advance reservations


setwd ("C:\\Users\\TomAlig06.15\\Documents\\Kaggle\\Japanese Restaurant")

# read in data
air.reserve <- read.csv (file.path("air_reserve.csv"))
head(air.reserve)
dim(air.reserve)
class(air.reserve)
anyNA(air.reserve)
str(air.reserve)
summary(air.reserve)

air.reserve2 <- air.reserve

# Get visit_datetime and reserve_datetime into date/time format
air.reserve.time.diff <- strptime( paste(air.reserve2[,2], air.reserve2[,3]), "%Y-%m-%d %H:%M:%S")
# Convert datetime to numeric, and take visit_datetime less reserve_datetime
# This gets us the time lag (in seconds) from when the reservation was made until
#     when the diner arrived at the restaurant. (We know how far in advance the
#     reservation was made)
air.reserve.time.diff2 <- as.numeric(difftime(strptime(paste(air.reserve2[,2]),"%Y-%m-%d %H:%M:%S"),
                    strptime(paste(air.reserve2[,3]),"%Y-%m-%d %H:%M:%S"))) 

head(air.reserve.time.diff2)

# Convert the seconds to number of hours in advance the reservation was made
air.reserv.advance <- air.reserve.time.diff2/(60*60)
head(air.reserv.advance)

air.reserve3 <- air.reserve

# Add air.reserve.advance as column to air.reserve3
air.reserve3$reserve_advance_hours <- air.reserv.advance
head(air.reserve3)

hist(air.reserve3$reserve_advance_hours)
quantile(air.reserve3$reserve_advance_hours, na.rm = TRUE)
boxplot(air.reserve3$reserve_advance_hours, na.rm = TRUE)
quantile(air.reserve3$reserve_advance_hours,c(.01,.25,.5,.75,.95,.99, 1), na.rm=TRUE)

# To visualize the bulk of the data in a meaningful way, will create new variable
#     to omit the highest 5% of values

air.reserve4 <- air.reserve3
air.reserve.95 <- air.reserve4[air.reserve4$reserve_advance_hours < 
               quantile(air.reserve4$reserve_advance_hours, na.rm = TRUE, 0.95), ]

hist(air.reserve.95$reserve_advance_hours)
quantile(air.reserve.95$reserve_advance_hours, na.rm = TRUE)
boxplot(air.reserve.95$reserve_advance_hours, na.rm = TRUE)
dim(air.reserve.95)
dim(air.reserve)

# Find restaurants w highest frequency, and include the frequency
air.reserve.freq.list<-table(air.reserve4$air_store_id)
head(air.reserve.freq.list)
air.reserve.sorted.freq.list<-sort(air.reserve.freq.list, decreasing=TRUE)
head(air.reserve.sorted.freq.list)
air.reserve.sorted.freq.list[221:222]

x = count(air.reserve4, c('air_store_id', 'reserve_advance_hours'))
head(x)
tail(x)

visitorcount <- count(air.reserve4$reserve_visitors)
head(visitorcount)

sum(air.reserve4$reserve_visitors)
# 414,015 total number of visitors from air.reserve

walkin <- air.reserve4[which(air.reserve4$reserve_advance_hours=='0'),]
head(walkin)
dim(walkin)
dim(air.reserve4)
6447/92738 # 7.0% of all reservations on air.reserve were not reservations but were walkins

sum(walkin$reserve_visitors)
20148/414015  # 4.9% of all visitors were from walkin reservations on air.reserve

# The % of walkins and visitors is telling me to focus on non-walkins

noshow <- air.reserve4[which(air.reserve4$reserve_visitors=='0'),]
head(noshow)
dim(noshow)
dim(air.reserve4)
0/92738 # 0% of all reservations on air.reserve were no-shows

# How many reservations were for 1 person? For 2? Etc?
air.reserve5 <- air.reserve4
air.reserve5$visitor.cat <- cut(air.reserve5$reserve_visitors, 
                                breaks = c(0, 2.5, 4.5, 8.5, 12.5, 9999),
                                labels = c("1-2", "3-4", "5-8", "9-12", ">12"))
head(air.reserve5)
hist(air.reserve5$reserve_visitors)
quantile(air.reserve5$reserve_visitors, na.rm = TRUE)
boxplot(air.reserve5$reserve_visitors, na.rm = TRUE)

#hist(air.reserve5$visitor.cat)
#barplot(air.reserve5$visitor.cat)
#quantile(air.reserve5$visitor.cat, na.rm = TRUE)
#boxplot(air.reserve5$visitor.cat, na.rm = TRUE)


cc <- count(air.reserve5, 'visitor.cat')
cc3 <- table(air.reserve5$visitor.cat)
barplot(cc3, xlab = "Number of Patrons", ylab = "Frequency", main = "Air Reserve")


# How many reservations by month? By number of people?
visit.date.ar <- format(as.POSIXct(strptime(air.reserve5$visit_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y/%m/%d")
head(visit.date.ar)

visit.year.ar <- format(as.POSIXct(strptime(air.reserve5$visit_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y")
head(visit.year.ar)

visit.month.ar <- format(as.POSIXct(strptime(air.reserve5$visit_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m")
head(visit.month.ar)


air.reserve6 <- air.reserve5
air.reserve6$visit.date <- visit.date.ar
head(air.reserve6)
air.reserve6$visit.year <- visit.year.ar
head(air.reserve6)
air.reserve6$visit.month <- visit.month.ar
head(air.reserve6)
dim(air.reserve6)

tail(air.reserve6)

# training data = 2016; 45,296 records
# validation data = 2017 47,081 records

air.reserve7<-air.reserve6[which(air.reserve6$visit.year=="2017"),]
dim(air.reserve7)


cv <- count(air.reserve5, 'visitor.cat')

# convert visit.date from character to as.date
air.reserve6$visit.date <- as.Date(air.reserve6$visit.date, "%Y/%m/%d")

head(air.reserve6)
str(air.reserve6)
summary(air.reserve6)




cv<-aggregate(air.reserve6[,"reserve_visitors"],list(air.reserve6$visit.date),sum)
head(cv)
class(cv)
anyNA(cv)

plot(cv, xlab="Date", ylab="Num of Patrons", 
     main="Air Reserve Patrons")

hist(cv$x, xlab = "Number of Patrons per Day", main = "Air Reserve Histogram")

# make data a time series
air.reserve6.ts<-ts(air.reserve6, start=c(2016.01))
head(air.reserve6.ts)
tail(air.reserve6.ts)
head(air.reserve6)


air.reserve6.ts[1:10,]

str(air.reserve6.ts)

plot.ts(air.reserve6.ts)


# plot number of visitors by day, air reserve
plot(air.reserve6$reserve_visitors~air.reserve6$visit.date, main="Number of Patrons per Visit")
range(air.reserve6$reserve_visitors)



# split data into 80% training and 20% validation
dim(air.reserve6) # 92,378 rows of data
ar.train <- (air.reserve6[1:73902,])
ar.valid <- (air.reserve6[73903:92378,])
  
  
tail(ar.train)


air.reserve %>% distinct(air_store_id) %>% nrow()


# merge air stores and hpg stores
air.store <- read.csv (file.path("air_store_info.csv"))
hpg.store <- read.csv (file.path("hpg_store_info.csv"))
combined.store <- rbind(air.store, hpg.store)

dim(air.store)
dim(hpg.store)

