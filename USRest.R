datB <- readRDS("datB.rds") # 61k obs
datT <- readRDS("datT.rds") # 495k obs
datR <- readRDS("datR.rds") # 1,569k obs
datU <- readRDS("datU.rds") # 366k

## US ALL
# Subset by City = ALL

datB_US <- subset(datB, state == "AZ" | state == "IL" | state == "MA" | state == "NC" | state == "NV" | state == "OR" | state == "PA" | state == "SC" | state == "WA") # 50k obs
datT_US <- subset(datT, business_id %in% datB_US$business_id) # 474k obs
datR_US <- subset(datR, business_id %in% datB_US$business_id) # 1,446k obs
datU_US <- subset(datU, user_id %in% datR_US$user_id) # 340k

saveRDS(datB_US, file ="datB_US.rds")  
saveRDS(datT_US, file ="datT_US.rds")  
saveRDS(datR_US, file ="datR_US.rds")  
saveRDS(datU_US, file ="datU_US.rds")  

# Selects users with fans
fanscount <- subset(datU_US, fans >= 0)

# Select all users 
datRU_US <- subset(datR_US, user_id %in% fanscount$user_id) # 219k obs

# add User info
datRU_US <- merge(datRU_US, fanscount, by = "user_id", all.x = TRUE)

# add business info
datRUB_US <- merge(datRU_US, datB_US, by = "business_id", all.x = TRUE)

rm(datRU_US)
rm(fanscount)
rm(datB_US)
rm(datR_US)
rm(datT_US)
rm(datU_US)

# Remove irrelevant columns
myvars <- names(datRUB_US) %in% c("yelping_since", "friends", "type.y",
                                  "compliments", "elite", "full_address",
                                  "city", "neighborhoods", "longitude",
                                  "state", "latitude", "type.x", "name.x",
                                  "open", "type", "stars.y", "review_count.y",
                                  "votes.y")
datRUB_US <- datRUB_US[!myvars]

library(jsonlite)

# Calculates Hours on Weekdays and on Weekends
hoursW <- datRUB_US$hours
hoursW <- flatten(hoursW)

hoursW$Monday.close <- strptime(hoursW$Monday.close, format="%H:%M")
hoursW$Monday.open <- strptime(hoursW$Monday.open, format="%H:%M")
hoursW$Tuesday.close <- strptime(hoursW$Tuesday.close, format="%H:%M")
hoursW$Tuesday.open <- strptime(hoursW$Tuesday.open, format="%H:%M")
hoursW$Wednesday.close <- strptime(hoursW$Wednesday.close, format="%H:%M")
hoursW$Wednesday.open <- strptime(hoursW$Wednesday.open, format="%H:%M")
hoursW$Thursday.close <- strptime(hoursW$Thursday.close, format="%H:%M")
hoursW$Thursday.open <- strptime(hoursW$Thursday.open, format="%H:%M")
hoursW$Friday.close <- strptime(hoursW$Friday.close, format="%H:%M")
hoursW$Friday.open <- strptime(hoursW$Friday.open, format="%H:%M")

hoursW$WKD <- abs(hoursW$Monday.open - hoursW$Monday.close)/60/60 +
  abs(hoursW$Tuesday.open - hoursW$Tuesday.close)/60/60 +
  abs(hoursW$Wednesday.open - hoursW$Wednesday.close)/60/60 +
  abs(hoursW$Thursday.open - hoursW$Thursday.close)/60/60 +
  abs(hoursW$Friday.open - hoursW$Friday.close)/60/60

hoursW$WKD[is.na(hoursW$WKD)] <- 0

hoursW$Saturday.close <- strptime(hoursW$Saturday.close, format="%H:%M")
hoursW$Saturday.open <- strptime(hoursW$Saturday.open, format="%H:%M")
hoursW$Sunday.close <- strptime(hoursW$Sunday.close, format="%H:%M")
hoursW$Sunday.open <- strptime(hoursW$Sunday.open, format="%H:%M")

hoursW$WKND <- abs(hoursW$Sunday.open - hoursW$Sunday.close)/60/60 +
  abs(hoursW$Saturday.open - hoursW$Saturday.close)/60/60

hoursW$WKND[is.na(hoursW$WKND)] <- 0

datRUB_US$WKND <- hoursW$WKND
datRUB_US$WKD <- hoursW$WKD

# Remove irrelevant columns
myvars <- names(datRUB_US) %in% c("hours")
datRUB_US <- datRUB_US[!myvars]

# Characterizes categories for Restaurants (c for categ) #909k
datRUB_US$categ <- grepl ( "^(.*[Rr]estaurant.*)",  datRUB_US$categories)
datRUBc_US <- subset(datRUB_US, datRUB_US$categ == TRUE)  

datRUBcu_US <- datRUBc_US

rm(datRUBc_US)
rm(datRUB_US)
rm(datB)
rm(datR)
rm(datT)
rm(datU)

# Flattens 'attributes' and select variables for restaurant
attrib <- datRUBcu_US$attributes
attrib <- flatten(attrib)
# Remove irrelevant columns
myvars <- names(attrib) %in% c("Accepts Credit Cards", 
                               "Good For Groups", "Outdoor Seating", "Price Range",  
                               "Good for Kids", "Alcohol",  "Noise Level", "Has TV",  
                               "Attire", "Good For Dancing", "Delivery", "Coat Check",  
                               "Smoking", "Take-out",  "Takes Reservations",   "Waiter Service",  
                               "Wi-Fi",  "Caters",  "Drive-Thru",  "Wheelchair Accessible",  
                               "BYOB", "Corkage",  "BYOB/Corkage",  
                               "Order at Counter", "Good For Kids",  "Dogs Allowed",  
                               "Open 24 Hours", "Parking.garage", "Parking.street",  
                               "Parking.validated", "Parking.lot",  "Parking.valet")
attrib <- attrib[myvars]

datRUBcua_US <- cbind(datRUBcu_US, attrib)

rm(datRUBcu_US)
rm(attrib)

myvars <- names(datRUBcua_US) %in% c("attributes", "categories")
datRUBcua_US <- datRUBcua_US[!myvars]


# ##############################################################################
# # Sentiment analysis for 'text'  - O M M I T T E D
# #  


datRUBcuas_US_full <- datRUBcua_US

datRUBcuas_US_full <- readRDS("datRUBcuas_US_full.rds")

rm(datRUBcua_US)

# File for modeling and test
set.seed(123)
index <- sample(1:nrow(datRUBcuas_US_full), 500)
# index

testfile <- datRUBcuas_US_full[index, ]
forModel3cfUS <- datRUBcuas_US_full[-index, ]

# Remove irrelevant columns
myvars <- names(testfile) %in% c("business_id", "user_id", "review_id",
                                 "date", "text", "review_count.x",
                                 "name.y", "text", "categ", "Accepts Credit Cards")
testfile <- testfile[!myvars]

myvars <- names(forModel3cfUS) %in% c("business_id", "user_id", "review_id",
                                      "date", "text", "review_count.x",
                                      "name.y", "text", "categ", "Accepts Credit Cards")
forModel3cfUS <- forModel3cfUS[!myvars]

# saveRDS(datRUBcuas_US_full, file = "datRUBcuas_US_full.rds")
# 
# datRUBcuas_US_full <- readRDS("datRUBcuas_US_full.rds")


##############################################################################
##############################################################################
## 3 Class - Model

## Data Preparation
##

# forModel3c <- forModel #testfile
forModel3cfUS$stars.x <- as.factor(forModel3cfUS$stars.x)

levels(forModel3cfUS$stars.x) <- c(levels(forModel3cfUS$stars.x), '123')

forModel3cfUS$stars.x[forModel3cfUS$stars.x == '1']  <- '123'
forModel3cfUS$stars.x[forModel3cfUS$stars.x == '2']  <- '123'
forModel3cfUS$stars.x[forModel3cfUS$stars.x == '3']  <- '123'

forModel3cfUS$stars.x <- droplevels(forModel3cfUS$stars.x)

library(caret)
library(randomForest)
library(parallel)
library(doParallel)
library(jsonlite)

trainingRaw <- forModel3cfUS
trainingRaw <- flatten(trainingRaw)

## Cleaning

# Convert classe to factor

trainingRaw$stars.x <- as.factor(trainingRaw$stars.x)
trainingRaw[6:36] <- lapply(trainingRaw[6:36], factor)

# Characterize NAs as "NoRes" to expand predictors to 41-1
NoRes <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "NoRes")))
  return(x)
}

trainingRaw[6:36] <- as.data.frame(lapply(trainingRaw[6:36], NoRes))
trainingRaw[6:36][is.na(trainingRaw[6:36])] <- "NoRes"


trainingRaw$WKND <- as.numeric(trainingRaw$WKND)
trainingRaw$WKD <- as.numeric(trainingRaw$WKD)

str(trainingRaw)

# Set train-test files

set.seed(123456)
fortrain <- createDataPartition(trainingRaw$stars.x, p = 0.8, list=FALSE)
training <- trainingRaw[fortrain,]
validation <- trainingRaw[-fortrain,]


rm(datRUBcuas_US_full)
rm(forModel3cfUS)
rm(hoursW)
rm(trainingRaw)
rm(fortrain)


RocImpUS <- filterVarImp(x = training[, -ncol(training)], y = training$stars.x)
head(RocImpUS)

saveRDS(RocImpUS, file = "RocImpUS.rds")
