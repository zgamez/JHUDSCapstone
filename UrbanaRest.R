## Urbana Champaign Exploratory
# Subset by City = Urbana

datB <- readRDS("datB.rds") # 61k obs
datT <- readRDS("datT.rds") # 495k obs
datR <- readRDS("datR.rds") # 1569k obs
datU <- readRDS("datU.rds") # 366k

datB_UC <- subset(datB, city == "Urbana") # 213 obs
datT_UC <- subset(datT, business_id %in% datB_UC$business_id) # 402 obs
datR_UC <- subset(datR, business_id %in% datB_UC$business_id) # 3518 obs
datU_UC <- subset(datU, user_id %in% datR_UC$user_id) # 1772

saveRDS(datB_UC, "datB_UC.rds")
saveRDS(datT_UC, "datT_UC.rds")
saveRDS(datR_UC, "datR_UC.rds")
saveRDS(datU_UC, "datU_UC.rds")

# Selects users with fans
fanscount <- subset(datU_UC, fans >= 0)
#fanscount[is.na(fanscount)] <- 0

# Select all users 
datRU_UC <- subset(datR_UC, user_id %in% fanscount$user_id) # 3518 obs

# add User info
datRU_UC <- merge(datRU_UC, fanscount, by = "user_id", all.x = TRUE)

# add business info
datRUB_UC <- merge(datRU_UC, datB_UC, by = "business_id", all.x = TRUE)

rm(datRU_UC)
rm(fanscount)
rm(datB_UC)
rm(datR_UC)
rm(datT_UC)
rm(datU_UC)

# Remove irrelevant columns
myvars <- names(datRUB_UC) %in% c("yelping_since", "friends", "type.y",
                                  "compliments", "elite", "full_address",
                                  "city", "neighborhoods", "longitude",
                                  "state", "latitude", "type.x", "name.x",
                                  "open", "type", "stars.y", "review_count.y",
                                  "votes.y")
datRUB_UC <- datRUB_UC[!myvars]

library(jsonlite)

# Calculates Hours on Weekdays and on Weekends
hoursW <- datRUB_UC$hours
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

datRUB_UC$WKND <- hoursW$WKND
datRUB_UC$WKD <- hoursW$WKD

# Remove irrelevant columns
myvars <- names(datRUB_UC) %in% c("hours")
datRUB_UC <- datRUB_UC[!myvars]

# Characterizes categories for Restaurants (c for categ) 2399
datRUB_UC$categ <- grepl ( "^(.*[Rr]estaurant.*)",  datRUB_UC$categories)
datRUBc_UC <- subset(datRUB_UC, datRUB_UC$categ == TRUE)  

# Subsets for votes useful
# datRUBcu_PHX <- subset(datRUBc_PHX, datRUBc_PHX$votes.x$useful > 0)
# rm(datRUBc_PHX)

datRUBcu_UC <- datRUBc_UC

rm(datRUBc_UC)

# Flattens 'attributes' and select variables for restaurant
attrib <- datRUBcu_UC$attributes
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

datRUBcua_UC <- cbind(datRUBcu_UC, attrib)

rm(datRUBcu_UC)
rm(attrib)

myvars <- names(datRUBcua_UC) %in% c("attributes", "categories")
datRUBcua_UC <- datRUBcua_UC[!myvars]


##############################################################################
# Sentiment analysis for 'text'
#  

library(sentiment)

fortext <- datRUBcua_UC

vectexto <- fortext$text

#texto <- vectexto[ !is.na( vectexto ) ]

some_txt <- vectexto

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)

# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"


# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)



saveRDS(sent, file = "sent.rds")
saveRDS(datRUBcua_UC, file = "datRUBcua_UC.rds")



datRUBcuas_UC_full <- cbind(datRUBcua_UC, sent_df)

saveRDS(datRUBcuas_UC_full, file = "datRUBcuas_UC-full.rds")
# datRUBcuas_UC_full <- readRDS("datRUBcuas_UC-full.rds")

#############################################################################

rm(class_pol)
rm(class_emo)
rm(datRUBcua_UC)
rm(fortext)
rm(sent_df)
rm(emotion)
rm(polarity)
rm(some_txt)
rm(vectexto)
rm(hoursW)


# File for modeling and test
set.seed(123)
index <- sample(1:nrow(datRUBcuas_UC_full), 50)
#index

testfile <- datRUBcuas_UC_full[index, ]
forModel3cf <- datRUBcuas_UC_full[-index, ]

# Remove irrelevant columns
myvars <- names(testfile) %in% c("business_id", "user_id", "review_id",
                                 "date", "text", "review_count.x",
                                 "name.y", "text", "categ", "Accepts Credit Cards")
testfile <- testfile[!myvars]

myvars <- names(forModel3cf) %in% c("business_id", "user_id", "review_id",
                                    "date", "text", "review_count.x",
                                    "name.y", "text", "categ", "Accepts Credit Cards")
forModel3cf <- forModel3cf[!myvars]

rm(datRUB_UC)
rm(datB_UC)
rm(datR_UC)
rm(datT_UC)
rm(datU_UC)

##############################################################################
##############################################################################
## 3 Class - Model

## Data Preparation
##

# forModel3c <- forModel #testfile
forModel3cf$stars.x <- as.factor(forModel3cf$stars.x)

levels(forModel3cf$stars.x) <- c(levels(forModel3cf$stars.x), "123")

forModel3cf$stars.x[forModel3cf$stars.x == '1']  <- '123'
forModel3cf$stars.x[forModel3cf$stars.x == '2']  <- '123'
forModel3cf$stars.x[forModel3cf$stars.x == '3']  <- '123'

forModel3cf$stars.x <- droplevels(forModel3cf$stars.x)

str(forModel3cf)

library(caret)
library(randomForest)
library(parallel)
library(doParallel)
library(jsonlite)

trainingRaw <- forModel3cf
trainingRaw <- flatten(trainingRaw)

## Cleaning

# Convert classe to factor

trainingRaw$stars.x <- as.factor(trainingRaw$stars.x)
trainingRaw[6:38] <- lapply(trainingRaw[6:38], factor)

str(trainingRaw)

# Characterize NAs as "NoRes" to expand predictors to 41-1
NoRes <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "NoRes")))
  return(x)
}
trainingRaw[6:38] <- as.data.frame(lapply(trainingRaw[6:38], NoRes))
trainingRaw[6:38][is.na(trainingRaw[6:38])] <- "NoRes"

trainingRaw$WKND <- as.numeric(trainingRaw$WKND)
trainingRaw$WKD <- as.numeric(trainingRaw$WKD)

str(trainingRaw)
str(trainingRaw$`Outdoor Seating`)

# Set train-test files

set.seed(123456)
fortrain <- createDataPartition(trainingRaw$stars.x, p = 0.8, list=FALSE)
training <- trainingRaw[fortrain,]
validation <- trainingRaw[-fortrain,]

rm(datRUBcuas_UC_full)
rm(forModel3cf)
rm(trainingRaw)



# Train model - Boosted Tree C5.0
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  
  modFitC503cUC <- train(stars.x ~., method="C5.0", data=training, 
                         trControl=trainControl(method="cv", 10,
                                                allowParallel=TRUE),
                         tuneGrid = expand.grid(model = "tree", winnow = FALSE,
                                                trials = c(1:10, (1:5)*10)))
)

stopCluster(cl)

save(modFitC503cUC, file = "modelRUBcuas_UC-C503cp8cv10.rda")

##########################################################################

# To get the area under the ROC curve for each predictor, the filterVarImp function 
# can be used. The area under the ROC curve is computed for each class.

# # For multi--class outcomes, the problem is decomposed into all 
# pair-wise problems and the area under the curve is calculated for 
# each class pair (i.e class 1 vs. class 2, class 2 vs. class 3 etc.). 
# For a specific class, the maximum area under the curve across the relevant 
# pair--wise AUC's is used as the variable importance measure

RocImpUC <- filterVarImp(x = training[, -ncol(training)], y = training$stars.x)
head(RocImpUC)

saveRDS(RocImpUC, file = "RocImpUC.rds")
