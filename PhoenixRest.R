
## Phoenix Exploratory / Model
# Subset by City = Phoenix

library(jsonlite)
library(tidyjson)
library(dplyr) 

# read in individual JSON lines
json_file <- "yelp_academic_dataset_business.json"
datB <- fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=","))) 

# names$type
json_file <- "yelp_academic_dataset_review.json"
datR <- fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

# yelp_academic_dataset_checkin
json_file <- "yelp_academic_dataset_checkin.json"
datC <- fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

# yelp_academic_dataset_tip
json_file <- "yelp_academic_dataset_tip.json"
datT <- fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

# yelp_academic_dataset_user
json_file <- "yelp_academic_dataset_user.json"
datU <- fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))


saveRDS(datU, "datU.rds")
saveRDS(datB, "datB.rds")
saveRDS(datC, "datC.rds")
saveRDS(datT, "datT.rds")
saveRDS(datR, "datR.rds")


datB_PHX <- subset(datB, city == "Phoenix") # 8410 obs
datT_PHX <- subset(datT, business_id %in% datB_PHX$business_id) # 73k obs
datR_PHX <- subset(datR, business_id %in% datB_PHX$business_id) # 219k obs
datU_PHX <- subset(datU, user_id %in% datR_PHX$user_id) # 65k

# Selects users with fans
fanscount <- subset(datU_PHX, fans >= 0)
#fanscount[is.na(fanscount)] <- 0

# Select all users 
datRU_PHX <- subset(datR_PHX, user_id %in% fanscount$user_id) # 219k obs

# add User info
datRU_PHX <- merge(datRU_PHX, fanscount, by = "user_id", all.x = TRUE)

# add business info
datRUB_PHX <- merge(datRU_PHX, datB_PHX, by = "business_id", all.x = TRUE)

rm(datRU_PHX)
rm(fanscount)
rm(datB_PHX)
rm(datR_PHX)
rm(datT_PHX)
rm(datU_PHX)

# Remove irrelevant columns
myvars <- names(datRUB_PHX) %in% c("yelping_since", "friends", "type.y",
                                   "compliments", "elite", "full_address",
                                   "city", "neighborhoods", "longitude",
                                   "state", "latitude", "type.x", "name.x",
                                   "open", "type", "stars.y", "review_count.y",
                                   "votes.y")
datRUB_PHX <- datRUB_PHX[!myvars]

library(jsonlite)

# Calculates Hours on Weekdays and on Weekends
hoursW <- datRUB_PHX$hours
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

datRUB_PHX$WKND <- hoursW$WKND
datRUB_PHX$WKD <- hoursW$WKD

# Remove irrelevant columns
myvars <- names(datRUB_PHX) %in% c("hours")
datRUB_PHX <- datRUB_PHX[!myvars]

# Characterizes categories for Restaurants (c for categ) #141k
datRUB_PHX$categ <- grepl ( "^(.*[Rr]estaurant.*)",  datRUB_PHX$categories)
datRUBc_PHX <- subset(datRUB_PHX, datRUB_PHX$categ == TRUE)  

datRUBcu_PHX <- datRUBc_PHX

rm(datRUBc_PHX)

# Flattens 'attributes' and select variables for restaurant
attrib <- datRUBcu_PHX$attributes
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

datRUBcua_PHX <- cbind(datRUBcu_PHX, attrib)

rm(datRUBcu_PHX)
rm(attrib)

myvars <- names(datRUBcua_PHX) %in% c("attributes", "categories")
datRUBcua_PHX <- datRUBcua_PHX[!myvars]

###########################################################################
# Sentiment analysis for 'text'
###########################################################################
# LONG ROUTINE - SUBSET/RUN FOR EVERY 10K LINES (20min)

library(sentiment)

fortext <- datRUBcua_PHX

vectexto <- fortext$text

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
saveRDS(datRUBcua_PHX, file = "datRUBcua_PHX.rds")

datRUBcuas_PHX_full <- cbind(datRUBcua_PHX, sent)

saveRDS(datRUBcuas_PHX_full, file = "datRUBcuas_PHX-full.rds")
datRUBcuas_PHX_full <- readRDS("datRUBcuas_PHX-full.rds")

#############################################################################

rm(class_pol)
rm(class_emo)
rm(datRUBcua_PHX)
rm(fortext)
rm(sent)
rm(emotion)
rm(polarity)
rm(some_txt)
rm(vectexto)
rm(hoursW)

##############################################################################
# File for modeling and test
##############################################################################


set.seed(123)
index <- sample(1:nrow(datRUBcuas_PHX_full), 500)
#index

testfile <- datRUBcuas_PHX_full[index, ]
forModel3cf <- datRUBcuas_PHX_full[-index, ]

# Remove irrelevant columns
myvars <- names(testfile) %in% c("business_id", "user_id", "review_id",
                                 "date", "text", "review_count.x",
                                 "name.y", "text", "categ", "Accepts Credit Cards")
testfile <- testfile[!myvars]

myvars <- names(forModel3cf) %in% c("business_id", "user_id", "review_id",
                                    "date", "text", "review_count.x",
                                    "name.y", "text", "categ", "Accepts Credit Cards")
forModel3cf <- forModel3cf[!myvars]


rm(datRUB_PHX)
rm(datB_PHX)
rm(datR_PHX)
rm(datT_PHX)
rm(datU_PHX)

##############################################################################
## Data Preparation
##############################################################################

##

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
table(trainingRaw$`Outdoor Seating`)

# Set train-test files

set.seed(123456)
fortrain <- createDataPartition(trainingRaw$stars.x, p = 0.8, list=FALSE)
training <- trainingRaw[fortrain,]
validation <- trainingRaw[-fortrain,]

rm(datRUBcuas_PHX_full)
rm(forModel3cf)
rm(trainingRaw)

training$stars.x <- make.names(training$stars.x, unique = FALSE, allow_ = TRUE)
validation$stars.x <- make.names(validation$stars.x, unique = FALSE, allow_ = TRUE)

saveRDS(training, file = "training.rds")
saveRDS(testfile, file = "testfile.rds")
saveRDS(validation, file = "validation.rds")

###########################################################################
## 3 Class - Models
###########################################################################

###########################################################################
# Train model - LOGISTICS REGRESSION


cl <- makePSOCKcluster(2)
registerDoParallel(cl)

#metric = "Mean_ROC",

system.time(
  modelFitLR3c41MC <- train(stars.x ~., method="multinom", data=training,
                            metric = "Mean_ROC",
                            trControl=trainControl(method="cv", 10,
                                                   allowParallel=TRUE,
                                                   classProbs = TRUE,
                                                   summaryFunction = multiClassSummary))
)

stopCluster(cl)
save(modelFitLR3c41MC, file = "modelRUBcuas_PHXRest-LR3cp8cv10v41MC.rda")

# user  system elapsed 
# 64.65    0.80 1169.40 


# summary(modelFitLR3c41MC$finalModel)



# Linear Models: the absolute value of the t-statistic for each model parameter is used. 

###########################################################################
# Train model - RANDOM FOREST


cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modelFitRF3c41MCsam <- train(stars.x ~., method="rf", data=training[sample(1:nrow(training), 0.01*nrow(training)), ],
                               metric = "Mean_ROC",  
                               trControl=trainControl(method="cv", 10,
                                                      classProbs = TRUE,
                                                      summaryFunction = multiClassSummary,
                                                      allowParallel=TRUE))
)

stopCluster(cl)
save(modelFitRF3c41MCsam, file = "modelRUBcuas_PHXRest-RF3cp8cv10v41MCsam.rda")

# user   system  elapsed 
# 1979.81     6.58 39780.59 

#print(modFit$finalModel)

# summary(modelFitRF3c41MC$finalModel)



###########################################################################
# Train model - Boosted Tree C5.0
# This method uses the same approach as a single tree, but sums the importances 
# over each boosting iteration

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

c50Grid <- expand.grid(.trials = c(1:9, (1:5)*10),
                       .model = "tree",
                       .winnow = FALSE)

system.time(
  
  modelFitC503cMCsam <- train(stars.x ~., method="C5.0", data=training[sample(1:nrow(training), 0.01*nrow(training)), ],
                              metric = "Mean_ROC", 
                              trControl=trainControl(method="cv", 10,
                                                     classProbs = TRUE,
                                                     summaryFunction = multiClassSummary,
                                                     allowParallel=TRUE),
                              tuneGrid = c50Grid)
)

stopCluster(cl)
save(modelFitC503cMCsam, file = "modelRUBcuas_PHX-C503cp8cv10MCsam.rda")


###########################################################################
# Train model - SVM

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modelFitSVM3cMCsam <- train(stars.x ~., method="svmLinear", data=training[sample(1:nrow(training), 0.01*nrow(training)), ],
                              metric = "Mean_ROC",
                              trControl=trainControl(method="cv", 10,
                                                     classProbs = TRUE,
                                                     summaryFunction = multiClassSummary,
                                                     allowParallel=TRUE))
)

stopCluster(cl)
save(modelFitSVM3cMCsam, file = "modelRUBcuas_PHX-SVM3cp8cv10MCsam.rda")


##########################################################################
# Model Compare
##########################################################################

rValues <- resamples(list(svm=modelFitSVM3cMC,rf=modelFitRF3c41MCsam,c50=modelFitC503cMCsam,
                          LR=modelFitLR3c41MCsam))
rValues$values

# bwplot(rValues,metric="Mean_ROC")	

jpeg('bwplot.jpg')
bwplot(rValues,metric="Mean_ROC")	
dev.off()

##########################################################################
#modFit
##########################################################################

# For most classification models, each predictor will have a separate variable 
# importance for each class (the exceptions are classification trees, bagged 
# trees and boosted trees).

# http://www.inside-r.org/packages/cran/caret/docs/varimp
# http://topepo.github.io/caret/varimp.html

VI <- varImp(modelFitLR3c41MC, scale = FALSE)  ### Change model here  
plot(VI, main = "Variable Importance", top = 10)

plot(modelFitLR3c41MC)

getTrainPerf(modelFitLR3c41MC)


# Accuracy validation set
# ValPred <- predict(modelFitLR3c41MC, validation)  ### Change model here  <- <-
ValPredProb <- predict(modelFitLR3c41MC, validation, type = "prob") 

library(pROC)

ROCX5auc <- roc(ifelse(validation[,"stars.x"] == "X5", 1, 0), ValPredProb$X5)$auc
# Area under the curve: 0.7565
ROCX4auc <- roc(ifelse(validation[,"stars.x"] == "X4", 1, 0), ValPredProb$X4)$auc
# Area under the curve: 0.6533

ROCX5 <- roc(ifelse(validation[,"stars.x"] == "X5", 1, 0), ValPredProb$X5) #ValPredProb[[2]]
plot(ROCX5, col = "blue", main="X5", sub="fig-#")

ROCX4 <- roc(ifelse(validation[,"stars.x"] == "X4", 1, 0), ValPredProb$X4) #ValPredProb[[2]]
plot(ROCX4, col = "blue", main="X4", sub="fig-#")


##################################################################################
# To get the area under the ROC curve for each predictor, the filterVarImp function 
# can be used. The area under the ROC curve is computed for each class.

# # For multi--class outcomes, the problem is decomposed into all 
# pair-wise problems and the area under the curve is calculated for 
# each class pair (i.e class 1 vs. class 2, class 2 vs. class 3 etc.). 
# For a specific class, the maximum area under the curve across the relevant 
# pair--wise AUC's is used as the variable importance measure

RocImp <- filterVarImp(x = training[, -ncol(training)], y = training$stars.x)
head(RocImp)

saveRDS(RocImpPHX, file = "RocImpPHX.rds")
head(RocImpPHX, 11)

x5Imp <- RocImp
x5Imp <- gamImp[order(gamImp$X5, decreasing = TRUE), , drop = FALSE]
head(x5Imp,7)

x4Imp <- RocImp
x4Imp <- gamImp[order(gamImp$X4, decreasing = TRUE), , drop = FALSE]
head(x4Imp,7)
