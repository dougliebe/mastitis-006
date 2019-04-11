library(lme4)
library(rpart)
library(party)
library(factoextra)
library(randomForest)
library(pca3d)
library(glmnet)
library(dplyr)
library(pROC)
library(ROCR)
library(scatterplot3d)
library(fastAdaboost)

###################### Train set loading with scale and outleirs out

setwd('C:/Users/Doug/Documents/PhD Papers/Mastitis Prediction')
# Start here!!
train <- read.csv('update_scale_127_day_values.csv')
train$X <- NULL
train$Lactno <- as.factor(ifelse(as.numeric(train$Lactno) > 2, 2, ifelse(train$Lactno == 2,1,0)))
train$GramNeg <- as.factor(ifelse(train$Bact.Group == 'GramNeg' & train$Day.relative.to.CM %in% c(0,-1,-2),1,0))
train$GramPos <- as.factor(ifelse(train$Bact.Group == 'GramPos' & train$Day.relative.to.CM %in% c(0,-1,-2,-3,-4,-5),1,0))
train$NoIsolation <- as.factor(ifelse(train$Bact.Group == 'NoIsolation',1,0))
train$Breed <- ifelse(train$Breed == "HO",'holstein', ifelse(train$Breed == 'JE','jersey', 'mixed'))

train$aDIM <- cut(train$DIM,seq(0,300,by=50))

vars = c('lactose','fat','protein','Cond','Activity','TotalRestTime','RestDur','DailyRestBout',
         'dailyyield')

# Change 7 day values to slopes
for(j in 1:9) {
  for(k in 1:2) {
    daySeven = train[,32+(3*(j-1))+(2)]
    train[,32+(3*(j-1))+(k-1)] <- (daySeven - train[,32+(3*(j-1))+(k-1)])/(7-k)
  }
}

train.complete <- train[complete.cases(train[,32:59]),]

# truncate values
for(i in 1:26) {
  train.complete[,32+(i-1)] <- train.complete[,32+(i-1)]*(train.complete[,32+(i-1)] <= 1 & train.complete[,32+(i-1)] >= -1) + 
    ((train.complete[,32+(i-1)]< -1)*-1)+(train.complete[,32+(i-1)]>1)*1
}
# Change day7 values + slopes into predicted d0 values
train.complete <- cbind(train.complete, matrix(rep(0,9*nrow(train.complete)),ncol = 9))
for(j in 1:9) {
  daySeven = train.complete[,32+(3*(j-1))+(2)]
  slope <- train.complete[,32+(3*(j-1))+(1-1)]
  train.complete[,ncol(train.complete)-9+(j)] <- daySeven+(slope*7)
}

colnames(train.complete)[(ncol(train.complete)-8):(ncol(train.complete))] <- paste(rep(vars[seq(1,9)],each = 1),'.exp', sep = "")

# Add residuals for d0 values
train.complete <- cbind(train.complete, matrix(rep(0,9*nrow(train.complete)),ncol = 9))
for(j in 1:9) {
  exp_value = train.complete[,ncol(train.complete)-17+(j-1)]
  true_value <- train.complete[,which(colnames(train.complete) == vars[j])]
  train.complete[,ncol(train.complete)-9+(j)] <- exp_value-true_value
}
colnames(train.complete)[(ncol(train.complete)-8):(ncol(train.complete))] <- paste(rep(vars[seq(1,9)],each = 1),'.resid', sep = "")




################ Pairs data loaded with outliers gone ################ 


setwd('C:/Users/Doug/Documents/PhD Papers/Mastitis Prediction')
train.pairs <- read.csv('SteeleMastitisFULL-paired.csv')


#Rename/make some variables
train.pairs$X <- NULL
# train.pairs$CM <- as.factor(train.pairs$CM)
train.pairs$GramPos <- as.factor(ifelse(train.pairs$BacteriaGroup == 'GramPos' & train.pairs$day %in% c(0,-1,-2,-3,-4,-5),1,0))
train.pairs$GramNeg <- as.factor(ifelse(train.pairs$BacteriaGroup == 'GramNeg' & train.pairs$day %in% c(0,-1,-2),1,0))

# train.pairs$GramPos <- as.factor((train.pairs$BacteriaGroup=="GramPos")*(1))
# train.pairs$GramNeg <- as.factor((train.pairs$BacteriaGroup=="GramNeg")*(1))

# train.pairs <- train.pairs[complete.cases(train.pairs),]
train.pairs <- subset(train.pairs, train.pairs$day <= 0)
train.pairs <- subset(train.pairs, !is.na(train.pairs$DailyRestBout) & train.pairs$DailyRestBout < mean(train.pairs$DailyRestBout, na.rm = T)+sd(train.pairs$DailyRestBout, na.rm = T)*5)
train.pairs <- subset(train.pairs, !is.na(train.pairs$RestDur) & train.pairs$RestDur < mean(train.pairs$RestDur, na.rm = T)+sd(train.pairs$RestDur, na.rm = T)*5)
train.pairs <- subset(train.pairs, !is.na(train.pairs$Cond) & train.pairs$Cond < mean(train.pairs$Cond, na.rm = T)+sd(train.pairs$Cond, na.rm = T)*5)
train.pairs <- subset(train.pairs, !is.na(train.pairs$Activity) & train.pairs$Activity < mean(train.pairs$Activity, na.rm = T)+sd(train.pairs$Activity, na.rm = T)*5)

vars = c('lactose','fat','protein','Cond','Activity','TotalRestTime','RestDur','DailyRestBout',
         'dailyyield')

# Scale all the vars of interest
train.pairs[,9:19] <- scale(train.pairs[,9:19])
train.pairs$Lactno <- as.factor(ifelse(as.numeric(train.pairs$Lactno) > 2, 2, ifelse(train.pairs$Lactno == 2,1,0)))

train.pairs <- remove_outliers(train.pairs)[,1:36]

# Add the seven days out values
get.cow.value <- function(cowID, date) {
  data = train.pairs
  vars = c('lactose','fat','protein','Cond','Activity','TotalRestTime','RestDur','DailyRestBout',
           'dailyyield')
  output = data.frame()
  for(var in vars) {
    for(i in c(1,2,3,4,5,6,7)){
      
      output = rbind(output,ifelse(length(data[data$Cow == cowID & data$afidate == (date-i),
                                               var])==0,NA,data[data$Cow == cowID & data$afidate == (date-i),
                                                                var]), make.row.names = F)
      
    }}
  return(t(output))
}

train.pairs <- cbind(train.pairs, matrix(rep(0,63*nrow(train.pairs)),ncol = 63))
#Cow list
for(i in 1:nrow(train.pairs)) {
  train.pairs[i,(ncol(train.pairs)-62):(ncol(train.pairs))] <- get.cow.value(train.pairs$Cow[i], train.pairs$afidate[i])
}
colnames(train.pairs)[(ncol(train.pairs)-62):(ncol(train.pairs))] <- paste(rep(vars[seq(1,9)],each = 7),c(1,2,3,4,5,6,7), sep = "")

# Change 7 day values to slopes
for(j in 1:9) {
  for(k in 1:6) {
    daySeven = train.pairs[,37+(3*(j-1))+(6)]
    train.pairs[,37+(7*(j-1))+(k-1)] <- (daySeven - train.pairs[,37+(7*(j-1))+(k-1)])/(7-k)
  }
}

pairs.complete <- train.pairs[complete.cases(train.pairs[,37:63]),]

# truncate values
for(i in 1:63) {
  pairs.complete[,37+(i-1)] <- pairs.complete[,37+(i-1)]*(pairs.complete[,37+(i-1)] <= 1 & pairs.complete[,37+(i-1)] >= -1) + 
    ((pairs.complete[,37+(i-1)]< -1)*-1)+(pairs.complete[,37+(i-1)]>1)*1
}

# Change day7 values + slopes into predicted d0 values
pairs.complete <- cbind(pairs.complete, matrix(rep(0,9*nrow(pairs.complete)),ncol = 9))
for(j in 1:9) {
  daySeven = pairs.complete[,37+(3*(j-1))+(2)]
  slope <- pairs.complete[,37+(3*(j-1))+(1-1)]
  pairs.complete[,ncol(pairs.complete)-9+(j)] <- daySeven+(slope*7)
}

colnames(pairs.complete)[(ncol(pairs.complete)-8):(ncol(pairs.complete))] <- paste(rep(vars[seq(1,9)],each = 1),'.exp', sep = "")

# Add residuals for d0 values
pairs.complete <- cbind(pairs.complete, matrix(rep(0,9*nrow(pairs.complete)),ncol = 9))
for(j in 1:9) {
  exp_value = pairs.complete[,ncol(pairs.complete)-17+(j-1)]
  true_value <- pairs.complete[,which(colnames(pairs.complete) == vars[j])]
  pairs.complete[,ncol(pairs.complete)-9+(j)] <- exp_value-true_value
}
colnames(pairs.complete)[(ncol(pairs.complete)-8):(ncol(pairs.complete))] <- paste(rep(vars[seq(1,9)],each = 1),'.resid', sep = "")
pairs.complete$aDIM <- cut(pairs.complete$DIM,seq(0,350,by=50))


# 
# #Rename/make some variables
# train.pairs$X <- NULL
# # train.pairs$CM <- as.factor(train.pairs$CM)
# train.pairs$GramPos <- as.factor(ifelse(train.pairs$BacteriaGroup == 'GramPos' & train.pairs$day %in% c(0,-1,-2,-3,-4,-5),1,0))
# train.pairs$GramNeg <- as.factor(ifelse(train.pairs$BacteriaGroup == 'GramNeg' & train.pairs$day %in% c(0,-1,-2),1,0))
# 
# # train.pairs$GramPos <- as.factor((train.pairs$BacteriaGroup=="GramPos")*(1))
# # train.pairs$GramNeg <- as.factor((train.pairs$BacteriaGroup=="GramNeg")*(1))
# 
# # train.pairs <- train.pairs[complete.cases(train.pairs),]
# train.pairs <- subset(train.pairs, train.pairs$day <= 0)
# train.pairs <- subset(train.pairs, !is.na(train.pairs$DailyRestBout) & train.pairs$DailyRestBout < mean(train.pairs$DailyRestBout, na.rm = T)+sd(train.pairs$DailyRestBout, na.rm = T)*5)
# train.pairs <- subset(train.pairs, !is.na(train.pairs$RestDur) & train.pairs$RestDur < mean(train.pairs$RestDur, na.rm = T)+sd(train.pairs$RestDur, na.rm = T)*5)
# train.pairs <- subset(train.pairs, !is.na(train.pairs$Cond) & train.pairs$Cond < mean(train.pairs$Cond, na.rm = T)+sd(train.pairs$Cond, na.rm = T)*5)
# train.pairs <- subset(train.pairs, !is.na(train.pairs$Activity) & train.pairs$Activity < mean(train.pairs$Activity, na.rm = T)+sd(train.pairs$Activity, na.rm = T)*5)
# 
# vars = c('lactose','fat','protein','Cond','Activity','TotalRestTime','RestDur','DailyRestBout',
#          'dailyyield')
# 
# # Scale all the vars of interest
# train.pairs[,9:19] <- scale(train.pairs[,9:19])
# train.pairs$Lactno <- as.factor(ifelse(as.numeric(train.pairs$Lactno) > 2, 2, ifelse(train.pairs$Lactno == 2,1,0)))
# 
# # Add the seven days out values
# get.cow.value <- function(cowID, date) {
#   data = train.pairs
#   vars = c('lactose','fat','protein','Cond','Activity','TotalRestTime','RestDur','DailyRestBout',
#            'dailyyield')
#   output = data.frame()
#   for(var in vars) {
#     for(i in c(1,2,7)){
# 
#       output = rbind(output,ifelse(length(data[data$Cow == cowID & data$afidate == (date-i),
#                                                var])==0,NA,data[data$Cow == cowID & data$afidate == (date-i),
#                                                                 var]), make.row.names = F)
# 
#     }}
#   return(t(output))
# }
# 
# train.pairs <- cbind(train.pairs, matrix(rep(0,27*nrow(train.pairs)),ncol = 27))
# #Cow list
# for(i in 1:nrow(train.pairs)) {
#   train.pairs[i,(ncol(train.pairs)-26):(ncol(train.pairs))] <- get.cow.value(train.pairs$Cow[i], train.pairs$afidate[i])
# }
# colnames(train.pairs)[(ncol(train.pairs)-26):(ncol(train.pairs))] <- paste(rep(vars[seq(1,9)],each = 3),c(1,2,7), sep = "")
# 
# # Change 7 day values to slopes
# for(j in 1:9) {
#   for(k in 1:2) {
#     daySeven = train.pairs[,37+(3*(j-1))+(2)]
#     train.pairs[,37+(3*(j-1))+(k-1)] <- (daySeven - train.pairs[,37+(3*(j-1))+(k-1)])/(7-k)
#   }
# }
# 
# pairs.complete <- train.pairs[complete.cases(train.pairs[,37:63]),]
# pairs.complete$aDIM <- cut(pairs.complete$DIM,seq(0,350,by=50))
# 
# # truncate values
# for(i in 1:26) {
#   pairs.complete[,37+(i-1)] <- pairs.complete[,37+(i-1)]*(pairs.complete[,37+(i-1)] <= 1 & pairs.complete[,37+(i-1)] >= -1) + 
#     ((pairs.complete[,37+(i-1)]< -1)*-1)+(pairs.complete[,37+(i-1)]>1)*1
# }
# 
# # Don't need the actual day 7 values
# pairs.complete <- pairs.complete[,-seq(39, 39+(3*8),3)]
# 
# #separate dataset in GP and GN cases for training
# training_grampos_pairs <- pairs.complete[,c(37:55,7,9,35)]
# training_gramneg_pairs <- pairs.complete[,c(37:55,7,9,36)]
# training_gramneg_pairs <- training_gramneg_pairs[complete.cases(training_gramneg_pairs),]
# training_grampos_pairs <- training_grampos_pairs[complete.cases(training_grampos_pairs),]
# 









# # Using train data
# 
# 
# ################# Already done and saved in csv for speed ##############################
# 
# 
# ### I want to adj each milk component measurement
# ### for the DIM and Wt of the cow as a pct of maximum, not compared to herd avg!
# 
# 
# train <- read.csv('train.csv')
# train$GramNeg <- as.factor(ifelse(train$Bact.Group == 'GramNeg',1,0))
# train$GramPos <- as.factor(ifelse(train$Bact.Group == 'GramPos',1,0))
# train$NoIsolation <- as.factor(ifelse(train$Bact.Group == 'NoIsolation',1,0))
# 
# # Remove outliers
# train <- subset(train, !is.na(train$DailyRestBout) & train$DailyRestBout < mean(train$DailyRestBout, na.rm = T)+sd(train$DailyRestBout, na.rm = T)*5)
# train <- subset(train, !is.na(train$RestDur) & train$RestDur < mean(train$RestDur, na.rm = T)+sd(train$RestDur, na.rm = T)*5)
# train <- subset(train, !is.na(train$Cond) & train$Cond < mean(train$Cond, na.rm = T)+sd(train$Cond, na.rm = T)*5)
# train <- subset(train, !is.na(train$Activity) & train$Activity < mean(train$Activity, na.rm = T)+sd(train$Activity, na.rm = T)*5)
# 
# # Scale all the vars of interest
# train[,8:19] <- scale(train[,8:19])
# # train[,6] <- scale(train[,6])
# train$Lactno <- as.factor(ifelse(train$Lactno > 2, 2, ifelse(train$Lactno == 2,1,0)))
# colnames(train)[c(2,5)] <- c('Cow','afidate')
# train <- data.frame(remove_outliers(train))[1:31]
# # Add the seven days out values
# get.cow.value <- function(cowID, date) {
#   data = train
#   vars = c('lactose','fat','protein','Cond','Activity','TotalRestTime','RestDur','DailyRestBout',
#            'dailyyield')
#   output = data.frame()
#   for(var in vars) {
#     for(i in c(1,2,7)){
# 
#       output = rbind(output,ifelse(length(data[data$Animal_ID == cowID & data$Date.1 == (date-i),
#                                                var])==0,NA,data[data$Animal_ID == cowID & data$Date.1 == (date-i),
#                                                                 var]), make.row.names = F)
# 
#     }}
#   return(t(output))
# }
# 
# train <- cbind(train, matrix(rep(0,27*nrow(train)),ncol = 27))
# #Cow list
# for(i in 1:nrow(train)) {
#   train[i,(ncol(train)-26):(ncol(train))] <- get.cow.value(train$Animal_ID[i], train$Date.1[i])
# }
# colnames(train)[(ncol(train)-26):(ncol(train))] <- paste(rep(vars[seq(1,9)],each = 3),c(1,2,7), sep = "")
# 







# # Start here!!
# train <- read.csv('proper_scale_127_day_values.csv')
# train$X <- NULL
# train$Lactno <- as.factor(ifelse(as.numeric(train$Lactno) > 2, 2, ifelse(train$Lactno == 2,1,0)))
# train$GramNeg <- as.factor(ifelse(train$Bact.Group == 'GramNeg' & train$Day.relative.to.CM %in% c(0,-1,-2),1,0))
# train$GramPos <- as.factor(ifelse(train$Bact.Group == 'GramPos' & train$Day.relative.to.CM %in% c(0,-1,-2,-3,-4,-5),1,0))
# train$NoIsolation <- as.factor(ifelse(train$Bact.Group == 'NoIsolation',1,0))
# train$Breed <- ifelse(train$Breed == "HO",'holstein', ifelse(train$Breed == 'JE','jersey', 'mixed'))
# #change names of %max
# train$aDIM <- cut(train$DIM,seq(0,300,by=50))
# 
# # Change 7 day values to slopes
# for(j in 1:9) {
#   for(k in 1:2) {
#     daySeven = train[,32+(3*(j-1))+(2)]
#     train[,32+(3*(j-1))+(k-1)] <- (daySeven - train[,32+(3*(j-1))+(k-1)])/(7-k)
#   }
# }
# 
# train.complete <- train[complete.cases(train[,32:58]),]
# 
# # truncate values
# for(i in 1:26) {
#   train.complete[,32+(i-1)] <- train.complete[,32+(i-1)]*(train.complete[,32+(i-1)] <= 1 & train.complete[,32+(i-1)] >= -1) + 
#     ((train.complete[,32+(i-1)]< -1)*-1)+(train.complete[,32+(i-1)]>1)*1
# }
# 
# 
# # Don't need the actual day 7 values
# train.complete <- train.complete[,-seq(34,34+(3*8),3)]






