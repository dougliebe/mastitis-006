library(lme4)
library(rpart)
library(party)
library(factoextra)
library(randomForest)
library(pca3d)
library(glmnet)
library(dplyr)
library(reshape2)
library(pROC)
library(ROCR)
library(caret)
library(tidyverse)

###################### Train set loading with scale and outleirs out

setwd('C:/Users/Doug/Documents/PhD Papers/Mastitis Prediction')
# Start here!!
train <- read.csv('train.csv')
train$X <- NULL
train$Lactno <- as.factor(ifelse(as.numeric(train$Lactno) > 2, 2, ifelse(train$Lactno == 2,1,0)))
train$GramNeg <- as.factor(ifelse(train$Bact.Group == 'GramNeg' & train$Day.relative.to.CM %in% c(0,-1,-2),1,0))
train$GramPos <- as.factor(ifelse(train$Bact.Group == 'GramPos' & train$Day.relative.to.CM %in% c(0,-1,-2,-3,-4,-5),1,0))
train$NoIsolation <- as.factor(ifelse(train$Bact.Group == 'NoIsolation',1,0))
train$Breed <- ifelse(train$Breed == "HO",'holstein', ifelse(train$Breed == 'JE','jersey', 'mixed'))

train$aDIM <- cut(train$DIM,seq(0,300,by=50))
train$aDIM <- ifelse(is.na(train$aDIM), '(300,350]', as.character(train$aDIM))

vars = c('lactose','fat','protein','Cond','Activity','TotalRestTime','RestDur','DailyRestBout',
         'dailyyield')
# train <- subset(train, train$Day.relative.to.CM <= 0)
train <- subset(train, !is.na(train$DailyRestBout) & train$DailyRestBout < mean(train$DailyRestBout, na.rm = T)+sd(train$DailyRestBout, na.rm = T)*5)
train <- subset(train, !is.na(train$RestDur) & train$RestDur < mean(train$RestDur, na.rm = T)+sd(train$RestDur, na.rm = T)*5)
train <- subset(train, !is.na(train$Cond) & train$Cond < mean(train$Cond, na.rm = T)+sd(train$Cond, na.rm = T)*5)
train <- subset(train, !is.na(train$Activity) & train$Activity < mean(train$Activity, na.rm = T)+sd(train$Activity, na.rm = T)*5)


# Scale all the vars of interest
train[,8:19] <- scale(train[,8:19])
train$Lactno <- as.factor(ifelse(as.numeric(train$Lactno) > 2, 2, ifelse(train$Lactno == 2,1,0)))
colnames(train)[c(2,5)] <- c('Cow','afidate')
remove_outliers <- function(data) {
  outliers <- data %>%
    group_by(Cow) %>%
    mutate(c = pmin(abs(dailyyield - lag(dailyyield, order_by = afidate, n = 4)),
                    abs(dailyyield - lag(dailyyield, order_by = afidate, n = 3)),
                    abs(dailyyield - lag(dailyyield, order_by = afidate, n = 2)),
                    abs(dailyyield - lag(dailyyield, order_by = afidate)))) %>%
    mutate(dailyyield = ifelse(c > 0.4,  (lag(dailyyield, order_by = afidate, n = 1)+
                                            lag(dailyyield, order_by = afidate, n = 2)+
                                            lag(dailyyield, order_by = afidate, n = 3)+
                                            lag(dailyyield, order_by = afidate, n = 4))/4,dailyyield)) %>%
    mutate(d = pmin(abs(Cond - lag(Cond, order_by = afidate, n = 4)),
                    abs(Cond - lag(Cond, order_by = afidate, n = 3)),
                    abs(Cond - lag(Cond, order_by = afidate, n = 2)),
                    abs(Cond - lag(Cond, order_by = afidate)))) %>%
    mutate(Cond = ifelse(d > 0.4,  (lag(Cond, order_by = afidate, n = 1)+
                                      lag(Cond, order_by = afidate, n = 2)+
                                      lag(Cond, order_by = afidate, n = 3)+
                                      lag(Cond, order_by = afidate, n = 4))/4,Cond)) %>%
    mutate(e = pmin(abs(lactose - lag(lactose, order_by = afidate, n = 4)),
                    abs(lactose - lag(lactose, order_by = afidate, n = 3)),
                    abs(lactose - lag(lactose, order_by = afidate, n = 2)),
                    abs(lactose - lag(lactose, order_by = afidate)))) %>%
    mutate(lactose = ifelse(e > 1, (lag(lactose, order_by = afidate, n = 1)+
                                      lag(lactose, order_by = afidate, n = 2)+
                                      lag(lactose, order_by = afidate, n = 3)+
                                      lag(lactose, order_by = afidate, n = 4))/4,lactose)) %>%
    mutate(f = pmin(abs(DailyRestBout - lag(DailyRestBout, order_by = afidate, n = 4)),
                    abs(DailyRestBout - lag(DailyRestBout, order_by = afidate, n = 3)),
                    abs(DailyRestBout - lag(DailyRestBout, order_by = afidate, n = 2)),
                    abs(DailyRestBout - lag(DailyRestBout, order_by = afidate)))) %>%
    mutate(DailyRestBout = ifelse(f > 1, (lag(DailyRestBout, order_by = afidate, n = 1)+
                                            lag(DailyRestBout, order_by = afidate, n = 2)+
                                            lag(DailyRestBout, order_by = afidate, n = 3)+
                                            lag(DailyRestBout, order_by = afidate, n = 4))/4,DailyRestBout)) %>%
    mutate(g = pmin(abs(Activity - lag(Activity, order_by = afidate, n = 4)),
                    abs(Activity - lag(Activity, order_by = afidate, n = 3)),
                    abs(Activity - lag(Activity, order_by = afidate, n = 2)),
                    abs(Activity - lag(Activity, order_by = afidate)))) %>%
    mutate(Activity = ifelse(g > 0.25, (lag(Activity, order_by = afidate, n = 1)+
                                          lag(Activity, order_by = afidate, n = 2)+
                                          lag(Activity, order_by = afidate, n = 3)+
                                          lag(Activity, order_by = afidate, n = 4))/4,Activity)) %>%
    mutate(h = pmin(abs(RestDur - lag(RestDur, order_by = afidate, n = 4)),
                    abs(RestDur - lag(RestDur, order_by = afidate, n = 3)),
                    abs(RestDur - lag(RestDur, order_by = afidate, n = 2)),
                    abs(RestDur - lag(RestDur, order_by = afidate)))) %>%
    mutate(RestDur = ifelse(h > 0.4, (lag(RestDur, order_by = afidate, n = 1)+
                                        lag(RestDur, order_by = afidate, n = 2)+
                                        lag(RestDur, order_by = afidate, n = 3)+
                                        lag(RestDur, order_by = afidate, n = 4))/4,RestDur)) %>%
    mutate(j = pmin(abs(TotalRestTime - lag(TotalRestTime, order_by = afidate, n = 4)),
                    abs(TotalRestTime - lag(TotalRestTime, order_by = afidate, n = 3)),
                    abs(TotalRestTime - lag(TotalRestTime, order_by = afidate, n = 2)),
                    abs(TotalRestTime - lag(TotalRestTime, order_by = afidate)))) %>%
    mutate(TotalRestTime = ifelse(j > 1, (lag(TotalRestTime, order_by = afidate, n = 1)+
                                            lag(TotalRestTime, order_by = afidate, n = 2)+
                                            lag(TotalRestTime, order_by = afidate, n = 3)+
                                            lag(TotalRestTime, order_by = afidate, n = 4))/4,TotalRestTime))
  return(data.frame(outliers))
}          

train <- remove_outliers(train)[,1:32]

# repeat vars to get less NAs
train <- train %>% 
  group_by(Cow) %>% 
  arrange(afidate) %>%
  fill(dailyyield, Activity, DailyRestBout, RestDur, TotalRestTime, lactose, protein, Cond, .direction = "up") %>%
  data.frame()


Date <- unique(train$Date)
Day <- seq(1:length(Date))
datedata <- data.frame(Date, Day)
datedata$Day <- datedata$Day-max(Day)
testday <- merge(testday, datedata, by="Date")


output <- NULL
for(k in 1:length(c(min(train$afidate, na.rm = T):max(train$afidate, na.rm = T)))){
  #Calculate slopes 
  slp <- NULL
  for(i in 1:length(unique(train$Cow))) {
    p <- subset(train, train$Cow==unique(train$Cow)[i] & train$afidate < c(min(train$afidate, na.rm = T):max(train$afidate, na.rm = T))[k])
    out <- NULL
    for(j in 1:3) {
      f <- subset(p, p$afidate>=c(min(train$afidate, na.rm = T):max(train$afidate, na.rm = T))[k]-7 & p$afidate<=(c(min(train$afidate, na.rm = T):max(train$afidate, na.rm = T))[k]-j))
      f$sDay <- f$afidate+j 
      LS <- ifelse(is.na(mean(f$lactose, na.rm=TRUE))==TRUE,NA,coefficients(lm(lactose~sDay, data=f))[2])
      FS <- ifelse(is.na(mean(f$fat, na.rm=TRUE))==TRUE,NA,coefficients(lm(fat~sDay, data=f))[2])
      PS <- ifelse(is.na(mean(f$protein, na.rm=TRUE))==TRUE,NA,coefficients(lm(protein~sDay, data=f))[2])
      CS <- ifelse(is.na(mean(f$Cond, na.rm=TRUE))==TRUE,NA,coefficients(lm(Cond~sDay, data=f))[2])
      AS <- ifelse(is.na(mean(f$Activity, na.rm=TRUE))==TRUE,NA,coefficients(lm(Activity~sDay, data=f))[2])
      RTS <- ifelse(is.na(mean(f$TotalRestTime, na.rm=TRUE))==TRUE,NA,coefficients(lm(TotalRestTime~sDay, data=f))[2])
      RDS <- ifelse(is.na(mean(f$RestDur, na.rm=TRUE))==TRUE,NA,coefficients(lm(RestDur~sDay, data=f))[2])
      RBS <- ifelse(is.na(mean(f$DailyRestBout, na.rm=TRUE))==TRUE,NA,coefficients(lm(DailyRestBout~sDay, data=f))[2])
      MYS <- ifelse(is.na(mean(f$dailyyield, na.rm=TRUE))==TRUE, NA, coefficients(lm(dailyyield~sDay, data=f))[2])
      row <- data.frame(LS, FS, PS, CS, AS, RTS, RDS, RBS, MYS)
      if(j==1) {out <- row} else{out <- rbind(out, row) }
    }
    c <- melt(out, id=NULL)
    c$SlopeDay.relative.to.CM <- rep(seq(1,3), length(out[1,]))
    c$Var <- rep(c("LS", "FS", "PS", "CS", "AS", "RTS", "RDS", "RBS", "MYS"), each=length(out[,1]))
    c$ID <- paste(c$Var, c$SlopeDay.relative.to.CM, sep="")
    c$Cow <- unique(train$Cow)[i]
    a <- dcast(c, Cow~ID)
    if(i==1) {slp <- a} else{slp <- rbind(slp,a)}
  }
  slp$afidate <- c(min(train$afidate, na.rm = T):max(train$afidate, na.rm = T))[k]
  if(k == 1) {output <- slp} else {output <- rbind(output, slp)}
  output <- subset(output, !is.na(output$AS3))
}


#Merge the slopes into a smaller dataframe of all cow observations
train.complete <- merge(train, output, by=c("Cow", 'afidate'))




# Or just load the data - already processed for NAs etc.
train.complete <- read.csv('train_last_7.csv')
train.complete$X <- NULL



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

# repeat vars to get less NAs
train.pairs <- train.pairs %>% 
  group_by(Cow) %>% 
  arrange(afidate) %>%
  fill(dailyyield, Activity, DailyRestBout, RestDur, TotalRestTime, lactose, protein, Cond, .direction = "up") %>%
  data.frame()


#Calculate slopes 
slp <- NULL
for(i in 1:length(unique(train.pairs$Cow))) {
  p <- subset(train.pairs, train.pairs$Cow==unique(train.pairs$Cow)[i] & train.pairs$day<0)
  out <- NULL
  for(j in 1:3) {
    f <- subset(p, p$day>=-7 & p$day<=(-j))
    f$sday <- f$day+j 
    LS <- ifelse(is.na(mean(f$lactose, na.rm=TRUE))==TRUE,NA,coefficients(lm(lactose~sday, data=f))[2])
    FS <- ifelse(is.na(mean(f$fat, na.rm=TRUE))==TRUE,NA,coefficients(lm(fat~sday, data=f))[2])
    PS <- ifelse(is.na(mean(f$protein, na.rm=TRUE))==TRUE,NA,coefficients(lm(protein~sday, data=f))[2])
    CS <- ifelse(is.na(mean(f$Cond, na.rm=TRUE))==TRUE,NA,coefficients(lm(Cond~sday, data=f))[2])
    AS <- ifelse(is.na(mean(f$Activity, na.rm=TRUE))==TRUE,NA,coefficients(lm(Activity~sday, data=f))[2])
    RTS <- ifelse(is.na(mean(f$TotalRestTime, na.rm=TRUE))==TRUE,NA,coefficients(lm(TotalRestTime~sday, data=f))[2])
    RDS <- ifelse(is.na(mean(f$RestDur, na.rm=TRUE))==TRUE,NA,coefficients(lm(RestDur~sday, data=f))[2])
    RBS <- ifelse(is.na(mean(f$DailyRestBout, na.rm=TRUE))==TRUE,NA,coefficients(lm(DailyRestBout~sday, data=f))[2])
    MYS <- ifelse(is.na(mean(f$dailyyield, na.rm=TRUE))==TRUE, NA, coefficients(lm(dailyyield~sday, data=f))[2])
    row <- data.frame(LS, FS, PS, CS, AS, RTS, RDS, RBS, MYS)
    if(j==1) {out <- row} else{out <- rbind(out, row) }
  }
  c <- melt(out, id=NULL)
  c$Slopeday <- rep(seq(1,3), length(out[1,]))
  c$Var <- rep(c("LS", "FS", "PS", "CS", "AS", "RTS", "RDS", "RBS", "MYS"), each=length(out[,1]))
  c$ID <- paste(c$Var, c$Slopeday, sep="")
  c$Cow <- unique(train.pairs$Cow)[i]
  a <- dcast(c, Cow~ID)
  if(i==1) {slp <- a} else{slp <- rbind(slp,a)}
}


#Merge the slopes into a smaller dataframe of all cow observations
train.pairs <- merge(train.pairs, slp, by="Cow")
train.pairs$aDIM <- cut(train.pairs$DIM,seq(0,350,by=50))
train.pairs$aDIM <- ifelse(is.na(train.pairs$aDIM), '(300,350]', as.character(train.pairs$aDIM))


#### Make the full dataset


train.all <- rbind(train.complete[,c(1,2,6:7,15,29,30,32:59)], train.pairs[,c(1,2,7:9,35:64)])
train.all$aDIM <- as.factor(train.all$aDIM)
