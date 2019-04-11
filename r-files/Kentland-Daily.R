
# This just loads the packages you need
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, dplyr, tidyverse, reshape2, ggplot2, lme4, lmerTest, epiR, MuMIn, xgboost)


#______________________READ IN DAILY DATA FOR TEST___________________________


#Set the working directory
setwd("C:/Users/Doug/Documents/PhD Papers/Mastitis Prediction")



#read in new dataset
testday <- read.csv("VT KENTLAND FULL data Nov2017 updated 21 may 2018.csv")

#Read in the models
# bstDMatrix.neg <- xgb.load("GN_pred.model")
# bstDMatrix.pos <- xgb.load("GP_pred.model")

#UK breed is 100% Holstein


#Fix data types
# testday$Weight1 <- as.numeric(as.character(testday$Weight1))
# testday$Weight2 <- as.numeric(as.character(testday$Weight2))
# testday$FAT1 <- as.numeric(as.character(testday$FAT1))
# testday$FAT2 <- as.numeric(as.character(testday$FAT2))
# testday$PRO1 <- as.numeric(as.character(testday$PRO1))
# testday$PRO2 <- as.numeric(as.character(testday$PRO2))
# testday$LAC1 <- as.numeric(as.character(testday$LAC1))
# testday$LAC2 <- as.numeric(as.character(testday$LAC2))
# testday$MY1 <- as.numeric(as.character(testday$MY1))
# testday$MY2 <- as.numeric(as.character(testday$MY2))
# testday$COND1 <- as.numeric(as.character(testday$COND1))
# testday$COND2 <- as.numeric(as.character(testday$COND2))
# testday$Activity1 <- as.numeric(as.character(testday$Activity1))
# testday$Activity2 <- as.numeric(as.character(testday$Activity2))
testday$RestTime <- as.numeric(as.character(testday$RestTime))
testday$RestDur <- as.numeric(as.character(testday$RestPerBout))
testday$RestBout <- as.numeric(as.character(testday$RestBout))
testday$FAT <- as.numeric(as.character(testday$Fat))
testday$PRO <- as.numeric(as.character(testday$Protein))
testday$LAC <- as.numeric(as.character(testday$Lactose))
testday$MY <- as.numeric(as.character(testday$Yield))
testday$COND <- as.numeric(as.character(testday$Conductivity))
testday$Activity <- as.numeric(as.character(testday$Activity))
# testday$SCC1 <- as.character(testday$SCC1)
# testday$SCC_1 <- ifelse(testday$SCC1=="0-200", 100, 
#                         ifelse(testday$SCC1=="200-400", 300,
#                                ifelse(testday$SCC1=="400-800", 600, 
#                                       ifelse(testday$SCC1=="800+", 1200, NA))))
# testday$SCC2 <- as.character(testday$SCC2)
# testday$SCC_2 <- ifelse(testday$SCC2=="0-200", 100, 
#                         ifelse(testday$SCC2=="200-400", 300,
#                                ifelse(testday$SCC2=="400-800", 600, 
#                                       ifelse(testday$SCC2=="800+", 1200, NA))))
# testday$SCC <- ifelse(is.na(testday$SCC_1)==FALSE & is.na(testday$SCC_2)==FALSE, (testday$SCC_1+testday$SCC_2)/2,
#                       ifelse(is.na(testday$SCC_1)==TRUE & is.na(testday$SCC_2)==FALSE, testday$SCC_2,
#                              ifelse(is.na(testday$SCC_1)==FALSE & is.na(testday$SCC_2)==TRUE, testday$SCC_1, NA)))
# 
# testday$SCC <- testday$SCC*1000
# testday$SCC <- ifelse(testday$SCC==0, NA, testday$SCC)
# testday$SCC <- log(testday$SCC)


# #combine session 1 and 2 data 
# testday$Weight <- (testday$Weight1+testday$Weight2)/2
# testday$Cow <- as.character(testday$Cow)
# testday$Date <- as.character(testday$Date)
# testday$cowdate <- paste(testday$Cow, testday$Date, sep="")
# 
# testday$FAT <- (testday$FAT1 + testday$FAT2)/2
# testday$MY <- (testday$MY1+testday$MY2)
# testday$MY <- (testday$MY*0.453592)
# testday$PRO <- (testday$PRO1+testday$PRO2)/2
# testday$LAC <- (testday$LAC1+testday$LAC2)/2
# testday$COND <- (testday$COND1+testday$COND2)/2
# testday$Activity <- (testday$Activity1+testday$Activity2)/2

Date <- unique(testday$RealDate)
Day <- seq(1:length(Date))
datedata <- data.frame(Date, Day)
datedata$Day <- datedata$Day-max(Day)
testday <- merge(testday, datedata, by.x = 'RealDate',by.y="Date")

testday$Cow <- as.numeric(as.character(testday$AnimalID))
#testday$Date <- as.numeric(as.character(testday$Date))
testday$Lactno <- as.numeric(as.character(testday$Lactation))

# Scale all the vars of interest
testday[,c(14,21,22, 24:30)] <- scale(testday[,c(14,21,22, 24:30)])
testday$Lactno <- as.factor(ifelse(as.numeric(testday$Lactno) > 2, 2, ifelse(testday$Lactno == 2,1,0)))
testday$afidate <- as.numeric(as.Date(testday$RealDate, "%m/%d/%Y"))

## Remove outliers formula
remove_outliers <- function(data) {
  outliers <- data %>%
    group_by(Cow) %>%
    mutate(c = pmin(abs(MY - lag(MY, order_by = afidate, n = 4)),
                    abs(MY - lag(MY, order_by = afidate, n = 3)),
                    abs(MY - lag(MY, order_by = afidate, n = 2)),
                    abs(MY - lag(MY, order_by = afidate)))) %>%
    mutate(MY = ifelse(c > 0.4,  (lag(MY, order_by = afidate, n = 1)+
                                    lag(MY, order_by = afidate, n = 2)+
                                    lag(MY, order_by = afidate, n = 3)+
                                    lag(MY, order_by = afidate, n = 4))/4,MY)) %>%
    mutate(d = pmin(abs(COND - lag(COND, order_by = afidate, n = 4)),
                    abs(COND - lag(COND, order_by = afidate, n = 3)),
                    abs(COND - lag(COND, order_by = afidate, n = 2)),
                    abs(COND - lag(COND, order_by = afidate)))) %>%
    mutate(COND = ifelse(d > 0.4,  (lag(COND, order_by = afidate, n = 1)+
                                      lag(COND, order_by = afidate, n = 2)+
                                      lag(COND, order_by = afidate, n = 3)+
                                      lag(COND, order_by = afidate, n = 4))/4,COND)) %>%
    mutate(e = pmin(abs(LAC - lag(LAC, order_by = afidate, n = 4)),
                    abs(LAC - lag(LAC, order_by = afidate, n = 3)),
                    abs(LAC - lag(LAC, order_by = afidate, n = 2)),
                    abs(LAC - lag(LAC, order_by = afidate)))) %>%
    mutate(LAC = ifelse(e > 1, (lag(LAC, order_by = afidate, n = 1)+
                                  lag(LAC, order_by = afidate, n = 2)+
                                  lag(LAC, order_by = afidate, n = 3)+
                                  lag(LAC, order_by = afidate, n = 4))/4,LAC)) %>%
    mutate(f = pmin(abs(RestBout - lag(RestBout, order_by = afidate, n = 4)),
                    abs(RestBout - lag(RestBout, order_by = afidate, n = 3)),
                    abs(RestBout - lag(RestBout, order_by = afidate, n = 2)),
                    abs(RestBout - lag(RestBout, order_by = afidate)))) %>%
    mutate(RestBout = ifelse(f > 1, (lag(RestBout, order_by = afidate, n = 1)+
                                       lag(RestBout, order_by = afidate, n = 2)+
                                       lag(RestBout, order_by = afidate, n = 3)+
                                       lag(RestBout, order_by = afidate, n = 4))/4,RestBout)) %>%
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
    mutate(j = pmin(abs(RestTime - lag(RestTime, order_by = afidate, n = 4)),
                    abs(RestTime - lag(RestTime, order_by = afidate, n = 3)),
                    abs(RestTime - lag(RestTime, order_by = afidate, n = 2)),
                    abs(RestTime - lag(RestTime, order_by = afidate)))) %>%
    mutate(RestTime = ifelse(j > 1, (lag(RestTime, order_by = afidate, n = 1)+
                                       lag(RestTime, order_by = afidate, n = 2)+
                                       lag(RestTime, order_by = afidate, n = 3)+
                                       lag(RestTime, order_by = afidate, n = 4))/4,RestTime))
  return(data.frame(outliers))
}          

testday <- remove_outliers(testday)[,1:34]

# repeat vars to get less NAs
testday <- testday %>% 
  group_by(Cow) %>% 
  arrange(afidate) %>%
  fill(MY, Activity, RestBout, RestDur, RestTime, LAC, PRO, COND, FAT, Weight, DIM, .direction = "up") %>%
  data.frame()


#Calculate slopes 
slp <- NULL
for(i in 1:length(unique(testday$Cow))) {
  p <- subset(testday, testday$Cow==unique(testday$Cow)[i] & testday$Day.y<0)
  out <- NULL
  for(j in 1:5) {
    f <- subset(p, p$Day.y>=-7 & p$Day.y<=(-j))
    f$sday <- f$Day.y+j 
    LS <- ifelse(is.na(mean(f$LAC, na.rm=TRUE))==TRUE,NA,coefficients(lm(LAC~sday, data=f))[2])
    FS <- ifelse(is.na(mean(f$FAT, na.rm=TRUE))==TRUE,NA,coefficients(lm(FAT~sday, data=f))[2])
    PS <- ifelse(is.na(mean(f$PRO, na.rm=TRUE))==TRUE,NA,coefficients(lm(PRO~sday, data=f))[2])
    CS <- ifelse(is.na(mean(f$COND, na.rm=TRUE))==TRUE,NA,coefficients(lm(COND~sday, data=f))[2])
    AS <- ifelse(is.na(mean(f$Activity, na.rm=TRUE))==TRUE,NA,coefficients(lm(Activity~sday, data=f))[2])
    RTS <- ifelse(is.na(mean(f$RestTime, na.rm=TRUE))==TRUE,NA,coefficients(lm(RestTime~sday, data=f))[2])
    RDS <- ifelse(is.na(mean(f$RestDur, na.rm=TRUE))==TRUE,NA,coefficients(lm(RestDur~sday, data=f))[2])
    RBS <- ifelse(is.na(mean(f$RestBout, na.rm=TRUE))==TRUE,NA,coefficients(lm(RestBout~sday, data=f))[2])
    MYS <- ifelse(is.na(mean(f$MY, na.rm=TRUE))==TRUE, NA, coefficients(lm(MY~sday, data=f))[2])
    row <- data.frame(LS, FS, PS, CS, AS, RTS, RDS, RBS, MYS)
    if(j==1) {out <- row} else{out <- rbind(out, row) }
  }
  c <- melt(out, id=NULL)
  c$SlopeDay <- rep(seq(1,5), length(out[1,]))
  c$Var <- rep(c("LS", "FS", "PS", "CS", "AS", "RTS", "RDS", "RBS", "MYS"), each=length(out[,1]))
  c$ID <- paste(c$Var, c$SlopeDay, sep="")
  c$Cow <- unique(testday$Cow)[i]
  a <- dcast(c, Cow~ID)
  if(i==1) {slp <- a} else{slp <- rbind(slp,a)}
}


#Merge the slopes into a smaller dataframe of all cow observations
testday <- merge(testday, slp, by="Cow")
# testday <- subset(testday, testday$Day==(0))

#Set up the data so that xgb can read it
testday$aDIM <- cut(testday$DIM,seq(0,350,by=50))
testday$aDIM <- as.factor(ifelse(is.na(testday$aDIM), '(300,350]', as.character(testday$aDIM)))


testday$Lactno <- as.numeric(testday$Lactno)
testday$aDIM <- as.numeric(testday$aDIM)

test = list(data = data.matrix(testday))
testday$GN_pred_xgb <- predict(bstDMatrix.neg, test$data)
testday$GP_pred_xgb <- predict(bstDMatrix.pos, test$data)

summary(testday$GN_pred_xgb)
summary(testday$GP_pred_xgb)

# I find the best cutoffs are GN = 0.0005, GP = 0.001
flagGN <- subset(testday, testday$GN_pred_xgb>0.01)
flagGP <- subset(testday, testday$GP_pred_xgb>0.001)

flagGN
flagGP






