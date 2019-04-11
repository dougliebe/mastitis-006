setwd('C:/Users/Doug/Downloads')
testday <- read.csv('Uk-Coldstream Afi daily data.csv')


#Rename/make some variables
testday$aDIM <- cut(testday$DIM,seq(0,300,by=50))
testday$aDIM <- as.factor(ifelse(is.na(testday$aDIM), '(250,300]', as.character(testday$aDIM)))

#Fix data types
testday$Weight1 <- as.numeric(as.character(testday$Weight1))
testday$Weight2 <- as.numeric(as.character(testday$Weight2))
testday$FAT1 <- as.numeric(as.character(testday$FAT1))
testday$FAT2 <- as.numeric(as.character(testday$FAT2))
testday$PRO1 <- as.numeric(as.character(testday$PRO1))
testday$PRO2 <- as.numeric(as.character(testday$PRO2))
testday$LAC1 <- as.numeric(as.character(testday$LAC1))
testday$LAC2 <- as.numeric(as.character(testday$LAC2))
testday$MY1 <- as.numeric(as.character(testday$MY1))
testday$MY2 <- as.numeric(as.character(testday$MY2))
testday$COND1 <- as.numeric(as.character(testday$COND1))
testday$COND2 <- as.numeric(as.character(testday$COND2))
testday$Activity1 <- as.numeric(as.character(testday$Activity1))
testday$Activity2 <- as.numeric(as.character(testday$Activity2))
testday$RestTime <- as.numeric(as.character(testday$RestTime))
testday$RestDur <- as.numeric(as.character(testday$RestDur))
testday$RestBout <- as.numeric(as.character(testday$RestBout))
#testday$Fat <- as.numeric(as.character(testday$Fat))
#testday$Protein <- as.numeric(as.character(testday$Protein))
#testday$Lactose <- as.numeric(as.character(testday$Lactose))
#testday$MY <- as.numeric(as.character(testday$MY))
#testday$Cond <- as.numeric(as.character(testday$Cond))
#testday$Act <- as.numeric(as.character(testday$Act))
testday$SCC1 <- as.character(testday$SCC1)
testday$SCC_1 <- ifelse(testday$SCC1=="0-200", 100, 
                        ifelse(testday$SCC1=="200-400", 300,
                               ifelse(testday$SCC1=="400-800", 600, 
                                      ifelse(testday$SCC1=="800+", 1200, NA))))
testday$SCC2 <- as.character(testday$SCC2)
testday$SCC_2 <- ifelse(testday$SCC2=="0-200", 100, 
                        ifelse(testday$SCC2=="200-400", 300,
                               ifelse(testday$SCC2=="400-800", 600, 
                                      ifelse(testday$SCC2=="800+", 1200, NA))))
testday$SCC <- ifelse(is.na(testday$SCC_1)==FALSE & is.na(testday$SCC_2)==FALSE, (testday$SCC_1+testday$SCC_2)/2,
                      ifelse(is.na(testday$SCC_1)==TRUE & is.na(testday$SCC_2)==FALSE, testday$SCC_2,
                             ifelse(is.na(testday$SCC_1)==FALSE & is.na(testday$SCC_2)==TRUE, testday$SCC_1, NA)))

testday$SCC <- testday$SCC*1000
testday$SCC <- ifelse(testday$SCC==0, NA, testday$SCC)
testday$SCC <- log(testday$SCC)

vars = c('lactose','fat','protein','Cond','Activity','TotalRestTime','RestDur','DailyRestBout',
         'dailyyield')


#combine session 1 and 2 data 
testday$Cow <- as.character(testday$Cow)
testday$Date <- as.character(testday$Date)
testday$cowdate <- paste(testday$Cow, testday$Date, sep="")

testday$Weight <- (testday$Weight1+testday$Weight2)/2
testday$fat <- (testday$FAT1 + testday$FAT2)/2
testday$dailyyield <- (testday$MY1+testday$MY2)
testday$dailyyield <- (testday$dailyyield*0.453592)
testday$protein <- (testday$PRO1+testday$PRO2)/2
testday$lactose <- (testday$LAC1+testday$LAC2)/2
testday$Cond <- (testday$COND1+testday$COND2)/2
testday$Activity <- (testday$Activity1+testday$Activity2)/2
testday <- subset(testday, !is.na(testday$Activity) & testday$Activity < 1000)
colnames(testday)[23] <- 'TotalRestTime'
colnames(testday)[25] <- 'DailyRestBout'
testday <- testday[,c(1,2,3,4,5,8,23,24,25,26,29:37)]
# Means and sds of training set to scale new data
mm <- c(1.994,3.994,3.017,9.413,124,727.7,71.938,11.62,33.692,1363.7)
ss <- c(0.7453431,0.6767803,0.5192338,0.9061494,55.0419,150.4106,28.71789,11.78295,11.14985,244.8796)



# Scale all the vars of interest
# testday$lactose <- (testday$lactose - mm[1])/ss[1]
# testday$fat <- (testday$fat - mm[2])/ss[2]
# testday$protein <- (testday$protein - mm[3])/ss[3]
# testday$Cond <- (testday$Cond - mm[4])/ss[4]
# testday$Activity <- (testday$Activity - mm[5])/ss[5]
# testday$TotalRestTime <- (testday$TotalRestTime - mm[6])/ss[6]
# testday$RestDur <- (testday$RestDur - mm[7])/ss[7]
# testday$DailyRestBout <- (testday$DailyRestBout - mm[8])/ss[8]
# testday$dailyyield <- (testday$dailyyield - mm[9])/ss[9]
# testday$Weight <- (testday$Weight - mm[10])/ss[10]
testday[,c(vars,"Weight")] <- scale(testday[,c(vars,"Weight")])
testday$Lactno <- as.factor(ifelse(as.numeric(testday$Lactno) > 2, 1, 0))
testday$afidate <- as.numeric(as.Date(testday$Date, "%d/%m/%Y"))
today <- max(testday$afidate,na.rm = T)
# testday <- subset(testday, testday$afidate >= max(testday$afidate)-15)

# Add the seven days out values
get.cow.value <- function(cowID, date) {
  data = testday
  vars = c('lactose','fat','protein','Cond','Activity','TotalRestTime','RestDur','DailyRestBout',
           'dailyyield')
  output = data.frame()
  for(var in vars) {
    for(i in c(1,2,7)){
      
      output = rbind(output,ifelse(length(data[data$Cow == cowID & data$afidate == (date-i),
                                               var])==0,NA,data[data$Cow == cowID & data$afidate == (date-i),
                                                                var]), make.row.names = F)
      
    }}
  return(t(output))
}

testday <- cbind(testday, matrix(rep(0,27*nrow(testday)),ncol = 27))
#Cow list
for(i in 1:nrow(testday)) {
  testday[i,(ncol(testday)-26):(ncol(testday))] <- get.cow.value(testday$Cow[i], testday$afidate[i])
}
colnames(testday)[(ncol(testday)-26):(ncol(testday))] <- paste(rep(vars[seq(1,9)],each = 3),c(1,2,7), sep = "")

# Change 7 day values to slopes
for(j in 1:9) {
  for(k in 1:2) {
    daySeven = testday[,21+(3*(j-1))+(2)]
    testday[,21+(3*(j-1))+(k-1)] <- (daySeven - testday[,21+(3*(j-1))+(k-1)])/(7-k)
  }
}

# testday <- testday[complete.cases(testday[,37:72]),]

# truncate values
for(i in 1:26) {
  testday[,21+(i-1)] <- testday[,21+(i-1)]*(testday[,21+(i-1)] <= 1 & testday[,21+(i-1)] >= -1) + 
    ((testday[,21+(i-1)]< -1)*-1)+(testday[,21+(i-1)]>1)*1
}

# # Don't need the actual day 7 values
# testday <- testday[,-which(names(testday) %in% c('lactose7','protein7','fat7','Cond7',"Activity7", "TotalRestTime7","RestDur7","DailyRestBout7","dailyyield7"))]

# Change day7 values + slopes into predicted d0 values
testday <- cbind(testday, matrix(rep(0,9*nrow(testday)),ncol = 9))
for(j in 1:9) {
  daySeven = testday[,21+(3*(j-1))+(2)]
  slope <- testday[,21+(3*(j-1))+(1-1)]
  testday[,ncol(testday)-9+(j)] <- daySeven+(slope*7)
}

colnames(testday)[(ncol(testday)-8):(ncol(testday))] <- paste(rep(vars[seq(1,9)],each = 1),'.exp', sep = "")

# Add residuals for d0 values
testday <- cbind(testday, matrix(rep(0,9*nrow(testday)),ncol = 9))
for(j in 1:9) {
  exp_value = testday[,ncol(testday)-17+(j-1)]
  true_value <- testday[,which(colnames(testday) == vars[j])]
  testday[,ncol(testday)-9+(j)] <- exp_value-true_value
}
colnames(testday)[(ncol(testday)-8):(ncol(testday))] <- paste(rep(vars[seq(1,9)],each = 1),'.resid', sep = "")


testday$GN_pred_ada <- predict(fit_neg, testday)$prob[,2]
testday$GN_pred_glm <- predict(fit_glm_neg, testday, type = 'response')
testday$GP_pred_ada <- predict(fit_pos, testday)$prob[,2]
testday$GP_pred_glm <- predict(fit_glm_pos, testday, type = 'response')

testday$Lactno <- as.numeric(testday$Lactno)
testday$aDIM <- as.numeric(testday$aDIM)

test = list(data = data.matrix(testday))
testday$GN_pred_xgb <- predict(bstDMatrix, test$data)
testday$GN_1 <- ifelse(testday$GN_pred_xgb > 0.16,1,0)
testday$GP_pred_xgb <- predict(bstDMatrix.pos, test$data)
# 
# # testday <- subset(testday, testday$afidate == today)
ggplot(testday[testday$Cow %in% unique(testday[testday$GN_1 == 1,]$Cow),],
       aes(DIM, dailyyield, color = Cow)) + geom_line()
# 
# ggplot(testday, aes(x = DIM, y = TotalRestTime)) + xlim(c(200,218)) +
#   # Calculate the mean based on y, set geom = line
#   stat_summary(fun.y = "mean", colour = "red", size = 0.25, geom = "line") +
#   geom_line(size = 1, data = testday[testday$Cow %in% unique(testday[!is.na(testday$GN_pred_glm) & testday$GN_pred_ada > 0.5,]$Cow),], aes(x = DIM, y = TotalRestTime, col = Cow))

