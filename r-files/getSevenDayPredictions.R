#Use all data to predict GN and GP then get 7 day values


library(xgboost)

#Read in the models
bstDMatrix.neg <- xgb.load("GN_pred.model")
bstDMatrix.pos <- xgb.load("GP_pred.model")

train.complete <- train.all
# XGBoost
train.complete$Lactno <- as.numeric(train.complete$Lactno)
train.complete$aDIM <- as.numeric(train.complete$aDIM)

# train.set <- train.complete[, c(7,15,30,29,32:59)]
# pair.set <- pairs.complete[, c(7,9,35,36,37:63,82)]
all <- train.complete
# all <- all[,c(1:4,32:50)]


### Negative training

train = list(data = data.matrix(all[, c(3,4,7:25)][,c(3,1,2,16,19,13,7)]),
             label = as.numeric(as.character(all$GramNeg)))
all$GN_pred <- predict(bstDMatrix.neg, train$data)

train = list(data = data.matrix(all[, c(3,4,7:25)]),
             label = as.numeric(as.character(all$GramPos)))
all$GP_pred <- predict(bstDMatrix.pos, train$data)

# Add the seven days out values
get.cow.value <- function(cowID, date) {
  data = all
  vars = c('GN_pred', 'GP_pred')
  output = data.frame()
  for(var in vars) {
    for(i in 1:7){
      
      output = rbind(output,ifelse(length(data[data$Cow == cowID & data$afidate == (date-i),
                              var])==0,NA,data[data$Cow == cowID & data$afidate == (date-i),
                              var]), make.row.names = F)
      
    }}
  return(t(output))
}

all <- cbind(all, matrix(rep(0,14*nrow(all)),ncol = 14))
#Cow list
for(i in 1:nrow(all)) {
  all[i,(ncol(all)-13):(ncol(all))] <- get.cow.value(all$Cow[i], all$afidate[i])
}
colnames(all)[(ncol(all)-13):(ncol(all))] <- paste(rep(vars[seq(1,2)],each = 7),c(1,2,3,4,5,6,7), sep = "")
