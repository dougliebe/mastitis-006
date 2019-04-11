library(xgboost)
train.complete <- train.all
# XGBoost
train.complete$Lactno <- as.numeric(train.complete$Lactno)
train.complete$aDIM <- as.numeric(train.complete$aDIM)

# train.set <- train.complete[, c(7,15,30,29,32:59)]
# pair.set <- pairs.complete[, c(7,9,35,36,37:63,82)]
all <- train.complete
# all <- all[,c(1:4,32:50)]


### Negative training

sample <- sample.int(n = nrow(all), size = floor(.75*nrow(all)), replace = F)
train = list(data = data.matrix(all[sample, c(4,8:26)]),#[,c(3,1,2,16,19,13,7)]),
             label = as.numeric(as.character(all[sample,]$GramNeg)))
test = list(data = data.matrix(all[-sample, c(4,8:26)]),#[,c(3,1,2,16,19,13,7)]),
            label = as.numeric(as.character(all[-sample,]$GramNeg)))
dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)
bstDMatrix <- xgboost(data = dtrain,
                      max.depth = 100,
                      eta = 0.5,
                      nthread = 4,
                      nround = 40,
                      error = 'error',
                      scale_pos_weight = 1,
                      objective = "binary:logistic", verbose = 0)
pred <- predict(bstDMatrix, test$data)
# plot(roc(test$label, pred),col=rgb(0, 1, 1, 0.4), lwd = 3)
# 
xgbpred <- pred
xgbpred <- ifelse(xgbpred > 0.075,1,0)
outputs <- rbind(outputs, confusionMatrix(as.factor(xgbpred), as.factor(test$label), positive = "1")$byClass[1:2])

confusionMatrix(as.factor(xgbpred), as.factor(test$label), positive = "1")
confusionMatrix(as.factor(xgbpred), as.factor(test$label), positive = "1")$byClass[1:2]
p <- prediction(pred, test$label)

perf <- performance(p,"tpr","fpr")
plot(perf)

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs$tot <- cutoffs$tpr/(cutoffs$fpr)
cutoffs <- cutoffs[order(cutoffs$tot, decreasing=T),]
head(subset(cutoffs, cutoffs$tpr> 0.5))
head(subset(cutoffs, cutoffs$fpr< 0.02))

importance_matrix <- xgb.importance(model = bstDMatrix)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


## Positive trianing

sample <- sample.int(n = nrow(all), size = floor(0.75*nrow(all)), replace = F)
train = list(data = data.matrix(all[sample, c(4,8:26)]),#[,c(3,1,2,15,9,13,18)]),
             label = as.numeric(as.character(all[sample,]$GramPos)))
test = list(data = data.matrix(all[-sample, c(4,8:26)]),#[,c(3,1,2,15,9,13,18)]),
            label = as.numeric(as.character(all[-sample,]$GramPos)))
dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)
bstDMatrix.pos <- xgboost(data = dtrain,
                          max.depth = 100,
                          eta = 0.5,
                          nthread = 4,
                          nround = 40,
                          scale_pos_weight = 1,
                          error = 'auc',
                          objective = "binary:logistic", verbose = 0)
pred <- predict(bstDMatrix.pos, test$data)
# plot(roc(test$label, pred),col=rgb(0, 0, 0, 0.4), lwd = 3)
xgbpred <- pred
xgbpred <- ifelse(xgbpred > 0.15,1,0)

confusionMatrix(as.factor(xgbpred), as.factor(test$label), positive = "1")
confusionMatrix(as.factor(xgbpred), as.factor(test$label), positive = "1")$byClass[1:2]
outputs <- rbind(outputs, confusionMatrix(as.factor(xgbpred), as.factor(test$label), positive = "1")$byClass[1:2])
p <- prediction(pred, test$label)


perf <- performance(p,"tpr","fpr")
plot(perf)

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs$tot <- cutoffs$tpr/(cutoffs$fpr)
cutoffs <- cutoffs[order(cutoffs$tot, decreasing=T),]
head(subset(cutoffs, cutoffs$tpr> 0.5))

importance_matrix <- xgb.importance(model = bstDMatrix.pos)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)








# Predict on the whoel trianing set to look at dist and weights

train.all$Lactno <- as.numeric(train.all$Lactno)
train.all$aDIM <- as.numeric(train.all$aDIM)

test = list(data = data.matrix(train.all))
train.all$GN_pred <- predict(bstDMatrix, test$data)
train.all$GP_pred <- predict(bstDMatrix.pos, test$data)



################ Training cases where both have high prob ################

train.disagree <- subset(train.all, train.all$GN_pred > 0.9 & train.all$GP_pred > 0.9)
train.disagree$Case <- ifelse(train.disagree$GramNeg == 1, 1, 0)

sample <- sample.int(n = nrow(train.disagree), size = floor(.75*nrow(train.disagree)), replace = F)
train = list(data = data.matrix(train.disagree[sample, c(1:5,8:53)]),
             label = as.numeric(as.character(train.disagree[sample,]$GramPos)))
test = list(data = data.matrix(train.disagree[-sample, c(1:5,8:53)]),
            label = as.numeric(as.character(train.disagree[-sample,]$GramPos)))
dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)
bstDMatrix <- xgboost(data = dtrain,
                      max.depth = 2,
                      eta = 0.5,
                      nthread = 4,
                      nround = 40,
                      scale_pos_weight = 5000,
                      objective = "binary:logistic")
pred <- predict(bstDMatrix, test$data)
plot(roc(test$label, pred),col=rgb(0, 1, 1, 0.4), lwd = 3)
p <- prediction(pred, test$label)

perf <- performance(p,"tpr","fpr")
plot(perf)

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=T),]
head(subset(cutoffs, cutoffs$fpr<0.2))

importance_matrix <- xgb.importance(model = bstDMatrix)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)