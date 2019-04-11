library(smooth)
library(Mcomp)
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
library(rgl)


### Testing cutoffs
training_grampos <- train.complete[,c(59:77,7,15,30)]
training_gramneg <- train.complete[,c(59:77,7,15,29)]
training_gramneg <- training_gramneg[complete.cases(training_gramneg),]
training_grampos <- training_grampos[complete.cases(training_grampos),]

# Break into train and test
## 75% of the sample size
smp_size <- floor(0.70 * nrow(training_gramneg))
## set the seed to make your partition reproductible

train_ind <- sample(seq_len(nrow(training_gramneg)), size = smp_size)
training_gramneg <- training_gramneg[train_ind,]
test_gramneg <- training_gramneg[-train_ind,]
train_ind <- sample(seq_len(nrow(training_grampos)), size = smp_size)
training_grampos <- training_grampos[train_ind,]
test_grampos <- training_grampos[-train_ind,]

fit_glm_neg <- glm(as.numeric(GramNeg)-1 ~ .+aDIM*fat.resid+aDIM*Cond.resid+aDIM*dailyyield.resid+
            Weight*Cond.resid+Weight*Activity.resid, training_gramneg, family=binomial(link="logit"))
fit_glm_pos <- glm(as.numeric(GramPos)-1 ~ .+aDIM*fat.resid+aDIM*Cond.resid+aDIM*dailyyield.resid+
            Weight*Cond.resid+Weight*Activity.resid, training_grampos, family=binomial(link="logit"))

glm_response_scores <- predict(fit_glm_neg, training_gramneg, type="response")

# fit_glm2 <- glm(GramNeg ~ ., training_gramneg, family=binomial(link="logit"))
# 
glm_response_scores2 <- predict(fit_glm_pos, training_grampos, type="response")
# 
plot(roc(training_grampos$GramPos, glm_response_scores2),col=rgb(1, 0, 0, 0.4), lwd = 3)
plot(roc(training_gramneg$GramNeg, glm_response_scores),col=rgb(0, 1, 0, 0.4), lwd = 3)
# 


fit_neg <- adaboost(GramNeg~.,data=training_gramneg, 20)
plot(roc(training_gramneg$GramNeg, predict(fit_neg, training_gramneg)$prob[,2]), col=rgb(1, 0, 0, 0.4))
fit_pos <- adaboost(GramPos~.,data=train.combined.pos, 20)
plot(roc(training_grampos$GramPos, predict(fit_pos, training_grampos)$prob[,2]), col=rgb(1, 0, 0, 0.4))


preds <- predict(fit_neg, training_gramneg)$prob[,2]
p <- prediction(preds, training_gramneg$GramNeg)

perf <- performance(p,"tpr","fpr")
plot(perf)

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=T),]
head(subset(cutoffs, cutoffs$fpr<0.2))

