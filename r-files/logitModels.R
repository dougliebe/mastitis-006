

## Gram Positive Logit 

# Look at models using only slopes and characteristics
training_grampos <- train.complete[,c(59,15,7,41:58,39)]
training_gramneg <- train.complete[,c(59,15,7,41:58,38)]
training_gramneg <- training_gramneg[complete.cases(training_gramneg),]
training_grampos <- training_grampos[complete.cases(training_grampos),]

fit_glm <- glm(as.factor(GramPos) ~ .+aDIM*FS1+aDIM*FS2+aDIM*FS3+
                 aDIM*CS1+aDIM*CS2+aDIM*CS3+aDIM*MYS1+aDIM*MYS2+aDIM*MYS3, all[sample, c(4,7,8:26)], family=binomial(link="logit"))

glm_response_scores <- predict(fit_glm, all[sample, c(4,7,8:26)], type="response")

plot(roc(as.factor(all[sample, c(4,7,8:26)]$GramPos), glm_response_scores))
x <- roc(training_grampos$GramPos, glm_response_scores)


## Train some models using only the pctmax data w/ charac
training_grampos <- train.complete[,c(2,59,15,7,29:37,39)]
training_gramneg <- train.complete[,c(2,59,15,7,29:37,38)]
training_gramneg <- training_gramneg[complete.cases(training_gramneg),]
training_grampos <- training_grampos[complete.cases(training_grampos),]

fit_glm2 <- glm(GramPos ~ .+aDIM*fat.pctmax+aDIM*Cond.pctmax+aDIM*dailyyield.pctmax+Weight*Cond.pctmax+Weight*Activity.pctmax, training_grampos, family=binomial(link="logit"))

glm_response_scores2 <- predict(fit_glm2, training_grampos, type="response")

plot(roc(training_grampos$GramPos, glm_response_scores2))
y <- roc(training_grampos$GramPos, glm_response_scores2)
lines(x, col = 'red')

legend("bottomright", inset=.05, title="Logit Models",
       c("Old G+",'Old G-', 'New G+', 'New G-'), col=c('black','black', 'red','red'),lty = c(1,2,1,2),lwd = 2, horiz=F)
# choose cutoff 
pred <- prediction(predict(fit_glm2,training_grampos), training_grampos$GramPos)
perf <- performance(pred,"tpr","fpr")

########## Gram Neg #######

# Look at models using only slopes and characteristics
training_grampos <- train.complete[,c(59,15,7,41:58,39)]
training_gramneg <- train.complete[,c(59,15,7,41:58,38)]
training_gramneg <- training_gramneg[complete.cases(training_gramneg),]
training_grampos <- training_grampos[complete.cases(training_grampos),]

fit_glm <- glm(as.factor(GramNeg) ~ .+aDIM*FS1+aDIM*FS2+aDIM*FS3+
                 aDIM*CS1+aDIM*CS2+aDIM*CS3+aDIM*MYS1+aDIM*MYS2+aDIM*MYS3, all[sample, c(4,6,8:26)], family=binomial(link="logit"))

glm_response_scores <- predict(fit_glm, all[sample, c(4,6,8:26)], type="response")

plot(roc(as.factor(all[sample, c(4,6,8:26)]$GramNeg), glm_response_scores))

z <- roc(training_gramneg$GramNeg, glm_response_scores)


## Train some models using only the pctmax data w/ charac
training_grampos <- train.complete[,c(59,15,7,29:37,39)]
training_gramneg <- train.complete[,c(59,15,7,29:37,38)]
training_gramneg <- training_gramneg[complete.cases(training_gramneg),]
training_grampos <- training_grampos[complete.cases(training_grampos),]






# Look at models using only slopes and characteristics
training_grampos <- train.complete[,c(59,15,7,41:58,39)]
training_gramneg <- train.complete[,c(59,15,7,41:58,38)]
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

fit_glm_neg <- glm(as.numeric(GramNeg)-1 ~ .+aDIM*fat1+aDIM*Cond1+aDIM*dailyyield1+
                     Weight*Cond1+Weight*Activity1+aDIM*fat2+aDIM*Cond2+aDIM*dailyyield2+
                     Weight*Cond2+Weight*Activity2, train.combined, family=binomial(link="logit"))
fit_glm_pos <- glm(as.numeric(GramPos)-1 ~ .+aDIM*fat1+aDIM*Cond1+aDIM*dailyyield1+
                     Weight*Cond1+Weight*Activity1+aDIM*fat2+aDIM*Cond2+aDIM*dailyyield2+
                     Weight*Cond2+Weight*Activity2, train.combined.pos, family=binomial(link="logit"))

glm_response_scores <- predict(fit_glm_neg, training_gramneg, type="response")

# fit_glm2 <- glm(GramNeg ~ ., training_gramneg, family=binomial(link="logit"))
# 
glm_response_scores2 <- predict(fit_glm_pos, training_grampos, type="response")
# 
plot(roc(training_grampos$GramPos, glm_response_scores2),col=rgb(1, 0, 0, 0.4), lwd = 3)
plot(roc(training_gramneg$GramNeg, glm_response_scores),col=rgb(0, 1, 0, 0.4), lwd = 3)
# 
