
###Using logit on pca results with all data
training_grampos <- train.complete[,c(41:59,7,15,29:37,39)]
training_gramneg <- train.complete[,c(41:59,7,15,29:37,38)]
training_gramneg <- training_gramneg[complete.cases(training_gramneg),]
training_grampos <- training_grampos[complete.cases(training_grampos),]

pca.neg <- prcomp(training_gramneg[training_gramneg$GramNeg == 0,c(-19,-20,-31)],center = T, scale. = T, rank. = 10)
training_gramneg <- cbind(training_gramneg, predict(pca.neg, training_gramneg))

ideal <- class.ind(training_gramneg$GramNeg)
smp_size <- floor(0.75 * nrow(training_gramneg))

## set the seed to make your partition reproductible
set.seed(1)
train_ind <- sample(seq_len(nrow(training_gramneg)), size = smp_size)

training_gramneg <- training_gramneg[train_ind,]
test_gramneg <- training_gramneg[-train_ind,]


negANN <- nnet(training_gramneg[,c(-19,-20,-31)], ideal[train_ind,], size = 25,maxit = 100, softmax = T)
pp <- prediction( predict(negANN, test_gramneg[,c(-19,-20,-31)], type = 'raw')[,1],test_gramneg$GramNeg)
perf <- performance(pp,"tpr","fpr")
plot(perf)

fit_glm3 <- glm(GramNeg~.+aDIM*fat.pctmax+aDIM*Cond.pctmax+aDIM*dailyyield.pctmax+
                  Weight*Cond.pctmax+Weight*Activity.pctmax+aDIM*fat1+aDIM*Cond1+aDIM*dailyyield1+
                  Weight*Cond1+Weight*Activity1, data = training_gramneg, family = binomial())
preds <- predict(fit_glm3, training_gramneg, type = 'response')
lines(roc(training_gramneg$GramNeg, preds))


## Visualize PCA
train.complete <- train[complete.cases(train[,29:37]),]
train.complete$aDIM <- cut(train.complete$DIM,seq(0,300,by=50))

training_grampos <- train.complete[,c(2,5,68,15,7,29,30,32,33,37,27,39)]
training_gramneg <- train.complete[,c(2,5,68,15,7,29,30,32,33,37,27,38)]
training_gramneg <- training_gramneg[complete.cases(training_gramneg),]
training_grampos <- training_grampos[complete.cases(training_grampos),]


pca.neg1 <- prcomp(training_gramneg[training_gramneg$aDIM == '(0,50]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
pca.neg2 <- prcomp(training_gramneg[training_gramneg$aDIM == '(50,100]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
pca.neg3 <- prcomp(training_gramneg[training_gramneg$aDIM == '(100,150]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
pca.neg4 <- prcomp(training_gramneg[training_gramneg$aDIM == '(150,200]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
pca.neg5 <- prcomp(training_gramneg[training_gramneg$aDIM == '(200,250]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
pca.neg6 <- prcomp(training_gramneg[training_gramneg$aDIM == '(250,300]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
# pca.neg7 <- prcomp(training_gramneg[training_gramneg$aDIM == '(300,350]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 3)

predpca <- function(data) {
  output = data.frame()
  for(i in 1:nrow(data)) {
    output <- rbind(output,predict(pca.neg1, data[i,])*(data[i,]$aDIM == '(0,50]') +
                      predict(pca.neg1, data[i,])*(data[i,]$aDIM == '(50,100]') +
                      predict(pca.neg1, data[i,])*(data[i,]$aDIM == '(100,150]') +
                      predict(pca.neg1, data[i,])*(data[i,]$aDIM == '(150,200]') +
                      predict(pca.neg1, data[i,])*(data[i,]$aDIM == '(200,250]') +
                      predict(pca.neg1, data[i,])*(data[i,]$aDIM == '(250,300]'))
  }
  return(output)
}

pca.pos1 <- prcomp(training_grampos[training_grampos$aDIM == '(0,50]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
pca.pos2 <- prcomp(training_grampos[training_grampos$aDIM == '(50,100]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
pca.pos3 <- prcomp(training_grampos[training_grampos$aDIM == '(100,150]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
pca.pos4 <- prcomp(training_grampos[training_grampos$aDIM == '(150,200]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
pca.pos5 <- prcomp(training_grampos[training_grampos$aDIM == '(200,250]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
pca.pos6 <- prcomp(training_grampos[training_grampos$aDIM == '(250,300]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 6)
# pca.pos7 <- prcomp(training_grampos[training_grampos$aDIM == '(300,350]',c(-1,-2,-3,-5,-11,-12)], center = T, scale. = T, rank. = 3)

predpca <- function(data) {
  output = data.frame()
  for(i in 1:nrow(data)) {
    output <- rbind(output,predict(pca.pos1, data[i,])*(data[i,]$aDIM == '(0,50]') +
                      predict(pca.pos1, data[i,])*(data[i,]$aDIM == '(50,100]') +
                      predict(pca.pos1, data[i,])*(data[i,]$aDIM == '(100,150]') +
                      predict(pca.pos1, data[i,])*(data[i,]$aDIM == '(150,200]') +
                      predict(pca.pos1, data[i,])*(data[i,]$aDIM == '(200,250]') +
                      predict(pca.pos1, data[i,])*(data[i,]$aDIM == '(250,300]'))
  }
  return(output)
}
### Make a function that calculates euclidean distance in n dim

eudis <- function(data, n) {
  cols <- data[,c(paste(rep('PC',n),seq(1,n),sep = ''))]
  d = data.frame()
  for(i in 1:n) {
    d <- rbind(d,(lag(cols[,i])-cols[,i])^2)
  }
  d <- t(d)
  d <- cbind(d,sqrt(rowSums(d)))
  return(d[,n+1])
}

distocenter <- function(data, n) {
  cols <- data[,c(paste(rep('PC',n),seq(1,n),sep = ''))]
  d = data.frame()
  for(i in 1:n) {
    d <- rbind(d,(0-cols[,i])^2)
  }
  d <- t(d)
  d <- cbind(d,sqrt(rowSums(d)))
  return(d[,n+1])
}



