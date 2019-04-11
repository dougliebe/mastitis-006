cost <- function(Se, Sp, pct_pos) {
  return(((pct_pos/100)/(1-pct_pos/100)*(Se/100)/(1-(Sp/100))*132)-165)
}

y <- roc(as.factor(test$label),pred)
x <- cost(Se = y$sensitivities*100, Sp = y$specificities*100, 0.3)
plot(y$thresholds, x, ylab = "Savings, per cow")
