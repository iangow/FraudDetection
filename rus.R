# Random under-sampling
rus <- function(y_train, wts, ir = 1) {
  # ir = Imbalance Ratio. (how many times majority instances are over minority instances)
  
  tab <- table(y_train)
  maj_class = ifelse(tab[2] >= tab[1], names(tab[2]), names(tab[1]))
  
  p <- which(y_train != maj_class)
  n <- sample(which(y_train == maj_class), length(p) * ir, replace = TRUE)
  rows <- c(p, n)
  w <- wts[rows]/sum(wts[rows])

  sample(rows, length(rows), replace = TRUE, prob = w)
}

w.update <- function(prob, prediction, actual, w, smooth, learn_rate = 1) {
  
  # Pseudo-loss calculation for AdaBoost.M2
  f <- which(prediction != actual)
  diff <- ifelse(actual[f] == "1", prob[f, "0"], prob[f, "1"])
  err <- sum( w[f] * diff)
  
  # Update weights with prediction smoothing, dealing with err == 0
  alpha <- learn_rate * (err + smooth) / (1 - err + smooth)
  w[f] <- rep(1/length(f), length(f)) * alpha^(1 - diff)
  
  # Scale weights
  w <- w / sum(w)
  
  return(list(w = w, alpha = alpha))
}

rusboost <- function(formula, data, size, ir = 1, learn_rate = 1,
                     control) {
    target <- as.character(as.formula(formula)[[2]])
    weakLearners <- list()
    alpha <- 0
    w <- rep(1/nrow(data), nrow(data))
    label <- data[, target]
    
    for (i in 1:size) {
      
        # Get training sample
        rows_final <- rus(data[[target]], w, ir)
        
        # Fit model
        fm <- rpart::rpart(formula, data = data[rows_final, ],
                           control = control)
        prob <- predict(fm, data, type = "prob")
        pred <- predict(fm, data, type = "class")
        
        # Get updated weights
        new <- w.update(prob = prob, prediction = pred, learn_rate = learn_rate,
                          actual = label, w = w, smooth = 1/length(rows_final))
        w <- new[["w"]]
        weakLearners[[i]] <- fm
        alpha[i] <- new[["alpha"]]
    }
    result <- list(weakLearners = weakLearners, alpha = alpha)
    attr(result, "class") <- "rusboost"
    return(result)
}

# Prediction for Boosting-based method
predict.rusboost <- function(object, newdata, type = "prob", ...) {
  models <- object[["weakLearners"]]
  
  predict_pos <- function(model) {
    predict(model, newdata, type = "prob")[, "1"]
  }
  
  probs <- lapply(models, predict_pos)
  
  alpha <- object[["alpha"]] 
  
  # Weight models
  prob <- rowSums(mapply("*", probs, alpha))
  
  if (type == "class") {
    pred <- as.factor(as.integer(prob > 0.5))
    return(pred)
  }
  else if (type == "prob") { 
    return(prob) 
  }
}
