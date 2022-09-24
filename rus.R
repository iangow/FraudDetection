# Random under-sampling
.ru <- function(y_train, wts, ir = 1) {
  # ir = Imbalance Ratio. (how many times majority instances are over minority instances)
  p <- which(y_train == "1")
  n <- sample(which(y_train == "0"), length(p) * ir, replace = TRUE)
  rows <- c(p, n)
  w <- wts[rows]/sum(wts[rows])

  sample(rows, length(rows), replace = TRUE, prob = w)
}

D.update <- function(prob, prediction, actual, D, smooth) {
  
  # Pseudo-loss calculation for AdaBoost.M2
  fp <- which(prediction == "1" & actual == "0")
  fn <- which(prediction == "0" & actual == "1")
  f <- which(prediction != actual)
  p_diff <- 2 * prob[fp, "1"] - 1 
  n_diff <- 2 * prob[fn, "0"] - 1

  # pseudo-loss
  p_loss <- 0.5 * sum( D[fp] * (1 + p_diff),  
                       D[fn] * (1 + n_diff))
  
  # Weight updater with prediction smoothing, dealing with a == 0
  beta <- (p_loss + smooth) / (1 - p_loss + smooth)
  D[f] <- rep(1/length(f), length(f))
  w_fn <- 0.5 * (1 - n_diff)
  w_fp <- 0.5 * (1 - p_diff)
  
  D[fn] <- D[fn] * beta^w_fn
  D[fp] <- D[fp] * beta^w_fp
  D <- D / sum(D)
  return(list(D = D, beta = beta))
}

rusboost <- function(formula, data, size, ir = 1) {
    target <- as.character(as.formula(formula)[[2]])
    weakLearners <- list()
    beta <- 0
    D <- rep(1/nrow(data), nrow(data))
    label <- data[, target]
    
    for (i in 1:size) {
      
        # Get training sample
        rows_final <- .ru(data[[target]], D, ir)
        
        # Fit model
        fm <- rpart::rpart(formula, data = data[rows_final, ],
                           control = rpart.control(minbucket = 5))
        prob <- predict(fm, data, type = "prob")
        pred <- predict(fm, data, type = "class")
        
        # Get updated weights
        new <- D.update(prob = prob, prediction = pred, 
                          actual = label, D = D, smooth = 1/length(rows_final))
        D <- new[["D"]]
        weakLearners[[i]] <- fm
        beta[i] <- new[["beta"]]
    }
    result <- list(weakLearners = weakLearners, beta = beta)
    attr(result, "class") <- "rusboost"
    return(result)
}

# Prediction for Boosting-based method
predict.rusboost <- function(object, newdata, type = "prob", learn_rate = 0.1, ...) {
  models <- object[["weakLearners"]]
  
  predict_pos <- function(model) {
    predict(model, newdata, type = "prob")[, "1"]
  }
  
  probs <- lapply(models, predict_pos)
  
  beta <- object[["beta"]] 
  beta <- beta * learn_rate ^ (0:(length(beta)-1))
  # normalize beta values
  beta <- log(1/beta) / sum(log(1/beta))
  
  # Weight models
  prob <- rowSums(mapply("*", probs, beta))
  
  if (type == "class") {
    pred <- as.factor(as.integer(prob > 0.5))
    return(pred)
  }
  else if (type == "prob") { 
    return(prob) 
  }
}
