# Random under-sampling
.ru <- function(target, data, ir = 1) {
  # ir = Imbalance Ratio. (how many times majority instances are over minority instances)
  p <- data[which(data[ ,target] == "1"), ]
  n <- data[which(data[ ,target] == "0"), ]
  n <- n[sample(nrow(n), nrow(p) * ir, replace = TRUE), ]
  train <- rbind(p, n)
  w <- train$w/sum(train$w)
  train <- train[sample(nrow(train), nrow(train), replace = TRUE, 
            prob = w), ]
  train$w <- NULL
  return(train)
}

.wt.update <- function(probability, prediction, actual, wt, smooth) {
  
  # Weight update/ pseudo-loss calculation for AdaBoost.M2
  fp <- which(prediction == "1" & actual == "0")
  fn <- which(prediction == "0" & actual == "1")
  f_len <- length(fp) + length(fn)
  p_fp0 <- probability[fp, ][ ,"0"]
  p_fp1 <- probability[fp, ][ ,"1"]
  p_fn0 <- probability[fn, ][ ,"0"]
  p_fn1 <- probability[fn, ][ ,"1"]
  
  p_loss <- 0.5 * sum( wt[fp] * (1 - p_fp0 + p_fp1),  # pseudo-loss
                       wt[fn] * (1 - p_fn1 + p_fn0))
   # weight updater with prediction smoothing, dealing with a == 0
  a <- (p_loss + smooth) / (1 - p_loss + smooth)
  wt[c(fp, fn)] <- rep(1/f_len, f_len)
  wt[fn] <- wt[fn] * a^(0.5 * (1 + p_fn1 - p_fn0))
  wt[fp] <- wt[fp] * a^(0.5 * (1 + p_fp0 - p_fp1))
  wt <- wt / sum(wt)
  return(list(w = wt, a = a))
}

rus <- function (formula, data, size, ir = 1, rf.ntree = 50, svm.ker = "radial") 
{
    target <- as.character(as.formula(formula)[[2]])
    list_model <- list()
    a <- 0
    data$w <- rep(1/nrow(data), nrow(data))
    label <- data[, target]
    
    for (i in 1:size) {
      
        # Get training sample
        train <- .ru(target, data, ir)
        
        # Fit model
        fm <- rpart::rpart(formula, data = train,
                                        control = rpart.control(minbucket = 5))
        prob <- predict(fm, data, type = "prob")
        pred <- predict(fm, data, type = "class")
        
        # Get updated weights
        new <- .wt.update(probability = prob, prediction = pred, 
                          actual = label, wt = data$w, smooth = 1/nrow(train))
        data$w <- new[["w"]]
        list_model[[i]] <- fm
        a[i] <- new[["a"]]
    }
    result <- list(weakLearners = list_model, errorEstimation = a)
    attr(result, "class") <- "modelBst"
    return(result)
}

# Prediction for Boosting-based method
predict.modelBst <- function(object, newdata, type = "prob", ...)
{
  list_model <- object[[1]]
  a <- object[[2]] 
  a <- a * 0.1 ^ (0:(length(a)-1))
  # normalize alpha values into percentage
  a <- log(1/a) / sum(log(1/a)) 
  prob <- lapply(lapply(list_model, predict, newdata, type = "prob"), subset, select = "1")
  prob <- rowSums(mapply("*", prob, a))
  if(type == "class") {
    pred <- as.factor(ifelse(prob > 0.5, 1, 0))
    return(pred)
  }
  else if(type == "prob") { return(prob) }
}
