# Random under-sampling
.ru <- function(target, data, ir = 1)    # ir = Imbalance Ratio. (how many times majority instances are over minority instances)
{
  p <- data[which(data[ ,target] == "1"), ]
  n <- data[which(data[ ,target] == "0"), ]
  n <- n[sample(nrow(n), nrow(p) * ir, replace = TRUE), ]
  result <- rbind(p, n)
  return(result)
}

# Weight update/ pseudo-loss calculation for AdaBoost.M2
.wt.update <- function(probability, prediction, actual, wt, smooth)
{
  fp <- which(ifelse(prediction == "1" & actual == "0", TRUE, FALSE) == TRUE)
  fn <- which(ifelse(prediction == "0" & actual == "1", TRUE, FALSE) == TRUE)
  p_loss <- 0.5 * sum( wt[fp] * (1 - probability[fp, ][ ,"0"] + probability[fp, ][ ,"1"]),  # pseudo-loss
                       wt[fn] * (1 - probability[fn, ][ ,"1"] + probability[fn, ][ ,"0"]) )
  a <- (p_loss + smooth) / (1 - p_loss + smooth) # weight updater with prediction smoothing, dealing with a == 0
  wt[c(fp, fn)] <- rep(1/(length(fp) + length(fn)), (length(fp) + length(fn)))
  wt[fn] <- wt[fn] * a^(0.5 * (1 + probability[fn, ][ ,"1"] - probability[fn, ][ ,"0"]))
  wt[fp] <- wt[fp] * a^(0.5 * (1 + probability[fp, ][ ,"0"] - probability[fp, ][ ,"1"]))
  wt <- wt / sum(wt)
  result <- list()
  result[[1]] <- wt
  result[[2]] <- a
  return(result)
}

rus <- function (formula, data, size, ir = 1, rf.ntree = 50, svm.ker = "radial") 
{
    target <- as.character(as.formula(formula)[[2]])
    # data[, target] <- as.factor(data[, target])
    list_model <- list()
    a <- 0
    data$w <- rep(1/nrow(data), nrow(data))
    label <- data[, target]
    
    for (i in 1:size) {
        train <- .ru(target, data, ir)
        train$w <- train$w/sum(train$w)
        train <- train[sample(nrow(train), nrow(train), replace = TRUE, 
            prob = train$w), ]
        train$w <- NULL
        
        # Fit model
        list_model[[i]] <- rpart::rpart(formula, data = train,
                                        control = rpart.control(minbucket = 5))
        prob <- predict(list_model[[i]], data, type = "prob")
        
        pred <- as.factor(ifelse(prob[, "1"] >= 0.5, 1, 0))
        new <- .wt.update(probability = prob, prediction = pred, 
            actual = label, wt = data$w, smooth = 1/nrow(train))
        data$w <- new[[1]]
        a[i] <- new[[2]]
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
