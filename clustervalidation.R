searchlist <- list(cp = c(0.005, 0.001, 0.0005, 0.0002, 0.0001,
                   0.00005, 0.00002, 0.00001, 0.000005, 0.000002,
                   0.000001, 0.0000005, 0.0000002, 0.0000001, 0.00000005,
                   0.00000002, 0.00000001, 0.000000005, 0.000000002, 0.000000001),
                   maxdepth = 3:30,
                   minsplit = seq(1000, 3000, by = 100),
                   minbucket = seq(100, 500, by = 50))
controlstart <- list(minsplit = 2000, 
  cp = 0.0001,
  minbucket = 100,  
  maxdepth = 10)
control <- controlstart
for(param_name in names(controlstart)){
  param_name <- names(searchlist)[4]
  ParamEvalList <- data.frame()
  for(param_value in searchlist[[param_name]]){
    control <- controlstart
    control[[param_name]] <- param_value
    EvalList <- data.frame()
    for(i in 1:13){
      ind <- seq(1:13)
      ind <- ind[ind != i]
      valid <- data[yearmonth_index == i, ]
      train <- data[yearmonth_index %in% ind, ]
      tree <- rpart(class ~., data = train, control = control)
      predtree <- predict(tree, as.data.frame(valid))
      evaltree <- dfeval(predtree, valid$class)
      evaltree <- apply(evaltree, 2, function(col) {
        ifelse(sapply(col, is.nan), 0, col)
      })
      evaltree <- as.data.frame(evaltree)
      evaltree <- pred1count(evaltree, predtree, valid$class)
      treshold <- table(valid$class)[2] * 0.25
      prochrow <- evaltree %>% filter(pred1 > treshold) %>% slice(which.max(Precision))###temp row creation for clarity
      EvalList <- rbind(EvalList, as.data.frame((prochrow)))
    }
    ParamEvalList <- rbind(ParamEvalList, colMeans(EvalList))
    colnames(ParamEvalList) <- colnames(EvalList)
  }
  # Update the starting value for the current hyperparameter
  controlstart[[param_name]] <- searchlist[[param_name]][which.max(ParamEvalList[, "Precision"])]
})