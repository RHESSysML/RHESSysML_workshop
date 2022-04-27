tune_rf_model <- function(df) {
  
  tuning <- vector(length = (ncol(df)-1))
  
  x = df[,-1]
  y = df[,1]
  
  for (i in 1:length(tuning)) {
    rf_tuning <- randomForest(x, y, mtry = i, ntree = 50)
    tuning[i] <- tail(rf_tuning$mse, 1)
  }
  
  return(tuning)
}