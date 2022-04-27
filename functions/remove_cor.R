remove_cor <- function(predictors.df, cor.threshold = 0.75, preference.order = pref_order0) {
  if (!(class(cor.threshold) %in% c("numeric"))) {
    stop("Correlation threshold must be numeric.")
  }
  if(cor.threshold < 0 | cor.threshold > 1) {
    stop("Correlation threshold must be between 0 and 1.")
  }
  
  variable.selection <- auto_cor(x = predictors.df,
                                 cor.threshold = cor.threshold,
                                 preference.order = preference.order) 
  
  return(variable.selection)
}