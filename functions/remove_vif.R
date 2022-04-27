# Remove variables based on VIF and Correlation thresholds
remove_vif <- function(predictors.df, vif.threshold = 5, preference.order = pref_order0) {
  if (!(class(vif.threshold) %in% c("numeric"))) {
    stop("VIF threshold must be numeric.")
  }
  if (vif.threshold < 1) {
    stop("VIF threshold must be greater than or equal to 1.")
  }
  
  variable.selection <- auto_vif(x = predictors.df,
                                 vif.threshold = vif.threshold,
                                 preference.order = preference.order) 
  
  return(variable.selection)
}