get_important_predictors <- function(df_imp, rank) {
  df_imp$Variable[df_imp$Rank == rank]
}