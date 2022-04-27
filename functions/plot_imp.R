plot_imp <- function(varimp_df) {
  df_name <- deparse(substitute(varimp_df))
  ggplot(data = varimp_df, aes(x = Overall, y = reorder(Variable, Overall))) +
    geom_col() +
    theme_light() +
    theme(legend.position = "none",
          axis.text.x = element_blank()) +
    labs(title = paste0("Variable Importance for \n climate scenario ", str_sub(df_name, -length(df_name))),
         x = "Importance",
         y = "Variable")
}