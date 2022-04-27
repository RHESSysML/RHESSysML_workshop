plot_removed_imp <- function(clim) {
  
  removed_imp_plot <- ggplot(summarize_removed_vars(clim = clim),
                             aes(x = importance, y = reorder(variable, importance), fill = selected)) +
    geom_col() + labs(x = "Preliminary Importance", y = "Variable") +
    theme_light()
  
  return(removed_imp_plot)
  
}