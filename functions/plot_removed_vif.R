plot_removed_vif <- function(clim) {
  
  removed_vif_plot <- ggplot(summarize_removed_vars(clim = clim),
                             aes(x = vif, y = reorder(variable, vif), fill = selected)) + 
    geom_col() + labs(x = "Variable Inflation Factor", y = "Variable") +
    theme_light()
  
  return(removed_vif_plot)
  
}