summarize_removed_vars <- function(clim, table = FALSE) {
  
  select_variables <- get(paste0("wy", clim, "_select_variables"))
  imp <- get(paste0("imp", clim))
  df <- get(paste0("df_wy", clim))
  all_preds.df <- df %>% 
    select(!response)
  
  
  removed_importance <- imp$finalModel["importance"] %>% 
    data.frame() %>% 
    rownames_to_column("variable") %>%
    rename("importance" = "importance..IncMSE") %>% 
    select(!importance.IncNodePurity) %>% 
    mutate("importance_rank" = rank(-importance)) %>% 
    mutate(selected = case_when(variable %in% select_variables ~ "selected",
                                !variable %in% select_variables ~ "removed")) %>% 
    relocate("selected", .after = "variable")
  
  
  # Calculating vif again - auto_vif() only returns values for selected variables
  removed_vif <- vif(all_preds.df)
  
  # joining dfs to create summary table of removed and selected variables
  removed_summary <- removed_importance %>% 
    left_join(removed_vif, by = "variable") %>% 
    filter(!(variable %in% factor_vars))
  
  if (table == TRUE) {
    
    removed_summary <- removed_summary %>% 
      datatable()
    
  }
  
  return(removed_summary)
  
}