create_binned_df <- function(df_wy) {
  df_pred_binned <- df_wy %>% 
    select(all_of(pred1_clim0), all_of(pred2_clim0), response) %>%
    mutate(pred1 = df_wy[[pred1_clim0]],
           pred2 = df_wy[[pred2_clim0]]) %>% 
    mutate(bin = as.numeric(ntile(pred2, 4)))
  
  return(df_pred_binned)
}