create_binned_plot <- function(df_binned) {
  df_name <- deparse(substitute(df_binned))
  if (df_name == "df0_pred_binned") {
    p1 = pred1_clim0
    p2 = pred2_clim0
    scen = "0"
  }
  if (df_name == "df2_pred_binned") {
    p1 = pred1_clim2
    p2 = pred2_clim2
    scen = "2"
  }
  
  ggplot(df_binned, aes(x = pred1, y = .data[["response"]])) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap("bin") +
    labs(x = str_to_title(pred1_clim0), y = response_var,
         title = paste(p1, "vs", response_var, "given different value classes of",
                       p2, "\n in", scen, "degree climate scenario")) +
    theme_minimal()
}