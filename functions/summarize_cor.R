summarize_cor <- function(clim, selected_removed) {
  
  num_preds.df <-  get(paste0("df_wy", clim, "_num_preds"))
  cor_matrix <- cor(num_preds.df)
  select_variables <- get(paste0("wy", clim, "_select_variables"))
  
  cor.df <- reshape2::melt(cor_matrix)
  cor.df <- cor.df %>% 
    filter(Var1 != Var2)
  dups <- duplicated(sort(cor.df$value))
  cor.df <- cor.df[!dups, ] %>% 
    rename("correlation" = "value") %>% 
    mutate(var1_selected = case_when(Var1 %in% select_variables ~ "selected",
                                     !Var1 %in% select_variables ~ "removed")) 
  
  if (selected_removed == "selected") {
    cor.df <- cor.df %>% 
      filter(var1_selected == "selected")
  }
  
  if (selected_removed == "removed") {
    cor.df <- cor.df %>% 
      filter(var1_selected == "removed")
  }
  
  cor.df <- cor.df[order(cor.df$Var1), ]
  
  cor.table <- cor.df %>%
    mutate_if(is.numeric, round, 4) %>% 
    datatable(options = list())
  
  return(cor.table)
  
}