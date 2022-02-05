
# Setup -------------------------------------------------------------------

options(scipen=999)

## Standard packages
library(tidyverse)
library(here)
library(patchwork)
library(psych)
library(kableExtra)

## Machine Learning packages
library(caret)
library(spatialRF)
library(randomForest)
library(party)
library(partykit)
library(permimp)

# Load Data ---------------------------------------------------------------

df <- read_csv(here("data", "sageres.csv")) 

# Clean & Aggregate Data --------------------------------------------------

## USER INPUTS
group_cols <- c("wy", "stratumID", "clim", "scen")
factor_vars <- c("wy", "stratumID", "clim", "scen")
response_var <- "npp"

## Convert categorical variables to factors
df[,factor_vars] <- lapply(df[,factor_vars], factor)

df_wy <- df %>% 
  ## Change aspect and splope from radians to degrees
  mutate(aspect=aspect*(180/pi),
         slope=slope*(180/pi)) %>% 
  group_by(across(all_of(group_cols))) %>% 
  summarise_if(is.numeric, mean) %>% 
  ungroup() %>% 
  ## Reorder response variables first
  select(!!response_var, everything()) %>% 
  ## Remove unwanted variables (manually?)
  select(-c(day, month, year, basinID, hillID, zoneID, patchID, wy))

## Create data frame with only climate scenario 0
df_wy_0 <- df_wy %>% 
  filter(clim==0)

## Create data frame with only climate scenario 2
df_wy_2 <- df_wy %>% 
  filter(clim==2)

# Dataset description -----------------------------------------------------

summarize_data <- function(df) {
  describe(df_wy_0) %>% 
    select(vars, n, mean, sd, min, max, range, skew) %>% 
    kable()
}

summarize_data(df_wy_0)

# Identify correlated variables -------------------------------------------

## Find number of response variables specified
num_response <- length(response_var)

## Save dataframe of predictor variables
numericpredictors.df <- df_wy[,(num_response+1):ncol(df_wy)] %>% select(where(is.numeric))
allpredictors.df <- df_wy[,(num_response+1):ncol(df_wy)]
factorpredictors.df <- df_wy[,(num_response+1):ncol(df_wy)] %>% select(where(is.factor))

## Correlation Matrix
# find_correlation <- function(threshold=0.75, data) {
#   ## Create correlation matrix of all numeric non-response variables
#   correlationMatrix <- cor(data)
#   ## Identify highly correlated variables by index
#   highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=threshold)
#   colnames(correlationMatrix[,highlyCorrelated])
# }
# correlated_variables <- find_correlation(threshold=0.75, data=numericpredictors.df)

## Multicollinearity reduction based on VIF and Correlation Thresholds

## Reference: https://gist.github.com/BlasBenito/768a45951a3d0c5ba355f52be94b05de
## Find preliminary importance using random forest
imp <- rf(data=df_wy,
          dependent.variable.name=response_var,
          predictor.variable.names=colnames(allpredictors.df),
          verbose=FALSE)

## Set preference order based on variable importance. This can be done manually if users have preferred variables of interest
preference.order <- imp$importance$per.variable$variable

## Remove variables based on VIF and Correlation thresholds
remove_multicollinearity <- function(vif.threshold=5, cor.threshold=0.75) {
  variable.selection <- auto_vif(x=allpredictors.df,
                                 vif.threshold=vif.threshold,
                                 preference.order=preference.order) %>% 
    auto_cor(cor.threshold=cor.threshold,
             preference.order=preference.order)
  
  variable.selection$selected.variables
}

## Create list of selected variables
noncollinear_variables <- remove_multicollinearity()

## Remove numeric variables with multicollinearity
df_wy_reduced <- df_wy %>% 
  select(c(response_var, colnames(factorpredictors.df), all_of(noncollinear_variables)))
  
# Perform Feature Selection/Importance ------------------------------------

## Random Forest package with replace=FALSE
rf1 <- randomForest(formula=npp~.,
                    data=df_wy_reduced,
                    replace=FALSE,
                    importance=TRUE)

permimp_rf1 <- permimp(rf1, 
                       conditional=TRUE,
                       do_check=FALSE)

## Random Forest package with replace=TRUE
rf2 <- randomForest(formula=npp~.,
                    data=df_wy_reduced,
                    replace=TRUE,
                    importance=TRUE)

permimp_rf2 <- permimp(rf2, 
                       conditional=TRUE,
                       do_check=FALSE)

## Party package with replace=FALSE
rf3_cforest_control <- cforest_control(replace=FALSE)

rf3 <- party::cforest(formula=npp~.,
                      data=df_wy_reduced,
                      controls=rf3_cforest_control)

permimp_rf3 <- permimp(rf3, 
                       conditional=TRUE,
                       do_check=FALSE)

## Party package with replace=TRUE
rf4_cforest_control <- cforest_control(replace=TRUE)

rf4 <- party::cforest(formula=npp~.,
                      data=df_wy_reduced,
                      controls=rf4_cforest_control)

permimp_rf4 <- permimp(rf4, 
                       conditional=TRUE,
                       do_check=FALSE)

# Visualize Results -------------------------------------------------------

permimp_to_table <- function(permimp) {
  df_permimp <- permimp$values %>%
    data.frame() %>% 
    rownames_to_column("variable")
}

df_permimp_rf1 <- permimp_to_table(permimp_rf1)
df_permimp_rf2 <- permimp_to_table(permimp_rf2)
df_permimp_rf3 <- permimp_to_table(permimp_rf3)
df_permimp_rf4 <- permimp_to_table(permimp_rf4)

plot_permimp <- function(permimp_df) {
  ggplot(data=permimp_df, aes(x=., y=reorder(variable, .), fill=variable)) +
    geom_col() +
    theme_light() +
    theme(legend.position = "none")
}

rf1_plot <- plot_permimp(df_permimp_rf1)
rf2_plot <- plot_permimp(df_permimp_rf2)
rf3_plot <- plot_permimp(df_permimp_rf3)
rf4_plot <- plot_permimp(df_permimp_rf4)

(rf1_plot+rf2_plot)/(rf3_plot+rf4_plot)
