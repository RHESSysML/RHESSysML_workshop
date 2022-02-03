
# Setup -------------------------------------------------------------------

options(scipen=999)

## Standard packages
library(tidyverse)
library(here)
library(patchwork)

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

df_by_wy <- df %>% 
  ## Change aspect from radians to degrees
  mutate(aspect = aspect*(180/pi)) %>% 
  group_by(across(all_of(group_cols))) %>% 
  summarise_if(is.numeric, mean) %>% 
  ungroup() %>% 
  ## Reorder response variables first
  select(!!response_var, everything()) %>% 
  ## Remove unwanted variables (manually?)
  select(-c(day, month, year, basinID, hillID, zoneID, patchID, wy))

# Identify correlated variables -------------------------------------------

## Find number of response variables specified
num_response <- length(response_var)

## Save dataframe of predictor variables
predictors.df <- df_by_wy[,(num_response+1):ncol(df_by_wy)] %>% select(where(is.numeric))
allpredictors.df <- df_by_wy[,(num_response+1):ncol(df_by_wy)]
factorpredictors.df <- df_by_wy[,(num_response+1):ncol(df_by_wy)] %>% select(where(is.factor))


## Correlation Matrix

find_correlation <- function(threshold=0.75, data) {
  
  ## Create correlation matrix of all numeric non-response variables
  correlationMatrix <- cor(data)
  
  ## Identify highly correlated variables by index
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=threshold)
  
  colnames(correlationMatrix[,highlyCorrelated])
}

correlated_variables <- find_correlation(threshold=0.75, data=predictors.df)

### Not sure this is the way to go

## VIF

### Computes VIF. Could just remove variables over a certain threshold
vif(predictors.df)

### Computes VIF and removes collinear variables. Produces slightly different VIF values
auto_vif(predictors.df)

## Multicollinearity reduction via Pearson correlation

## Reference: https://gist.github.com/BlasBenito/768a45951a3d0c5ba355f52be94b05de
imp <- rf(data=df_by_wy,
          dependent.variable.name=response_var,
          predictor.variable.names=colnames(allpredictors.df),
          verbose=FALSE)

## Set preference order based on variable importance
preference.order <- imp$importance$per.variable$variable

## Remove variables based on vif and correlation thresholds
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

## Remove multicollinear numeric variables
df_by_wy_reduced <- df_by_wy %>% 
  select(c(response_var, colnames(factorpredictors.df), all_of(noncollinear_variables)))
  
# Perform Feature Selection/Importance ------------------------------------

# rf1 <- rf(data=df_by_wy_reduced,
#           dependent.variable.name=response_var,
#           predictor.variable.name=colnames(df_by_wy_reduced[,(num_response+1):ncol(df_by_wy_reduced)]),
#           verbose=FALSE)
# 
# permimp_rf1 <- permimp(rf1)

## Cannot figure out how to add general response variable term
rf2 <- randomForest(formula=npp~.,
                    data=df_by_wy_reduced,
                    replace=FALSE,
                    importance=TRUE)

permimp_rf2 <- permimp(rf2)

rf3 <- randomForest(formula=npp~.,
                    data=df_by_wy_reduced,
                    replace=TRUE,
                    importance=TRUE)

permimp_rf3 <- permimp(rf3)

## Cannot figure out how to add general response variable term. cforest is more computationally expensive because it is built in R.
rf4_cforest_control <- cforest_control(replace = TRUE)

rf4 <- party::cforest(formula=npp~.,
                      data=df_by_wy_reduced,
                      controls=rf4_cforest_control)

permimp_rf4 <- permimp(rf4)

rf5_cforest_control <- cforest_control(replace = TRUE)

rf5 <- party::cforest(formula=npp~.,
                      data=df_by_wy_reduced,
                      controls=rf4_cforest_control)

permimp_rf5 <- permimp(rf5)

# Visualize Results -------------------------------------------------------

## RF1
# df_permimp_rf1 <- permimp_rf1$values %>%
#   data.frame() %>% 
#   rownames_to_column("variable")
# 
# ggplot(data=df_permimp_rf1, aes(x=., y=reorder(variable, .))) +
#   geom_col() +
#   theme_light()

## RF2
df_permimp_rf2 <- permimp_rf2$values %>%
  data.frame() %>% 
  rownames_to_column("variable")

rf2_plot <- ggplot(data=df_permimp_rf2, aes(x=., y=reorder(variable, .))) +
  geom_col() +
  theme_light()

## RF3
df_permimp_rf3 <- permimp_rf3$values %>%
  data.frame() %>% 
  rownames_to_column("variable")

rf3_plot <- ggplot(data=df_permimp_rf3, aes(x=., y=reorder(variable, .))) +
  geom_col() +
  theme_light()

## RF4
df_permimp_rf4 <- permimp_rf4$values %>%
  data.frame() %>% 
  rownames_to_column("variable")

rf4_plot <- ggplot(data=df_permimp_rf4, aes(x=., y=reorder(variable, .))) +
  geom_col() +
  theme_light()

## RF5
df_permimp_rf5 <- permimp_rf5$values %>%
  data.frame() %>% 
  rownames_to_column("variable")

rf5_plot <- ggplot(data=df_permimp_rf5, aes(x=., y=reorder(variable, .))) +
  geom_col() +
  theme_light()

rf2_plot + rf3_plot

rf4_plot + rf5_plot
