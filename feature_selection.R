
# Setup -------------------------------------------------------------------

options(scipen=999)

## Standard packages
library(tidyverse)
library(here)

## Machine Learning packages
library(caret)
library(spatialRF)
library(randomForest)

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
predictors.df <- df_by_wy[,(num_responses+1):ncol(df_by_wy)] %>% select(where(is.numeric))
allpredictors.df <- df_by_wy[,(num_responses+1):ncol(df_by_wy)]

## Correlation Matrix

find_correlation <- function(threshold=0.75) {
  
  ## Create correlation matrix of all non-response variables
  correlationMatrix <- cor(predictors.df)
  
  ## Identify highly correlated variables by index
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=threshold)
  
  colnames(correlationMatrix[,highlyCorrelated])
}

find_correlation()

## VIF

vif(predictors.df)

# Useful reference for the following method: https://gist.github.com/BlasBenito/768a45951a3d0c5ba355f52be94b05de

## Multicollinearity reduction via Pearson correlation

## Review this
imp <- rf(data=df_by_wy,
          dependent.variable.name=response_var,
          predictor.variable.names=colnames(predictors.df),
          verbose=FALSE)

## Set preference order based on variable importance
preference.order <- imp$importance$per.variable$variable

## Remove variables based on vif and correlation thresholds
remove_multicollinearity <- function(vif.threshold=2.5, cor.threshold=0.5) {
  variable.selection <- auto_vif(x=predictors.df,
                                 vif.threshold=vif.threshold,
                                 preference.order=preference.order) %>% 
    auto_cor(cor.threshold=cor.threshold,
             preference.order=preference.order)
  
  variable.selection$selected.variables
}

remove_multicollinearity()
  
# Perform Feature Selection/Importance ------------------------------------

## Error
# rf <- randomForest(formula=response_var~.,
#                    data=df_by_wy)

# Visualize Results -------------------------------------------------------


