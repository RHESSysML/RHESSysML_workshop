# RHESSysML Shiny App for Sagehen Creek

# Attach Packages
library(shiny)
library(tidyverse)
library(here)
library(patchwork)
library(psych)
library(kableExtra)

#options(scipen=999)

setwd(here())

# Read in the dataset
df <- read.csv(here("data", "sageres.csv"))

### User Inputs
group_cols <- c("wy", "stratumID", "clim", "scen", "topo")
factor_vars <- c("wy", "stratumID", "clim", "scen", "topo")
response_var <- "npp"

## Convert categorical variables to factors
df[,factor_vars] <- lapply(df[,factor_vars], factor)

df_wy <- df %>% 
  ## Change aspect and slope from radians to degrees
  mutate(aspect=aspect*(180/pi),
         slope=slope*(180/pi)) %>% 
  group_by(across(all_of(group_cols))) %>% 
  mutate(jun_tavg = mean(tavg[month == 6]),
         jul_tavg = mean(tavg[month == 7]),
         aug_tavg = mean(tavg[month == 8]),
         sep_tavg = mean(tavg[month == 9]),
         oct_tavg = mean(tavg[month == 10]),
         nov_tavg = mean(tavg[month == 11]),
         dec_tavg = mean(tavg[month == 12]),
         jan_tavg = mean(tavg[month == 1]),
         feb_tavg = mean(tavg[month == 2]),
         mar_tavg = mean(tavg[month == 3]),
         apr_tavg = mean(tavg[month == 4]),
         may_tavg = mean(tavg[month == 5]),) %>% 
  mutate(peak_swe=max(swe)) %>%
  mutate(swe_precip_ratio=peak_swe/sum(precip)) %>% 
  summarise_if(is.numeric, mean) %>% 
  ungroup() %>% 
  ## Reorder response variables first
  select(!!response_var, everything()) %>% 
  ## Remove unwanted variables (manually?)
  select(-c(day, month, year, basinID, hillID, zoneID, patchID, wy))

## Create data frame with only climate scenario 0
df_wy0 <- df_wy %>% 
  filter(clim==0) %>% 
  select(-clim)

## Create data frame with only climate scenario 2
df_wy2 <- df_wy %>% 
  filter(clim==2) %>% 
  select(-clim)


# Create UI
ui <- fluidPage(
  titlePanel("RHESSys Output Exploration"),
  sidebarLayout(
    sidebarPanel("Varibles to Explore",
                 checkboxGroupInput("stratum_sel", label = h3("Select desired stratum to look at:"), 
                                    choices = unique(df_wy$stratumID),
                                    selected = unique(df_wy$stratumID)),
                 checkboxGroupInput("topo_sel", label = h3("Select topography types to look at:"), 
                                    choices = unique(df_wy$topo),
                                    selected = unique(df_wy$topo)),
                 varSelectInput(inputId = "dependent_variable",
                             label = "Select your dependent variable:",
                             data = df_wy,
                             selected = "npp"),
                 varSelectInput(inputId = "independent_variable",
                             label = "Select your independent variable:",
                             data = df_wy,
                             selected = "precip"),
                 selectInput(inputId = "pt_color",
                             label = "Select a point color:",
                             choices = c("Black" = "black",
                                         "Red" = "red",
                                         "Blue" = "blue",
                                         "Green" = "green"))),
    
    mainPanel("Visual Graph:",
              plotOutput(outputId = "variable_plot"))
  )
)

# Create a server
server <- function(input, output) {
  
  # Create a reactive dataframe
  df_wy_reactive <- reactive({
    df_wy %>% 
      filter(stratumID == input$stratum_sel, topo == input$topo_sel)
  })
  
  output$variable_plot <- renderPlot({
    ggplot(data = df_wy_reactive(), aes(x = !!input$independent_variable, y = !!input$dependent_variable)) +
      geom_point(color = input$pt_color) +
      geom_smooth(se = FALSE, method = lm)
  })
  
}

# Let R know that you want to combine UI and server into an app
shinyApp(ui = ui, server = server)

