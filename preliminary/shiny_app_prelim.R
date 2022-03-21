# RHESSysML Shiny App for Sagehen Creek

# Attach Packages
library(shiny)
library(tidyverse)
library(here)
library(patchwork)
library(psych)
library(kableExtra)
library(shinythemes)
library(lubridate)

#options(scipen=999)

# Set the working directory
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
  mutate(wy = as.Date(wy, format = "%Y")) %>% 
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
  select(-c(day, month, year, basinID, hillID, zoneID, patchID))

## Create data frame with only climate scenario 0
df_wy0 <- df_wy %>% 
  filter(clim==0) %>% 
  select(-clim)

## Create data frame with only climate scenario 2
df_wy2 <- df_wy %>% 
  filter(clim==2) %>% 
  select(-clim)

df_test <- df_wy %>% 
  mutate(quantile = paste0(ntile(peak_swe, 4), " Quantile"))


## Text for the welcome page

welcome <- "Welcome to the RHESSys Interpretation App. Use this app to explore your RHESSys output data."

intro_1 <- "- The \"Variable Importance\" tab will display the output from our machine learning workflows. If you have not gone through the machine learning workflows, we recommend doing that first. They can be found on the RHESSys Github Wiki." 

intro_2 <- "- The \"Visualizations\" tab will allow you explore your data's variables and their relationships." 
intro_3 <- "- Use the \"Metadata\" tab to learn more about each variable within your dataset."

importance_caption <- "The above graphs uses the random forest workflow to rank how important each varible is in predicting your responce variable, in this case Net Primary Productivity. The graph on the left ranks the variables in a normal climate scenario, and the graph on the right ranks the variables in a +2 degree C cliamte warming scenario."

## Inputs

stratum_sel <- checkboxGroupInput("stratum_sel",
                                  label = tags$h4("Select desired stratum to look at:"),
                                  choices = unique(df_wy$stratumID),
                                  selected = unique(df_wy$stratumID))

topo_sel <- checkboxGroupInput("topo_sel",
                               label = tags$h4("Select topography types to look at:"),
                               choices = c("Upslope" = "U",
                                           "Mid-slope" = "M",
                                           "Riparian" = "R"),
                               selected = c("Upslope" = "U",
                                            "Mid-slope" = "M",
                                            "Riparian" = "R"))

clim_sel <- checkboxGroupInput("clim_sel",
                               label = tags$h4("Select your climate scenario(s):"),
                               choices = c("Normal Scenario" = 0,
                                           "+2 Degree C Scenario" = 2),
                               selected = c("Normal Scenario" = 0,
                                            "+2 Degree C Scenario" = 2))

wy_sel <- sliderInput("wy_sel",
                      label = tags$h4("Select water year range:"),
                      min = min(df_wy$wy),
                      max = max(df_wy$wy),
                      value = c(min(df_wy$wy), max(df_wy$wy)))

dependent_variable <- varSelectInput(inputId = "dependent_variable",
                                    label = tags$h4("Select your dependent variable:"),
                                    data = df_wy,
                                    selected = "npp")

independent_variable <- varSelectInput(inputId = "independent_variable",
                                       label = tags$h4("Select your independent variable:"),
                                       data = df_wy,
                                       selected = "precip")

facet_variable <- varSelectInput(inputId = "facet_variable",
                                 label = tags$h4("Select variable to facet by:"),
                                 data = df_wy,
                                 selected = "rz_storage")

quantile_slider <- sliderInput("quantile_sel",
                               label = tags$h4("How many quantiles to facet?"),
                               min = 1,
                               max = 10,
                               value = 1)

# Create UI
ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  
  tags$h1("RHESSys Output Exploration"),
  
  navbarPage("Explore your dataset!",
             tabPanel("Welcome!",
                      tags$h2(welcome),
                      tags$h2(intro_3),
                      tags$h2(intro_1),
                      tags$h2(intro_2),
                      img(src = "RHESSys_logo_V2.png", height = 450, width = 450)),
             tabPanel("Metadata",
                      tags$h3("Explore the metadata:"),
                      img(src = "metadata_screenshot.png", height = 900, width = 1100)),
             tabPanel("Variable Importance",
                      tags$h3("Random Forest Variable Importance Output:"),
                      tags$h4("Your Responce Variable: Net Primary Productivity (NPP)"),
                      img(src = "importance_screenshot.png", height = 600, width = 1250),
                      tags$h6(importance_caption)),
             tabPanel("Visualizations",
                      sidebarPanel(stratum_sel,
                                   topo_sel,
                                   clim_sel,
                                   wy_sel,
                                   "Varibles to Explore:",
                                   dependent_variable,
                                   independent_variable,
                                   facet_variable,
                                   quantile_slider),
                      mainPanel("Visual Graph of your variable relationships:",
                                plotOutput(outputId = "variable_plot", height = 700))),
  )
)

# Create a server
server <- function(input, output) {
  
  # Create a reactive dataframe
  df_wy_reactive <- reactive({
    df_wy %>% 
      mutate(quantile = paste0(ntile(!!input$facet_variable, input$quantile_sel), " Quantile")) %>% 
      filter(stratumID %in% input$stratum_sel, 
             topo %in% input$topo_sel,
             clim %in% input$clim_sel,
             wy %in% input$wy_sel[1]:input$wy_sel[2])
  })
  
  output$variable_plot <- renderPlot({
    ggplot(data = df_wy_reactive(), aes(x = !!input$independent_variable, y = !!input$dependent_variable)) +
      geom_point(aes(color = clim)) +
      geom_smooth(se = FALSE, method = lm, color = "#B251F1") +
      scale_color_manual(values = c("0" = "#FEA346", 
                                    "2" = "#4BA4A4")) +
      labs(color = "Climate Scenario") +
      facet_wrap(~ quantile, dir = "v") +
      theme(text = element_text(size = 17))
  })
  
}

# Let R know that you want to combine UI and server into an app
shinyApp(ui = ui, server = server)

