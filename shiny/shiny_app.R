########################## RHESSysML Shiny App for Sagehen Creek

########## Attach Packages
library(shiny)
library(tidyverse)
library(tidyselect)
library(here)
library(patchwork)
library(psych)
library(kableExtra)
library(shinythemes)
library(lubridate)
library(DT)

#options(scipen=999)

########## Set the working directory
setwd(here())

########## Get the datasets from the feature importance workflow

# Raw dataset
df <- read.csv(here("data", "sageres.csv"))
# Main aggregated dataset
df_wy <- read.csv(here("shiny", "aggregated_datasets", "df_wy.csv"))
# Aggregated dataset for climate scenario 0
df_wy0 <- read.csv(here("shiny", "aggregated_datasets", "df_wy0.csv"))
# Aggregated dataset for climate scenario 2
df_wy2 <- read.csv(here("shiny", "aggregated_datasets", "df_wy2.csv"))

all_datasets <- c("df", "df_wy", "df_wy0", "df_wy2")

# Get the variable importance rank datasets
imp_wy0 <- read.csv(here("shiny", "aggregated_datasets", "imp_wy0.csv")) %>% 
  arrange(Rank)
imp_wy2 <- read.csv(here("shiny", "aggregated_datasets", "imp_wy2.csv")) %>% 
  arrange(Rank)

########## User Inputs
factor_vars <- c("stratumID", "scen", "topo")
response_var <- df_wy$response

# Convert categorical variables to factors
df_wy[,factor_vars] <- lapply(df_wy[,factor_vars], factor)
df_wy$clim <- as.factor(df_wy$clim)
df_wy0[,factor_vars] <- lapply(df_wy0[,factor_vars], factor)
df_wy2[,factor_vars] <- lapply(df_wy2[,factor_vars], factor)

######### Import the metadata and create a table out of it

metadata <- read.csv(here("shiny", "aggregated_datasets", "metadata.csv")) %>% 
  select("variable", "full_name", "units", "description")

######### Text for the welcome page

welcome <- "Welcome to the RHESSys Interpretation App. Use this app to explore your RHESSys output data."

intro_1 <- "- The \"Variable Importance\" tab will display the output from our machine learning workflows. If you have not gone through the machine learning workflows, we recommend doing that first. They can be found on the RHESSys Github Wiki." 

intro_2 <- "- The \"Visualizations\" tab will allow you explore your data's variables and their relationships." 

intro_3 <- "- Use the \"Metadata\" tab to learn more about each variable within your dataset. To add or remove variables specific to your dataset, you can do so using the metadata.Rmd file included within the github repo."

intro_4 <- "- The \"Dataset Viewer\" tab allows you to view your datasets, both raw and aggregated. Datasets get imported from the machine learning workflows. If you have not already run the workflows with your own RHESSys dataset, the default data will be from Sagehen Creek."

importance_caption <- "The above graphs uses the random forest workflow to rank how important each varible is in predicting your responce variable, in this case Net Primary Productivity. The graph on the left ranks the variables in a normal climate scenario, and the graph on the right ranks the variables in a +2 degree C cliamte warming scenario."

########## Inputs

dataset_sel <- selectInput(inputId = "dataset_sel",
                           label = tags$h4("Select your dataset to view:"),
                           choices = c("Raw Data",
                                       "Aggregated Data",
                                       "Aggregated Data (Normal Climate)",
                                       "Aggregated Data (+2 Degree C Climate)"),
                           selected = "Aggregated Data")

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
                      value = c(min(df_wy$wy), max(df_wy$wy)),
                      sep = "",
                      step = 1)

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
                               max = 9,
                               value = 1)

########## Create UI
ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  
  tags$h1("RHESSys Output Exploration"),
  
  navbarPage("Explore your dataset!",
             tabPanel("Welcome!",
                      tags$h2(welcome),
                      tags$h2(intro_3),
                      tags$h2(intro_4),
                      tags$h2(intro_1),
                      tags$h2(intro_2),
                      img(src = "RHESSys_logo_V2.png", height = 450, width = 450)),
             tabPanel("Metadata",
                      tags$h3("Explore the metadata:"),
                      DT::dataTableOutput("metadata_DT")),
             tabPanel("Dataset Viewer",
                      tags$h3("View your datasets:"),
                      dataset_sel,
                      DT::dataTableOutput("datatable_viewer")),
             tabPanel("Variable Importance",
                      tags$h3("Random Forest Variable Importance Output:"),
                      tags$h4("Your Response Variable: Net Primary Productivity (NPP)"),
                      img(src = "importance_plots.png", height = 800, width = 800),
                      tags$h6(importance_caption)),
             tabPanel("Visualizations",
                      sidebarPanel(stratum_sel,
                                   topo_sel,
                                   clim_sel,
                                   wy_sel,
                                   "Varibles to Explore:",
                                   independent_variable,
                                   facet_variable,
                                   quantile_slider),
                      mainPanel("Visual Graph of your variable relationships:",
                                plotOutput(outputId = "variable_plot", height = 700)))
  )
)

########## Create a server
server <- function(input, output) {
  
  data_display <- reactive({
    
    switch(input$dataset_sel,
           "Raw Data" = df,
           "Aggregated Data" = df_wy,
           "Aggregated Data (Normal Climate)" = df_wy0,
           "Aggregated Data (+2 Degree C Climate)" = df_wy2)
    
  })
  
  # Create a reactive dataframe
  df_wy_reactive <- reactive({
    df_wy %>% 
      mutate(quantile = paste0("Quantile ", ntile(!!input$facet_variable, input$quantile_sel))) %>% 
      filter(stratumID %in% input$stratum_sel, 
             topo %in% input$topo_sel,
             clim %in% input$clim_sel,
             wy %in% input$wy_sel[1]:input$wy_sel[2])
  })
  
  output$variable_plot <- renderPlot({
    ggplot(data = df_wy_reactive(), aes(x = !!input$independent_variable, y = response)) +
      geom_point(aes(color = clim)) +
      geom_smooth(se = FALSE, method = lm, color = "#B251F1") +
      scale_color_manual(values = c("0" = "#FEA346", 
                                    "2" = "#4BA4A4")) +
      labs(color = "Climate Scenario") +
      facet_wrap(~ quantile) +
      theme(text = element_text(size = 17))
  })
  
  output$metadata_DT <- DT::renderDataTable({
    
    DT::datatable(metadata,
                  options = list(pageLength = 30,
                                initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'color': '#FFFFFF'});",
                                     "}")),
                  caption = tags$caption(style = 'caption-side: top; text-align: left; color: white', 
                                         'Table : ', 
                                         tags$em('Metadata'),
                                         color = "white"))
    
  })
  
  output$datatable_viewer <- DT::renderDataTable({
    
    data_display() %>% 
      mutate(across(where(is.numeric), round, 6)) %>% 
      DT::datatable(options = list(pageLength = 15,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'color': '#FFFFFF'});",
                                   "}")),
                  caption = tags$caption(style = 'caption-side: top; text-align: left; color: white', 
                                         'Table : ',
                                         tags$em('Dataset'),
                                         color = 'white'))
    
  })
  
}

########## Let R know that you want to combine UI and server into an app
shinyApp(ui = ui, server = server)

