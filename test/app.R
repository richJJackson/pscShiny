library(shiny)
library(ggplot2)
library(knitr)
#install.packages("kableExtra")
library(kableExtra)
#install.packages("magrittr")
library(magrittr)  # <-- Add this line to fix the error!
library(bslib)
#library(thematic)

#setwd("M:/University/R shiny_richard/Richrd_we page")
setwd("~/Documents/GitHub/pscShiny/Data")

## Load cfm.ob object
#load("cfm.ob.R")

# Apply thematic styling
#thematic_shiny()

# Define UI
ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    success = "#86C7ED",
    base_font = font_google("Lato")
  ),
  
  titlePanel("Interactive Survival Analysis"),
  
  sidebar = sidebar(
    class = "bg-secondary",
    id = "side_bar",
    tags$style(HTML("
      #side_bar {
        padding-left: 20px;
        padding-right: 20px;
        margin-top: 20px;
      }
    ")),
    
    h3("Select Covariate Values"),
    selectInput("extent", "Extent of Resection:",
                choices = c("LA", "Met"), selected = "LA"),
    selectInput("primary", "Primary Disease Location:",
                choices = c("Distal", "Gall", "Hilar", "Intra", "NOS"),
                selected = "Distal"),
    sliderInput("alb", "Albumin Level (g/L):",
                min = 20, max = 50, value = 38),
    sliderInput("bil", "Bilirubin Level (umol/L):",
                min = 0, max = 100, value = 10)
  ),
  
  # Survival Plot and Table as separate cards
  layout_columns(
    card(full_screen = TRUE,
         card_header("Survival Plot", class = "h5 text-success"),
         plotOutput("survival_plot")
    ),
    
    card(
      card_header("Survival Table", class = "h5 text-success"),
      tableOutput("survival_table")
    )
  ),
  
  # Everything else inside Tabset Panel
  # Fixed height container with scrolling for tab panels
  div(
    style = "height: 500px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #222;",
    
    tabsetPanel(
      tabPanel("Model Validation",
               card(
                 card_header("Discrimination", class = "h5 text-success"),
                 tableOutput("discrimination_info")  # Now uses `kable()`
               ),
               card(
                 card_header("Calibration", class = "h5 text-success"),
                 tableOutput("calibration_info")  # Now uses `kable()`
               )
      ),
      
      tabPanel("Citation",
               card(
                 card_header("Citation Details", class = "h5 text-success"),
                 verbatimTextOutput("citation_info")
               )
      ),
      
      tabPanel("Model Details",
               card(
                 card_header("Model Formula", class = "h5 text-success"),
                 verbatimTextOutput("model_formula")
               ),
               card(
                 card_header("Covariates and Coefficients", class = "h5 text-success"),
                 tableOutput("model_coefficients")
               ),
               card(
                 card_header("Baseline Hazard Coefficients", class = "h5 text-success"),
                 verbatimTextOutput("hazard_coefficients")
               )
      ),
      
      tabPanel("Model Settings",
               card(
                 card_header("Model Settings", class = "h5 text-success"),
                 uiOutput("settings_pico"),
                 uiOutput("settings_data")
               )
      )
    )
  )
)



# Define Server logic
server <- function(input, output) {
  
}

# Run the application
shinyApp(ui = ui, server = server)
