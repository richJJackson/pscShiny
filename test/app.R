library(shiny)




# Define UI
ui <- page_sidebar(

  
  titlePanel("Interactive Survival Analysis"),
  
  sidebar = sidebar(
    id = "side_bar",
    
    h3("Select Covariate Values"),
    selectInput("extent", "Extent of Resection:",
                choices = c("LA", "Met"), selected = "LA")
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

    tabsetPanel(
      tabPanel("Model Validation",
               card(
                 card_header("Discrimination", class = "h5 text-success"),
                 tableOutput("discrimination_info")  # Now uses `kable()`
               )
      ),
      
      tabPanel("Citation",
               card(
                 card_header("Citation Details", class = "h5 text-success"),
                 verbatimTextOutput("citation_info")
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
