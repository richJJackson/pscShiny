library(shiny)
library(survival)
library(flexsurv)
library(ggplot2)
library(knitr)
#install.packages("kableExtra")
library(kableExtra)
#install.packages("magrittr")
library(magrittr)  # <-- Add this line to fix the error!
library(bslib)
library(thematic)

#setwd("M:/University/R shiny_richard/Richrd_we page")
#setwd("~/Documents/GitHub/pscShiny/Data")

# Load cfm.ob object
load("data/cfm.ob.R")

# Apply thematic styling
thematic_shiny()

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
  
  survival_probs <- reactive({
    coeffs <- cfm.ob$model$cov_co
    gamma0 <- cfm.ob$model$haz_co[1]
    
    linear_predictor <- gamma0 +
      coeffs["extentLA"] * (input$extent == "LA") +
      coeffs["extentMet"] * (input$extent == "Met") +
      coeffs["primaryGall"] * (input$primary == "Gall") +
      coeffs["primaryHilar"] * (input$primary == "Hilar") +
      coeffs["primaryIntra"] * (input$primary == "Intra") +
      coeffs["primaryNOS"] * (input$primary == "NOS") +
      coeffs["alb"] * input$alb +
      coeffs["bil"] * input$bil
    
    surv_times <- seq(0, 60, by = 1)
    survival_probs <- exp(-exp(linear_predictor) * surv_times)
    
    data.frame(Time = surv_times, Survival = c(1, survival_probs[-1]))
  })
  
  output$survival_plot <- renderPlot({
    surv_data <- survival_probs()
    ggplot(surv_data, aes(x = Time, y = Survival)) +
      geom_line(color = "blue") +
      labs(
        title = "Predicted Survival Curve",
        x = "Time (months)",
        y = "Survival Probability"
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      theme_minimal()
  })
  
  output$survival_table <- renderTable({
    survival_probs()
  }, rownames = TRUE)
  
  output$discrimination_info <- renderUI({
    if (inherits(cfm.ob$valid$discrim$cm, "coxph")) {
      # Convert coxph model to a table
      cm_table <- broom::tidy(cfm.ob$valid$discrim$cm)
    } else {
      cm_table <- as.data.frame(cfm.ob$valid$discrim$cm)
    }
    
    tableHTML <- knitr::kable(cm_table, format = "html", caption = "Discrimination Metrics")
    HTML(tableHTML)
  })
  
  output$calibration_info <- renderTable({
    data.frame(
      Metric = c("C-index", "SE(C)", "Slope"),
      Value = c(
        cfm.ob$valid$calib$c[1],  # C-index
        cfm.ob$valid$calib$c[2],  # SE(C)
        cfm.ob$valid$calib$slope$coef  # Slope
      )
    ) %>%
      knitr::kable(format = "html", caption = "Calibration Metrics") %>%
      kableExtra::kable_styling("striped", full_width = FALSE)
  }, sanitize.text.function = function(x) x)
  
  output$citation_info <- renderText({
    cfm.ob$citation
  })
  
  output$model_formula <- renderText({
    paste(deparse(cfm.ob$model$formula), collapse = " ")
  })
  
  output$model_coefficients <- renderTable({
    data.frame(Covariate = names(cfm.ob$model$cov_co),
               Coefficient = cfm.ob$model$cov_co)
  })
  
  output$hazard_coefficients <- renderPrint({
    cfm.ob$model$haz_co
  })
  
  output$settings_pico <- renderUI({
    tags$div(
      tags$h4("Model Settings (PICO)"),
      tags$ul(
        tags$li(tags$b("Population:"), cfm.ob$setting$pico$P),
        tags$li(tags$b("Intervention:"), cfm.ob$setting$pico$I),
        tags$li(tags$b("Comparison:"), cfm.ob$setting$pico$C),
        tags$li(tags$b("Outcome:"), cfm.ob$setting$pico$O)
      )
    )
  })
  
  output$settings_data <- renderUI({
    data_info <- cfm.ob$setting$data
    tags$div(
      tags$h4("Data Characteristics"),
      tags$ul(
        tags$li(tags$b("Extent Classes:"), paste(names(data_info$extent), data_info$extent, collapse = ", ")),
        tags$li(tags$b("Primary Disease Classes:"), paste(names(data_info$primary), data_info$primary, collapse = ", ")),
        tags$li(tags$b("Albumin Range:"), paste("Min:", data_info$alb["min"], "Max:", data_info$alb["max"], "IQR:", data_info$alb["miqr"])),
        
        tags$li(tags$b("Bilirubin Range:"), paste("Min:", data_info$bil["min"], "Max:", data_info$bil["max"], "IQR:", data_info$bil["miqr"]))
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
