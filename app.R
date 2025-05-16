# ---- Load required libraries ----
library(shiny)
library(shinythemes)
library(xgboost)
library(dplyr)
library(caret)

# ---- Load trained caret model ----
loaded_model <- readRDS("tuned_xgb_model.rds")

# ---- Define fixed mean and SD used during training ----
sale_prc_mean <- 663390.8
sale_prc_sd <- 400789.6

# ---- Define UI ----
ui <- navbarPage("NEDUM SERVICE LIMITED",
                 tabPanel("House Price Predictor",
                          fluidPage(
                            theme = shinytheme("flatly"),
                            tags$head(
                              tags$style(HTML("
          body {
            background-color: #f4f4f9;
            color: #2c3e50;
            font-family: 'Segoe UI', sans-serif;
          }
          .form-control {
            background-color: #ffffff;
            color: #2c3e50;
            border: 1px solid #ccc;
            border-radius: 8px;
            padding: 10px;
          }
          label {
            font-weight: bold;
            margin-top: 10px;
            color: #2c3e50;
          }
          .btn {
            background-color: #4a90e2;
            color: white;
            border-radius: 8px;
            font-weight: bold;
            padding: 10px 20px;
            margin-top: 20px;
            border: none;
          }
          .btn:hover {
            background-color: #357ABD;
          }
          .shiny-text-output {
            background-color: #ecf0f1;
            color: #2c3e50;
            border-radius: 8px;
            padding: 10px;
            margin-top: 20px;
            font-size: 16px;
          }
        "))
                            ),
                            
                            titlePanel("🏠 NEDUM REAL ESTATE SERVICES"),
                            sidebarLayout(
                              sidebarPanel(
                                h4("📋 Enter Property Details"),
                                fluidRow(
                                  column(6, textInput("username", "Enter your name:", "Chinedu")),
                                  column(6, numericInput("LND_SQFOOT", "Enter Land Area:", 11247))
                                ),
                                fluidRow(
                                  column(6, numericInput("TOT_LVG_AREA", "Enter Floor Area:", 4552)),
                                  column(6, numericInput("SPEC_FEAT_VAL", "Value of Special Features:", 2105))
                                ),
                                fluidRow(
                                  column(6, numericInput("RAIL_DIST", "Rail Distance:", 4871.9)),
                                  column(6, numericInput("OCEAN_DIST", "Ocean Distance:", 18507.2))
                                ),
                                fluidRow(
                                  column(6, numericInput("WATER_DIST", "Water Distance:", 375.8)),
                                  column(6, numericInput("CNTR_DIST", "Business Center Distance:", 43897.9))
                                ),
                                fluidRow(
                                  column(6, numericInput("SUBCNTR_DI", "Sub-Center Distance:", 40115.7)),
                                  column(6, numericInput("HWY_DIST", "Highway Distance:", 41917.1))
                                ),
                                fluidRow(
                                  column(6, numericInput("age", "House Age:", 42)),
                                  column(6, numericInput("structure_quality", "Quality (1-10):", 5))
                                ),
                                fluidRow(
                                  column(6, numericInput("avno60plus", "Avg. 60+ Residents:", 0)),
                                  column(6, numericInput("month_sold", "Month Sold:", 8))
                                ),
                                actionButton("predict", "Submit"),
                                hr(),
                                h6("💡 Tip: Fill out all details and click 'Submit'")
                              ),
                              mainPanel(
                                h3("🎉 Prediction Result"),
                                textOutput("prediction"),
                                br(),
                                h6("🔮 Powered by Machine Learning and R + Shiny ✨")
                              )
                            )
                          )
                 )
)

# ---- Define Server ----
server <- function(input, output) {
  observeEvent(input$predict, {
    input_data <- data.frame(
      LND_SQFOOT = input$LND_SQFOOT,
      TOT_LVG_AREA = input$TOT_LVG_AREA,
      SPEC_FEAT_VAL = input$SPEC_FEAT_VAL,
      RAIL_DIST = input$RAIL_DIST,
      OCEAN_DIST = input$OCEAN_DIST,
      WATER_DIST = input$WATER_DIST,
      CNTR_DIST = input$CNTR_DIST,
      SUBCNTR_DI = input$SUBCNTR_DI,
      HWY_DIST = input$HWY_DIST,
      age = input$age,
      avno60plus = input$avno60plus,
      structure_quality = input$structure_quality,
      month_sold = input$month_sold
    )
    
    # Apply scaling
    input_data$LND_SQFOOT       <- scale(input_data$LND_SQFOOT, center = 8621, scale = 6070)
    input_data$TOT_LVG_AREA     <- scale(input_data$TOT_LVG_AREA, center = 2058, scale = 814)
    input_data$SPEC_FEAT_VAL    <- scale(input_data$SPEC_FEAT_VAL, center = 9562, scale = 13891)
    input_data$RAIL_DIST        <- scale(input_data$RAIL_DIST, center = 8349, scale = 6178)
    input_data$HWY_DIST         <- scale(input_data$HWY_DIST, center = 7724, scale = 6069)
    input_data$OCEAN_DIST       <- scale(input_data$OCEAN_DIST, center = 31691, scale = 17595)
    input_data$WATER_DIST       <- scale(input_data$WATER_DIST, center = 11960, scale = 11933)
    input_data$CNTR_DIST        <- scale(input_data$CNTR_DIST, center = 68490, scale = 32008)
    input_data$SUBCNTR_DI       <- scale(input_data$SUBCNTR_DI, center = 41115, scale = 22162)
    input_data$age              <- scale(input_data$age, center = 30.7, scale = 21.2)
    
    # Match training column order
    input_data <- input_data[, loaded_model$finalModel$xNames]
    
    # Predict
    scaled_pred <- predict(loaded_model, newdata = input_data)
    unscaled_pred <- scaled_pred * sale_prc_sd + sale_prc_mean
    
    output$prediction <- renderText({
      paste(input$username, "Your House Price is £", format(round(unscaled_pred, 2), big.mark = ","))
    })
  })
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)
