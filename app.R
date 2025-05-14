library(shiny)
library(tidyverse)
library(tidymodels)
library(shinydashboard)
library(plotly)

model <- readRDS("hotel_cancellation_model.rds")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

# Modified prediction function with input validation and actual model prediction

predict_cancellation <- function(input, model) {
  booking_changes <- as.numeric(input$booking_changes %||% 0)
  
  booking_changes_cat <- factor(
    case_when(
      booking_changes == 0 ~ "(-Inf,0]",
      booking_changes <= 3 ~ "(0,3]",
      TRUE ~ "(3, Inf)"
    ),
    levels = c("(-Inf,0]", "(0,3]", "(3, Inf)")
  )
  
  agent_value <- input$agent %||% "NULL"
  agent_numeric <- if(agent_value == "NULL") NA_real_ else as.numeric(agent_value)
  
  company_value <- input$company %||% "NULL"
  company_numeric <- if(company_value == "NULL") NA_real_ else as.numeric(company_value)
  
  stays_in_weekend_nights <- as.numeric(input$stays_in_weekend_nights %||% 1)
  stays_in_week_nights <- as.numeric(input$stays_in_week_nights %||% 2)
  total_stay <- stays_in_weekend_nights + stays_in_week_nights
  
  # Create a data frame with all required columns

    new_booking <- data.frame(
    country = factor(input$country %||% "Other", 
                     levels = c("PRT", "GBR", "FRA", "ESP", "DEU", "ITA", "USA", "Other")),
    deposit_type = factor(input$deposit_type %||% "No Deposit", 
                          levels = c("No Deposit", "Non Refund", "Refundable")),
    lead_time = as.numeric(input$lead_time %||% 30),
    total_of_special_requests = as.numeric(input$total_of_special_requests %||% 0),
    market_segment = factor(input$market_segment %||% "Online TA", 
                            levels = c("Online TA", "Direct", "Corporate", "Groups")),
    
    agent = agent_numeric,
    
    arrival_date_year = as.numeric(input$arrival_date_year %||% 2025),
    customer_type = factor(input$customer_type %||% "Transient", 
                           levels = c("Transient", "Contract", "Group", "Transient-Party")),
    previous_cancellations = as.numeric(input$previous_cancellations %||% 0),
    adr = as.numeric(input$adr %||% 100),
    arrival_date_week_number = as.numeric(input$arrival_date_week_number %||% 25),
    required_car_parking_spaces = as.numeric(input$required_car_parking_spaces %||% 0),
    hotel = factor(input$hotel %||% "City Hotel", 
                   levels = c("Resort Hotel", "City Hotel")),
    assigned_room_type = factor(input$assigned_room_type %||% "A", 
                                levels = c("A", "B", "C", "D", "E", "F", "G", "H")),
    total_stay = total_stay,
    arrival_date_month = as.numeric(input$arrival_date_month %||% 6),
    arrival_date_day_of_month = as.numeric(input$arrival_date_day_of_month %||% 15),
    stays_in_weekend_nights = stays_in_weekend_nights,
    stays_in_week_nights = stays_in_week_nights,
    adults = as.numeric(input$adults %||% 2),
    children = as.numeric(input$children %||% 0),
    babies = as.numeric(input$babies %||% 0),
    meal = factor(input$meal %||% "BB", 
                  levels = c("BB", "HB", "FB", "SC", "Undefined")),
    distribution_channel = factor(input$distribution_channel %||% "TA/TO", 
                                  levels = c("TA/TO", "Direct", "Corporate", "GDS", "Other")),
    is_repeated_guest = as.numeric(input$is_repeated_guest %||% 0),
    previous_bookings_not_canceled = as.numeric(input$previous_bookings_not_canceled %||% 0),
    reserved_room_type = factor(input$reserved_room_type %||% "A", 
                                levels = c("A", "B", "C", "D", "E", "F", "G", "H")),
    booking_changes = booking_changes,
    company = company_numeric,  # Changed to numeric
    days_in_waiting_list = as.numeric(input$days_in_waiting_list %||% 0),
    booking_changes_cat = booking_changes_cat,
    is_canceled = factor(0, levels = c(0, 1))  # Ensure factor has both levels
  )
  
  print("New booking data structure:")
  print(str(new_booking))
  
  if(nrow(new_booking) != 1) {
    stop("Input data must contain exactly 1 row for prediction")
  }
  
  # Make prediction with actual model
  tryCatch({
    predictions <- predict(model, new_data = new_booking, type = "prob")
    if(is.null(predictions) || !(".pred_1" %in% names(predictions))) {
      warning("Prediction failed or returned unexpected structure")
      return(list(
        cancellation_probability = 0.5,
        risk_category = "Medium",
        interpretation = "Prediction error: Unable to generate valid prediction. Check model compatibility."
      ))
    }
    
    cancel_prob <- predictions$.pred_1
    risk_category <- case_when(
      cancel_prob < 0.3 ~ "Low",
      cancel_prob < 0.7 ~ "Medium",
      TRUE ~ "High"
    )
    
    list(
      cancellation_probability = round(cancel_prob, 3),
      risk_category = risk_category,
      interpretation = paste0(
        "This booking has a ", round(cancel_prob * 100), "% chance of cancellation, ",
        "which is considered ", tolower(risk_category), " risk."
      )
    )
  }, error = function(e) {
    warning(paste("Error in prediction:", e$message))
    return(list(
      cancellation_probability = 0.5,
      risk_category = "Medium",
      interpretation = paste("Prediction error:", e$message)
    ))
  })
}

ui <- dashboardPage(
  dashboardHeader(title = "Hotel Cancellation Predictor"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction", tabName = "prediction", icon = icon("dashboard")),
      menuItem("Feature Importance", tabName = "importance", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "prediction",
        fluidRow(
          # Top Features Box
          box(
            title = "Most Important Features", status = "primary", solidHeader = TRUE, width = 12,
            helpText("These features have the highest impact on cancellation prediction:"),
            fluidRow(
              column(3, selectInput("country", "Country", 
                                    choices = c("PRT", "GBR", "FRA", "ESP", "DEU", "ITA", "USA", "Other"),
                                    selected = "Other")),
              column(3, selectInput("deposit_type", "Deposit Type", 
                                    choices = c("No Deposit", "Non Refund", "Refundable"),
                                    selected = "No Deposit")),
              column(3, numericInput("lead_time", "Lead Time (days)", value = 30, min = 0, max = 365)),
              column(3, numericInput("total_of_special_requests", "Special Requests", value = 0, min = 0, max = 5))
            ),
            fluidRow(
              column(3, selectInput("market_segment", "Market Segment", 
                                    choices = c("Online TA", "Direct", "Corporate", "Groups"),
                                    selected = "Online TA")),
              column(3, selectInput("agent", "Agent Code", 
                                    choices = c("NULL", "1", "9", "240", "14", "Other"),
                                    selected = "NULL")),
              column(3, numericInput("adr", "Average Daily Rate (€)", value = 100, min = 0, max = 500)),
              column(3, numericInput("required_car_parking_spaces", "Parking Spaces", value = 0, min = 0, max = 3))
            )
          ),
          
          box(
            title = "Additional Booking Details", status = "info", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3, numericInput("arrival_date_month", "Arrival Month (1-12)", value = 6, min = 1, max = 12)),
              column(3, numericInput("arrival_date_day_of_month", "Arrival Day (1-31)", value = 15, min = 1, max = 31)),
              column(3, numericInput("stays_in_weekend_nights", "Weekend Nights", value = 1, min = 0)),
              column(3, numericInput("stays_in_week_nights", "Weekdays Nights", value = 2, min = 0))
            ),
            fluidRow(
              column(3, numericInput("adults", "Adults", value = 2, min = 1)),
              column(3, numericInput("children", "Children", value = 0, min = 0)),
              column(3, numericInput("babies", "Babies", value = 0, min = 0)),
              column(3, selectInput("meal", "Meal Package", 
                                    choices = c("BB", "HB", "FB", "SC", "Undefined"),
                                    selected = "BB"))
            ),
            fluidRow(
              column(3, selectInput("distribution_channel", "Distribution Channel", 
                                    choices = c("TA/TO", "Direct", "Corporate", "GDS", "Other"),
                                    selected = "TA/TO")),
              column(3, numericInput("is_repeated_guest", "Repeated Guest (0/1)", value = 0, min = 0, max = 1)),
              column(3, numericInput("previous_bookings_not_canceled", "Previous Successful Bookings", value = 0, min = 0)),
              column(3, selectInput("reserved_room_type", "Reserved Room Type", 
                                    choices = c("A", "B", "C", "D", "E", "F", "G", "H"),
                                    selected = "A"))
            ),
            fluidRow(
              column(3, numericInput("booking_changes", "Booking Changes", value = 0, min = 0)),
              column(3, textInput("company", "Company Code", value = "NULL")),
              column(3, numericInput("days_in_waiting_list", "Days in Waiting List", value = 0, min = 0)),
              column(3, selectInput("hotel", "Hotel Type", 
                                    choices = c("Resort Hotel", "City Hotel"),
                                    selected = "City Hotel"))
            ),
            fluidRow(
              column(3, selectInput("assigned_room_type", "Assigned Room Type", 
                                    choices = c("A", "B", "C", "D", "E", "F", "G", "H"),
                                    selected = "A")),
              column(3, selectInput("customer_type", "Customer Type", 
                                    choices = c("Transient", "Contract", "Group", "Transient-Party"),
                                    selected = "Transient")),
              column(3, numericInput("previous_cancellations", "Previous Cancellations", value = 0, min = 0)),
              column(3, numericInput("arrival_date_year", "Arrival Year", value = 2025, min = 2000)),
              column(3, numericInput("arrival_date_week_number", "Week Number (1-52)", value = 25, min = 1, max = 53))
            )
          ),
          
          
          # Prediction Box
          box(
            title = "Prediction Result", status = "success", solidHeader = TRUE, width = 12,
            actionButton("predict", "Predict Cancellation Risk", 
                         icon = icon("calculator"), 
                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            br(), br(),
            plotlyOutput("risk_gauge"),
            h3("Risk Assessment", align = "center"),
            verbatimTextOutput("prediction_text"),
            verbatimTextOutput("interpretation")
          )
        )
      ),
      tabItem(
        tabName = "importance",
        box(
          title = "Feature Importance", status = "info", solidHeader = TRUE, width = 12,
          img(src = "C:\\Users\\kaust\\Desktop\\DA\\Programming_ds\\feature_imp.jpg", height = "100%", width = "100%")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$predict, {
    cat("Input values for prediction:\n")
    print(reactiveValuesToList(input))
  })
  
  prediction_results <- eventReactive(input$predict, {
    withProgress(message = 'Making prediction...', {
      predict_cancellation(input, model)
    })
  })
  
  output$prediction_text <- renderText({
    req(prediction_results())
    paste0("Cancellation Probability: ", prediction_results()$cancellation_probability, 
           " (", prediction_results()$risk_category, " Risk)")
  })
  
  output$interpretation <- renderText({
    req(prediction_results())
    prediction_results()$interpretation
  })
  
  output$risk_gauge <- renderPlotly({
    req(prediction_results())
    prob <- prediction_results()$cancellation_probability
    gauge_color <- if(prob < 0.3) "green" else if(prob < 0.7) "orange" else "red"
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = prob * 100,
      title = list(text = "Cancellation Risk", font = list(size = 24)),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = gauge_color),
        steps = list(
          list(range = c(0, 30), color = "lightgreen"),
          list(range = c(30, 70), color = "orange"),
          list(range = c(70, 100), color = "red")
        )
      )
    ) %>% layout(margin = list(t=40, b=40, l=40, r=40), height = 250)
  })
}

shinyApp(ui = ui, server = server)