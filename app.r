library(shiny)
library(leaflet)
library(dplyr)
#library(readxl)
#library(readr)
library(lubridate)
library(stringr)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(shinyjqui)

#setwd("./dataset-vis")
data <- readRDS("sets-info.rds")

# Fix error in data (I checked on N drive)
#data$end_time[103] <- ymd_hms("2019-08-11 00:00:00")

# Add new data
#data_new <- read_csv("sets_update.csv") %>% head()
#  mutate(
#    Latitude = str_split(Latitude, pattern = " ") %>%
#      lapply(
#        ., 
#        function(x) {
#          as.numeric(x[1]) + (as.numeric(x[2]) / 60)
#        }
#      ) %>% unlist(),
#    Longitude = str_split(Longitude, pattern = " ") %>%
#      lapply(
#        ., 
#        function(x) {
#          as.numeric(x[1]) + (as.numeric(x[2]) / 60)
#        }
#      ) %>% unlist(),
#    Latitude = -Latitude,
#    start_time = dmy_hm(start_time),
#    end_time = dmy_hm(end_time)
#  )
#data <- rbind(data, data_new)
#saveRDS(data, "sets-info.rds")

ui <- fluidPage(
  titlePanel("Dataset locations"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "lat_range", 
        "Latitude Range", 
        -40, 
        -5,
        c(
          -40, 
          -5
        ),
        step = 0.01
      ),
      sliderInput(
        "lon_range",
        "Longitude Range",
        100,
        150,
        c(
          100,
          150
        ),
        step = 0.01
      ),
      dateRangeInput(
        "time_range",
        "Time Range",
        min(data$start_time, na.rm = T),
        max(data$end_time, na.rm = T)
      ),
      pickerInput(
        inputId = "select_dataset_id",
        label = "Select Dataset ID(s)",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            title = "Select data sets",
            header = "This is a title",
            style = "btn-group"
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", jqui_resizable(leafletOutput("map"))),
        tabPanel("Data", DTOutput("table")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )
)

server <- function(input, output, session) {

  # 2. Create a reactive expression to filter data
  data_for_picker <- reactive({
    data %>%
      filter(
        between(
          Latitude,
          left = input$lat_range[1],
          right = input$lat_range[2]
        ),
        between(
          Longitude,
          left = input$lon_range[1],
          right = input$lon_range[2]
        ),
        (between(
          as.Date(start_time), 
          left = as.Date(input$time_range[1]),
          right = as.Date(input$time_range[2])
        ) | between(
          as.Date(end_time), 
          left = as.Date(input$time_range[1]),
          right = as.Date(input$time_range[2])
          )
        )
      )
  })

  # 3. Use observe to update pickerInput choices
  observe({
    updatePickerInput(
      session,
      inputId = "select_dataset_id",
      choices = unique(data_for_picker()$set_number),
      selected = unique(data_for_picker()$set_number)
    )
  })

  filtered_data <- reactive({
    data %>%
      filter(
        between(
          Latitude,
          left = input$lat_range[1],
          right = input$lat_range[2]
        ),
        between(
          Longitude,
          left = input$lon_range[1],
          right = input$lon_range[2]
        ),
        (between(
          as.Date(start_time), 
          left = as.Date(input$time_range[1]),
          right = as.Date(input$time_range[2])
        ) | between(
          as.Date(end_time), 
          left = as.Date(input$time_range[1]),
          right = as.Date(input$time_range[2])
          )
        ),
        set_number %in% input$select_dataset_id
      )
  })

  output$map <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircles(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~paste(
          "Dataset ID:", set_number, "<br>",
          "Latitude:", Latitude, "<br>",
          "Longitude:", Longitude, "<br>",
          "Time:", start_time, "-", end_time, "<br>"
          # Add more columns as needed
          ),
        radius = 25000
      ) %>%
      addScaleBar()
  })

  output$table <- renderDT({
        datatable(filtered_data())
  })

  output$plot <- renderPlot({
    df <- filtered_data()
    dates_observed <- as.Date(c())
    for (i in 1:nrow(df)) {
      dates_observed <- c(
        dates_observed,
        seq(
          df$start_time[i],
          df$end_time[i],
          by = "days"
        )
      )
    }

    days_df <- data.frame(
      date = dates_observed,
      day_of_year = yday(dates_observed)
    )

    days_df %>%
      group_by(day_of_year) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = day_of_year, y = count)) +
      geom_line() +
      xlim(1, 365)
  })

}

shinyApp(ui, server)
