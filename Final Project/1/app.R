library(shiny)
library(tidyverse)
library(data.table)
library(RColorBrewer)

# Function to read data
read_data <- function(filename) {
  raw_data <- fread(filename)
  raw_data <- raw_data[-2, ]
  setnames(raw_data, names(raw_data))
  raw_data[, value := as.numeric(value)]
  return(raw_data)
}

data_files <- list.files(pattern = "hdro_indicators_.*\\.csv", full.names = FALSE)

countries <- str_remove(basename(data_files), "hdro_indicators_|\\.csv")
initial_data <- lapply(data_files, read_data)
names(initial_data) <- countries

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Human Development Indicators Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country", choices = countries, selected = countries[1], multiple = TRUE),
      uiOutput("column_selector"),
      sliderInput("rows", "Select Number of Rows", min = 1, max = 100, value = 10),
      sliderInput("year_range", "Select Year Range", min = 1990, max = 2022, value = c(2000, 2022), step = 1),
      selectInput("plot_type1", "Select Plot Type for Plot 1", choices = c("Bar Plot", "Line Plot")),
      selectInput("plot_type2", "Select Plot Type for Plot 2", choices = c("Scatter Plot", "Line Plot")),
      selectInput("color_palette", "Select Color Palette", choices = c("Set1", "Set2", "Set3")),
      checkboxInput("show_labels", "Show Data Labels", value = FALSE),
      fileInput("file", "Upload Additional Dataset", accept = ".csv"),
      actionButton("update", "Update Data")
    ),
    
    mainPanel(
      h3(textOutput("title")),
      tableOutput("table"),
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  selected_data <- reactive({
    req(input$country)
    data <- bind_rows(lapply(input$country, function(country) {
      initial_data[[country]]
    }))
    
    if (!is.null(input$file)) {
      additional_data <- fread(input$file$datapath)
      additional_data <- additional_data[-2, ]
      setnames(additional_data, names(initial_data[[1]]))
      additional_data[, value := as.numeric(value)]
      data <- bind_rows(data, additional_data)
    }
    
    data
  })
  
  filtered_data <- reactive({
    data <- selected_data()
    data <- data[year >= input$year_range[1] & year <= input$year_range[2]]
    data
  })
  
  calculate_indicators <- reactive({
    data <- filtered_data()
    
    # Calculate average HDI
    average_HDI_by_country <- data[index_name == "Human Development Index",
                                   .(average_HDI = base::mean(value, na.rm = TRUE)),
                                   keyby = .(country_name, year)]
    
    # Calculate latest year HDI
    latest_year_idx <- average_HDI_by_country[, .I[year == max(year)], 
                                              by = country_name]$V1
    latest_HDI_by_country <- average_HDI_by_country[latest_year_idx][order(-average_HDI)]
    
    # Calculate Gender Inequality Index
    gender_inequality_index <- data[index_name == "Gender Inequality Index",
                                    .(mean_value = base::mean(value, na.rm = TRUE)),
                                    keyby = .(country_name, year)]
    gender_inequality_index[, prev_value := shift(mean_value), by = country_name]
    gender_inequality_index[, change := mean_value - prev_value]
    
    list(
      average_HDI_by_country = average_HDI_by_country,
      latest_HDI_by_country = latest_HDI_by_country,
      gender_inequality_index = gender_inequality_index
    )
  })
  
  output$column_selector <- renderUI({
    data <- selected_data()
    checkboxGroupInput("columns", "Select Columns", choices = colnames(data), selected = colnames(data)[1:5])
  })
  
  output$title <- renderText({
    paste("Human Development Indicators Data -", paste(input$country, collapse = ", "))
  })
  
  output$table <- renderTable({
    data <- filtered_data()
    data <- data[, ..input$columns]
    head(data, input$rows)
  })
  
  output$plot1 <- renderPlot({
    indicators <- calculate_indicators()
    plot_data <- indicators$average_HDI_by_country
    
    p <- ggplot(plot_data, aes(x = year, y = average_HDI, fill = country_name, group = country_name)) +
      theme_minimal() + labs(title = "Average HDI Plot", x = "Year", y = "Average HDI")
    
    if (input$plot_type1 == "Bar Plot") {
      p <- p + geom_bar(stat = "identity", position = "dodge")
    } else if (input$plot_type1 == "Line Plot") {
      p <- p + geom_line(aes(color = country_name)) + geom_point(aes(color = country_name))
    }
    
    if (input$show_labels) {
      p <- p + geom_text(aes(label = round(average_HDI, 2)), position = position_dodge(width = 0.9), vjust = -0.5)
    }
    
    p + scale_fill_brewer(palette = input$color_palette) + scale_color_brewer(palette = input$color_palette)
  })
  
  output$plot2 <- renderPlot({
    indicators <- calculate_indicators()
    plot_data <- indicators$gender_inequality_index
    y_limits <- range(plot_data$mean_value, na.rm = TRUE)
    
    plot_data <- plot_data[complete.cases(plot_data), ]
    
    p <- ggplot(plot_data, aes(x = year, y = mean_value, color = country_name, group = country_name)) +
      theme_minimal() + labs(title = "Gender Inequality Index", x = "Year", y = "Mean Value")
    
    if (input$plot_type2 == "Scatter Plot") {
      p <- p + geom_point() + ylim(y_limits)
    } else if (input$plot_type2 == "Line Plot") {
      p <- p + geom_line() + geom_point() + ylim(y_limits)
    }
    
    if (input$show_labels) {
      p <- p + geom_text(aes(label = round(mean_value, 2)), position = position_dodge(width = 0.9), vjust = -0.5)
    }
    
    p + scale_color_brewer(palette = input$color_palette)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
