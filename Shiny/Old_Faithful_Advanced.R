###########################################################
#---------------------------------------------------------#
############ Shiny! Old Faithful Advanced 1 ###############
#---------------------------------------------------------#
###########################################################

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            
            selectInput("colour",
                        "Colour of histogram:",
                        list('colour' = c("red", "blue", "green"))
                        ),
            
            numericInput(inputId ="ylim", "Limit on y axis:", value = 0.1),
            
            textInput(inputId ="graph", "Graph description:", value = "histogram"),
            
            checkboxInput(inputId = "density",
                          label = strong("Show density estimate"),
                          value = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        waiting    <- faithful[, 2]
        bins <- seq(min(waiting), max(waiting), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(waiting, breaks = bins, probability = TRUE,
             col = input$colour, border = 'white', ylim = c(0,input$ylim),
             main = "Histogram of waiting time between eruptions")
        
        if(input$density) {
          dens <- density(waiting, adjust = 0.7)
          lines(dens, col = "blue", lwd = 2)
        }
    
    })
    
    output$text <- renderText({paste("This", input$graph, "has", input$bins, 
                                     "bins and has", input$colour,"coloured bins.")})
}

# Run the application 
shinyApp(ui = ui, server = server)
