library(datasets)
library(shiny)
library(dplyr)
library(magrittr)
library(lubridate)
require(ggplot2)
  
  shinyServer(function(input, output) {
    
    addClass(selector = "Sidebar", class = "sidebar-collapse")
    
    load("RforAp.RData")
    autoInvalidate = reactiveTimer(60000) #Updates the time retrieved from the R console
    
    # Needs to update the test dataframe for the following variables 
    testc$Semester = ifelse(testc$Month > 6, 1, 2)
    
    # Creates predictions and adds a timestamp
    filtered_test = reactive({
        test1 = filter(testc, term_week==input$Week)
        test2 = filter(test1, Day == input$Day)
        timestamp = paste(test2$Hour,":", test2$Minute,":00",sep="")
        df= cbind.data.frame(wait_time=test2$wait_time, timestamp, number=1:length(test2$wait_time))
    })
    
    # Prints the current estimated waiting time and refreshes
    output$text1 <- renderPrint({   
      autoInvalidate() # Needed for loops that refresh after set time
      Hour_p = Sys.time() %>% hour()
      Day_p = Sys.time() %>% weekdays()
      Min_p = Sys.time() %>% minute()
      
      # Here
      # Filters stored test data so that it matches current time
      test_p = filter(testc, Hour == Hour_p, Day == Day_p, Minute == Min_p, term_week==ifelse((Sys.time() %>% week())>36, 
                                                                                               (Sys.time() %>% week())-36, 0)) 
      # Weird hours where MSC shuts early
      if(nrow(test_p)==0){
          cat("The MSC is closed currently. It opens weekdays at 10am.")
      }else if(test_p$term_week == 2){ 
        time_now = round(test_p$wait_time, 0)
        cat("Estimated waiting time is", time_now, "minutes. *Please check timetable as some hours don't start until week 3.") 
      }else{
        time_now = round(test_p$wait_time, 0)
        cat("Estimated waiting time is", time_now, "minutes.") 
        }
      }) 
   
    ######### Specific for term ###################### 
   # output$text2 <- renderPrint({  
   #    if(input$Day == "Tuesday"){
   #       cat("**The MSC is shut at 5.30-6.30pm on Tuesday.") 
   #    }else if(input$Day == "Thursday"){
   #       cat("**The MSC is shut at 5.30-6.30pm on Thursday.") 
   #  }
   # })
    
    output$hist <- renderPlot({
      
      df= filtered_test()
      max_p = max(df[,1])
       ggplot(df, aes(x = timestamp, y = as.numeric(wait_time))) + 
         geom_point(size = 0.5, col="white") +
        stat_smooth(aes(x=number, y=wait_time), method="loess",span=0.1, se = FALSE) +
        scale_x_discrete(breaks = (df$timestamp)[c(T, rep(F, 59))]) +theme_bw()+
        labs(x="Time of Day", y="Waiting\nTime in\nMinutes") + 
        xlab( paste("Time for ", input$Day, " in Term Week ", input$Week, sep="") )+
        scale_y_continuous(breaks=seq(0, 60, 5))+ expand_limits(y=c(0,max_p+5))+
        theme(
          plot.title = element_text(lineheight=0, color='black', face="bold", size=14),
          axis.title.x=element_text(angle=0, color='black', face='bold', size=14),
          axis.title.y=element_text(angle=0, vjust = 1, color='black',face='bold', size=14),
          axis.text.x=element_text(angle=0, color='black', size=12),
          axis.text.y=element_text(angle=0, color='black',  size=13)
        )  +   theme(legend.position = "none") 
    })
    
})
    
    
  
