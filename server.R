library(shiny)
library(dplyr)
library(lubridate)
library(tibbletime)
library(plotly)

mydata <- data.table::fread("mydata.csv")

mydata2 <- mydata %>% as_data_frame %>% 
        mutate(Date = ymd_hms(Date)) %>% 
        as_tbl_time(index = Date) %>% 
        filter_time(time_formula = "2007-01-01" ~ "2007-12-31")

shinyServer(function(input, output) {
                
        dataset <- reactive({
                mydata2 %>% 
                filter_time(time_formula = input$daterange[1] ~ input$daterange[2])
        })
        
        output$text1 <- renderText({
                
                print <- paste("from",input$daterange[1],
                                        "to",input$daterange[2])
                
                if (input$`Global active power`){
                        print <- paste(print,"\n\ntotal Global active power")
                        print <- paste(print,
                                round(sum(dataset()["Global_active_power"])/60,
                                       digits = 2),"kWh")
                        }
                        
                if (input$`Global reactive power`){
                        print <- paste(print,"\n\ntotal Global reactive power")
                        print <- paste(print,
                                       round(sum(dataset()["Global_reactive_power"])/60,
                                              digits = 2),"kWh")
                }
                
                if (input$`Global intensity`){
                        print <- paste(print,"\n\ntotal Global intensity")
                        print <- paste(print,
                                       round(sum(dataset()["Global_intensity"])/60,
                                              digits = 2),"kWh")
                }
                        
                if (input$`Sub metering 1`){
                        print <- paste(print,"\n\ntotal Sub metering_1")
                        print <- paste(print,
                                       round(sum(dataset()["Sub_metering_1"])/60,
                                              digits = 2),"kWh")
                }
                        
                if (input$`Sub metering 2`){
                        print <- paste(print,"\n\ntotal Sub metering_2")
                        print <- paste(print,
                                       round(sum(dataset()["Sub_metering_2"])/60,
                                              digits = 2),"kWh")
                }
                        
                if (input$`Sub metering 3`){
                        print <- paste(print,"\n\ntotal Sub metering_3")
                        print <- paste(print,
                                       round(sum(dataset()["Sub_metering_3"])/60,
                                              digits = 2),"kWh")
                        
                }
                
                print
        }
        )
        
        output$distPlot <- renderPlotly({
                p <- plot_ly(dataset(),
                        x = ~Date,
                        y = NaN
                )
                if (input$`Global active power`){
                        p <- p %>% add_trace(
                                y = ~Global_active_power,
                                name = "Global active power",
                                type = 'scatter',mode = "lines")
                }
                if (input$`Global reactive power`){
                        p <- p %>% add_trace(
                                y = ~Global_reactive_power,
                                name = "Global reactive power",
                                type = 'scatter',mode = "lines")
                }
                if (input$Voltage){
                        p <- p %>% add_trace(
                                y = ~Voltage,
                                name = "Voltage",
                                type = 'scatter',mode = "lines")
                }
                if (input$`Global intensity`){
                        p <- p %>% add_trace(
                                y = ~Global_intensity,
                                name = "Global intensity",
                                type = 'scatter',mode = "lines")
                }
                if (input$`Sub metering 1`){
                        p <- p %>% add_trace(
                                y = ~Sub_metering_1,
                                name = "Sub metering 1",
                                type = 'scatter',mode = "lines")
                }
                if (input$`Sub metering 2`){
                        p <- p %>% add_trace(
                                y = ~Sub_metering_2,
                                name = "Sub metering 2",
                                type = 'scatter',mode = "lines")
                }
                if (input$`Sub metering 3`){
                        p <- p %>% add_trace(
                                y = ~Sub_metering_3,
                                name = "Sub metering 3",
                                type = 'scatter',mode = "lines")
                }
                print(p)
        })
        }
)