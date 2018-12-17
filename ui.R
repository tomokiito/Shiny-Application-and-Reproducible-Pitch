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

dataset <- mydata2
        
start <- ymd(substr(dataset$Date[1],1,10))
end   <- ymd(substr(dataset$Date[nrow(dataset)],1,10))

shinyUI(
        pageWithSidebar(
                
                # Application title
                headerPanel("Household Power Consumption"),
                
                # Sidebar with a slider input
                sidebarPanel(
                        dateRangeInput("daterange",
                                       paste("Data range",start,"~",end), 
                                       start = "2007-01-01",
                                       end = "2007-01-07"),
                        checkboxInput("Global active power",
                                      "Global active power", value = TRUE),
                        checkboxInput("Global reactive power",
                                      "Global reactive powe", value = FALSE),
                        checkboxInput("Voltage",
                                      "Voltage", value = FALSE),
                        checkboxInput("Global intensity",
                                      "Global intensity", value = FALSE),
                        checkboxInput("Sub metering 1",
                                      "Sub metering 1", value = FALSE),
                        checkboxInput("Sub metering 2",
                                      "Sub metering 2", value = FALSE),
                        checkboxInput("Sub metering 3",
                                      "Sub metering 3", value = FALSE)
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        plotlyOutput("distPlot"),
                        verbatimTextOutput("text1")
                )
        )
)