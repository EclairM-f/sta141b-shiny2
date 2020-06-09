#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinyBS)
library(shiny)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr)
library(rvest)
library(maps)
library(lubridate)
library(shinythemes)
usethis::edit_r_environ("project")
readRenviron(".Renviron")

endpoint <- "https://calendarific.com/api/v2/holidays"
html1 <- read_html("https://calendarific.com/supported-countries")
country_list <- html1 %>% html_nodes("td") %>% html_node("a")%>% html_text %>% na.omit
country_abb <- html1 %>% html_nodes("td.text-center") %>% html_text %>% na.omit
name_to_abb <- function(name){
    country_abb[which(country_list==name)]
     }

list1  <- function(x){table$type[[x]][1]}
states_list <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')


get_table <- function(r){
  json <- content(r, as = "text",encoding = "UTF-8")
  table <- fromJSON(json)$response$holidays
  if (length(table)==0){print("Oops! There aren't any holidays within the time frame of your choice. Please change year or month.")}else{
    table <- table %>% mutate(country=table$country$name,date=as.character(as.Date(table$date$iso)),type=sapply(1:length(table$type),list1),states=NULL)
    return(table)}}
state_to_abb <- function(state){
  info <- cbind(state.name,state.abb) %>% as.data.frame %>% filter(state.name==state)
  return(paste("us-",str_to_lower(info$state.abb),sep=""))
  }




ui <- fluidPage(theme = shinytheme("cerulean"),
    
    titlePanel("Holiday Calculator"),
    navbarPage(
      "A powerful date calculator",
      id = "main_navbar",
      ########### The first tabpanel
        tabPanel("Search All Holidays",
        sidebarLayout(
          textOutput("text1",container=h4),
          bsModal("modal", "Your Output", "go1", size = "large",
              textOutput("text2",container = h5),
              tableOutput("table1"),
              downloadButton('downloadPlot', 'Download'))
        
        ),
        
        
        sidebarPanel(
          selectInput('country', 'Please Enter a Country',country_list,selected=""),
          numericInput('year', 'Enter a Year Prior to 2049',2020,min=0,max=2049),
          selectInput("month",checkboxInput("check1","Month(Optional)",FALSE),c(1:12)), 
          actionButton("go1", "Go"),
          width=5
        )
        ),
      
      ########### The second tabpanel
      tabPanel("Find Next Holiday",
            
               textOutput("text3",container = h4),
               sidebarPanel(
                 selectInput('country2', 'Please Enter a Country',country_list,selected=""),
                 dateInput('date2', 'Enter a date Prior to 2049'),
                 actionButton("go2", "Go")
                 
               ),
               mainPanel(
                 textOutput("text8",container = h3),
                 textOutput("text4",container = h4),
                 textOutput("text7",container = h3),
                 textOutput("text5",container = h4)
                 )
               ),
      
      ########### The third tabpanel
      tabPanel("Distribution of Holidays(US)",
               sidebarPanel(
                 selectInput('country3', 'Country',"United States",selected="United States"),
                 selectInput('states', "Select the State You're in",states_list,selected="California"),
                 numericInput('year3', 'Enter a year Prior to 2049',2020)
                 
               ),
               mainPanel(
    
                 plotOutput ("plot1"),
                 textOutput("text9"),
                 tableOutput("table3")
               )
               
               )
        ),

)

#####################################################################################
server <- function(input, output) {

############## The second tabpanel: find next holiday  
    observeEvent(input$go2, {
       r <- GET("https://calendarific.com/api/v2/holidays",query=list(api_key=Sys.getenv("TOKEN"), country=name_to_abb(input$country2), year=year(input$date2)))
      
       list1  <- function(x){table$type[[x]][1]}

       get_table <- function(r){
          json <- content(r, as = "text",encoding = "UTF-8")
          table <- fromJSON(json)$response$holidays
          table <- table %>% mutate(country=table$country$name,date=as.character(as.Date(table$date$iso)),type=sapply(1:length(table$type),list1),states=NULL)
          return(table)
        }
      x <- input$date2
      table <- get_table(r)
      table <-  table%>% mutate(diff_time=difftime(x,table$date,units="days"))
      table <- table %>% arrange(desc(table$diff_time))
      closest_holiday <- table[which(table$diff_time<0)[1],]
      
      
      
      output$text4 <- renderText(
        paste("The next holiday after ",
            input$date2, 
            " in ",
            input$country2,
            " is [",
            closest_holiday$name,
            "]. It's on ",
            closest_holiday$date,
            if(floor(-closest_holiday$diff_time)==0){", which is exactly the same day as "}else{paste(", which is ",floor(-closest_holiday$diff_time)," days beyond ")},
            input$date2,
            ". It's type is [",
            closest_holiday$type,
            "].",
            sep=""))
      output$text7 <- renderText("Description:")
      output$text5 <- renderText(closest_holiday$description)
    })
    

    output$text2 <- renderText({
        paste("Country = ",input$country,", Year = ",input$year, ", Month = ", if (input$check1==FALSE) {"all"} else{input$month} ,sep="")
        })

   
    output$text1 <- renderText(paste("Today's Date:",Sys.Date()))
    output$text8 <- renderText("Holiday Information")
    
    ############## The first tabpanel plot: 
      output$plot1 <- renderPlot({
      r <- GET(endpoint,query=list(api_key=Sys.getenv("TOKEN"), country=name_to_abb(input$country3), year=input$year3,location=state_to_abb(input$states)))
      json <- content(r, as = "text",encoding = "UTF-8")
      table <- fromJSON(json)$response$holidays
      table <- table %>% mutate(MONTH=table$date$datetime$month,DAY=NULL,country=NULL,description=NULL,date=NULL,locations=NULL,states=NULL)
      
    ############## The third tabpanel histogram and table output: 
      output$text9 <- renderText(paste("There are ",length(table$MONTH)," holidays in ",input$states," in ",input$year3," in total.",sep=""))
      output$table4 <- renderTable({
        df <- table$type %>% unlist %>% as.data.frame
        colnames(df) <- c("TYPE")
        new_df <- df %>% group_by(TYPE) %>% count()
        colnames(new_df) <- c("Holiday Type","Number of Holidays")
        })
      output$table3 <-renderTable({new_df})
      hist(table$MONTH,xlim=c(0,12),breaks=c(0:12),xlab="Month",main="Histogram of Month vs. Frequency",col="lightgreen")
      })

    ############## Today's date output  
    output$text3 <- renderText(paste("Today's Date:",Sys.Date()))
    output$text6 <- renderText("Please enter a country and date to search for the next holiday")
    output$table1 <- renderTable({

        if (input$check1==FALSE){
        r <- GET(endpoint,query=list(api_key=Sys.getenv("TOKEN"), country=name_to_abb(input$country), year=input$year))
            }
        else{
        r <- GET(endpoint,query=list(api_key=Sys.getenv("TOKEN"), country=name_to_abb(input$country), year=input$year, month=input$month))
        }
         table <- get_table(r)
        table
        })
        
  
    ################ Download the table    
    output$downloadPlot <- downloadHandler(
        
        filename = paste("HolidayTable-",input$country,"-",input$year,"-",as.numeric(input$month)*input$check1,".csv",sep=""),
        content = function(file){
            if (input$check1==FALSE){
                r <- GET(endpoint,query=list(api_key=Sys.getenv("TOKEN"), country=name_to_abb(input$country), year=input$year))
            }
            else{
                r <- GET(endpoint,query=list(api_key=Sys.getenv("TOKEN"), country=name_to_abb(input$country), year=input$year, month=input$month))
            }
            table <- get_table(r)
            write.csv(table, file)
        }
        ) 

}

# Run the application 
shinyApp(ui = ui, server = server)


#还需要修改api key
