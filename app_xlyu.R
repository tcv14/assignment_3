#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# load in the required data and rbind to create one data frame for side by side barplots later
data2010 <- readRDS('./Data/PB Apprehensions 2010.rds')
data2017 <- readRDS('./Data/PB Apprehensions 2017.rds')

completedata <- rbind(data2010,data2017)
completedata$year[1:9] <- '2010'
completedata$year[10:18] <- '2017'

# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("Apprehensions in 2010 and 2017"),
  
  sidebarPanel(      
    
    # Define the sidebar with one input
    conditionalPanel(
      condition="input.tabselected==By Sector",
      selectInput("sector", "Sector:",choices=completedata$Sector)),
    conditionalPanel(
      condition="input.tabselected==By Year",
      selectInput("month","Month:",choices=colnames(completedata)[-1]))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("By Sector",plotOutput("ex1")),
      tabPanel("By Month",plotOutput("ex2")))
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$ex1 <- renderPlot({
    
    # Render a barplot
    barplot(height = as.matrix(rbind(data2010[data2010$Sector==input$sector,2:13],
                           data2017[data2017$Sector==input$sector,2:13])),
            beside = TRUE,
            las=2,
            main=input$sector,
            ylab="Number of Apprehensions",
            xlab="Month",
            col = c("green","blue"))
    legend("topright",c("2010","2017"),pch=19,col=c("green","blue"),bty="n")
  }
  )
  
  output$ex2 <- renderPlot({
    matrix <- as.matrix(rbind(data2010[,which(colnames(completedata)==input$month)],
                    data2017[,which(colnames(completedata)==input$month)]))
    colnames(matrix) <- data2010$Sector
      barplot(height= matrix,
              beside = TRUE,
              las=2,
              names.arg = data2010$Sector,
              main=input$month,
              ylab="Number of Apprehentions",
              xlab="Sector",
              col = c("green","blue"))
    legend("topright",c("2010","2017"),pch=19,col=c("green","blue"),bty="n")
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

