library(shiny)

# load in the required data
data2010 <- readRDS('./Data/PB Apprehensions 2010.rds')
data2017 <- readRDS('./Data/PB Apprehensions 2017.rds')

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  
  # title of the app
  headerPanel("Comparison of Apprehensions in 2010 and 2017"),
  
  # create a sidebar
  sidebarPanel(
    
    # display chosen panel only when a specified tab is selected by the user
    conditionalPanel(condition='input.tabselected=="By Sector"',
                     selectInput("sector", "Sector:",choices=data2010$Sector)),
    conditionalPanel(condition ='input.tabselected=="By Month"',
                     selectInput("month", "Month:",choices=colnames(data2010)[-1]))
  ),
  
  mainPanel(
    # create two tabs in the main panel to display two barplots
    tabsetPanel(
      id='tabselected',
      tabPanel("By Sector",plotOutput("ex1")),
      tabPanel("By Month",plotOutput("ex2")))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # render the first barplot, which is the comparison by sector
  output$ex1 <- renderPlot({
    barplot(height = as.matrix(rbind(data2010[data2010$Sector==input$sector,2:13],
                                     data2017[data2017$Sector==input$sector,2:13])),
            beside = TRUE,
            las=2,
            main=input$sector,
            ylab="Number of Apprehensions",
            xlab="Month",
            col = c("green","blue"),
            cex.names = 0.8)
    legend("topright",c("2010","2017"),pch=19,col=c("green","blue"),bty="n")
  }
  )
  
  # render the second barplot, which is the comparison by month
  output$ex2 <- renderPlot({
      barplot(height= as.matrix(rbind(data2010[,which(colnames(data2010)==input$month)],
                                      data2017[,which(colnames(data2017)==input$month)])),
              beside = TRUE,
              las=2,
              names.arg = data2010$Sector,
              main=input$month,
              ylab="Number of Apprehensions",
              xlab="Sector",
              col = c("red","orange"),
              cex.names=0.6)
    legend("topright",c("2010","2017"),pch=19,col=c("red","orange"),bty="n")
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

