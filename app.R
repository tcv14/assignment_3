#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# load in teh required data and rbind to create one data frame for side by side barplots later
data2010 <- readRDS('./Data/PB Apprehensions 2010.rds')
data2017 <- readRDS('./Data/PB Apprehensions 2017.rds')
completedata <- rbind(data2010,data2017)

# Define UI for application that draws a barplot
ui <- fluidPage(    
  
  # Give a title
  titlePanel("Apprehensions by Region in 2010 and 2017"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("sector", "Sector:", 
                  choices=completedata$Sector)
    ),
    
    # Create a barplot
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      plotOutput("distPlot")
    )
    
  )
)

# Define server logic required for a side by side barplot
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$distPlot <- renderPlot({
    
    # Render a barplot
    barplot(height=as.matrix(completedata[completedata$Sector==input$sector,2:13]),
            beside = TRUE,
            las=2,
            main=input$sector,
            ylab="Number of Apprehensions",
            xlab="Month",
            col = c("green","blue"))
    legend("topright",c("2010","2017"),pch=15,col=c("green","blue"),bty="n")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
