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
monthlysummaries <- readRDS('./Data/PB monthly summaries.rds')

completedata <- rbind(data2010,data2017)

# https://gist.github.com/aagarw30/c593799bc7d8557dc863411bb552e4f4

# Define UI for application that draws a barplot
ui <- pageWithSidebar(    
  
  # Give a title
  headerPanel("Apprehensions in 2010 and 2017"),
  
  # Generate a row with a sidebar
  sidebarPanel(      
    
    # Define the sidebar with one input
    conditionalPanel(
      condition="input.tabselected==Comparison between 2010 and 2017",
      selectInput("sector", "Sector:",choices=completedata$Sector)),
    conditionalPanel(
      condition="input.tabselected==By Year",
      selectInput("year","Year:",choices=monthlysummaries$year))
  ),
  #    sidebarPanel(
  #      selectInput("sector", "Sector:", 
  #                  choices=completedata$Sector)
  #    ),
  
  # Create a barplot
  mainPanel(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabsetPanel(
      id="tabselected",
      tabPanel("Comparison between 2010 and 2017",value=1,plotOutput("distPlot1")),
      tabPanel("By Year",value=2,plotOutput("distPlot2"))
    )
    #      plotOutput("distPlot")
  )
  
)

# Define server logic required for a side by side barplot
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$distPlot1 <- renderPlot({
    
    # Render a barplot
    barplot(height=as.matrix(completedata[completedata$Sector==input$sector,2:13]),
            beside = TRUE,
            las=2,
            main=input$sector,
            ylab="Number of Apprehensions",
            xlab="Month",
            col = c("green","blue"))
    legend("topright",c("2010","2017"),pch=19,col=c("green","blue"),bty="n")
  })
  output$distPlot2 <- renderPlot({
    barplot(height=as.matrix(monthlysummaries[monthlysummaries$year==input$year,2:13]),
            las=2,
            main=imput$year,
            ylab="Number of Apprehentions",
            xlab="Year")
  })
}

# Run the application and see what happens
shinyApp(ui = ui, server = server)
