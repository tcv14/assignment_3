#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

data2010 <- read.csv('./Data/BP Apprehensions 2010.csv')

# Define UI for application that draws a barplot
ui <- fluidPage(    
  
  # Give a title
  titlePanel("Apprehensions by Region in 2010"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("sector", "Sector:", 
                  choices=data2010$Sector)
    ),
    
    # Create a barplot
    mainPanel(
      plotOutput("distPlot")
#      tabsetPanel(type="tabs",
#                  tabPanel("2010 Plot", plotOutput("distPlot")),
#                  tabPanel("2017 Plot", plotOutput("distPlot"))
#                  )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$distPlot <- renderPlot({
    
    # Render a barplot
    barplot(height=as.matrix(data2010[data2010$Sector==input$sector,2:13]),
            main=input$sector,
            ylab="Number of Apprehensions",
            xlab="Month")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

###

data2017 <- read.csv('./Data/PB Apprehensions 2017.csv')

# Define UI for application that draws a barplot
ui <- fluidPage(    
  
  # Give a title
  titlePanel("Apprehensions by Region in 2017"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("sector", "Sector:", 
                  choices=data2017$Sector)
    ),
    
    # Create a barplot
    mainPanel(
      plotOutput("distPlot")
      #      tabsetPanel(type="tabs",
      #                  tabPanel("2010 Plot", plotOutput("distPlot")),
      #                  tabPanel("2017 Plot", plotOutput("distPlot"))
      #                  )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$distPlot <- renderPlot({
    
    # Render a barplot
    barplot(height=as.matrix(data2017[data2017$Sector==input$sector,2:13]),
            main=input$sector,
            ylab="Number of Apprehensions",
            xlab="Month")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
