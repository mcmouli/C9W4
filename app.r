#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(lattice)


# Loading Data into from csv file

indata <- read.csv("salesdata.csv", header=TRUE)
colnames(indata)[1]<- "Quarter"

ui <- fluidPage(
   
   # Industry wise sales data
   titlePanel("Industry wise Sales Data"),a("Click Here for Documentation", href=""),
   
   # Sidebar with a slider input to select Quarter, Operating Group 
   sidebarLayout( 
      sidebarPanel( 
        selectInput("qtr", "Select Quarter:",
                    choices = indata$Quarter),
        selectInput("ogp", "Select Operating Group:",
                    choices = indata$og),
        submitButton("Submit")
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3(textOutput("outstring" )),
         plotOutput("myplot")
      )
   )
)

# Define server logic required to plot

server <- function(input, output) {

   output$myplot <- renderPlot({
     
     fltr1 <- subset(indata, (indata$Quarter == input$qtr & indata$og == input$ogp))
     fltr1$amtinm = as.numeric(fltr1$amtinm)
     sumval <- sum(fltr1$amtinm,indata$Quarter == input$qtr & indata$og == input$ogp )
     
     output$outstring <- renderText(paste("Total sales value for Quarter " ,input$qtr ," and Operating Group ",input$ogp, " is ", sumval, "Million USD"))
     
    xyplot(Month~amtinm | input$qtr*input$ogp, fltr1)

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

