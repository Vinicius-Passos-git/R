install.packages("shiny")
library(shiny)

ui <- fluidPage(
  titlePanel(title = "Shiny Exercise"),
  sidebarPanel(width = 4,
               sliderInput("RandNum", label = "choose a number", 
                           value = 20, min = 10, max = 100)
  ),
  mainPanel(wideth = 8, plotOutput("scatter"))
)

server <- function(input, output)
{output$scatter <- renderPlot({
  x <- rnorm(input$RandNum)
  y <- rnorm(input$RandNum)
  plot(x, y, xlab = "x", ylab = "y", main = "Scatter plot")
})

}

shinyApp(ui = ui, server = server)
