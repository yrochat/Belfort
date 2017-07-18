library(shiny)
library(igraph)

g0 <- erdos.renyi.game(10,.3)
E(g0)$weight <- 1:ecount(g0)
g0$layout <- layout.auto(g0)

ui <- fluidPage(
  
  titlePanel("Manipulating thresholds"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "threshold",
                  label = "Threshold:",
                  min = 1,
                  max = max(E(g0)$weight),
                  step = 1,
                  value = 1)
      
    ),
    
    mainPanel(
      
      plotOutput(outputId = "distPlot")
      
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    g <- g0 %>% 
      delete_edges(E(g0)[weight < input$threshold])
    par(mar=c(0,0,0,0))
    plot(g)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)