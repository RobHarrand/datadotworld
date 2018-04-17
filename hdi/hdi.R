library(shiny)
library(plotly)

x1 = data.frame(rnorm(100))
x2 = data.frame(rnorm(100, 3))
y = data.frame(seq(from = 1, to = 100, by = 1))

dt = cbind(x1,x2,y)

colnames(dt) = c('x1', 'x2', 'y')


ui <- fluidPage(
  
  headerPanel("Diamonds Explorer"),
  sidebarPanel(
    selectizeInput("name",
                   label = "Country Name(s) of Interest",
                   #choices = unique(ideal$Name),
                   choices = c('x1','x2'),
                   multiple = T,
                   options = list(maxItems = 5, placeholder = 'Select a name'),
                   selected = 'x1')
  ),
  mainPanel(
    plotlyOutput('trendPlot', height = "900px")
  )
)

server <- function(input, output) {
  
  #add reactive data information. Dataset = built in diamonds data
  dataset <- reactive({
    dt
  })
  
  output$trendPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    # p <- ggplot(dataset(), aes_string(x = input$x1, y = input$y, color = 'red')) + 
    #   geom_point()
    
    p <- ggplot(dataset(), aes_string(x = input$x1, y = input$y)) + 
      geom_point()
    
    # if at least one facet column/row is specified, add it
    #facets <- paste(input$facet_row, '~', input$facet_col)
    #if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p) %>% 
      layout(height = input$plotHeight, autosize=TRUE)
    
  })
  
}

shinyApp(ui, server)