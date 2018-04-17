library(shiny)
library(plotly)

all = read.csv('all_data.csv')

t = list(
  family='Courier New, monospace',
  size=20,
  color='#7f7f7f'
)

shinyServer(function(input, output, session) {
  
  output$text2 <- renderText({input$select1})
  
  output$text3 <- renderText({input$select2})
  
  output$text4 <- renderText({input$select3})
  
  output$plot1 <- renderPlotly({

    x_title = gsub("_", " ", as.character(input$select1))
    y_title = gsub("_", " ", as.character(input$select2))
    
    x_dt = all[colnames(all) == as.character(input$select1)]
    y_dt = all[colnames(all) == as.character(input$select2)]
    z_dt = all[colnames(all) == as.character(input$select3)]
    
    if ('1' %in% input$checkGroup) 
    {x_dt = log(x_dt)
      x_title = paste('Log', gsub("_", " ", as.character(input$select1)))}
    if ('2' %in% input$checkGroup) 
    {y_dt = log(y_dt)
      y_title = paste('Log', gsub("_", " ", as.character(input$select2)))}
    
    max_z = max(z_dt[,1], na.rm = T)
    scale_factor = (sqrt(max_z)*2)/60
    
     plot_ly(x = x_dt[,1],
             y = y_dt[,1],
             alpha= 0.70,
             mode = "markers",
             type = "scatter", 
             showlegend = F,
             color = all$Country,
             size = I((sqrt(z_dt[,1])*2)/scale_factor),
             hoverinfo = "text",
             text = paste(toupper(all$Country),
                          "</br>",
                          gsub("_", " ", as.character(input$select1)),
                          x_dt[,1],
                          "</br>",
                          gsub("_", " ", as.character(input$select2)),
                          y_dt[,1],
                          "</br>",
                          gsub("_", " ", as.character(input$select3)),
                          z_dt[,1])) %>%
    
       layout(title ="Interactive, Customisable Plot (HDI, MPI, GDI & EF)",
              titlefont = t,
              plot_bgcolor='black',
              xaxis = list(title = x_title,
                           titlefont = t),
              yaxis = list(title = y_title,
                           titlefont = t))


  })
})



