library(shiny) #Import Shint
library(ggplot2) #Import ggplot2

all = read.csv('HDI_MPI_GDI_EF_Merged_v001.csv') #Load the data

#Set font
t = list(
  family='Courier New, monospace',
  size=20,
  color='#7f7f7f'
)

shinyServer(function(input, output, session) {
  
  output$text2 <- renderText({input$select1})
  
  output$text3 <- renderText({input$select2})
  
  output$text4 <- renderText({input$select3})
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({

    x_title = gsub("_", " ", as.character(input$select1)) #Set x-axis title to input1
    y_title = gsub("_", " ", as.character(input$select2)) #Set y-axis title to input2
    
    x_dt = all[colnames(all) == as.character(input$select1)]
    y_dt = all[colnames(all) == as.character(input$select2)]
    z_dt = all[colnames(all) == as.character(input$select3)]
    
    if ('1' %in% input$checkGroup) #Check to see if log tick-boxes are ticked
    {x_dt = log(x_dt)
    x_title = paste('Log', gsub("_", " ", as.character(input$select1)))}
    if ('2' %in% input$checkGroup) 
    {y_dt = log(y_dt)
    y_title = paste('Log', gsub("_", " ", as.character(input$select2)))}
    
    combo = cbind(all$Country, x_dt, y_dt,z_dt)
    
    ex = is.na(combo[,2] | combo[,3] | combo[,4])
    combo = combo[!ex,]
    
    max_z = max(combo[,4], na.rm = T) 
    scale_factor = (sqrt(max_z)*2)/30 #Scale the size, or else point sizes might be huge!
    
    x_pos_text = ((max(combo[,2])-min(combo[,2]))*0.5)+min(combo[,2])
    y_pos_text = ((max(combo[,3])-min(combo[,3]))*0.9)+min(combo[,3])
    
    #Plot,
    ggplot(data=combo, aes_string(x=names(combo)[2], 
                                       y=names(combo)[3],
                                       alpha= 0.70,
                                       color = combo[,1],
                                       size = I((sqrt(combo[,4])*2)/scale_factor))) +
        geom_point() +
        theme_bw() +
        theme(legend.position="none") + 
        geom_text(aes(label=combo[,1]),hjust=0, vjust=0, size=5, col='black') +
        xlab(x_title) +
        ylab(y_title) +
        theme(text = element_text(size=20)) +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)

  })
  
  
  output$text4 <- renderText({paste("Point size proportional to", gsub("_", " ", input$select3))})
  
  #Zoom function,
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
})
