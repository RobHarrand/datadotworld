opts = read.csv('opts.csv') #Load options file

shinyUI(fluidPage(
  
  # Application title
  titlePanel("data.world: HDI, MPI, GDI & EF Data"),
  
  sidebarPanel(
    selectInput(inputId = "select1", label = "Select x-axis variable", 
                choices = opts$x),
    selectInput(inputId = "select2", label = "Select y-axis variable", 
                choices = opts$x),
    selectInput(inputId = "select3", label = "Select point-size variable (auto-scaled)", 
                choices = opts$x),
    
    checkboxGroupInput("checkGroup", label = h4("Log axes?"), 
                       choices = list("Log x-axis" = 1, "Log y-axis" = 2)),
    
    tags$hr(style="border-color: black;"),
    h4("All data taken from data.world, specifically, from the discussion on Comparing Multi-dimensional Poverty Index and Ecological Footprint"),
    h5(" "),
    a("data.world", href="https://data.world/", target="_blank"),
    h5(" "),
    a("Discussion link", href="https://data.world/footprint/nfa-2017-edition/discuss/10657", target="_blank"),
    h5(" "),
    tags$hr(style="border-color: black;"),
    h5("Created by @tentotheminus9"),
    h5("Last update 25th April 2017")

  ),
  
  # Show a plot of the generated distribution,
  mainPanel(
    plotOutput('plot1', width = "1000px", height = "1000px",
               dblclick = "plot1_dblclick",
               brush = brushOpts(
               id = "plot1_brush",
               resetOnNew = TRUE
    )),
    textOutput("text4"),
    tags$head(tags$style("#text4{font-size: 22px}"))
)
))