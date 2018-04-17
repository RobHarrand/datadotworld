library(plotly)

GDI = read.csv('GDI_tidied.csv', stringsAsFactors = T, header = T)
HDI = read.csv('HDI_tidied.csv', stringsAsFactors = T, header = T)
MPI = read.csv('MPI_tidied.csv', stringsAsFactors = T, header = T)

MPI$Population_in_MDP_k = gsub(",", ".",  MPI$Population_in_MDP_k)
MPI$Population_in_MDP_k = as.numeric(MPI$Population_in_MDP_k)

all = merge(GDI, HDI, by.x = 'Country', by.y = 'Country')
all = merge(all, MPI, by.x = 'Country', by.y = 'Country', all.x = T)

all$Year_of_survey = gsub("/$|M|D|N|P", "", as.character(all$MPI_year_and_survey))
all$Year_of_survey = as.factor(all$Year_of_survey)


Sys.setenv("plotly_username"="robh")
Sys.setenv("plotly_api_key"="ILTpvNQjVCzCgH99TgYW")

p = plot_ly(x = all$Human_Development_Index, 
            y = all$MPI_index,
            alpha= 0.70,
            mode = "markers", 
            type = "scatter",
            showlegend = F, 
            color = all$Country,
            size = I(sqrt(all$Population_in_MDP_k)*2), 
            hoverinfo = "text", 
            text = paste(toupper(all$Country),
                    "</br>Human Development Index: ", 
                    all$Human_Development_Index,
                    "</br>Multi-dimensional Poverty Index: ", 
                    all$MPI_index,
                    "</br>Life expectency at birth: ",
                    all$Life_expectancy_at_birth,
                    "</br>Population in Multidimensional Poverty (thousands): ",
                    all$Population_in_MDP_k,
                    "</br>MPI Year of survey: ",
                    all$Year_of_survey)) %>%

layout(title ="HDI vs MPI", 
       annotations = list(text = paste("Point size proportional to </br> Population in Multi-dimensional Poverty </br> (thousands)"), 
                      x = 0.7,
                      y = 0.5,
                      showarrow = F,
                      font = list(size = 12,
                                  color = 'white')),
       titlefont = t,
       plot_bgcolor='black',
       xaxis = list(title = "Human Development Index", 
                    titlefont = t),
       yaxis = list(title = "Multi-dimensional Poverty Index", 
                    titlefont = t))


plotly_POST(p, sharing = 'public')



p
