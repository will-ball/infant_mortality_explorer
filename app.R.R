## TestApp

library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)

#################
## Shiny App
#################

## Load in Data matrix & create vectors
infmat <- as.matrix(read_rds("infmat.rds"))
Overall <- readRDS("Overall.rds")

xx <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

yy <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016",
        "2017")

#################
## User Interface
#################

ui <- fluidPage(
  
  titlePanel("Infant Mortality Explorer",
             windowTitle = "Infant Mortality Explorer"),
  
  # Sidebar
  sidebarLayout(
    
    sidebarPanel(
      p("The 3D plot included here is a mapping of Infant Mortality Rates for 
      Deprivation groups over time in England. The plot is interactive and can be manipulated
      using your mouse. Hover over any point on the surface for more information."),
      
      br(),
      
      h5(strong("Definitions:")),
      p(strong("Infant Mortality -"), "Deaths of live-born children under the age of 1 year"),
      
      p(strong("Infant Mortality Rate (IMR) -"), "The number of infant deaths in a given year, divided by the
      number of births in the same year."),
      
      p(strong("Index of Multiple Deprivation (IMD) -"), "The official measure of deprivation for small areas. It is designed to identify small
      areas where there are the highest concentrations of different types of deprivation."),
      br(),
      p("Deprivation groups here range from:"), 
      p("1 - Most Deprived"),
      p("10 - Least Deprived."),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      strong("Source:"), a("ONS", 
                           href = "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/010121livebirthsandinfantdeathsbynssec2011to2017andimd2008to2017"
                           )
    ),
  
    # Plotly to be shown in Main Panel
    mainPanel(
      h4("Infant Mortality by Deprivation Group, for England (2008 - 2017)"),
      plotlyOutput("InfMort",
                   width = "auto"),
      h4("Overall Infant Mortality, for England (2008 - 2017)"),
      plotlyOutput("Overall",
                  width = "auto")
      )
  )
)

#################
## Server 
#################

server <- function(input, output) {
  
  axx <- list(
    title = "IMD Group",
    nticks = 10,
    ticketmode = 'array',
    range = c(1,10)
  )
  
  axy <- list(
    title = "Year",
    nticks = 10
  )
  
  axz <- list(
    title = "Infant Mortality Rate"
  )
  
  n_imd <- length(xx)
  n_year <- length(yy)
  
  custom_text <- paste0(
    "For Deprivation group ",
    rep(xx, each = n_imd),
    
    ", in ",
    rep(yy, times = n_year),
    
    ", IMR was ",
    rep(infmat,)
  ) %>%
    matrix(length(yy), length(xx))
  
  ## Render Plotly
  output$InfMort <- renderPlotly(
    {
      p <-   plot_ly(
        x = ~ xx,
        y = ~ yy,
        z = ~ infmat
        ) %>% add_surface(
          colorbar = list(title = "IMR"),
          hoverinfo = 'text',
          text = custom_text
      ) %>% layout(
        scene = list(xaxis= axx,
                     yaxis= axy,
                     zaxis= axz,
                     aspectmode = 'cube')
        )

      return(p)
    }
  )
  
  ## Render line chart
     output$Overall <- renderPlotly(
       {
     ## lineplot
     xaxis <- list(
         nticks = 10,
         ticktext = list(yy)
       )
     
     yaxis <- list(
       nticks = 10,
       range = c(0,5)
     )
     
     p1 <- plot_ly(
       data = Overall,
       x = ~ Year,
       y = ~ IMR,
       type = 'scatter',
       mode = 'lines'
     ) %>% layout(
       scene = list(
         xaxis = xaxis,
         yaxis = yaxis)
       )
     
     return(p1)
       }
     )
}
     
##################
## Call App
##################

shinyApp(ui = ui, server = server)
