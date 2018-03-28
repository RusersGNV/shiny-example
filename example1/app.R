library(shiny)
library(tidyverse)
library(lubridate)
library(gridExtra)

dat <- read_csv("data.csv") %>% 
  select(Site, Date, Salinity, Temperature, Conductivity) %>%
  separate(Date, c("Date", "Time"), sep = " ") %>% 
  mutate(Date = ymd(Date),
         Site = paste("Site", Site))

ui <- fluidPage(
  
  titlePanel("WQ Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Site", 
                  choices=unique(dat$Site)),
      selectInput("site2", "Comparison Site", 
                  choices=c("None" = 0,unique(dat$Site))),
      dateRangeInput("date",
                     label = 'Date range input: yyyy-mm-dd',
                     start = "2017-08-01" , end = Sys.Date() + 14),
      checkboxGroupInput("variable",
                         label = h3("Observation Variable"),
                         choices = list("Salinity (ppt)" = "Salinity",
                                        "Conductivity (mS/cm)"= "Conductivity",
                                        "Temperature (C)" = "Temperature"),
                         selected = c("Salinity", "Temperature"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tables", dataTableOutput("table")),
        tabPanel("Plots", plotOutput("plot"))
      )
    )
  )
)

server <- shinyServer(function(input, output) {
  
  output$table <- renderDataTable({
    dat <- dat %>% 
      filter(Site == input$site,
             Date >= input$date[1] & Date <= input$date[2]) %>% 
      select(Site, Date, Time, input$variable)
    dat
  })
  
  output$plot <-renderPlot({
    dat <- dat %>% 
      filter(Site == input$site | Site == input$site2,
             Date >= input$date[1] & Date <= input$date[2]) %>% 
      select(Site, Date, Time, input$variable) %>% 
      gather("Variable", "Measurement", input$variable) 
    
    if (input$site2 == 0) {
      ggplot(dat, aes(x = Date, y = Measurement)) +
        geom_point() +
        facet_wrap(~ Variable + Site, ncol = 1, scales = "free_y") +
        ylab("")
    } else {
      ggplot(dat, aes(x = Date, y = Measurement)) +
        geom_point() +
        facet_wrap(~ Variable + Site, ncol = 2, scales = "free_y") +
        ylab("")
    }
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
