library(tidyverse)   
library(shiny)

gpmdata <- read_csv("Gapminder_data.csv")

# user interface
ui6 <- fluidPage(
  
  # application title
  titlePanel("Life Expectancy by Income"),
  
  sidebarLayout(
    sidebarPanel(
      
      # input for year
      sliderInput(inputId = "year",
                  label = "Year:",
                  min = 1800,
                  max = 2020,
                  step = 1,
                  value = 1800), 
      
      # input to select x-axis variable
      varSelectInput(inputId = "xvar", 
                     label = "Select Variable to Display on x-axis", 
                     data = gpmdata %>% dplyr::select(lifeExp, pop, gdpPercap)),
      
      # input to select y-axis variable
      varSelectInput(inputId = "yvar", 
                     label = "Select Variable to Display to y-axis", 
                     data = gpmdata %>% dplyr::select(lifeExp, pop, gdpPercap)),
      
      # input to select 'size' variable
      varSelectInput(inputId = "sizevar", 
                     label = "Select Variable to Display by Size", 
                     data = gpmdata %>% dplyr::select(lifeExp, pop, gdpPercap)),
      
      # interactive output
      uiOutput("countries_menu")
    ),
    
    # show plot and table
    mainPanel(
      fluidRow(plotOutput("plot")),
      
      fluidRow(tableOutput("table"))
    )
  )
)


# server logic
server6 <- function(input, output) {
  
  # reactive expression
  filter_data <- reactive({
    
    year_input <- input$year
    data <- gpmdata %>% filter(year == year_input)
    return(data)
    
  })
  
  # get the list of countries    
  get_countries <- reactive({  
    
    countries <- filter_data() %>% arrange(desc(pop))
    return(countries$country[1:15])   # top 15 countries
    
  })
  
  # check boxes for list of countries
  output$countries_menu <- renderUI({
    
    checkboxGroupInput(inputId = 'countries_menu', 
                       label = 'Names of Most Populous Countries to Display:',
                       choices = get_countries())
  })  
  
  
  # create plot
  output$plot <- renderPlot({
    
    ggplot(data = filter_data()) + 
      geom_point(aes(x = !!input$xvar, y=!!input$yvar, color=continent, size=!!input$sizevar)) +
      geom_text(data=filter_data() %>% filter(country %in% input$countries_menu), 
                aes(x = !!input$xvar, y=!!input$yvar, label=country), vjust=-2) + 
      scale_x_continuous(trans='log2')
    
  })
  
  # create table
  output$table <- renderTable({
    
    filter_data() %>% 
      group_by(continent) %>% 
      dplyr::summarize(Mean_GDP = mean(gdpPercap), 
                Mean_LifeExp = mean(lifeExp),
                Mean_Population = mean(pop))
    
  })
  
}

# run the application 
shinyApp(ui = ui6, server = server6)
