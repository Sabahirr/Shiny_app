
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

# Load datasets
injuries <- read_csv("injuries.csv")
products <- read_csv("products.csv")
population <- read_csv("population.csv")

merged_df <- merge(injuries, products, by = "prod_code")


# UI
ui <- fluidPage(
  theme = shinythemes::shinytheme('flatly'),
  titlePanel(title = h1('Injury Analysis Based on Product Title', align = 'center'),
             windowTitle = 'DSA - Case Study'),
  
  
    selectInput("title", "Select Product Title:", unique(merged_df$title)),
    
   
    tabsetPanel(
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Histogram of Ages", plotlyOutput("age_histogram")),
      tabPanel("Top Locations", plotlyOutput("top_locations"))),
    
    fluidRow(
      column(2,
             dateRangeInput('date',
                            label = 'When the person was seen in hospital',
                            start = min(merged_df$trmt_date),
                            end = max(merged_df$trmt_date)),
             br(),
             sliderInput('age',
                         label = 'Age of patients:',
                         min = min(merged_df$age),
                         max = max(merged_df$age),
                         value = c(20, 45))),
      
      column(5,
             h3(strong('Places where the accident occurred:')),
             plotOutput('Location_result')),
      
      column(5,
             h3(strong('Basic diagnosis of injury:')),
             plotOutput('Diag_result'))),
      
  
  fluidRow(
    column(7,
           h3(strong('Injuries per 10000 people:')),
           plotOutput('injuries_per_result')),
    column(5,
           h3(strong('Location of the injury on the body:')),
           plotOutput('Bodypart_result')))


  )


# Server
server <- function(input, output) {
  
  # Filter data based on selected title
  filtered_data <- reactive({
    subset(merged_df, title == input$title)
  })
  
  # Summary
  output$summary <- renderPrint({
    summary(filtered_data())
  })
  
  # Age Histogram
  output$age_histogram <- renderPlotly({
    ggplotly(
      ggplot(filtered_data(), aes(x = age)) +
        geom_histogram(binwidth = 5, fill = "#66c2a5") +
        labs(title = "Age Distribution", x = "Age", y = "Frequency")
    )
  })
  
  # Top Locations
  output$top_locations <- renderPlotly({
    top_locations <- filtered_data() %>%
      group_by(location) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10)
    
    ggplotly(
      ggplot(top_locations, aes(x = reorder(location, -count), y = count)) +
        geom_bar(stat = "identity", fill = "#fc8d62") +
        labs(title = "Top Locations for Injuries", x = "Location", y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  
  output$Location_result <- renderPlot({
    ggplot(data = filtered_data() %>%
             filter(between(trmt_date, min(input$date), max(input$date)),
                    between(age, min(input$age), max(input$age)))%>% 
             count(location, sort = TRUE), aes(x = '', y = n, fill = location)) +
      geom_col(color = 'black') +
      geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
      coord_polar(theta = 'y') +
      scale_fill_brewer() +
      theme_bw()
  })
  
  output$Diag_result <-  renderPlot({
    ggplot(data = filtered_data() %>%
             filter(between(trmt_date, min(input$date), max(input$date)),
                    between(age, min(input$age), max(input$age))) %>%
             count(diag, sort = T), aes(x = n , y = diag))+
      geom_col(aes(fill = diag))+
      theme(legend.position = 'none')+scale_fill_hue(c = 20)+
      geom_text(aes(label = n), vjust = 0.2, size = 3, col = 'black')
  })
  
  output$injuries_per_result <- renderPlot({
    filtered_data() %>%
      filter(between(trmt_date, min(input$date), max(input$date)),
             between(age, min(input$age), max(input$age))) %>%
      count(age, sex) %>%
      merge(population, by =c('age', 'sex'), all.x = T) %>%
      mutate(rate = n / population * 10000) %>%
      ggplot(aes(age, rate, colour = sex))+
      geom_line()
  })
  output$Bodypart_result <- renderPlot({
    ggplot(data = filtered_data() %>%
             filter(between(trmt_date, min(input$date), max(input$date)),
                    between(age, min(input$age), max(input$age))) %>%
             count(body_part, sort = T), aes(x = n , y = body_part))+
      geom_col(aes(fill = 'yellow'))+
      theme(legend.position = 'none')+scale_fill_hue(c = 20)+
      geom_text(aes(label = n), vjust = 0.2, size = 3, col = 'black')
  })
  
  
  
}

# Run the Shiny app
shinyApp(ui, server)
