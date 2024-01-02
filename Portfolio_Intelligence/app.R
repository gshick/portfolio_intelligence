
library(DT)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(readxl)
library(tidyquant)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Enable Shiny Dashboard functions
  useShinydashboard(),
  
  # Enable the use of font awesome icons
  # tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
  
  # Modify the default color to match the darkly theme
  tags$style(".small-box.bg-yellow { background-color: #375A7F !important; color: #000000 !important; }"),
  
  # Open navbar page and customize the theme
  navbarPage(
    title = "Portfolio Intelligence",
    theme = shinytheme("darkly"),
    tags$head(tags$style(HTML('.navbar-static-top {background-color: #375A7F;}',))),
  ),
  
  
  # Application title
  # titlePanel("Portfolio Intelligence"),
  
  
  # theme = shinytheme("darkly"),
  # tags$head(tags$style(HTML('.navbar-static-top {background-color: #081c3c;}',))),
  
# Select ticker symbols for portfolio
################################################################################ 
  sidebarLayout(
    sidebarPanel(
      # Date selector
      dateRangeInput("dates", label = h3("Date range")),
      # Ticker Selector
      selectInput("ticker", 
                  label = h3("Choose a ticker symbol"), 
                  choices = data$Ticker,
                  multiple = TRUE,
                  selectize = TRUE),
      # Weights Inputs
      textInput("text", label = h3('Enter weights seperated by ";"'), value = "0.00"),
      # Choose Benchmark
      radioButtons("radio", label = h3("Choose Benchmark"),
                   choices = list("S&P 500" = 1, "VIX" = 2, "Dow Jones" = 3), 
                   selected = 1),
      # Action Buttons
      fluidRow(
        # Update Metrics
        actionButton("action", label = "Update Metrics"),
        # Clear Selections
        actionButton("action", label = "Clear ")
      ),
      
      # hr(),
      # fluidRow(column(3, verbatimTextOutput("ticker"))),
      # 
      # DTOutput('values')
      
    ),
    
    # mainPanel(
      fluidPage(
      # plotOutput("distPlot")
      fluidRow(
        valueBoxOutput("box1", width = 2)
      )
      ,

      fluidRow(
        DTOutput('ticker_table')
      )
    )
  ) # Close sidebar
) # Close fluidPage

# table_data <- data.frame(Ticker = as.character(), Weights = as.numeric(), check.names = FALSE)

# data = readxl::read_xlsx("./data/data.xlsx", sheet = 1)

################################################################################
##Server
################################################################################
server <- function(input, output) {

# Read in source data
################################################################################
  data = readxl::read_xlsx("./data/data.xlsx", sheet = 1) 

  output$box1 = renderValueBox({
    
    # tmp =  map_data %>% filter(map_data$city_dt == input$dates) 
    
    valueBox(
      color = 'yellow',
      value = "0.00",
      subtitle = "Portfolio Beta"
    )
  })
  
  
  dt = datatable(data.frame(observe(input$ticker)))
  # %>% 
  #   formatStyle(color = 'white')

  
  output$ticker_table = DT::renderDataTable({
    dt
    # temp = data.frame(input$ticker, check.names = FALSE)
  })
  
  # df = reactive(data.frame(input$ticker))
  # output$values = renderDT(df
  #   # datatable(df()) 
  #   # %>% 
  #   #   formatStyle(columns = names(df), color = "black")
  # )
  
  # Ticker Inputs
  # output$ticker <- renderPrint({ input$ticker })
  
  # output$value <- renderPrint({ input$text })
  
  # output$value <- renderPrint({ input$dates })
  
  # observe({
  #   updateSelectInput(session, "ticker", choices = data()$Ticker)
  # })

}

# Run the application 
shinyApp(ui = ui, server = server)
