library(DT)
library(plotly)
library(readxl)
library(shiny)
library(shinydashboard)
library(tidyquant)

################################################################################
## Import Data
################################################################################

data = readxl::read_xlsx("./data/data.xlsx", sheet = 1) %>% as.data.frame()

################################################################################
## UI code
################################################################################

ui <- fluidPage(
  
  # Enable Shiny Dashboard functions
  useShinydashboard(),
  
  # Enable the use of font awesome icons
  tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
  
  # Modify the default color to match the darkly theme
  tags$style(".small-box.bg-yellow { background-color: #375A7F !important; color: #000000 !important; }"),
  
  tags$style(".fa-alpha:before { font-weight: 700; content: '\03b1'; }"),
  
  tags$style(".value-box .caption { color: white; }"),
  
  
  # Open navbar page and customize the theme
  navbarPage(
    title = "Portfolio Intelligence",
    theme = shinytheme("darkly"),
    tags$head(tags$style(HTML('.navbar-static-top {background-color: #375A7F;}',)))
  ),

# Select ticker symbols for portfolio
################################################################################ 
sidebarLayout(
  sidebarPanel(
    # Date selector
    dateRangeInput("dates", label = h3("Date range"), 
                   start = "2023-01-01", end = "2023-12-31"),
    # Ticker Selector
    selectInput("ticker", 
                label = h3("Choose a ticker symbol"), 
                # choices = c("A","B","C","D","E"),
                choices = data$Ticker,
                multiple = TRUE,
                selectize = TRUE),
    # Weights Inputs
    textInput("weights", label = h3('Enter weights seperated by ";"'), 
              value = "0.00"),
    # Choose Benchmark
    radioButtons("benchmark", label = h3("Choose Benchmark"),
                 choices = list("S&P 500" = 1, "VIX" = 2, "Dow Jones" = 3), 
                 selected = 1),
    # Action Buttons
    fluidRow(
      # Create Portfolio
      actionButton("create", label = "Create Portfolio"),
      # Calculate Portfolio Metrics
      actionButton("calc", label = "Calculate Metrics"),
      # Clear Selections
      actionButton("reset", label = "Reset")
      ),
    
    DTOutput("table1")
    ),
  
    mainPanel(
      fluidRow(
        valueBoxOutput("box1", width = 3),
      
        valueBoxOutput("box2", width = 3),
        
        valueBoxOutput("box3", width = 3),
        
        valueBoxOutput("box4", width = 3)
      ),
      
      plotlyOutput("plot",height=400),
      
      # fluidPage(
        fluidRow(
          DTOutput("table2")
        )
      # )
    ) # Close mainPanel
  ) # Close sidebar
) # Close fluidPage

################################################################################
## Server
################################################################################

server <- function(input, output, session) {
  
# Reactive data table with tickers and weights from user inputs
################################################################################

  df1 <- reactiveVal(
    data.frame(
      Ticker = character(0), 
      Weight = numeric(0)
    )
  )
  
  observeEvent(input$create, {
    newdat <- data.frame(
      Ticker = input$ticker, 
      Weight = as.numeric(strsplit(input$weights, split = ";")[[1]])
    ) 
    df1(dplyr::bind_rows(df1(), newdat))
  })

  output$table1 = renderDT({
    datatable(df1(), options = list(dom = 't')) %>% 
      formatStyle(
        c('Ticker','Weight'),
        color = 'white'
      )
  })

# Clear user inputs on reset button click
################################################################################
  observeEvent(input$reset, {
    
    updateSelectInput(session, "ticker", selected = character(0))
    
    updateTextInput(session, "weights", value = "0.00")
  })

# Calculate portfolio metrics
################################################################################
  # wts <- c(0.33, 0.33, 0.33)
  
  df2 = reactiveVal(
    data.frame(
      symbol = character(0),
      # date = integer(0),
      Ra = numeric(0)
    )
  )
  
  df3 = reactiveVal(
    data.frame(
      symbol = character(0),
      # date = integer(0),
      Ra = numeric(0)
    )
  )
  
  
  observeEvent(input$calc, {
    
    # Calculate 
    Ra = data.frame(symbol = input$ticker) %>% 
      tq_get(get  = "stock.prices",
           from = input$dates[1], 
           to   = input$dates[2]) %>% 
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "Ra")
    
    df2(dplyr::bind_rows(df2(), Ra))
    
      # Calculate Benchmark returns    
    Rb = data.frame(symbol = dplyr::case_when(
      input$benchmark == 1 ~ "^GSPC",
      input$benchmark == 2 ~ "VIX",
      input$benchmark == 3 ~ "DJI"
    )) %>%
      tq_get(get  = "stock.prices",
             from = input$dates[1], 
             to   = input$dates[2]) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "Rb")
    
    # Calculate Portfolio returns
    Rp = Ra %>%
      tq_portfolio(assets_col  = symbol, 
                   returns_col = Ra, 
                   weights     = as.numeric(strsplit(input$weights, split = ";")[[1]]),
                   col_rename  = "Ra")
    
    RpRb = left_join(Rp, 
                     Rb,
                     by = "date") %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
    
    df3(dplyr::bind_rows(df3(), RpRb))

  })

  
  output$table2 = renderDT({
    datatable(df3(), options = list(dom = 't')) %>% 
      formatStyle(
        c('symbol','Ra'),
        color = 'white'
      )
  })

# Output portfolio metrics to valueBoxes
################################################################################
  output$box1 = renderValueBox({
    
    valueBox(
      color = 'yellow',
      value = as.numeric(df3()$Alpha),
      subtitle = "Portfolio Alpha",
      icon = icon(name = "fa-alpha", style = "color: #ffffff", lib = "font-awesome")
    )
  })
  
  output$box2 = renderValueBox({
    
    valueBox(
      color = 'yellow',
      value = as.numeric(df3()$Beta),
      subtitle = "Portfolio Beta",
      icon = icon(name = "fa-beta", style = "color: #ffffff", lib = "font-awesome")
    )
  })
  
  output$box3 = renderValueBox({
    
    valueBox(
      color = 'yellow',
      value = as.numeric(df3()$Beta),
      subtitle = "Excess Return",
      icon = icon(name = "fa-beta", style = "color: #ffffff", lib = "font-awesome")
    )
  })
  
  output$box4 = renderValueBox({
    
    valueBox(
      color = 'yellow',
      value = 0.00,
      subtitle = "Another Metric",
      icon = icon(name = "credit-card", style = "color: #ffffff", lib = "font-awesome")
    )
  })
  
# Create chart of individual stock performance 
################################################################################  

  output$plot <- renderPlotly({
    print(
      ggplotly(df2() %>%
                 ggplot(aes(date, Ra, colour = symbol)) +
                 geom_line(size = 1, alpha = .9)
      )
    )
  })
  
}

shinyApp(ui = ui, server = server)

################################################################################
## ~fin~
################################################################################


