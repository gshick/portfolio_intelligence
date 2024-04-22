library(dplyr)
library(DT)
library(plotly)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyWidgets)
library(tidyquant)

################################################################################
## Import Data
################################################################################

data = readxl::read_xlsx("./data/data.xlsx", sheet = 1) %>% as.data.frame()

################################################################################
## UI code
################################################################################

ui <- fluidPage(

  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "pi_style.css")
  ),
  
  # Enable Shiny Dashboard functions
  useShinydashboard(),
  
  # Enable the use of font awesome icons
  tags$style("@import url(https://use.fontawesome.com/releases/v6.5.0/css/all.css);"),
  
  # Modify the default color to match the darkly theme
  # tags$style(".small-box.bg-blue { background-color: #375A7F !important; color: #000000 !important; }"),
  # 
  tags$style(".alpha:before { font-weight: 700; content: '\03b1'; }"),

  
           # <li class="active">
           #   <a href="#tab-9422-1" data-toggle="tab" data-bs-toggle="tab" data-value="Plot">
           #     <i aria-label="bar-chart-o icon" class="far fa-bar-chart-o fa-fw" role="presentation"></i>
           #     Plot
           #   </a>
           # </li>
           # 
  

  
  
  # Open navbar page and customize the theme
  navbarPage(
    # title = "Portfolio Metrics",
    title = div(img(src='header_icon.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60), "Portfolio Metrics"),
    windowTitle="Portfolio Metrics",
    theme = shinytheme("darkly"),
    # tags$head(tags$style(HTML('.navbar-static-top {background-color: #375A7F;}',)))
  ),

# Select ticker symbols for portfolio
################################################################################ 
sidebarLayout(
  sidebarPanel(
    
    h3("Step 1: Select a Date Range to Analyze", style = 'font-weight: bold;'),
    
    # Date selector
    dateRangeInput("dates", label = h4("Date range"), 
                   start = "2023-01-01", end = "2023-12-31"),
    
    # Line to separate the date range inputs from the calculator input section
    hr(),
    
    h3("Step 2a: Manually Create a Portfolio", style = 'font-weight: bold;'),
    
    # Ticker Selector
    selectInput("ticker", 
                label = h4("Choose a ticker symbol"), 
                # choices = c("A","B","C","D","E"),
                choices = data$Ticker,
                multiple = TRUE,
                selectize = TRUE),
    
    # Use Equal Weights for Portfolio
    actionButton("equal", label = "Use Equal Weighting"),
    
    # Weights Inputs
    textInput("weights", label = h4('Enter weights seperated by a ","'), 
              value = "0.00"),
    
    # Choose Benchmark
    radioButtons("benchmark", label = h4("Choose Benchmark"),
                 choices = list("S&P 500" = 1, "VIX" = 2, "Dow Jones" = 3), 
                 selected = 1),
    # Action Buttons
    fluidRow(
      # Calculate Portfolio Metrics
      actionButton("calc", label = "Calculate Metrics from Inputs"),
      # Clear Selections
      actionButton("reset", label = "Clear Inputs")
    ),
    
    # Line to separate the calculator inputs from the file input section
    hr(),
    
    h3("Step 2b: Or Upload a File to Analyze", style = 'font-weight: bold;'),
    
    # Copy the line below to make a file upload manager
    fileInput("file", label = h4("Select a file with tickers and weights"), 
              accept = c(".xlsx")),
    
    hr(),
    
    # Action Buttons
    fluidRow(
      # Create Portfolio
      actionButton("file_calc", label = "Calculate Metrics from File"),
    ),
    
    # fluidRow(column(4, verbatimTextOutput("value"))),
    
    # DTOutput("table1")
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
  
  # You can access the value of the widget with input$file, e.g.
  output$value <- renderPrint({
    str(input$file)
  })
  

  # df1 <- reactiveVal(
  #   data.frame(
  #     Ticker = character(0), 
  #     Weight = numeric(0)
  #   )
  # )
  
  # observeEvent(input$ticker, {
  #   newdat <- data.frame(
  #     Ticker = input$ticker, 
  #     Weight = as.numeric(strsplit(input$weights, split = ",")[[1]])
  #   ) 
  #   # df1(dplyr::bind_rows(df1(), newdat))
  #   df1(newdat)
  # })

  output$table1 = renderDT({
    datatable(df1(), options = list(dom = 't')) %>% 
      formatStyle(
        c('Ticker','Weight'),
        color = 'white'
      )
  })
  
  
# Use equal weighting on selected portfolio
################################################################################
observeEvent(input$equal, {
  
  updateTextInput(session, "weights", value = rep(round(1/length(input$ticker),2), each = length(input$ticker)))
})

# Clear user inputs on reset button click
################################################################################
  observeEvent(input$reset, {
    
    updateSelectInput(session, "ticker", selected = character(0))
    
    updateTextInput(session, "weights", value = "0.00")
    
    # session$reload  
  })

# Upload data via a file instead of through the calculator
################################################################################  
  
  input_file = reactive({
    inFile <- input$file
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  })

# Calculate portfolio metrics from calculator inputs
################################################################################
  # wts <- c(0.33, 0.33, 0.33)
  
  # df2 = reactiveVal(
  #   data.frame(
  #     symbol = character(0),
  #     # date = integer(0),
  #     Ra = numeric(0)
  #   )
  # )
  
  df3 = reactiveVal(
    data.frame(
      symbol = character(0),
      # date = integer(0),
      Ra = numeric(0)
      # Alpha = numeric(0)
    )
  )
  
  df2 <- reactiveVal()

  
  
  observeEvent(input$calc, {
    
    df2 = data.frame(
        symbol = character(0),
        Ra = numeric(0)
      )
    
    plot_tbl = data.frame()
    
  
    
# Error Handling for inputs
################################################################################
    input_error <- dplyr::case_when(
      # !is.numeric(input$weights) ~ "Portfolio weights need to be a number",
      sum(as.numeric(strsplit(input$weights, split = ";")[[1]])) != 1 ~ "Portfolio weights must sum to 1",
      TRUE ~ ""
    )
    if (input_error != "") {
      showModal(modalDialog(
        title = "input_error",
        input_error,
        easyClose = TRUE
      ))
      return() # exit the function here
    }
    

    
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
    
    # df2(dplyr::bind_rows(df2(), Ra))
    
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
                   weights     = as.numeric(strsplit(input$weights, split = ",")[[1]]),
                   col_rename  = "Ra")
    
    # Join Portfolio returns with Benchmark Returns
    RpRb = left_join(Rp, 
                     Rb,
                     by = "date")
    
    # Create table for the time series plot
    plot_tbl = rbind(
      RpRb %>% 
        select(date, Ra) %>% 
        mutate(symbol = "Portfolio") %>% 
        rename(value = Ra)
      ,
      RpRb %>%
        select(date, Rb) %>%
        mutate(symbol = "Benchmark") %>% 
        rename(value = Rb)
      ,
      Ra %>%rename(value = Ra)
    )
    
    df2(dplyr::bind_rows(df2(), plot_tbl))
    
    # Calculate the CAPM metrics
    CAPM = RpRb %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
    
    # Reactive table for metric oputput
    df3(dplyr::bind_rows(df3(), CAPM))
        # %>% select(-symbol, -Ra))

  })


  
  output$table2 = renderDT({
    datatable(df3() %>% select(Alpha, Beta, Correlation, TreynorRatio, `Correlationp-value`, InformationRatio, TrackingError, `R-squared`), options = list(dom = 't')) %>%
      formatStyle(
        # c('symbol','Ra'),
        names(df3() %>% select(Alpha, Beta, Correlation, TreynorRatio, `Correlationp-value`, InformationRatio, TrackingError, `R-squared`)),
        color = 'white',
        
      )
  })
  
  
# Calculate portfolio metrics from file input
################################################################################

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
      # Alpha = numeric(0)
    )
  )

  observeEvent(input$file_calc, {
    
    # Calculate
    Ra = data.frame(symbol = unique(input_file()$Ticker)) %>%
      tq_get(get  = "stock.prices",
             from = input$dates[1],
             to   = input$dates[2]) %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = "monthly",
                   col_rename = "Ra")

    # df2(dplyr::bind_rows(df2(), Ra))

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
                   # weights     = as.numeric(strsplit(input$weights, split = ",")[[1]]),
                   weights     = input_file()$Weight,
                   col_rename  = "Ra")

    # Join Portfolio returns with Benchmark Returns
    RpRb = left_join(Rp,
                     Rb,
                     by = "date")

    # Create table for the time series plot
    plot_tbl = rbind(
      RpRb %>%
        select(date, Ra) %>%
        mutate(symbol = "Portfolio") %>%
        rename(value = Ra)
      ,
      RpRb %>%
        select(date, Rb) %>%
        mutate(symbol = "Benchmark") %>%
        rename(value = Rb)
      ,
      Ra %>%rename(value = Ra)
    )

    df2(dplyr::bind_rows(df2(), plot_tbl))

    # Calculate the CAPM metrics
    CAPM = RpRb %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

    # Reactive table for metric oputput
    df3(dplyr::bind_rows(df3(), CAPM))
    
  })
  

  output$table2 = renderDT({
    datatable(df3() %>% select(Alpha, Beta, Correlation, TreynorRatio, `Correlationp-value`, InformationRatio, TrackingError, `R-squared`), options = list(dom = 't')) %>%
      formatStyle(
        # c('symbol','Ra'),
        names(df3() %>% select(Alpha, Beta, Correlation, TreynorRatio, `Correlationp-value`, InformationRatio, TrackingError, `R-squared`)),
        color = 'white',
        
      )
  })

# Output portfolio metrics to valueBoxes
################################################################################
  output$box1 = renderValueBox({
    
    if(is.null(df3()$Alpha)) { return (
      
      valueBox(
        color = 'blue',
        value = 0,
        subtitle = "Portfolio Alpha",
        icon = icon(name = "fa-solid fa-a", style = "color: #ffffff", lib = "font-awesome")
      )
    )}

    valueBox(
      color = 'blue',
      value = df3() %>% filter(row_number() >= (n())) %>% select(Alpha),
      subtitle = "Portfolio Alpha",
      icon = icon(name = "fa-solid fa-a", style = "color: #ffffff", lib = "font-awesome")
    )
  })
  
  
  # if(is.null(df3()$Alpha)) { return (
  
  # valueBox(
  #   color = 'blue',
  #   value = 0,
  #   subtitle = "Portfolio Alpha",
  #   icon = icon(
  #     name = NULL,
  #     style = icon(name = "credit-card", style = "color: #ffffff", lib = "font-awesome")
  #   )
  # )
  # )}
  # 
  # else
    
  
  output$box2 = renderValueBox({
    
    if(is.null(df3()$Beta)) { return (
      
      valueBox(
        color = 'blue',
        value = 0,
        subtitle = "Portfolio Beta",
        icon = icon(name = "fa-solid fa-b", style = "color: #ffffff", lib = "font-awesome")
      )
    )}
    
    valueBox(
      color = 'blue',
      value = df3() %>% filter(row_number() >= (n())) %>% select(Beta),
      subtitle = "Portfolio Beta",
      icon = icon(name = "fa-solid fa-b", style = "color: #ffffff", lib = "font-awesome")
    )
  })
  
  output$box3 = renderValueBox({
    
    if(is.null(df3()$Correlation)) { return (
      
      valueBox(
        color = 'blue',
        value = 0,
        subtitle = "Correlation",
        icon = icon(name = "fa-solid fa-c", style = "color: #ffffff", lib = "font-awesome")
      )
    )}
    
    valueBox(
      color = 'blue',
      value = df3() %>% filter(row_number() >= (n())) %>% select(Correlation),
      subtitle = "Correlation",
      icon = icon(name = "fa-solid fa-c", style = "color: #ffffff", lib = "font-awesome")
    )
  })
  
  output$box4 = renderValueBox({
    
    if(is.null(df3()$TreynorRatio)) { return (
      
      valueBox(
        color = 'blue',
        value = 0,
        subtitle = "Treynor Ratio",
        icon = icon(name = "fa-solid fa-t", style = "color: #ffffff", lib = "font-awesome")
      )
    )}
    
    valueBox(
      color = 'blue',
      value = df3() %>% filter(row_number() >= (n())) %>% select(TreynorRatio),
      subtitle = "Treynor Ratio",
      icon = icon(name = "fa-solid fa-t", style = "color: #ffffff", lib = "font-awesome")
    )
  })
  
  
# Create chart of individual stock performance 
################################################################################  

  output$plot = renderPlotly({

    # validate(
    # 
    #   # Outpur a message if table is empty
    #   need(nrow(df2()) > 0, 'No data exists, please select a Category')
    # )
    
    if(is.null(df2()$date)) { return (
      ggplotly(
        ggplot() +
          # geom_line(linewidth = 1, alpha = .9) +
          theme_minimal(base_size=16) +
          theme(axis.title = element_blank(),
                plot.background  = element_rect(fill = "#222222"),
                panel.background = element_rect(fill = "#222222"),
                panel.grid       = element_blank(),
                legend.text      = element_text(colour = "white"))
      )
    )}
    
    ggplotly(
      # df2() %>%
      ggplot(data = df2()) +
        # ggplot(aes(date, value, colour = symbol)) +
        geom_line(data  = . %>% filter(! symbol %in% c("Portfolio", "Benchmark")), 
                  aes(date, value, color = symbol), size = 1, alpha = .9) +
        geom_line(data  = . %>% filter(symbol == "Portfolio"), 
                  aes(date, value, color = symbol), size = 1, alpha = .9, linetype = 'dotted') +
        geom_line(data  = . %>% filter(symbol == "Benchmark"), 
                  aes(date, value, color = symbol), size = 1, alpha = .9, linetype = 'dashed') +
        # scale_color_manual(values = c(
        #   'Y1' = 'darkblue',
        #   'Y2' = 'red')) +
        labs(color = 'Legend') +
        theme_minimal(base_size=16) +
        theme(axis.title = element_blank(),
              plot.background  = element_rect(fill = "#222222"),
              panel.background = element_rect(fill = "#222222"),
              panel.grid       = element_blank(),
              legend.text      = element_text(colour = "white"),
              legend.title     = element_text(colour = "white"))
    )
    # )
  })
  
}

shinyApp(ui = ui, server = server)

################################################################################
## ~fin~
################################################################################


