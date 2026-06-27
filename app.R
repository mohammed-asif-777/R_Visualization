###############################################################################
## app.R – Combined dashboard (data + multi-ticker stocks) – final, debugged ##
###############################################################################
library(shiny)
library(shinydashboard)
library(shinymanager)
library(DT)
library(quantmod)
library(dplyr)
library(ggplot2)
library(colourpicker)
library(readxl)
library(writexl)
library(shinythemes)

# ── 1  Login credentials -----------------------------------------------------
credentials <- data.frame(user = "admin", password = "pass", stringsAsFactors = FALSE)

# ── 2  UI --------------------------------------------------------------------
ui <- secure_app(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Combined Dashboard",
                    tags$li(class = "dropdown",
                            actionButton("logout", "Logout", icon = icon("sign-out-alt"),
                                         style = "color:#fff;background:none;border:none")
                    )
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Upload / Load", tabName = "data",      icon = icon("upload")),
        menuItem("Visualise",     tabName = "visualise", icon = icon("chart-bar")),
        menuItem("Live Stocks",   tabName = "stocks",    icon = icon("chart-line"))
      )
    ),
    dashboardBody(
      tags$head(tags$style(HTML("
        .box {border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,.1);}
        .btn-primary {background:#007bff;border-color:#007bff;}
      "))),
      tabItems(
        # ── Data tab ─────────────────────────────────────────────────────────
        tabItem(tabName = "data",
                fluidRow(
                  box(width=4, title="Upload / Create",
                      fileInput("file1","Upload CSV/XLSX"),
                      checkboxInput("autoRefresh","Auto-refresh (5 s)", FALSE),
                      actionButton("newData","New dataset", icon=icon("plus")),
                      downloadButton("dlDataExcel","Export data.xlsx")
                  ),
                  box(width=8, title="Editable table",
                      DTOutput("dataTable"), height="500px")
                )
        ),
        # ── Visualise tab ───────────────────────────────────────────────────
        tabItem(tabName = "visualise",
                box(width=12, title="Plot controls",
                    fluidRow(
                      column(3, selectInput("geom","Plot type",
                                            c("Scatter","Line","Bar","Boxplot","Histogram","Frequency polygon","Pie"))),
                      column(3, uiOutput("x_ui")),
                      column(3, uiOutput("y_ui")),
                      column(3, colourInput("manualColor","Colour","#2E86C1"))
                    )
                ),
                box(width=12, title="Visualisation",
                    plotOutput("plot", height="550px"))
        ),
        # ── Stocks tab ──────────────────────────────────────────────────────
        tabItem(tabName = "stocks",
                fluidRow(
                  box(width=4, title="Controls",
                      textInput("tickers","Tickers (comma-separated)","AAPL,MSFT"),
                      dateRangeInput("dateRange","Date range",
                                     start=Sys.Date()-30,end=Sys.Date()),
                      sliderInput("refresh","Auto-refresh (sec)",10,600,60),
                      numericInput("threshold","Alert if Close ≥", NA, min=0),
                      colourInput("lineColor","Line colour","#2E86C1"),
                      actionButton("refreshStock","⟳ Refresh", class="btn-primary"),
                      downloadButton("dlStockExcel","Export stock.xlsx")
                  ),
                  box(width=8, title=textOutput("stockTitle"),
                      plotOutput("stockPlot", height="500px"))
                )
        )
      )
    )
  ),
  theme = shinytheme("flatly"),
  choose_language = FALSE
)

# ── 3  Server ----------------------------------------------------------------
server <- function(input, output, session) {
  secure_server(check_credentials(credentials))
  observeEvent(input$logout, session$reload())
  
  ## ── 3·1  Data-tab reactive values ---------------------------------------
  vals <- reactiveValues(df=NULL, path=NULL, mtime=Sys.time())
  
  # upload
  observeEvent(input$file1, {
    ext  <- tolower(tools::file_ext(input$file1$name))
    vals$path  <- input$file1$datapath
    vals$mtime <- file.info(vals$path)$mtime
    vals$df <- if (ext %in% c("csv","txt"))
      read.csv(vals$path, check.names=FALSE)
    else
      read_excel(vals$path)
  })
  
  # auto-refresh
  observe({
    req(vals$path, input$autoRefresh)
    invalidateLater(5000, session)
    mt <- file.info(vals$path)$mtime
    if (!is.na(mt) && mt > vals$mtime) {
      ext <- tolower(tools::file_ext(vals$path))
      vals$df <- if (ext %in% c("csv","txt"))
        read.csv(vals$path, check.names=FALSE)
      else
        read_excel(vals$path)
      vals$mtime <- mt
      showNotification("Data file reloaded", type="message")
    }
  })
  
  # new dataset
  observeEvent(input$newData, {
    showModal(modalDialog(
      title="Create new data",
      numericInput("rows","Rows",5,1),
      textInput("cols","Comma-separated columns","A,B,C"),
      footer=tagList(modalButton("Cancel"),
                     actionButton("confirmNew","Create", class="btn-primary"))
    ))
  })
  
  observeEvent(input$confirmNew, {
    removeModal()
    nm <- strsplit(input$cols,",\\s*")[[1]]
    vals$df <- as.data.frame(matrix(NA, nrow=input$rows, ncol=length(nm)))
    names(vals$df) <- nm
  })
  
  # DT render / edit
  output$dataTable <- renderDT({
    req(vals$df)
    datatable(vals$df, editable=TRUE, options=list(scrollX=TRUE))
  })
  
  observeEvent(input$dataTable_cell_edit, {
    info <- input$dataTable_cell_edit
    vals$df[info$row, info$col] <- coerceValue(info$value, vals$df[[info$col]])
  })
  
  # download Excel
  output$dlDataExcel <- downloadHandler(
    filename = function() "data.xlsx",
    content  = function(f) write_xlsx(vals$df, f)
  )
  
  ## ── 3·2  Visualise tab ---------------------------------------------------
  output$x_ui <- renderUI({ req(vals$df); selectInput("xcol","X-axis",names(vals$df)) })
  output$y_ui <- renderUI({ req(vals$df); selectInput("ycol","Y-axis",names(vals$df)) })
  
  output$plot <- renderPlot({
    req(vals$df, input$xcol, input$ycol)
    df <- vals$df
    if (input$geom=="Pie") {
      df[[input$ycol]] <- as.numeric(df[[input$ycol]])
      df[[input$ycol]][is.na(df[[input$ycol]])] <- 0
      pie_df <- df %>% group_by(.data[[input$xcol]]) %>%
        summarise(value=sum(.data[[input$ycol]],na.rm=TRUE), .groups="drop") %>%
        mutate(prop=value/sum(value))
      ggplot(pie_df, aes(x="", y=prop, fill=.data[[input$xcol]]))+
        geom_col(width=1, colour="white")+
        coord_polar(theta="y")+
        theme_void()
    } else {
      p <- ggplot(df, aes_string(x=input$xcol, y=input$ycol))+
        theme_minimal()
      layer <- switch(input$geom,
                      "Scatter"           = geom_point(color=input$manualColor),
                      "Line"              = geom_line(color=input$manualColor),
                      "Bar"               = geom_bar(stat="identity", fill=input$manualColor),
                      "Boxplot"           = geom_boxplot(fill=input$manualColor),
                      "Histogram"         = geom_histogram(bins=30, fill=input$manualColor),
                      "Frequency polygon" = geom_freqpoly(bins=30, color=input$manualColor)
      )
      p+layer
    }
  })
  
  ## ── 3·3  Stocks tab ------------------------------------------------------
  autoTimer  <- reactiveTimer(1000)
  lastFetch  <- reactiveVal(Sys.time()-3600)
  stockData  <- reactiveVal(NULL)
  
  loadStocks <- function(){
    syms <- trimws(unlist(strsplit(input$tickers,",", fixed=TRUE)))
    syms <- syms[syms!=""]
    if (length(syms)==0) return(NULL)
    rang <- input$dateRange; if (any(is.na(rang))) return(NULL)
    
    dfs <- lapply(syms, function(sym){
      xt <- try(getSymbols(sym, src="yahoo", from=rang[1], to=rang[2], auto.assign=FALSE), silent=TRUE)
      if (inherits(xt,"try-error")) return(NULL)
      df <- data.frame(date=index(xt), coredata(xt))
      names(df) <- c("date","open","high","low","close","volume","adjusted")
      df$ticker <- sym
      df
    })
    bind_rows(dfs)
  }
  
  observe({
    autoTimer()
    req(!is.null(input$refresh))
    needsRefresh <- difftime(Sys.time(), lastFetch(), units="secs") >= input$refresh
    if ( input$refreshStock > 0 || needsRefresh ) {
      df <- loadStocks()
      if (!is.null(df)) stockData(df)
      lastFetch(Sys.time())
      isolate(updateActionButton(session,"refreshStock",label="⟳ Refresh"))
    }
  })
  
  output$stockTitle <- renderText({
    paste("Tickers:", input$tickers,
          " | Range:", format(input$dateRange[1]), "to", format(input$dateRange[2]))
  })
  
  output$stockPlot <- renderPlot({
    df <- stockData(); req(df)
    df <- df %>% filter(!is.na(close))
    pal <- rep(input$lineColor, length(unique(df$ticker)))
    p <- ggplot(df, aes(date, close, color=ticker))+
      geom_line(size=1.2)+
      scale_color_manual(values=pal)+
      theme_minimal()+
      labs(y="Close price", x=NULL)
    if (!is.na(input$threshold))
      p <- p+geom_hline(yintercept=input$threshold, linetype="dashed", color="red")
    p
  })
  
  output$dlStockExcel <- downloadHandler(
    filename=function() "stock_data.xlsx",
    content = function(f) write_xlsx(stockData(), f)
  )
  
  observe({
    df <- stockData()
    if (is.null(df) || is.na(input$threshold)) return()
    crossed <- df %>%
      filter(!is.na(close), close >= input$threshold) %>%
      pull(ticker) %>% unique()
    if (length(crossed))
      showNotification(
        paste0("🚨 ", paste(crossed, collapse=", "),
               " ≥ ", input$threshold),
        type="warning", duration=8
      )
  })
}

# ── 4  Run app with mobile access -------------------------------------------
shinyApp(ui, server)
