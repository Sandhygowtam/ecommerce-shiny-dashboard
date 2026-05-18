pkgs <- c("shiny", "shinydashboard", "ggplot2", "dplyr", "plotly", "DT")
missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

options(shiny.maxRequestSize = 100 * 1024^2)  # Allow uploads up to 100 MB

# ── Sample data (used when no file is uploaded) ───────────────────────────────
generate_sample_data <- function() {
  set.seed(42)
  n <- 500
  countries  <- c("United States", "United Kingdom", "Germany", "France",
                  "Canada", "Australia", "India", "Brazil", "Japan", "Mexico")
  products   <- c("Laptop", "Smartphone", "Tablet", "Headphones", "Smartwatch",
                  "Camera", "Speaker", "Keyboard", "Monitor", "Mouse")
  categories <- c("Electronics", "Electronics", "Electronics", "Accessories",
                  "Wearables", "Electronics", "Accessories", "Accessories",
                  "Electronics", "Accessories")
  
  prod_idx <- sample(seq_along(products), n, replace = TRUE)
  
  data.frame(
    OrderID     = paste0("ORD-", sprintf("%04d", seq_len(n))),
    Date        = sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), n, replace = TRUE),
    Country     = sample(countries, n, replace = TRUE, prob = c(.30,.15,.12,.10,.08,.07,.06,.05,.04,.03)),
    Product     = products[prod_idx],
    Category    = categories[prod_idx],
    Quantity    = sample(1:10, n, replace = TRUE),
    UnitPrice   = round(runif(n, 15, 1500), 2),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Revenue    = round(Quantity * UnitPrice, 2),
      YearMonth  = format(Date, "%Y-%m")
    ) %>%
    arrange(Date)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "E-Commerce Sales"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",     tabName = "overview",  icon = icon("chart-line")),
      menuItem("Data Table",   tabName = "datatable", icon = icon("table")),
      menuItem("Revenue Trend",tabName = "revenue",   icon = icon("dollar-sign")),
      menuItem("Country Sales",tabName = "country",   icon = icon("globe")),
      menuItem("Products",     tabName = "products",  icon = icon("box"))
    ),
    hr(),
    fileInput("file", "Upload CSV",
              accept = ".csv",
              buttonLabel = "Browse…",
              placeholder = "No file selected"),
    div(style = "padding: 0 15px; font-size: 12px; color: #aaa;",
        "Leave blank to use built-in sample data.")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f6f9; }
      .small-box .icon { font-size: 60px !important; top: 10px !important; }
      .info-box { min-height: 80px; }
      .info-box-icon { line-height: 80px; width: 80px; }
      .info-box-content { padding-top: 10px; }
      .box { border-top: 3px solid #3c8dbc; }
    "))),
    
    tabItems(
      
      # ── Overview ────────────────────────────────────────────────────────────
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("box_revenue",  width = 3),
                valueBoxOutput("box_orders",   width = 3),
                valueBoxOutput("box_customers",width = 3),
                valueBoxOutput("box_avg",      width = 3)
              ),
              fluidRow(
                box(title = "Revenue by Category", width = 6, solidHeader = TRUE,
                    status = "primary", plotlyOutput("cat_chart", height = 280)),
                box(title = "Monthly Orders", width = 6, solidHeader = TRUE,
                    status = "primary", plotlyOutput("monthly_orders", height = 280))
              ),
              fluidRow(
                box(title = "Dataset Summary", width = 12, solidHeader = TRUE,
                    status = "info", verbatimTextOutput("data_summary"))
              )
      ),
      
      # ── Data Table ──────────────────────────────────────────────────────────
      tabItem(tabName = "datatable",
              fluidRow(
                box(title = "Sales Data", width = 12, solidHeader = TRUE,
                    status = "primary",
                    fluidRow(
                      column(4, selectizeInput("filter_country", "Filter by Country",
                                               choices = "All", selected = "All",
                                               options = list(maxOptions = 300))),
                      column(4, selectizeInput("filter_product", "Filter by Product",
                                               choices = NULL, selected = NULL,
                                               options = list(placeholder = "All products...",
                                                              maxOptions = 5000))),
                      column(4, dateRangeInput("filter_date", "Date Range",
                                               start = "2024-01-01", end = "2024-12-31"))
                    ),
                    DTOutput("data_table"))
              )
      ),
      
      # ── Revenue Trend ───────────────────────────────────────────────────────
      tabItem(tabName = "revenue",
              fluidRow(
                box(title = "Monthly Revenue Trend", width = 12, solidHeader = TRUE,
                    status = "primary", plotlyOutput("revenue_trend", height = 400))
              ),
              fluidRow(
                box(title = "Cumulative Revenue", width = 6, solidHeader = TRUE,
                    status = "warning", plotlyOutput("cumulative_rev", height = 300)),
                box(title = "Revenue by Category Over Time", width = 6, solidHeader = TRUE,
                    status = "warning", plotlyOutput("cat_trend", height = 300))
              )
      ),
      
      # ── Country Sales ───────────────────────────────────────────────────────
      tabItem(tabName = "country",
              fluidRow(
                box(title = "Top Countries by Revenue", width = 7, solidHeader = TRUE,
                    status = "primary", plotlyOutput("country_chart", height = 420)),
                box(title = "Country Metrics", width = 5, solidHeader = TRUE,
                    status = "primary", DTOutput("country_table"))
              )
      ),
      
      # ── Products ────────────────────────────────────────────────────────────
      tabItem(tabName = "products",
              fluidRow(
                box(title = "Product Revenue", width = 7, solidHeader = TRUE,
                    status = "primary", plotlyOutput("product_chart", height = 420)),
                box(title = "Top 10 Products", width = 5, solidHeader = TRUE,
                    status = "primary", DTOutput("product_table"))
              ),
              fluidRow(
                box(title = "Units Sold vs Revenue", width = 12, solidHeader = TRUE,
                    status = "success", plotlyOutput("scatter_chart", height = 350))
              )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive: load uploaded CSV or fall back to sample data
  sales_data <- reactive({
    if (!is.null(input$file)) {
      df <- tryCatch(
        read.csv(input$file$datapath, stringsAsFactors = FALSE),
        error = function(e) { showNotification(paste("Error reading file:", e$message), type = "error"); NULL }
      )
      if (!is.null(df)) {
        names(df) <- trimws(names(df))
        
        # Alias: InvoiceDate -> Date
        if (!"Date" %in% names(df) && "InvoiceDate" %in% names(df))
          df$Date <- df$InvoiceDate
        
        # Alias: Description -> Product
        if (!"Product" %in% names(df) && "Description" %in% names(df))
          df$Product <- df$Description
        
        # Alias: StockCode -> Category (best proxy when no Category column)
        if (!"Category" %in% names(df) && "StockCode" %in% names(df))
          df$Category <- substr(df$StockCode, 1, 3)
        
        # Compute Revenue if missing
        if (!"Revenue" %in% names(df) && all(c("Quantity","UnitPrice") %in% names(df)))
          df$Revenue <- df$Quantity * df$UnitPrice
        
        # Parse Date and derive YearMonth
        if ("Date" %in% names(df)) {
          df$Date <- as.Date(df$Date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y",
                                                     "%d/%m/%Y", "%Y/%m/%d",
                                                     "%d-%m-%Y", "%m-%d-%Y"))
          df$YearMonth <- format(df$Date, "%Y-%m")
        }
        return(df)
      }
    }
    generate_sample_data()
  })
  
  # Update filter dropdowns when data changes
  observeEvent(sales_data(), {
    df <- sales_data()
    countries <- if ("Country" %in% names(df)) c("All", sort(unique(df$Country))) else "All"
    products  <- if ("Product" %in% names(df)) c("All", sort(unique(df$Product))) else "All"
    updateSelectInput(session, "filter_country", choices = countries)
    updateSelectizeInput(session, "filter_product", choices = products, server = TRUE)
    if ("Date" %in% names(df) && !all(is.na(df$Date))) {
      updateDateRangeInput(session, "filter_date",
                           start = min(df$Date, na.rm = TRUE),
                           end   = max(df$Date, na.rm = TRUE))
    }
  })
  
  # Filtered data for the table tab
  filtered_data <- reactive({
    df <- sales_data()
    if ("Country" %in% names(df) && input$filter_country != "All")
      df <- df %>% filter(Country == input$filter_country)
    if ("Product" %in% names(df) && input$filter_product != "All")
      df <- df %>% filter(Product == input$filter_product)
    if ("Date" %in% names(df) && !all(is.na(df$Date)))
      df <- df %>% filter(Date >= input$filter_date[1], Date <= input$filter_date[2])
    df
  })
  
  # ── Overview value boxes ──────────────────────────────────────────────────
  output$box_revenue <- renderValueBox({
    rev <- if ("Revenue" %in% names(sales_data())) sum(sales_data()$Revenue, na.rm = TRUE) else 0
    valueBox(paste0("$", formatC(rev, format = "f", digits = 0, big.mark = ",")),
             "Total Revenue", icon = icon("dollar-sign"), color = "blue")
  })
  
  output$box_orders <- renderValueBox({
    valueBox(formatC(nrow(sales_data()), big.mark = ","),
             "Total Orders", icon = icon("shopping-cart"), color = "green")
  })
  
  output$box_customers <- renderValueBox({
    n <- if ("Country" %in% names(sales_data())) length(unique(sales_data()$Country)) else "–"
    valueBox(n, "Countries", icon = icon("globe"), color = "purple")
  })
  
  output$box_avg <- renderValueBox({
    avg <- if ("Revenue" %in% names(sales_data()) && nrow(sales_data()) > 0)
      mean(sales_data()$Revenue, na.rm = TRUE) else 0
    valueBox(paste0("$", formatC(avg, format = "f", digits = 2, big.mark = ",")),
             "Avg Order Value", icon = icon("chart-bar"), color = "orange")
  })
  
  # ── Overview: category donut ──────────────────────────────────────────────
  output$cat_chart <- renderPlotly({
    df <- sales_data()
    req("Category" %in% names(df), "Revenue" %in% names(df))
    cat_df <- df %>% group_by(Category) %>% summarise(Revenue = sum(Revenue, na.rm = TRUE))
    plot_ly(cat_df, labels = ~Category, values = ~Revenue, type = "pie", hole = 0.45,
            textinfo = "label+percent",
            marker = list(colors = c("#3c8dbc","#00a65a","#f39c12","#dd4b39","#605ca8"))) %>%
      layout(showlegend = TRUE, margin = list(t = 10, b = 10))
  })
  
  # ── Overview: monthly orders bar ─────────────────────────────────────────
  output$monthly_orders <- renderPlotly({
    df <- sales_data()
    req("YearMonth" %in% names(df))
    mo <- df %>% group_by(YearMonth) %>% summarise(Orders = n()) %>% arrange(YearMonth)
    plot_ly(mo, x = ~YearMonth, y = ~Orders, type = "bar",
            marker = list(color = "#3c8dbc")) %>%
      layout(xaxis = list(title = "Month"), yaxis = list(title = "Orders"),
             margin = list(t = 10))
  })
  
  # ── Summary text ─────────────────────────────────────────────────────────
  output$data_summary <- renderPrint({ summary(sales_data()) })
  
  # ── Data table ───────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 15, scrollX = TRUE,
                             columnDefs = list(list(className = "dt-center", targets = "_all"))),
              rownames = FALSE, class = "table table-striped table-hover")
  })
  
  # ── Revenue trend ────────────────────────────────────────────────────────
  output$revenue_trend <- renderPlotly({
    df <- sales_data()
    req(all(c("YearMonth","Revenue") %in% names(df)))
    trend <- df %>% group_by(YearMonth) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>% arrange(YearMonth)
    plot_ly(trend, x = ~YearMonth, y = ~Revenue, type = "scatter", mode = "lines+markers",
            line = list(color = "#3c8dbc", width = 3),
            marker = list(color = "#3c8dbc", size = 8)) %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Revenue ($)", tickprefix = "$"),
             hovermode = "x unified")
  })
  
  output$cumulative_rev <- renderPlotly({
    df <- sales_data()
    req(all(c("YearMonth","Revenue") %in% names(df)))
    cum <- df %>% group_by(YearMonth) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
      arrange(YearMonth) %>% mutate(Cumulative = cumsum(Revenue))
    plot_ly(cum, x = ~YearMonth, y = ~Cumulative, type = "scatter", mode = "lines",
            fill = "tozeroy", fillcolor = "rgba(60,141,188,0.2)",
            line = list(color = "#3c8dbc")) %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Cumulative Revenue ($)", tickprefix = "$"))
  })
  
  output$cat_trend <- renderPlotly({
    df <- sales_data()
    req(all(c("YearMonth","Category","Revenue") %in% names(df)))
    ct <- df %>% group_by(YearMonth, Category) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop")
    plot_ly(ct, x = ~YearMonth, y = ~Revenue, color = ~Category,
            type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Revenue ($)"),
             legend = list(orientation = "h", y = -0.3))
  })
  
  # ── Country sales ────────────────────────────────────────────────────────
  country_summary <- reactive({
    df <- sales_data()
    req(all(c("Country","Revenue") %in% names(df)))
    df %>% group_by(Country) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE),
                Orders  = n(),
                AvgOrder = mean(Revenue, na.rm = TRUE)) %>%
      arrange(desc(Revenue))
  })
  
  output$country_chart <- renderPlotly({
    cs <- country_summary() %>% slice_head(n = 10) %>%
      mutate(Country = reorder(Country, Revenue))
    plot_ly(cs, x = ~Revenue, y = ~Country, type = "bar", orientation = "h",
            marker = list(color = ~Revenue,
                          colorscale = list(c(0,"#aec7e8"), c(1,"#1f77b4")))) %>%
      layout(xaxis = list(title = "Revenue ($)", tickprefix = "$"),
             yaxis = list(title = ""), margin = list(l = 120))
  })
  
  output$country_table <- renderDT({
    datatable(country_summary() %>%
                mutate(Revenue  = paste0("$", formatC(Revenue,  format = "f", digits = 2, big.mark = ",")),
                       AvgOrder = paste0("$", formatC(AvgOrder, format = "f", digits = 2, big.mark = ","))),
              options = list(pageLength = 10, dom = "tp"), rownames = FALSE)
  })
  
  # ── Product performance ──────────────────────────────────────────────────
  product_summary <- reactive({
    df <- sales_data()
    req(all(c("Product","Revenue") %in% names(df)))
    qty_col <- if ("Quantity" %in% names(df)) "Quantity" else NULL
    if (!is.null(qty_col)) {
      df %>% group_by(Product) %>%
        summarise(Revenue = sum(Revenue, na.rm = TRUE),
                  Units   = sum(Quantity, na.rm = TRUE),
                  Orders  = n()) %>% arrange(desc(Revenue))
    } else {
      df %>% group_by(Product) %>%
        summarise(Revenue = sum(Revenue, na.rm = TRUE), Orders = n()) %>%
        arrange(desc(Revenue))
    }
  })
  
  output$product_chart <- renderPlotly({
    ps <- product_summary() %>% slice_head(n = 10) %>%
      mutate(Product = reorder(Product, Revenue))
    plot_ly(ps, x = ~Revenue, y = ~Product, type = "bar", orientation = "h",
            marker = list(color = ~Revenue,
                          colorscale = list(c(0,"#b5e0c4"), c(1,"#00a65a")))) %>%
      layout(xaxis = list(title = "Revenue ($)", tickprefix = "$"),
             yaxis = list(title = ""), margin = list(l = 120))
  })
  
  output$product_table <- renderDT({
    datatable(product_summary() %>% slice_head(n = 10) %>%
                mutate(Revenue = paste0("$", formatC(Revenue, format = "f", digits = 2, big.mark = ","))),
              options = list(pageLength = 10, dom = "tp"), rownames = FALSE)
  })
  
  output$scatter_chart <- renderPlotly({
    ps <- product_summary()
    req("Units" %in% names(ps))
    plot_ly(ps, x = ~Units, y = ~Revenue, text = ~Product,
            type = "scatter", mode = "markers",
            marker = list(size = 12, color = ~Revenue,
                          colorscale = "Blues", showscale = TRUE,
                          colorbar = list(title = "Revenue")),
            hovertemplate = "<b>%{text}</b><br>Units: %{x}<br>Revenue: $%{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "Units Sold"),
             yaxis = list(title = "Revenue ($)", tickprefix = "$"))
  })
}

# ── Launch ────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)