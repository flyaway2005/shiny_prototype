

pacman::p_load(shiny, tidyverse) 

library(shiny)
GP <- read_csv("data/GovernmentProcurementviaGeBiz.csv")
print(GP)

library(shiny)
library(bslib)
GP$award_date <- as.Date(GP$award_date, format="%d/%m/%Y")
# 載入必要的套件
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(lubridate)

# 轉換 award_date 為日期格式
GP$award_date <- as.Date(GP$award_date, format="%d/%m/%Y")

# ----------------- Procurement Dashboard Module -----------------
procurementUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Procurement Dashboard"),
     
    
    fluidRow(
      column(6, selectInput(ns("selected_agency"), "Select Agency:", 
                            choices = c("All", sort(unique(GP$agency))),
                            selected = "All", multiple = FALSE)),
      column(6, selectInput(ns("selected_year"), "Select Year:", 
                            choices = sort(unique(year(GP$award_date))),
                            selected = max(year(GP$award_date)), multiple = FALSE))
    ),
    
    # ✅ 包住 `valueBoxOutput()` 讓它正確顯示
    fluidRow(
      column(12, valueBoxOutput(ns("total_amount")))
    ),
    
    plotOutput(ns("trend_plot"))
  )
}

procurementServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    library(ggplot2)
    library(dplyr)
    library(lubridate) 
    library(shinydashboard)  # 解析年份
    
    # 過濾數據 (依 Agency 與年份)
    filtered_data <- reactive({
      req(input$selected_year)  # 確保 input$selected_year 有值
      data <- GP
      if (input$selected_agency != "All") {
        data <- data %>% filter(agency == input$selected_agency)
      }
      data <- data %>% filter(year(award_date) == input$selected_year)
      
      # ✅ 檢查是否有數據
      if (nrow(data) == 0) return(NULL)  
      
      return(data)
    })
    
    
    # 計算該年度的總採購金額
    output$total_amount <- renderValueBox({
      data <- filtered_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(valueBox("No data", subtitle = "Total Awarded Amount", color = "red"))
      }
      
      total_amt <- sum(data$awarded_amt, na.rm = TRUE)
      
      valueBox(
        formatC(total_amt, format = "f", big.mark = ",", digits = 2),
        subtitle = paste("Total Awarded Amount in", input$selected_year),
        color = "teal"
      )
    })
    
    
    # 計算該 Agency 歷年趨勢
    trend_data <- reactive({
      req(input$selected_agency)
      
      trend <- GP %>%
        filter(agency == input$selected_agency) %>%
        mutate(year = year(award_date)) %>%
        group_by(year) %>%
        summarise(Total_Amount = sum(awarded_amt, na.rm = TRUE), .groups = "drop")
      
      # 如果沒有數據，返回 NULL
      if (nrow(trend) == 0) return(NULL)
      
      return(trend)
    })
    
    
    # 繪製趨勢圖
    output$trend_plot <- renderPlot({
      req(trend_data())
      
      ggplot(trend_data(), aes(x = year, y = Total_Amount)) +
        geom_line(color = "blue", size = 1) +
        geom_point(color = "red", size = 3) +
        labs(title = paste("Procurement Trend for", input$selected_agency),
             x = "Year", y = "Total Awarded Amount") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal()
    })
    
  })
}


# ----------------- G2B Network Module -----------------
g2nUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("G2B Network"),
    p("This section provides insights into government-to-network relationships.")
  )
}

g2nServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 在這裡加入數據處理
  })
}

# ----------------- Tender Analysis & Categorization Module -----------------
tenderUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("analysis_type"), "Choose Analysis Type:", 
                choices = c("By Agency", "By Supplier"), 
                selected = "By Agency"),
    
    uiOutput(ns("dynamic_dropdown")),
    
    plotOutput(ns("wordcloud"))  # 只顯示詞雲
  )
}

tenderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    library(tidytext)
    library(dplyr)
    library(wordcloud)
    library(RColorBrewer)
      # LDA
    
    # 自訂 Stopwords
    custom_stopwords <- c("tender", "supply", "invitation", "contract",
                          "authority", "installation", "government", "public", 
                          "services", "provision", "singapore")
    
    # 產生動態下拉選單
    output$dynamic_dropdown <- renderUI({
      ns <- session$ns
      if (input$analysis_type == "By Agency") {
        selectizeInput(ns("selected_agency"), "Select Agency:", 
                       choices = c("All", sort(unique(GP$agency))), 
                       selected = "All", multiple = FALSE)
      } else {
        selectizeInput(ns("selected_supplier"), "Select Supplier:", 
                       choices = c("All", sort(unique(GP$supplier_name))), 
                       selected = "All", multiple = FALSE)
      }
    })
    
    # 過濾數據 (依照 Agency 或 Supplier)
    filtered_text <- reactive({
      if (input$analysis_type == "By Agency") {
        if (input$selected_agency == "All") {
          GP$tender_description
        } else {
          GP %>% filter(agency == input$selected_agency) %>% pull(tender_description)
        }
      } else {
        if (input$selected_supplier == "All") {
          GP$tender_description
        } else {
          GP %>% filter(supplier_name == input$selected_supplier) %>% pull(tender_description)
        }
      }
    })
    
    # 計算詞頻（不使用 TF-IDF，僅使用 count）
    word_freq <- reactive({
      req(filtered_text())  
      
      word_data <- data.frame(text = filtered_text()) %>%
        unnest_tokens(word, text) %>%
        count(word, sort = TRUE) %>%
        filter(!word %in% stop_words$word,  
               !word %in% custom_stopwords)  
      
      # 確保有足夠的詞頻
      word_data <- word_data %>% filter(n > 1)  
      
      return(word_data)
    })
    
    # 繪製 Word Cloud（不使用 TF-IDF）
    output$wordcloud <- renderPlot({
      req(word_freq())
      
      # 如果詞頻為空，顯示無數據
      if (nrow(word_freq()) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No words available")
        return()
      }
      
      wordcloud(words = word_freq()$word, 
                freq = word_freq()$n,  # **使用詞頻 (n) 作為頻率**
                max.words = 100, 
                colors = brewer.pal(8, "Dark2"),
                scale = c(3, 0.3))  # 限制最大字體大小，避免錯誤
    })
  })
}

# ----------------- Market Analysis Module -----------------
marketUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Market Analysis"),
    p("This section provides market trends and forecasting insights.")
  )
}

marketServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 在這裡加入數據處理
  })
}

# ----------------- UI -----------------
ui <- page_sidebar(
  title = "SG Public Money Tracker",
  theme = bs_theme(bootswatch = "simplex", primary = "#007BFF"),
  
  sidebar = sidebar(
    "Menu", position = "left",
    sidebar_open = FALSE,  # ✅ 預設隱藏 sidebar
    selectInput("feature", "Select Feature:",
                choices = c("Procurement Dashboard", 
                            "G2B Network", 
                            "Tender Analysis & Categorization", 
                            "Market Analysis"),
                selected = "Procurement Dashboard"),
    
    # 回到主畫面按鈕
    actionButton("go_home", "Back to Home", icon = icon("home"))
  ),
  
  # 根據選擇的功能顯示對應 UI
  uiOutput("dynamicUI")
)


# ----------------- Server -----------------
server <- function(input, output, session) {
  library(shinydashboard)
  
  observeEvent(input$go_home, {
    updateSelectInput(session, "feature", selected = "Procurement Dashboard")
  })
  
  # 動態顯示不同的頁面內容
  output$dynamicUI <- renderUI({
    switch(input$feature,
           "Procurement Dashboard" = procurementUI("procurement"),
           "G2N Network" = tagList(
             h3("G2B Network"),
             p("This section analyzes the Government-to-Network relationships.")
           ),
           "Tender Analysis & Categorization" = tenderUI("tender"),
           "Competitive Intelligence" = tagList(
             h3("Competitive Intelligence"),
             p("This section provides insights into market competition.")
           ),
           "Market Analysis" = tagList(
             h3("Market Analysis"),
             p("This section focuses on market trends and forecasting.")
           )
    )
  })
  
  # 啟動 Procurement Dashboard Server
  procurementServer("procurement")
  tenderServer("tender") 
}

# 啟動 Shiny App
shinyApp(ui = ui, server = server)
