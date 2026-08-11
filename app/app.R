library(shiny)
library(jsonlite)
library(dplyr)
library(purrr)
library(plotly)

# h-index 計算関数
calc_h_index <- function(citations) {
  if (length(citations) == 0) return(0)
  sorted_cites <- sort(citations, decreasing = TRUE)
  h <- max(which(sorted_cites >= seq_along(sorted_cites)), 0)
  return(ifelse(is.infinite(h), 0, h))
}

ui <- fluidPage(
  titlePanel("OpenAlex Author Dashboard (Shinylive Ready)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      textInput("author_id", "著者ID (OpenAlex):", value = "A5074884601"),
      actionButton("fetch_btn", "データ取得・更新", class = "btn-primary", style = "width: 100%; margin-bottom: 15px;"),
      hr(),
      uiOutput("year_filter_ui"),
      hr(),
      helpText("※ webR上で直接OpenAlex APIからデータ(最大200件)を取得します。")
    ),
    
    mainPanel(
      width = 10,
      fluidRow(
        column(4, wellPanel(style = "text-align: center; padding: 10px;",
                            h4("論文数"), h2(textOutput("total_papers")))),
        column(4, wellPanel(style = "text-align: center; padding: 10px;",
                            h4("総被引用数"), h2(textOutput("total_citations")))),
        column(4, wellPanel(style = "text-align: center; padding: 10px;",
                            h4("h-index"), h2(textOutput("h_index"))))
      ),
      hr(),
      # 上段: 年推移 (幅7) & トピック (幅5)
      fluidRow(
        column(7, plotlyOutput("time_series_plot", height = "450px")),
        column(5, plotlyOutput("coauthor_bar_plot", height = "450px"))
        #column(5, plotlyOutput("topic_pie_plot", height = "350px"))
      ),
      br(),
      # 下段: キーワード (幅4) & ジャーナルTop10 (幅4) & 共著者Top10 (幅4)
      fluidRow(
        column(4, plotlyOutput("wordcloud_plot", height = "450px")),
        column(4, plotlyOutput("topic_pie_plot", height = "450px")),
        column(4, plotlyOutput("journal_bar_plot", height = "450px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # APIからのデータ取得
  raw_data <- eventReactive(input$fetch_btn, {
    req(input$author_id)
    clean_id <- trimws(input$author_id)
    
    # OpenAlex API URL (最大200件取得)
    url <- sprintf(
      "https://api.openalex.org/works?filter=author.id:%s&per-page=200&mailto=example@example.com", 
      clean_id
    )
    
    res <- tryCatch({
      jsonlite::fromJSON(url, simplifyVector = FALSE)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(res) || length(res$results) == 0) return(NULL)
    res$results
  }, ignoreNULL = FALSE)
  
  # 年度スライダーのUI生成
  output$year_filter_ui <- renderUI({
    works <- raw_data()
    if (is.null(works)) return(NULL)
    
    years <- map_int(works, ~ .x$publication_year %||% 2000)
    min_yr <- min(years, na.rm = TRUE)
    max_yr <- max(years, na.rm = TRUE)
    
    sliderInput("year_range", "対象年の絞り込み:",
                min = min_yr, max = max_yr,
                value = c(min_yr, max_yr), step = 1, sep = "")
  })
  
  # フィルタリング済みデータの reactive オブジェクト
  filtered_works <- reactive({
    works <- raw_data()
    if (is.null(works)) return(list())
    
    if (!is.null(input$year_range)) {
      works <- keep(works, function(w) {
        yr <- w$publication_year
        !is.null(yr) && yr >= input$year_range[1] && yr <= input$year_range[2]
      })
    }
    works
  })
  
  # 1. 指標の計算と表示
  output$total_papers <- renderText({
    length(filtered_works())
  })
  
  output$total_citations <- renderText({
    works <- filtered_works()
    sum(map_int(works, ~ .x$cited_by_count %||% 0))
  })
  
  output$h_index <- renderText({
    works <- filtered_works()
    cites <- map_int(works, ~ .x$cited_by_count %||% 0)
    calc_h_index(cites)
  })
  
  # 年推移グラフ (論文数 & 被引用数)
  output$time_series_plot <- renderPlotly({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    df <- tibble(
      year = map_int(works, ~ .x$publication_year %||% NA_integer_),
      cites = map_int(works, ~ .x$cited_by_count %||% 0)
    ) %>%
      filter(!is.na(year)) %>%
      group_by(year) %>%
      summarise(papers = n(), citations = sum(cites), .groups = "drop") %>%
      arrange(year)
    
    plot_ly(df, x = ~year) %>%
      add_trace(y = ~papers, name = "論文数", type = "bar", marker = list(color = "#4c78a8")) %>%
      add_trace(y = ~citations, name = "被引用数", type = "scatter", mode = "lines+markers", 
                yaxis = "y2", line = list(color = "#e15759")) %>%
      layout(
        title = "年別 論文数と被引用数の推移",
        xaxis = list(title = "年", dtick = 1),
        yaxis = list(title = "論文数", side = "left"),
        yaxis2 = list(title = "被引用数", overlaying = "y", side = "right"),
        legend = list(x = 0.05, y = 0.95)
      )
  })
  
  # 2. トピック（円グラフ）
  output$topic_pie_plot <- renderPlotly({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    topics <- map(works, ~ .x$topics) %>%
      compact() %>%
      flatten() %>%
      map_chr(~ .x$display_name %||% NA_character_) %>%
      na.omit()
    
    if (length(topics) == 0) return(NULL)
    
    df_topic <- tibble(topic = topics) %>%
      count(topic, sort = TRUE) %>%
      slice_head(n = 10)
    
    plot_ly(df_topic, labels = ~topic, values = ~n, type = "pie",
            textinfo = "label+percent", hoverinfo = "label+value+percent") %>%
      layout(title = "主要トピック構成 (Top 10)")
  })
  
  # 3. キーワード (Plotlyで再現するワードクラウド風散布図)
  output$wordcloud_plot <- renderPlotly({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    keywords <- map(works, ~ .x$keywords) %>%
      compact() %>%
      flatten() %>%
      map_chr(~ .x$display_name %||% NA_character_) %>%
      na.omit()
    
    if (length(keywords) == 0) return(NULL)
    
    df_kw <- tibble(word = keywords) %>%
      count(word, sort = TRUE) %>%
      slice_head(n = 30)
    
    if (nrow(df_kw) == 0) return(NULL)
    
    # 疑似ワードクラウド用のランダム座標割り当て
    set.seed(123)
    df_kw <- df_kw %>%
      mutate(
        x = runif(n(), 1, 10),
        y = runif(n(), 1, 10),
        size = 12 + (n - min(n)) / (max(n) - min(n) + 1e-5) * 24
      )
    
    plot_ly(df_kw, x = ~x, y = ~y, text = ~word, type = "scatter", mode = "text",
            textfont = list(size = ~size, color = "#2b5c8f"),
            hoverinfo = "text", hovertext = ~paste0(word, ": ", n, "回")) %>%
      layout(
        title = "出現キーワード (Top 30)",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "")
      )
  })
  
  # 4. ジャーナル Top 10
  output$journal_bar_plot <- renderPlotly({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    journals <- map_chr(works, function(w) {
      w$primary_location$source$display_name %||% "Unknown / Preprints"
    })
    
    df_j <- tibble(journal = journals) %>%
      filter(journal != "Unknown / Preprints") %>%
      count(journal, sort = TRUE) %>%
      slice_head(n = 10) %>%
      arrange(n)
    
    if (nrow(df_j) == 0) return(NULL)
    
    plot_ly(df_j, x = ~n, y = ~factor(journal, levels = journal), type = "bar",
            orientation = "h", marker = list(color = "#59a14f")) %>%
      layout(title = "掲載ジャーナル Top 10",
             xaxis = list(title = "論文数"),
             yaxis = list(title = ""))
  })
  
  # 5. 共著者 Top 10
  output$coauthor_bar_plot <- renderPlotly({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    current_author_id <- gsub(".*authors/", "", input$author_id)
    
    coauthors <- map(works, ~ .x$authorships) %>%
      compact() %>%
      flatten() %>%
      map(function(a) {
        id <- gsub(".*authors/", "", a$author$id %||% "")
        name <- a$author$display_name %||% ""
        list(id = id, name = name)
      }) %>%
      discard(~ .x$id == current_author_id || .x$name == "") %>%
      map_chr(~ .x$name)
    
    if (length(coauthors) == 0) return(NULL)
    
    df_co <- tibble(author = coauthors) %>%
      count(author, sort = TRUE) %>%
      slice_head(n = 10) %>%
      arrange(n)
    
    plot_ly(df_co, x = ~n, y = ~factor(author, levels = author), type = "bar",
            orientation = "h", marker = list(color = "#f28e2b")) %>%
      layout(title = "共著者 Top 10",
             xaxis = list(title = "共著論文数"),
             yaxis = list(title = ""))
  })
}

shinyApp(ui = ui, server = server)