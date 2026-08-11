library(shiny)
library(jsonlite)
library(plotly)

# h-index 計算関数
calc_h_index <- function(citations) {
  if (length(citations) == 0) return(0)
  sorted_cites <- sort(citations, decreasing = TRUE)
  h <- max(which(sorted_cites >= seq_along(sorted_cites)), 0)
  return(ifelse(is.infinite(h), 0, h))
}

# ヌル合体演算子の代替 (base R)
`%||%` <- function(x, y) if (is.null(x)) y else x

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-waiting-base { opacity: 0.3 !important; }
      /* テーブルの見た目を整えるスタイル */
      .custom-table table { width: 100%; border-collapse: collapse; }
      .custom-table th { background-color: #f8f9fa; border-bottom: 2px solid #dee2e6; padding: 8px; font-size: 0.9em; }
      .custom-table td { padding: 8px; border-bottom: 1px solid #dee2e6; font-size: 0.85em; }
      .custom-table td:nth-child(2) { text-align: center; } /* 論文数カラムを中央寄せ */
    "))
  ),

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
        column(4, wellPanel(style = "text-align: center; padding: 10px;", h4("論文数"), h2(textOutput("total_papers")))),
        column(4, wellPanel(style = "text-align: center; padding: 10px;", h4("総被引用数"), h2(textOutput("total_citations")))),
        column(4, wellPanel(style = "text-align: center; padding: 10px;", h4("h-index"), h2(textOutput("h_index"))))
      ),
      hr(),
      fluidRow(
        column(8, plotlyOutput("time_series_plot", height = "450px")),
        column(4, div(class = "custom-table", tableOutput("coauthor_table")))
      ),
      br(),
      fluidRow(
        column(4, plotlyOutput("wordcloud_plot", height = "450px")),
        column(4, plotlyOutput("topic_pie_plot", height = "450px")),
        column(4, div(class = "custom-table", tableOutput("journal_table")))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # APIからのデータ取得
  raw_data <- eventReactive(input$fetch_btn, {
    req(input$author_id)
    clean_id <- trimws(input$author_id)
    
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
    
    years <- sapply(works, function(w) w$publication_year %||% 2000)
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
      works <- Filter(function(w) {
        yr <- w$publication_year
        !is.null(yr) && yr >= input$year_range[1] && yr <= input$year_range[2]
      }, works)
    }
    works
  })
  
  # 1. 指標の計算と表示
  output$total_papers <- renderText({
    length(filtered_works())
  })
  
  output$total_citations <- renderText({
    works <- filtered_works()
    sum(sapply(works, function(w) w$cited_by_count %||% 0))
  })
  
  output$h_index <- renderText({
    works <- filtered_works()
    cites <- sapply(works, function(w) w$cited_by_count %||% 0)
    calc_h_index(cites)
  })
  
  # 年推移グラフ (論文数 & 被引用数)
  output$time_series_plot <- renderPlotly({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    years <- sapply(works, function(w) w$publication_year %||% NA_integer_)
    cites <- sapply(works, function(w) w$cited_by_count %||% 0)
    
    valid <- !is.na(years)
    if (!any(valid)) return(NULL)
    
    years <- years[valid]
    cites <- cites[valid]
    
    tbl_papers <- table(years)
    tbl_cites <- tapply(cites, years, sum)
    
    df <- data.frame(
      year = as.numeric(names(tbl_papers)),
      papers = as.numeric(tbl_papers),
      citations = as.numeric(tbl_cites)
    )
    df <- df[order(df$year), ]
    
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
    
    topics_list <- unlist(lapply(works, function(w) {
      if (is.null(w$topics)) return(NULL)
      sapply(w$topics, function(t) t$display_name %||% NA_character_)
    }))
    topics <- na.omit(topics_list)
    
    if (length(topics) == 0) return(NULL)
    
    tbl <- sort(table(topics), decreasing = TRUE)
    top10 <- head(tbl, 10)
    
    df_topic <- data.frame(
      topic = names(top10),
      n = as.numeric(top10),
      stringsAsFactors = FALSE
    )
    
    plot_ly(df_topic, labels = ~topic, values = ~n, type = "pie",
            textinfo = "label+percent", hoverinfo = "label+value+percent") %>%
      layout(title = "主要トピック構成 (Top 10)")
  })
  
  # 3. キーワード
  output$wordcloud_plot <- renderPlotly({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    kw_list <- unlist(lapply(works, function(w) {
      if (is.null(w$keywords)) return(NULL)
      sapply(w$keywords, function(k) k$display_name %||% NA_character_)
    }))
    keywords <- na.omit(kw_list)
    
    if (length(keywords) == 0) return(NULL)
    
    tbl <- sort(table(keywords), decreasing = TRUE)
    top25 <- head(tbl, 25)
    
    if (length(top25) == 0) return(NULL)
    
    df_kw <- data.frame(
      word = names(top25),
      n = as.numeric(top25),
      stringsAsFactors = FALSE
    )
    
    num_words <- nrow(df_kw)
    a <- 0
    b <- 1.8
    golden_angle <- 2.39996
    
    idx <- seq_len(num_words) - 1
    theta <- idx * golden_angle
    r <- a + b * sqrt(idx)
    
    df_kw$x <- r * cos(theta)
    df_kw$y <- r * sin(theta)
    
    min_n <- min(df_kw$n)
    max_n <- max(df_kw$n)
    df_kw$size <- 11 + (df_kw$n - min_n) / (max_n - min_n + 1e-5) * 18
    
    plot_ly(df_kw, x = ~x, y = ~y, text = ~word, type = "scatter", mode = "text",
            textfont = list(size = ~size, color = "#2b5c8f"),
            hoverinfo = "text", hovertext = ~paste0(word, ": ", n, "回")) %>%
      layout(
        title = list(text = "出現キーワード (Top 25)", font = list(size = 14)),
        margin = list(t = 40, b = 20, l = 20, r = 20),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "")
      )
  })
  
  # 4. ジャーナル Top 10 (Shiny標準の renderTable に変更)
  output$journal_table <- renderTable({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    journals <- lapply(works, function(w) {
      src <- w$primary_location$source
      if (is.null(src) || is.null(src$display_name)) return(NULL)
      list(name = src$display_name, id = src$id %||% "")
    })
    journals <- Filter(Negate(is.null), journals)
    
    if (length(journals) == 0) return(NULL)
    
    names_vec <- sapply(journals, function(j) j$name)
    ids_vec <- sapply(journals, function(j) j$id)
    
    valid <- !is.na(names_vec) & names_vec != "Unknown / Preprints"
    names_vec <- names_vec[valid]
    ids_vec <- ids_vec[valid]
    
    if (length(names_vec) == 0) return(NULL)
    
    key <- paste(names_vec, ids_vec, sep = "___")
    tbl <- sort(table(key), decreasing = TRUE)
    top10_keys <- head(names(tbl), 10)
    top10_counts <- head(as.numeric(tbl), 10)
    
    split_keys <- strsplit(top10_keys, "___")
    j_names <- sapply(split_keys, `[`, 1)
    j_ids <- sapply(split_keys, `[`, 2)
    
    j_links <- ifelse(
      j_ids != "",
      sprintf('<a href="%s" target="_blank" style="text-decoration: none; color: #2b5c8f; font-weight: bold;">%s 🔗</a>', j_ids, j_names),
      j_names
    )
    
    data.frame(
      `ジャーナル名` = j_links,
      `論文数` = as.integer(top10_counts),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, sanitize.text.function = identity) # HTMLタグ（リンク）を有効化
  
  # 5. 共著者 Top 10 (Shiny標準の renderTable に変更)
  output$coauthor_table <- renderTable({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    clean_id <- gsub(".*openalex\\.org/|.*authors/", "", input$author_id)
    
    coauthor_list <- unlist(lapply(works, function(w) {
      if (is.null(w$authorships)) return(NULL)
      lapply(w$authorships, function(a) {
        id_raw <- a$author$id %||% ""
        id <- gsub(".*authors/", "", id_raw)
        name <- a$author$display_name %||% ""
        if (id == clean_id || name == "") return(NULL)
        list(id_raw = id_raw, name = name)
      })
    }), recursive = FALSE)
    
    coauthor_list <- Filter(Negate(is.null), coauthor_list)
    if (length(coauthor_list) == 0) return(NULL)
    
    names_vec <- sapply(coauthor_list, function(a) a$name)
    urls_vec <- sapply(coauthor_list, function(a) a$id_raw)
    
    key <- paste(names_vec, urls_vec, sep = "___")
    tbl <- sort(table(key), decreasing = TRUE)
    top10_keys <- head(names(tbl), 10)
    top10_counts <- head(as.numeric(tbl), 10)
    
    split_keys <- strsplit(top10_keys, "___")
    co_names <- sapply(split_keys, `[`, 1)
    co_urls <- sapply(split_keys, `[`, 2)
    
    co_links <- ifelse(
      co_urls != "",
      sprintf('<a href="%s" target="_blank" style="text-decoration: none; color: #2b5c8f; font-weight: bold;">%s 🔗</a>', co_urls, co_names),
      co_names
    )
    
    data.frame(
      `共著者名` = co_links,
      `共著論文数` = as.integer(top10_counts),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, sanitize.text.function = identity)
}

shinyApp(ui = ui, server = server)