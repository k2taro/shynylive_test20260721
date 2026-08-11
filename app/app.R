library(shiny)
library(jsonlite)
library(dplyr)
library(purrr)
library(plotly)
library(DT)

# h-index 計算関数
calc_h_index <- function(citations) {
  if (length(citations) == 0) return(0)
  sorted_cites <- sort(citations, decreasing = TRUE)
  h <- max(which(sorted_cites >= seq_along(sorted_cites)), 0)
  return(ifelse(is.infinite(h), 0, h))
}

ui <- fluidPage(
tags$head(
    tags$style(HTML("
      /* データ計算中の要素を半透明にし、メッセージを表示 */
      .shiny-output-waiting-base {
        opacity: 0.3 !important;
      }
      /* カスタムローディングスピナーのCSS */
      .loading-notice {
        background-color: #e8f4f8;
        border-left: 4px solid #2b5c8f;
        padding: 10px 15px;
        margin-bottom: 15px;
        border-radius: 4px;
        font-size: 0.9em;
        color: #2b5c8f;
      }
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
        column(8, plotlyOutput("time_series_plot", height = "450px")),
        column(4, DTOutput("coauthor_table", height = "450px"))
        #column(5, plotlyOutput("topic_pie_plot", height = "350px"))
      ),
      br(),
      # 下段: キーワード (幅4) & ジャーナルTop10 (幅4) & 共著者Top10 (幅4)
      fluidRow(
        column(4, plotlyOutput("wordcloud_plot", height = "450px")),
        column(4, plotlyOutput("topic_pie_plot", height = "450px")),
        column(4, DTOutput("journal_table", height = "450px"))
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
  
 # 3. キーワード (中心集中的なワードクラウド風散布図)
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
      slice_head(n = 25)
    
    if (nrow(df_kw) == 0) return(NULL)
    
    # 螺旋アルゴリズムで高頻度ワードを中心に配置
    num_words <- nrow(df_kw)
    # アルキメデスの螺旋のパラメータ (r = a + b * theta)
    a <- 0
    b <- 1.8
    # 黄金角 (約137.5度 = 2.3999 rad) で均等に分散
    golden_angle <- 2.39996
    
    df_kw <- df_kw %>%
      mutate(
        idx = row_number() - 1, # 1位(0)を中心に
        theta = idx * golden_angle,
        r = a + b * sqrt(idx),   # 中心付近を密にするためsqrtを使用
        x = r * cos(theta),
        y = r * sin(theta),
        # フォントサイズの計算 (Top 1~25に応じたメリハリ)
        size = 11 + (n - min(n)) / (max(n) - min(n) + 1e-5) * 18
      )
    
    plot_ly(df_kw, x = ~x, y = ~y, text = ~word, type = "scatter", mode = "text",
            textfont = list(size = ~size, color = "#2b5c8f"),
            hoverinfo = "text", hovertext = ~paste0(word, ": ", n, "回")) %>%
      layout(
        title = list(text = "出現キーワード (Top 25)", font = list(size = 14)),
        margin = list(t = 40, b = 20, l = 20, r = 20),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "", zerolinecolor = 'transparent'),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "", zerolinecolor = 'transparent')
      )
  })
  
  # 4. ジャーナル Top 10 (DT表 + 論文数カラム + 降順ソート + OpenAlexリンク)
  output$journal_table <- renderDT({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    # 掲載誌の表示名とOpenAlex IDを取得
    journal_data <- map(works, function(w) {
      src <- w$primary_location$source
      if (is.null(src) || is.null(src$display_name)) return(NULL)
      
      list(
        name = src$display_name,
        id = src$id %||% NA_character_
      )
    }) %>%
      compact()
    
    if (length(journal_data) == 0) return(NULL)
    
    # 集計・降順ソート・HTMLリンク生成
    df_j <- tibble(
      name = map_chr(journal_data, ~ .x$name),
      id = map_chr(journal_data, ~ .x$id)
    ) %>%
      filter(!is.na(name), name != "Unknown / Preprints") %>%
      group_by(name, id) %>%
      summarise(`論文数` = n(), .groups = "drop") %>%
      arrange(desc(`論文数`)) %>% # 論文数の降順（多い順）に並び替え
      slice_head(n = 10) %>%
      mutate(
        `ジャーナル名` = ifelse(
          !is.na(id) & id != "",
          sprintf('<a href="%s" target="_blank" style="text-decoration: none; color: #2b5c8f; font-weight: bold;">%s 🔗</a>', id, name),
          name
        )
      ) %>%
      select(`ジャーナル名`, `論文数`) # カラム順序の指定
    
    # DTの描画設定
    datatable(
      df_j,
      escape = FALSE, # HTMLリンクを有効化
      rownames = FALSE,
      options = list(
        dom = 't',          # 余計なUI（検索窓やページネーション）を非表示
        pageLength = 10,
        ordering = FALSE,   # 自動並び替えをオフにして抽出時の降順を保持
        columnDefs = list(
          list(className = 'dt-center', targets = 1) # 論文数（1列目）を中央揃え
        )
      )
    )
  })
  
 # 5. 共著者 Top 10 (DT表 + 論文数カラム + 降順ソート + OpenAlexリンク)
  output$coauthor_table <- renderDT({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    clean_id <- gsub(".*openalex\\.org/|.*authors/", "", input$author_id)
    
    # 共著者の名前とOpenAlex IDを取得
    coauthor_data <- map(works, ~ .x$authorships) %>%
      compact() %>%
      flatten() %>%
      map(function(a) {
        id_raw <- a$author$id %||% ""
        id <- gsub(".*authors/", "", id_raw)
        name <- a$author$display_name %||% ""
        
        list(id_raw = id_raw, id = id, name = name)
      }) %>%
      discard(~ .x$id == clean_id || .x$name == "")
    
    if (length(coauthor_data) == 0) return(NULL)
    
    # 集計・降順ソート・HTMLリンク生成
    df_co <- tibble(
      name = map_chr(coauthor_data, ~ .x$name),
      url = map_chr(coauthor_data, ~ .x$id_raw)
    ) %>%
      group_by(name, url) %>%
      summarise(`共著論文数` = n(), .groups = "drop") %>%
      arrange(desc(`共著論文数`)) %>% # 降順に並び替え
      slice_head(n = 10) %>%
      mutate(
        `共著者名` = ifelse(
          !is.na(url) & url != "",
          sprintf('<a href="%s" target="_blank" style="text-decoration: none; color: #2b5c8f; font-weight: bold;">%s 🔗</a>', url, name),
          name
        )
      ) %>%
      select(`共著者名`, `共著論文数`) # カラム順序の指定
    
    # DTの描画設定
    datatable(
      df_co,
      escape = FALSE, # HTMLリンクを有効化
      rownames = FALSE,
      options = list(
        dom = 't',          # 余計なUI（検索窓やページネーション）を非表示
        pageLength = 10,
        ordering = FALSE,   # 抽出時の降順ソートを保持
        columnDefs = list(
          list(className = 'dt-center', targets = 1) # 共著論文数（1列目）を中央揃え
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)