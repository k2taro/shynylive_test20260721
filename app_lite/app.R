library(shiny)
library(jsonlite)
library(echarts4r)

# h-index 計算関数
calc_h_index <- function(citations) {
  if (length(citations) == 0) return(0)
  sorted_cites <- sort(citations, decreasing = TRUE)
  h <- max(which(sorted_cites >= seq_along(sorted_cites)), 0)
  return(ifelse(is.infinite(h), 0, h))
}

# ヌル合体演算子 (base R)
`%||%` <- function(x, y) if (is.null(x)) y else x

# カード型コンテナヘッダー用ヘルパー関数
card_header <- function(title, help_id) {
  div(
    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; border-bottom: 2px solid #e9ecef; padding-bottom: 5px;",
    h4(title, style = "margin: 0; font-weight: bold; color: #2b5c8f;"),
    actionLink(help_id, label = " 💡解説", icon = icon("info-circle"), style = "font-size: 0.85em; color: #6c757d;")
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* データ計算中の要素を半透明にし、メッセージを表示 */
      .shiny-output-waiting-base { opacity: 0.3 !important; }
      .loading-notice { background-color: #e8f4f8; border-left: 4px solid #2b5c8f; padding: 10px 15px; margin-bottom: 15px; border-radius: 4px; font-size: 0.9em; color: #2b5c8f; }
      /* テーブルのカスタムCSS (DTの代替) */
      .custom-table table { width: 100%; border-collapse: collapse; }
      .custom-table th { background-color: #f8f9fa; border-bottom: 2px solid #dee2e6; padding: 8px; font-size: 0.9em; }
      .custom-table td { padding: 8px; border-bottom: 1px solid #dee2e6; font-size: 0.85em; }
      .custom-table td:nth-child(2) { text-align: center; }
      /* ネットワーク用ラジオボタン一覧の高さ固定・スクロール設定 */
      .network-paper-list { max-height: 440px; overflow-y: auto; padding: 10px; border: 1px solid #dee2e6; border-radius: 4px; background-color: #fff; }
      .network-paper-list .radio { margin-top: 5px; margin-bottom: 8px; font-size: 0.85em; }
      /* カードコンテナスタイリング */
      .dashboard-card { background: #ffffff; border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); }
    "))
  ),

  titlePanel("OpenAlex Author Dashboard (echarts4r / Shinylive Ready)"),
  
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
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          title = "📊 ダッシュボード",
          br(),
          # サマリー指標
          fluidRow(
            column(4, wellPanel(style = "text-align: center; padding: 10px;", h4("論文数"), h2(textOutput("total_papers")))),
            column(4, wellPanel(style = "text-align: center; padding: 10px;", h4("総被引用数"), h2(textOutput("total_citations")))),
            column(4, wellPanel(style = "text-align: center; padding: 10px;", h4("h-index"), h2(textOutput("h_index"))))
          ),
          hr(),
          # 上段: 年推移 (幅8) & 共著者Top10 (幅4)
          fluidRow(
            column(8, 
              div(class = "dashboard-card",
                card_header("年別 研究業績推移（論文数・被引用数）", "help_time_series"),
                echarts4rOutput("time_series_plot", height = "400px")
              )
            ),
            column(4, 
              div(class = "dashboard-card",
                card_header("主要な共著者 Top 10", "help_coauthors"),
                div(class = "custom-table", style = "height: 400px; overflow-y: auto;", tableOutput("coauthor_table"))
              )
            )
          ),
          # 中段: キーワード (幅4) & トピック (幅4) & ジャーナルTop10 (幅4)
          fluidRow(
            column(4, 
              div(class = "dashboard-card",
                card_header("研究キーワード (Top 25)", "help_keywords"),
                echarts4rOutput("wordcloud_plot", height = "400px")
              )
            ),
            column(4, 
              div(class = "dashboard-card",
                card_header("主要研究トピック構成 (Top 10)", "help_topics"),
                echarts4rOutput("topic_pie_plot", height = "400px")
              )
            ),
            column(4, 
              div(class = "dashboard-card",
                card_header("主要掲載ジャーナル Top 10", "help_journals"),
                div(class = "custom-table", style = "height: 400px; overflow-y: auto;", tableOutput("journal_table"))
              )
            )
          ),
          # 下段（3段目）: 左側2/3(幅8)にネットワーク、右側1/3(幅4)に論文一覧
          fluidRow(
            column(8, 
              div(class = "dashboard-card",
                card_header("自著論文間の引用ネットワーク", "help_network"),
                echarts4rOutput("paper_network_plot", height = "450px")
              )
            ),
            column(4, 
              div(class = "dashboard-card",
                card_header("ネットワーク掲載 論文一覧", "help_paper_list"),
                helpText("選択した論文が左のネットワーク上でオレンジ色に強調されます。"),
                div(class = "network-paper-list", uiOutput("network_paper_selector"))
              )
            )
          )
        ),
        
        # 2つ目のタブ：各分析の詳細解説
        tabPanel(
          title = "📖 各分析の詳細解説",
          br(),
          fluidRow(
            column(10, offset = 1,
              h3("OpenAlex 研究パフォーマンス分析の解説", style = "color: #2b5c8f; border-bottom: 2px solid #2b5c8f; padding-bottom: 8px;"),
              
              h4("1. 年別 研究業績推移（論文数・被引用数）"),
              p("指定された著者が各年に発表した「論文数（青棒グラフ）」と、それらの論文が獲得した「被引用数（赤折れ線グラフ）」の年別推移を示します。研究活動の波や、特定の時期に発表された論文の影響力の高まりを把握できます。"),
              hr(),
              
              h4("2. 主要な共著者 Top 10"),
              p("該当著者が最も頻繁に共同執筆を行っている研究者 Top 10 の一覧です。共著回数の多い順に表示され、名前をクリックすると相手の OpenAlex 著者ページを開くことができます。"),
              hr(),
              
              h4("3. 研究キーワード (Top 25)"),
              p("著者の全論文のメタデータから抽出された頻出キーワード上位25件をワードクラウド形式で視覚化しています。文字が大きい単語ほど、著者の研究において中心的なテーマであることを示します。"),
              hr(),
              
              h4("4. 主要研究トピック構成 (Top 10)"),
              p("OpenAlexが自動分類している研究分野・トピック（Topics）の比率を Top 10 まで円グラフで表示します。著者がどの学術領域を中心に貢献しているか、あるいは複数の領域にまたがる学際的な研究を行っているかがわかります。"),
              hr(),
              
              h4("5. 主要掲載ジャーナル Top 10"),
              p("論文が多く掲載されている学術誌（ジャーナル）や国際会議の一覧です。クリックすることで各ジャーナルの詳細情報（OpenAlex）にアクセスできます。"),
              hr(),
              
              h4("6. 自著論文間の引用ネットワーク"),
              p("著者自身の発表論文同士の「引用・被引用関係（自己引用ネットワーク）」を可視化したネットワーク図です。"),
              tags$ul(
                tags$li("ノード（円）：個別の論文を表します。"),
                tags$li("ノードの大きさ：自身の「他の論文から引用された回数（自著内被引用数）」に比例します。大きければ大きいほど、自身の研究におけるコア・基盤となった論文です。"),
                tags$li("矢印（エッジ）：引用元から引用先（過去の自著論文）に向かって伸びます。"),
                tags$li("右側リスト連動：右側の「ネットワーク掲載 論文一覧」から特定の論文を選択すると、該当ノードがオレンジ色に強調表示されます。")
              ),
              br()
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # APIからのデータ取得
  raw_data <- eventReactive(input$fetch_btn, {
    req(input$author_id)
    clean_id <- trimws(input$author_id)
    
    url <- sprintf("https://api.openalex.org/works?filter=author.id:%s&per-page=200&mailto=example@example.com", clean_id)
    
    res <- tryCatch({
      jsonlite::fromJSON(url, simplifyVector = FALSE)
    }, error = function(e) return(NULL))
    
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
    
    sliderInput("year_range", "対象年の絞り込み:", min = min_yr, max = max_yr, value = c(min_yr, max_yr), step = 1, sep = "")
  })
  
  # フィルタリング済みデータ
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
  output$total_papers <- renderText({ length(filtered_works()) })
  output$total_citations <- renderText({ sum(sapply(filtered_works(), function(w) w$cited_by_count %||% 0)) })
  output$h_index <- renderText({ calc_h_index(sapply(filtered_works(), function(w) w$cited_by_count %||% 0)) })
  
  # --- モーダル解説ダイアログを表示する処理 ---
  show_help <- function(title, text) {
    showModal(modalDialog(
      title = title,
      p(text),
      easyClose = TRUE,
      footer = modalButton("閉じる")
    ))
  }
  
  observeEvent(input$help_time_series, show_help("年別 研究業績推移", "年ごとの論文執筆数（棒グラフ）と、被引用数の推移（折れ線グラフ）を表示します。過去から現在までの研究インパクトの変化を確認できます。"))
  observeEvent(input$help_coauthors, show_help("主要な共著者 Top 10", "この著者と共同で執筆した回数が最も多い共著者上位10名を表示します。名前をクリックするとOpenAlexの著者ページを開きます。"))
  observeEvent(input$help_keywords, show_help("研究キーワード (Top 25)", "論文メタデータから抽出された主要キーワード上位25件です。大きく表示されている単語ほど多用されています。"))
  observeEvent(input$help_topics, show_help("主要研究トピック構成 (Top 10)", "OpenAlexが自動分類した研究領域・トピックの構成比率Top10です。主要な研究ドメインがわかります。"))
  observeEvent(input$help_journals, show_help("主要掲載ジャーナル Top 10", "論文が多く掲載されているジャーナルや会議体の上位10件です。"))
  observeEvent(input$help_network, show_help("自著論文間の引用ネットワーク", "自身の過去の論文をどの程度参照・発展させているか（自著内引用関係）を可視化します。円の大きさは自著論文からの引用回数を示します。"))
  observeEvent(input$help_paper_list, show_help("ネットワーク掲載 論文一覧", "ネットワークに含まれる論文の一覧です。論文を選択すると、ネットワーク上で該当ノードがオレンジ色に強調表示されます。"))

  # 年推移グラフ (echarts4r)
  output$time_series_plot <- renderEcharts4r({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    years <- sapply(works, function(w) w$publication_year %||% NA_integer_)
    cites <- sapply(works, function(w) w$cited_by_count %||% 0)
    
    valid <- !is.na(years)
    if (!any(valid)) return(NULL)
    
    tbl_papers <- table(years[valid])
    tbl_cites <- tapply(cites[valid], years[valid], sum)
    
    df <- data.frame(
      year = as.numeric(names(tbl_papers)),
      papers = as.numeric(tbl_papers),
      citations = as.numeric(tbl_cites)
    )
    df <- df[order(df$year), ]
    
    min_year <- min(df$year)
    max_year <- max(df$year)
    
    p <- e_charts(df, year)
    p <- e_bar(p, papers, name = "論文数", y_index = 0, itemStyle = list(color = "#4c78a8"))
    p <- e_line(p, citations, name = "被引用数", y_index = 1, itemStyle = list(color = "#e15759"))
    p <- e_y_axis(p, name = "論文数", index = 0)
    p <- e_y_axis(p, name = "被引用数", index = 1)
    
    p <- e_x_axis(p, min = min_year, max = max_year, interval = 1, axisLabel = list(formatter = "{value}"))
    p <- e_tooltip(p, trigger = "axis")
    p <- e_legend(p, right = 10)
    p
  })
  
  # 2. トピック（円グラフ）
  output$topic_pie_plot <- renderEcharts4r({
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
    
    df_topic <- data.frame(topic = names(top10), count = as.numeric(top10), stringsAsFactors = FALSE)
    
    p <- e_charts(df_topic, topic)
    p <- e_pie(p, count, radius = c("40%", "70%"))
    p <- e_tooltip(p, trigger = "item")
    p <- e_legend(p, show = FALSE)
    p
  })
  
  # 3. キーワード (WordCloud)
  output$wordcloud_plot <- renderEcharts4r({
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
    
    df_kw <- data.frame(word = names(top25), n = as.numeric(top25), stringsAsFactors = FALSE)
    
    df_kw |> 
      e_charts(word) |> 
      e_cloud(word, n, sizeRange = c(12, 35)) |> 
      e_tooltip(formatter = htmlwidgets::JS("
        function(params){
          return params.name + ': ' + params.value + '回';
        }
      "))
  })
  
  # 4. ジャーナル Top 10 (Shiny renderTable)
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
    key <- paste(names_vec[valid], ids_vec[valid], sep = "___")
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
    
    data.frame(`ジャーナル名` = j_links, `論文数` = as.integer(top10_counts), check.names = FALSE, stringsAsFactors = FALSE)
  }, sanitize.text.function = identity)
  
  # 5. 共著者 Top 10 (Shiny renderTable)
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
    
    data.frame(`共著者名` = co_links, `共著論文数` = as.integer(top10_counts), check.names = FALSE, stringsAsFactors = FALSE)
  }, sanitize.text.function = identity)

  # ネットワーク共有用の計算リアクティブ
  network_data <- reactive({
    works <- filtered_works()
    if (length(works) == 0) return(NULL)
    
    paper_ids <- unname(sapply(works, function(w) gsub(".*openalex\\.org/", "", w$id %||% "")))
    paper_titles <- unname(sapply(works, function(w) w$title %||% "Untitled"))
    paper_years <- unname(sapply(works, function(w) w$publication_year %||% "不明"))
    paper_journals <- unname(sapply(works, function(w) {
      src <- w$primary_location$source
      if (!is.null(src) && !is.null(src$display_name)) src$display_name else "不明 / プレプリント"
    }))
    total_citations <- unname(sapply(works, function(w) w$cited_by_count %||% 0))
    
    edges_list <- lapply(works, function(w) {
      source_id <- gsub(".*openalex\\.org/", "", w$id %||% "")
      referenced_ids <- sapply(w$referenced_works %||% list(), function(ref) gsub(".*openalex\\.org/", "", ref))
      valid_refs <- intersect(referenced_ids, paper_ids)
      if (length(valid_refs) == 0) return(NULL)
      data.frame(source = source_id, target = valid_refs, stringsAsFactors = FALSE)
    })
    
    edges <- do.call(rbind, Filter(Negate(is.null), edges_list))
    if (is.null(edges) || nrow(edges) == 0) return(NULL)
    
    self_cited_tbl <- table(edges$target)
    self_cited_counts <- unname(sapply(paper_ids, function(id) {
      if (id %in% names(self_cited_tbl)) as.numeric(self_cited_tbl[id]) else 0
    }))
    
    list(
      works = works,
      paper_ids = paper_ids,
      paper_titles = paper_titles,
      paper_years = paper_years,
      paper_journals = paper_journals,
      total_citations = total_citations,
      self_cited_counts = self_cited_counts,
      edges = edges
    )
  })

  # 右側UI: ネットワーク内論文セレクターの生成
  output$network_paper_selector <- renderUI({
    net <- network_data()
    if (is.null(net)) return(p("引用関係のある論文はありません。"))
    
    ord <- order(net$self_cited_counts, decreasing = TRUE)
    
    choices <- net$paper_ids[ord]
    names(choices) <- sprintf("[%s年] %s (自著被引用:%d回)", 
                              net$paper_years[ord], 
                              net$paper_titles[ord], 
                              net$self_cited_counts[ord])
    
    choices <- c("選択解除" = "none", choices)
    radioButtons("selected_net_paper", label = NULL, choices = choices, selected = "none")
  })

  # 6. 論文間の引用ネットワーク (選択された論文をオレンジ色で強調表示)
  output$paper_network_plot <- renderEcharts4r({
    net <- network_data()
    if (is.null(net)) {
      return(
        e_charts() |> 
          e_title("※取得データ内に論文同士の引用関係が見つかりませんでした")
      )
    }
    
    selected_id <- input$selected_net_paper %||% "none"
    
    min_sc <- min(net$self_cited_counts)
    max_sc <- max(net$self_cited_counts)
    node_sizes <- 15 + (net$self_cited_counts - min_sc) / (max_sc - min_sc + 1e-5) * 30
    
    nodes_list <- lapply(seq_along(net$paper_ids), function(i) {
      is_selected <- (net$paper_ids[i] == selected_id)
      
      fill_color <- if (is_selected) "#ff7f0e" else "rgba(144, 202, 249, 0.75)"
      border_color <- if (is_selected) "#d62728" else "#2b5c8f"
      border_w <- if (is_selected) 3.0 else 1.5
      
      list(
        name = as.character(net$paper_ids[i]),
        value = as.numeric(net$self_cited_counts[i]),
        total_cites = as.numeric(net$total_citations[i]),
        symbolSize = round(as.numeric(node_sizes[i])) + (if (is_selected) 6 else 0),
        title = as.character(net$paper_titles[i]),
        year = as.character(net$paper_years[i]),
        journal = as.character(net$paper_journals[i]),
        itemStyle = list(
          color = fill_color,
          borderColor = border_color,
          borderWidth = border_w
        )
      )
    })
    
    links_list <- lapply(seq_len(nrow(net$edges)), function(i) {
      list(
        source = as.character(net$edges$source[i]),
        target = as.character(net$edges$target[i])
      )
    })
    
    p <- e_charts() |> 
      e_graph(
        layout = "force", 
        roam = TRUE, 
        draggable = TRUE, 
        focusNodeAdjacency = TRUE,
        edgeSymbol = c("none", "arrow"),
        edgeSymbolSize = c(0, 8),
        lineStyle = list(color = "#aaaaaa", opacity = 0.6, width = 1)
      )
    
    p$x$opts$series[[1]]$data <- unname(nodes_list)
    p$x$opts$series[[1]]$links <- unname(links_list)
    
    p |> 
      e_tooltip(formatter = htmlwidgets::JS("
        function(params){
          if(params.dataType === 'node'){
            return '<b>' + params.data.title + '</b><br/>' +
                   '<b>年:</b> ' + params.data.year + '<br/>' +
                   '<b>ジャーナル:</b> ' + params.data.journal + '<br/>' +
                   '<b>自著からの被引用数:</b> ' + params.value + '回<br/>' +
                   '<span style=\"color:#777; font-size:0.9em;\">(総被引用数: ' + params.data.total_cites + '回)</span>';
          } else if(params.dataType === 'edge'){
            return '引用関係 (引用元 → 引用先)';
          }
        }
      ")) |> 
      e_labels(show = FALSE)
  })

}

shinyApp(ui = ui, server = server)