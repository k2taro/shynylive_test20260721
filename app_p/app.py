import re
import collections
from shiny import App, ui, render, reactive
from pyecharts import options as opts
from pyecharts.charts import Line, Bar, Pie, Graph

# --- ヘルパー関数群 ---

def calc_h_index(citations):
    """h-indexを計算する関数"""
    if not citations:
        return 0
    sorted_cites = sorted(citations, reverse=True)
    h = 0
    for i, cite in enumerate(sorted_cites):
        if cite >= i + 1:
            h = i + 1
        else:
            break
    return h

def card_header(title: str, help_id: str):
    """カード型コンテナヘッダー用ヘルパー関数"""
    return ui.div(
        ui.h4(title, style="margin: 0; font-weight: bold; color: #2b5c8f;"),
        ui.input_action_link(help_id, " 💡解説", style="font-size: 0.85em; color: #6c757d; text-decoration: none;"),
        style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; border-bottom: 2px solid #e9ecef; padding-bottom: 5px;"
    )

# --- UI定義 ---

app_ui = ui.page_fluid(
    ui.head_content(
        ui.tags.script(
            src="https://cdn.jsdelivr.net/npm/echarts@6.1.0/dist/echarts.min.js"
        ),
        ui.tags.style("""
            .shiny-output-waiting-base { opacity: 0.3 !important; }
            .loading-notice { background-color: #e8f4f8; border-left: 4px solid #2b5c8f; padding: 10px 15px; margin-bottom: 15px; border-radius: 4px; font-size: 0.9em; color: #2b5c8f; }
            .custom-table table { width: 100%; border-collapse: collapse; }
            .custom-table th { background-color: #f8f9fa; border-bottom: 2px solid #dee2e6; padding: 8px; font-size: 0.9em; text-align: left; }
            .custom-table td { padding: 8px; border-bottom: 1px solid #dee2e6; font-size: 0.85em; }
            .network-paper-list { max-height: 440px; overflow-y: auto; padding: 10px; border: 1px solid #dee2e6; border-radius: 4px; background-color: #fff; }
            .dashboard-card { background: #ffffff; border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); }
            .control-panel { background: #fdfdfd; border-right: 1px solid #e0e0e0; padding: 15px; min-height: 100vh; }
        """),
        # ★ JavaScriptによるネイティブデータ取得ロジック
        ui.tags.script("""
            $(document).on('click', '#fetch_btn', async function() {
                var authorId = $('#author_id').val().trim();
                if (!authorId) return;
                
                // クリーンなIDの抽出
                authorId = authorId.replace(/.*openalex\\.org\\/|.*authors\\//g, "");
                var url = `https://api.openalex.org/works?filter=author.id:${authorId}&per-page=200&mailto=example@example.com`;
                
                try {
                    let response = await fetch(url);
                    if (!response.ok) throw new Error('Network response was not ok');
                    let data = await response.json();
                    let results = data.results || [];
                    
                    // Python側の input.js_fetched_data にデータを送信
                    Shiny.setInputValue('js_fetched_data', results, {priority: 'event'});
                } catch (err) {
                    console.error("JS Fetch Error:", err);
                    Shiny.setInputValue('js_fetched_data', [], {priority: 'event'});
                }
            });
        """)
    ),
    ui.panel_title("OpenAlex Author Dashboard (JSフェッチ連携版)"),
    
    ui.row(
        # 左側：常時表示のコントロールパネル (幅2)
        ui.column(2,
            ui.div(
                ui.h4("操作パネル", style="font-weight: bold; color: #2b5c8f; font-size: 1.1em; margin-bottom: 15px;"),
                ui.input_text("author_id", "著者ID (OpenAlex):", value="A5074884601"),
                ui.input_action_button("fetch_btn", "データ取得・更新", class_="btn-primary", style="width: 100%; margin-bottom: 15px;"),
                ui.hr(),
                ui.output_ui("year_filter_ui"),
                ui.hr(),
                ui.help_text("※ ブラウザのJS機能(fetch)を用いてOpenAlex APIから直接データを取得します。"),
                class_="control-panel"
            )
        ),
        # 右側：メインコンテンツ (幅10)
        ui.column(10,
            ui.navset_card_tab(
                ui.nav_panel(
                    "📊 ダッシュボード",
                    ui.br(),
                    # サマリー指標
                    ui.row(
                        ui.column(4, ui.value_box("論文数", ui.output_text("total_papers"), theme="primary")),
                        ui.column(4, ui.value_box("総被引用数", ui.output_text("total_citations"), theme="info")),
                        ui.column(4, ui.value_box("h-index", ui.output_text("h_index"), theme="success"))
                    ),
                    ui.hr(),
                    # 上段: 年推移 & 共著者Top10
                    ui.row(
                        ui.column(8, 
                            ui.div(
                                card_header("年別 研究業績推移（論文数・被引用数）", "help_time_series"),
                                ui.output_ui("time_series_plot"),
                                class_="dashboard-card"
                            )
                        ),
                        ui.column(4, 
                            ui.div(
                                card_header("主要な共著者 Top 10", "help_coauthors"),
                                ui.div(ui.output_table("coauthor_table"), class_="custom-table", style="height: 400px; overflow-y: auto;"),
                                class_="dashboard-card"
                            )
                        )
                    ),
                    # 中段: キーワード円グラフ & トピック & ジャーナルTop10
                    ui.row(
                        ui.column(4, 
                            ui.div(
                                card_header("主要研究キーワード構成 (Top 10)", "help_keywords"),
                                ui.output_ui("keyword_pie_plot"),
                                class_="dashboard-card"
                            )
                        ),
                        ui.column(4, 
                            ui.div(
                                card_header("主要研究トピック構成 (Top 10)", "help_topics"),
                                ui.output_ui("topic_pie_plot"),
                                class_="dashboard-card"
                            )
                        ),
                        ui.column(4, 
                            ui.div(
                                card_header("主要掲載ジャーナル Top 10", "help_journals"),
                                ui.div(ui.output_table("journal_table"), class_="custom-table", style="height: 400px; overflow-y: auto;"),
                                class_="dashboard-card"
                            )
                        )
                    ),
                    # 下段: 引用ネットワーク & 論文一覧
                    ui.row(
                        ui.column(8, 
                            ui.div(
                                card_header("自著論文間の引用ネットワーク", "help_network"),
                                ui.output_ui("paper_network_plot"),
                                class_="dashboard-card"
                            )
                        ),
                        ui.column(4, 
                            ui.div(
                                card_header("ネットワーク掲載 論文一覧", "help_paper_list"),
                                ui.help_text("選択した論文が左のネットワーク上でオレンジ色に強調されます。"),
                                ui.div(ui.output_ui("network_paper_selector"), class_="network-paper-list"),
                                class_="dashboard-card"
                            )
                        )
                    )
                ),
                ui.nav_panel(
                    "📖 各分析の詳細解説",
                    ui.br(),
                    ui.row(
                        ui.column(10, 
                            ui.h3("OpenAlex 研究パフォーマンス分析の解説", style="color: #2b5c8f; border-bottom: 2px solid #2b5c8f; padding-bottom: 8px;"),
                            ui.h4("1. 年別 研究業績推移（論文数・被引用数）"),
                            ui.p("指定された著者が各年に発表した「論文数（青棒グラフ）」と、それらの論文が獲得した「被引用数（赤折れ線グラフ）」の年別推移を示します。"),
                            ui.hr(),
                            ui.h4("2. 主要な共著者 Top 10"),
                            ui.p("該当著者が最も頻繁に共同執筆を行っている研究者 Top 10 の一覧です。"),
                            ui.hr(),
                            ui.h4("3. 主要研究キーワード構成 (Top 10)"),
                            ui.p("著者の全論文のメタデータから抽出された頻出キーワード上位10件の構成比率を円グラフで表示します。"),
                            ui.hr(),
                            ui.h4("4. 主要研究トピック構成 (Top 10)"),
                            ui.p("OpenAlexが自動分類している研究分野・トピック（Topics）の比率を円グラフで表示します。"),
                            ui.hr(),
                            ui.h4("5. 主要掲載ジャーナル Top 10"),
                            ui.p("論文が多く掲載されている学術誌（ジャーナル）や国際会議の一覧です。"),
                            ui.hr(),
                            ui.h4("6. 自著論文間の引用ネットワーク"),
                            ui.p("著者自身の発表論文同士の「引用・被引用関係」を可視化したネットワーク図です。"),
                            ui.tags.ul(
                                ui.tags.li("ノード（円）：個別論文を表し、大きさは自著内被引用数に比例します。"),
                                ui.tags.li("矢印（エッジ）：引用元から過去の自著論文に向かって伸びます。"),
                                ui.tags.li("右側リスト連動：論文を選択すると該当ノードがオレンジ色に強調されます。")
                            ),
                            offset=1
                        )
                    )
                ),
                id="main_tabs"
            )
        )
    )
)

# --- サーバーロジック定義 ---

def server(input, output, session):
    
    # ★ JSから送られてきたデータを受け取るリアクティブ関数（通常の同期 def）
    @reactive.calc
    def raw_data():
        data = input.js_fetched_data()
        if not data:
            return []
        return data

    # 年度スライダーのUI生成
    @output
    @render.ui
    def year_filter_ui():
        works = raw_data()
        if not works:
            return None
        
        years = [w.get("publication_year", 2000) for w in works if w.get("publication_year") is not None]
        if not years:
            return None
        
        min_yr, max_yr = min(years), max(years)
        return ui.input_slider("year_range", "対象年の絞り込み:", min=min_yr, max=max_yr, value=[min_yr, max_yr], step=1, sep="")

    # フィルタリング済みデータ（通常の同期 def に変更）
    @reactive.calc
    def filtered_works():
        works = raw_data()
        if not works:
            return []
        
        try:
            yr_range = input.year_range()
        except Exception:
            return works
            
        if yr_range:
            works = [w for w in works if w.get("publication_year") and yr_range[0] <= w.get("publication_year") <= yr_range[1]]
        return works

    # 1. サマリー指標の計算
    @output
    @render.text
    def total_papers():
        return str(len(filtered_works()))

    @output
    @render.text
    def total_citations():
        return str(sum(w.get("cited_by_count", 0) for w in filtered_works()))

    @output
    @render.text
    def h_index():
        cites = [w.get("cited_by_count", 0) for w in filtered_works()]
        return str(calc_h_index(cites))

    # --- モーダル解説ダイアログを表示する処理 ---
    def show_help(title, text):
        m = ui.modal(
            ui.p(text),
            title=title,
            easy_close=True,
            footer=ui.modal_button("閉じる")
        )
        ui.modal_show(m)

    @reactive.effect
    @reactive.event(input.help_time_series)
    def _(): show_help("年別 研究業績推移", "年ごとの論文執筆数（棒グラフ）と、被引用数の推移（折れ線グラフ）を表示します。")

    @reactive.effect
    @reactive.event(input.help_coauthors)
    def _(): show_help("主要な共著者 Top 10", "この著者と共同で執筆した回数が最も多い共著者上位10名を表示します。")

    @reactive.effect
    @reactive.event(input.help_keywords)
    def _(): show_help("主要研究キーワード構成 (Top 10)", "論文メタデータから抽出された主要キーワード上位10件の構成比率です。")

    @reactive.effect
    @reactive.event(input.help_topics)
    def _(): show_help("主要研究トピック構成 (Top 10)", "OpenAlexが自動分類した研究領域・トピックの構成比率Top10です。")

    @reactive.effect
    @reactive.event(input.help_journals)
    def _(): show_help("主要掲載ジャーナル Top 10", "論文が多く掲載されているジャーナルや会議体の上位10件です。")

    @reactive.effect
    @reactive.event(input.help_network)
    def _(): show_help("自著論文間の引用ネットワーク", "自身の過去の論文をどの程度参照・発展させているかを可視化します。")

    @reactive.effect
    @reactive.event(input.help_paper_list)
    def _(): show_help("ネットワーク掲載 論文一覧", "ネットワークに含まれる論文の一覧です。選択した論文がオレンジ色に強調されます。")

    # --- グラフ描画（PyEcharts 統合） ---

    @output
    @render.ui
    def time_series_plot():
        works = filtered_works()
        if not works: return None
        
        yearly_papers = collections.defaultdict(int)
        yearly_citations = collections.defaultdict(int)
        
        for w in works:
            yr = w.get("publication_year")
            if yr:
                yearly_papers[yr] += 1
                yearly_citations[yr] += w.get("cited_by_count", 0)
                
        if not yearly_papers: return None
        
        sorted_years = sorted(list(yearly_papers.keys()))
        x_data = [str(yr) for yr in sorted_years]
        papers_y = [yearly_papers[yr] for yr in sorted_years]
        citations_y = [yearly_citations[yr] for yr in sorted_years]
        
        bar = (Bar(init_opts=opts.InitOpts(width="100%", height="360px"))
               .add_xaxis(x_data)
               .add_yaxis("論文数", papers_y, yaxis_index=0, color="#4c78a8")
               .extend_axis(yaxis=opts.AxisOpts(name="被引用数", type_="value"))
               .set_global_opts(
                   yaxis_opts=opts.AxisOpts(name="論文数"),
                   tooltip_opts=opts.TooltipOpts(trigger="axis"),
                   legend_opts=opts.LegendOpts(pos_right="10")
               ))
        
        line = (Line()
                .add_xaxis(x_data)
                .add_yaxis("被引用数", citations_y, yaxis_index=1, color="#e15759", label_opts=opts.LabelOpts(is_show=False)))
        
        bar.overlap(line)
        return ui.HTML(bar.render_embed())

    @output
    @render.ui
    def topic_pie_plot():
        works = filtered_works()
        if not works: return None
        
        topic_counts = collections.Counter()
        for w in works:
            for t in w.get("topics", []):
                if t.get("display_name"):
                    topic_counts[t["display_name"]] += 1
                    
        if not topic_counts: return None
        top10 = topic_counts.most_common(10)
        
        pie = (Pie(init_opts=opts.InitOpts(width="100%", height="360px"))
               .add("", [list(item) for item in top10], radius=["40%", "70%"])
               .set_global_opts(legend_opts=opts.LegendOpts(is_show=False), tooltip_opts=opts.TooltipOpts(trigger="item")))
        return ui.HTML(pie.render_embed())

    @output
    @render.ui
    def keyword_pie_plot():
        works = filtered_works()
        if not works: return None
        
        kw_counts = collections.Counter()
        for w in works:
            for k in w.get("keywords", []):
                if k.get("display_name"):
                    kw_counts[k["display_name"]] += 1
                    
        if not kw_counts: return ui.p("キーワードがありません")
        top10 = kw_counts.most_common(10)
        
        pie = (Pie(init_opts=opts.InitOpts(width="100%", height="360px"))
               .add("", [list(item) for item in top10], radius=["40%", "70%"])
               .set_global_opts(legend_opts=opts.LegendOpts(is_show=False), 
                   tooltip_opts=opts.TooltipOpts(trigger="item")
               ))
        return ui.HTML(pie.render_embed())

    # 4. ジャーナル Top 10 Table
    @output
    @render.ui
    def journal_table():
        works = filtered_works()
        if not works: return None
        
        journal_counts = collections.Counter()
        for w in works:
            src = w.get("primary_location", {}).get("source")
            if src and src.get("display_name") and src["display_name"] != "Unknown / Preprints":
                journal_counts[src["display_name"]] += 1
                
        if not journal_counts: return None
        top10 = journal_counts.most_common(10)
        
        rows = []
        for name, count in top10:
            rows.append(ui.tags.tr(ui.tags.td(name), ui.tags.td(str(count), style="text-align: right;")))
            
        return ui.tags.table(
            ui.tags.thead(ui.tags.tr(ui.tags.th("ジャーナル名"), ui.tags.th("論文数", style="text-align: right;"))),
            ui.tags.tbody(*rows)
        )

    # 5. 共著者 Top 10 Table
    @output
    @render.ui
    def coauthor_table():
        works = filtered_works()
        if not works: return None
        
        clean_id = re.sub(r".*openalex\\.org/|.*authors/", "", input.author_id())
        coauthor_counts = collections.Counter()
        for w in works:
            for auth in w.get("authorships", []):
                a_info = auth.get("author", {})
                a_id = re.sub(r".*authors/", "", a_info.get("id", ""))
                a_name = a_info.get("display_name", "")
                if a_id != clean_id and a_name:
                    coauthor_counts[a_name] += 1
                    
        if not coauthor_counts: return None
        top10 = coauthor_counts.most_common(10)
        
        rows = []
        for name, count in top10:
            rows.append(ui.tags.tr(ui.tags.td(name), ui.tags.td(str(count), style="text-align: right;")))
            
        return ui.tags.table(
            ui.tags.thead(ui.tags.tr(ui.tags.th("共著者名"), ui.tags.th("共著論文数", style="text-align: right;"))),
            ui.tags.tbody(*rows)
        )

    # ネットワークデータ集計用の共通リアクティブ
    @reactive.calc
    def network_data():
        works = filtered_works()
        if not works: return None
        
        paper_ids = [re.sub(r".*openalex\\.org/", "", w.get("id", "")) for w in works]
        titles = [w.get("title", "Untitled") for w in works]
        years = [str(w.get("publication_year", "不明")) for w in works]
        total_citations = [w.get("cited_by_count", 0) for w in works]
        
        edges = []
        paper_ids_set = set(paper_ids)
        for w in works:
            source_id = re.sub(r".*openalex\\.org/", "", w.get("id", ""))
            refs = [re.sub(r".*openalex\\.org/", "", r) for r in w.get("referenced_works", [])]
            valid_refs = set(refs).intersection(paper_ids_set)
            for r_id in valid_refs:
                edges.append({"source": source_id, "target": r_id})
                
        if not edges: return None
        
        target_counts = collections.Counter([e["target"] for e in edges])
        self_cited_counts = [target_counts[pid] for pid in paper_ids]
        
        return {
            "paper_ids": paper_ids,
            "titles": titles,
            "years": years,
            "total_citations": total_citations,
            "self_cited_counts": self_cited_counts,
            "edges": edges
        }

    # 右側UI: ネットワーク内論文セレクター
    @output
    @render.ui
    def network_paper_selector():
        net = network_data()
        if not net:
            return ui.p("引用関係のある論文はありません。")
            
        items = []
        for i in range(len(net["paper_ids"])):
            items.append({
                "id": net["paper_ids"][i],
                "title": net["titles"][i],
                "year": net["years"][i],
                "count": net["self_cited_counts"][i]
            })
        items.sort(key=lambda x: x["count"], reverse=True)
        
        choices = {"none": "選択解除"}
        for item in items:
            choices[item["id"]] = f"[{item['year']}年] {item['title'][:40]}... (自著被引用:{item['count']}回)"
            
        return ui.input_radio_buttons("selected_net_paper", None, choices, selected="none")

    # 6. 論文間の引用ネットワーク
    @output
    @render.ui
    def paper_network_plot():
        net = network_data()
        if not net:
            return ui.p("※取得データ内に論文同士の引用関係が見つかりませんでした")
            
        selected_id = "none"
        try:
            selected_id = input.selected_net_paper()
        except Exception:
            pass
            
        nodes = []
        min_sc = min(net["self_cited_counts"]) if net["self_cited_counts"] else 0
        max_sc = max(net["self_cited_counts"]) if net["self_cited_counts"] else 1
        
        for i, pid in enumerate(net["paper_ids"]):
            is_sel = (pid == selected_id)
            sc = net["self_cited_counts"][i]
            
            size = 15 + ((sc - min_sc) / (max_sc - min_sc + 1e-5)) * 30
            if is_sel: size += 6
            
            nodes.append({
                "name": pid,
                "value": sc,
                "symbolSize": int(size),
                "label": {"show": False},
                "itemStyle": {
                    "color": "#ff7f0e" if is_sel else "rgba(144, 202, 249, 0.75)",
                    "borderColor": "#d62728" if is_sel else "#2b5c8f",
                    "borderWidth": 3 if is_sel else 1.5
                },
                "tooltip": f"<b>{net['titles'][i]}</b><br/>年: {net['years'][i]}<br/>自著内被引用: {sc}回<br/>総被引用: {net['total_citations'][i]}回"
            })
            
        links = [{"source": e["source"], "target": e["target"]} for e in net["edges"]]
        
        g = (Graph(init_opts=opts.InitOpts(width="100%", height="360px"))
             .add("", nodes, links, layout="force", repulsion=400, edge_symbol=["none", "arrow"], edge_symbol_size=8,
                  linestyle_opts=opts.LineStyleOpts(color="#aaaaaa", opacity=0.6, width=1))
             .set_global_opts(tooltip_opts=opts.TooltipOpts(trigger="item")))
             
        return ui.HTML(g.render_embed())


app = App(app_ui, server)
