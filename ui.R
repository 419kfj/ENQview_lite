# UI block　----
shinyUI(
fluidPage(
  titlePanel("ENQview_lite RDAファイルからデータフレーム選択版"),

  tabPanel(
    "基本集計",
    sidebarLayout(   # 基本集計sidebarPanel  ----
      sidebarPanel(
        # バージョン情報の表示
        helpText(paste("Version:", app_version)),

        fileInput("rda_file", "RDAファイルを選択", accept = ".rda"),
        uiOutput("rda_objects_ui"),
        uiOutput("variables_ui"),
        tags$hr(),
        uiOutput("cross_var_ui"),
        uiOutput("layer_var_ui"),
        uiOutput("dynamic_stratify_filter"), # 層化変数選択の下あたりに配置
        uiOutput("variables2_ui"),
        uiOutput("hist_var_ui")
      ),

      mainPanel( # mainPanel ----
                 tabsetPanel(
                   type = "tabs",

                   tabPanel("単変数集計", ## 単数集計 ----
                            h2("棒グラフと度数分布"),
                            plotOutput("barchart"),
                            DT::dataTableOutput("simple_table")
                   ),

                   tabPanel("2変数分析", ## ２変数分析 ----
                            h2("クロス集計（gtsummary::tbl_cross )"),
                            gt_output(outputId = "my_gt_table2"),
                            plotOutput("crosschart", width = 600, height = 600),
                            h3("χ2乗検定"),
                            verbatimTextOutput("chisq_test2")
                   ),

                   tabPanel("pairs", ## pairs ----
                            h2("GGally::pairs"),
                            plotOutput("pairs", width = 600, height = 600)
                   ),

                   tabPanel("pairs_multi", ## pairs=multi ----
                            h2("GGally::pairs 多変数"),
                            plotOutput("pairs_multi", width = 900, height = 900)
                   ),

                   tabPanel("2変数分析（層化）", ## ２変数分析（層化） ----
                            h2("クロス集計（gtsummary::tbl_cross )"),
                            gt_output(outputId = "my_gt_table"),
                            plotOutput("crosschart2", width = 900, height = 600),
                            h3("χ2乗検定"),
                            verbatimTextOutput("chisq_test")
                   ),

                   tabPanel("MA plot(Bar)", ## MA plot(Bar） ----
                            h2("MA変数集計"),
                            plotOutput("MAplot", width = 600, height = 600)
                   ),

                   tabPanel("MA plot(Dot)", ## MA plot(Dot） ----
                            h2("MA変数集計"),
                            plotOutput("MAplot_Dot", width = 600, height = 600)
                   ),

                   tabPanel("層化 MA plot", ## 層化MA plot ----
                            h2("層化MA変数集計"),
                            plotOutput("MAplot_lineDot", width = 600, height = 400),
                            plotOutput("MAplot_lineDotwarp", width = 600, height = 600)
                   ),

                   tabPanel("層化 MA plot2",## 層化MA plot２ ----
                            h2("層化MA変数集計（Legendなし）"),
                            plotOutput("MAplot_lineDot2", width = 600, height = 400),
                            plotOutput("MAplot_lineDotwarp", width = 600, height = 600)
                   ),

                   tabPanel("Grid回答 General mosaic表示",## Grid回答 General mosaic表示 ----
                            h2("Grid回答mosaic表示"),
                            plotOutput("GridAnswerG_mosaic", width = 600, height = 600),
                            plotOutput("GridAnswerG_CA", width = 700, height = 700)
                   ),

                   tabPanel("単変数check", ## 単変数check ----
                            h2("棒グラフと度数分布"),
                            plotOutput("barchart2"),
                            DT::dataTableOutput("simple_table2")
                   ),

                   tabPanel("選択変数のデータ一覧",## 選択変数のデータ一覧 ----
                            h2("データ一覧"),
                            DT::dataTableOutput("table_for_plot")
                   ),
                   tabPanel("使い方", ## 使い方 ----
                            p("アプリの詳細な使い方は、以下のリンク先をご確認ください。"),
                            a("Shinyアプリの使い方ガイド（外部サイト）",
                              href = "https://www.fujimotolabo.uk/Shiny_app_how2/",
                              target = "_blank") # 新しいタブで開く設定

                   )
                 )  # ← tabsetPanel の閉じかっこ
      )    # ← mainPanel の閉じかっこ
    )      # ← sidebarLayout の閉じかっこ
  )        # ← tabPanel("基本集計") の閉じかっこ
)          # ← fluidPage の閉じかっこ
)
