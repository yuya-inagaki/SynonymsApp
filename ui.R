library(shiny)

shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h2("類義語解析App"),
        tags$hr(),
        textAreaInput("text", "解析対象のテキスト", "テキストを入力", width="100%", height="200px"),
        tags$hr(),
        h2("実行"),
        p("解析を開始するには実行するをクリックしてください"),
        verbatimTextOutput("value"),
        actionButton("submit", "この文章の解析を実行する"),
        tags$hr(),
        p("Created by yuya-inagaki 2020"),
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
          tabPanel("単語一覧",
            tags$br(),
            p("文章に出現する単語を出現数順に表示します(最大100個)"),
            tableOutput('words_list')
          ),
          tabPanel("解析結果", tableOutput("result"))
        )
      )
    )
  )
)