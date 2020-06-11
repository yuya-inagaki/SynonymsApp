library(shiny)
library(shinyjs)
library(DT)

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
        p("yuya-inagaki 2020"),
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
          tabPanel("単語一覧",
            tags$br(),
            DT::dataTableOutput('words_list')
          ),
          tabPanel("解析結果",
            tags$br(),
            DT::dataTableOutput("result")
          )
        )
      )
    )
  )
)