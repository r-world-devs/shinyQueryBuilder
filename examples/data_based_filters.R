pkgload::load_all()
library(shiny)
library(dplyr)
library(bslib)

ui <- page_fluid(
  title = "Data Based Filters Demo",
  card(
    card_header("queryBuilder"),
    queryBuilderInput("iris_query", genQueryFilters(iris)),
    verbatimTextOutput("query_expr"),
    tableOutput("filtered_data")
  )
)

server <- function(input, output, session) {

  query <- reactive({
    req(input$iris_query)
    print(input$iris_query)
    queryToExpr(input$iris_query)
  })

  output$query_expr <- renderPrint({
    print(query())
  })

  output$filtered_data <- renderTable({
    iris %>%
      dplyr::filter(!!query())
  })
}

shinyApp(ui, server)

