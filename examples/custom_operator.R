pkgload::load_all()
library(shiny)
library(dplyr)
library(bslib)

setQueryOperators(
  exclude = queryOperator(`!=`)
)
mapOperator("exclude", "basic", 1, FALSE, apply_to = c("character", "factor"))

listMappedOperators("character")

ui <- page_fluid(
  title = "Custom Operator Demo",
  card(
    card_header("queryBuilder"),
    queryBuilderInput(
      "iris_query",
      filters = genQueryFilters(iris, settings = list(Species = list(operators = c("in", "exclude"), multiple = TRUE)))
    ),
    verbatimTextOutput("query_expr"),
    tableOutput("filtered_data")
  )
)

server <- function(input, output, session) {

  query <- reactive({
    req(input$iris_query)
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
