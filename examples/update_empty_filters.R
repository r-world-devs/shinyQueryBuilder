pkgload::load_all()
library(shiny)

setQueryOperators(
  exclude = queryOperator(`!=`)
)
mapOperator("exclude", "basic", 1, FALSE, apply_to = c("character", "factor"))

library(shiny)
library(bslib)

ui <- page_fluid(
  title = "Data Based Filters Demo",
  card(
    card_header("queryBuilder"),
    tags$head(tags$script("$(document).on('shiny:inputchanged', function(event) {console.log(event);})")),
    queryBuilderInput(
      "query",
      conditions = c("AND", "OR"),
      rules = NULL,
      select_placeholder = "- - - - -",
      lang = list(operators = list("exclude" = "Exclude"))
    ),
    actionButton("update", "Update")
  )
)

server <- function(input, output, session) {
  observeEvent(input$query, {
    print(queryToExpr(input$query))
  })
  observeEvent(input$update, {
    updateQueryBuilderInput(
      session, "query", 
      rules = queryGroup(
        condition = "AND",
        queryRule("book", "in", "Muminki"),
        queryGroup(
          condition = "OR",
          queryRule("book", "equal", "Hobbit"),
          queryRule("book", "in", "Muminki")
        )
      ),
      filters = list(
        queryFilter(
          id = "book", label = "Book", type = "string", values = c("Muminki", "Hobbit", "Witcher"),
          placeholder = "Choose book", operators = c("in", "equal", "exclude"), input = "select",
          value_separator = " <-> ", multiple = TRUE
        ),
        queryFilter(
          id = "amount", label = "Number", type = "integer",
          placeholder = "Choose book", operators = c("in", "equal", "greater_or_equal", "less"),
          value_separator = " <-> "
        )
      )
    )
  })
}

shinyApp(ui, server)

