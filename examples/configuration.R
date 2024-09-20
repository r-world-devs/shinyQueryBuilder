library(shiny)
library(bslib)
pkgload::load_all()

ui_out <- function(title, ..., update = FALSE) {
  page_fluid(
    title = title,
    if (update) actionButton("update", "Update"),
    queryBuilderInput(
      "qb", 
      ...
    ),
    shiny::verbatimTextOutput("expr")
  )
}

standard_server <- function(input, output, session) {
  observeEvent(input$qb, {
    print("rendered")
  })
  output$expr <- renderPrint({
    print(queryToExpr(input$qb))
  })
}

server_fun <- function(...) {
  function(input, output, session) {
    observeEvent(input$qb, {
      print("rendered")
    })
    output$expr <- renderPrint({
      print(queryToExpr(input$qb))
    })
    observeEvent(input$update, {
      updateQueryBuilderInput(session, "qb", ...)
    })
  }
}

run_app <- function(ui, server) {
  shinyApp(ui, server, options = list("launch.browser" = TRUE))
}

ui <- ui_out(
  title = "Filters with all the operators",
  filters = list(
    queryFilter("Species", type = "character", values = levels(iris$Species), multiple = TRUE),
    queryFilter("Sepal.Length", type = "numeric", values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  )
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "Filters with limited operators (for each at least two)",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), 
      values = levels(iris$Species),
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  )
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "Filters with limited operators (one filter with single operator)",
  filters = list(
    queryFilter("Species", type = "character", operators = c("equal", "in"), values = levels(iris$Species)),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  )
)

run_app(ui, standard_server)

ui <- ui_out(
  title = "Another set of operators - selected needs to be set by rule as well",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("begins_with", "in"), 
      values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", c("setosa", "virginica")),
    queryRule("Sepal.Length", "between", c(5, 7))
  )
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "Set of operators including 'equal' but not first",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), 
      values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  )
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "No rules and default_operator for initialized filter-based rules",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("equal", "in"), values = levels(iris$Species),
      default_operator = "in"
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = list()
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "No rules (as list()) and no default_operator for initialized filter-based rules - 1st should be taken",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = list()
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "No rules (as NULL) and no default_operator for initialized filter-based rules - 1st should be taken",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = NULL
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "No filters (defined as NULL) - creating rules will be allowed after adding filters by update",
  filters = NULL,
  rules = list()
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "No filters (defined as list()) - creating rules will be allowed after adding filters by update",
  filters = list(),
  rules = list()
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "Allow max 1 nested group",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species),
      unique = TRUE
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  ),
  allow_groups = 1
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "No groups allowed",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species),
      unique = TRUE
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  ),
  allow_groups = FALSE
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "No new rules allowed",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species),
      unique = TRUE
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  ),
  allow_add_rules = FALSE
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "Removing rules is not allowed",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species),
      unique = TRUE
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  ),
  allow_rm_rules = FALSE
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "Removing groupes is not allowed",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species),
      unique = TRUE
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  ),
  allow_rm_groups = FALSE
)
run_app(ui, standard_server)

ui <- ui_out(
  title = "It's allowed to configure empty filters when rules are empty",
  filters = NULL,
  rules = NULL
)
run_app(ui, standard_server)

tryCatch(ui_out(
  title = "It's not allowed to configure rules without filters",
  filters = NULL,
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "equal", 5)
  )
), error = function(e) print(e$message))

# Update method

ui <- ui_out(
  title = "No initial rules. Rules added by update method.",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = list(),
  update = TRUE
)

server <- server_fun(
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  )
)

run_app(ui, server)

ui <- ui_out(
  title = "Initial rules that are overwritten by update method.",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", "setosa"),
    queryRule("Sepal.Length", "less", 5)
  ),
  update = TRUE
)

server <- server_fun(
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  )
)

run_app(ui, server)

ui <- ui_out(
  title = "Update method prevents adding new rules",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", "setosa"),
    queryRule("Sepal.Length", "less", 5)
  ),
  update = TRUE
)

server <- server_fun(
  allow_add_rules = FALSE
)

run_app(ui, server)

ui <- ui_out(
  title = "Update method allows to add new rules",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", "setosa"),
    queryRule("Sepal.Length", "less", 5)
  ),
  allow_add_rules = FALSE,
  update = TRUE
)

server <- server_fun(
  allow_add_rules = TRUE
)

run_app(ui, server)

ui <- ui_out(
  title = "Update method allows setting groups",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", "setosa"),
    queryRule("Sepal.Length", "less", 5)
  ),
  allow_groups = FALSE,
  update = TRUE
)

server <- server_fun(
  allow_groups = TRUE
)

run_app(ui, server)

ui <- ui_out(
  title = "Update method prevents from setting groups",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", "setosa"),
    queryRule("Sepal.Length", "less", 5)
  ),
  allow_groups = TRUE,
  update = TRUE
)

server <- server_fun(
  allow_groups = FALSE
)

run_app(ui, server)

ui <- ui_out(
  title = "Update method prevents from removing groups",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", "setosa"),
    queryRule("Sepal.Length", "less", 5)
  ),
  update = TRUE
)

server <- server_fun(
  allow_rm_groups = FALSE
)

run_app(ui, server)

ui <- ui_out(
  title = "Update method allows to remove groups",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", "setosa"),
    queryRule("Sepal.Length", "less", 5)
  ),
  allow_rm_groups = FALSE,
  update = TRUE
)

server <- server_fun(
  allow_rm_groups = TRUE
)

run_app(ui, server)

ui <- ui_out(
  title = "Update method prevents from removing rules",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", "setosa"),
    queryRule("Sepal.Length", "less", 5)
  ),
  update = TRUE
)

server <- server_fun(
  allow_rm_rules = FALSE
)

run_app(ui, server)

ui <- ui_out(
  title = "Update method allows to remove rules",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", "setosa"),
    queryRule("Sepal.Length", "less", 5)
  ),
  allow_rm_rules = FALSE,
  update = TRUE
)

server <- server_fun(
  allow_rm_rules = TRUE
)

run_app(ui, server)

ui <- ui_out(
  title = "Update method allows to modify filters (unmatching rules will be removed)",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", "setosa"),
    queryRule("Sepal.Length", "less", 5)
  ),
  update = TRUE
)

server <- server_fun(
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "not_equal"), values = levels(iris$Species)[-1]
    ),
    queryFilter("Petal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  )
)

run_app(ui, server)

ui <- ui_out(
  title = "Update method allows to add initially nonexisting filters (rules not-initialized)",
  filters = NULL,
  rules = NULL,
  update = TRUE
)

server <- server_fun(
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species)
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  )
)

run_app(ui, server)

# Extra configuration

ui <- ui_out(
  title = "Defining filter optgroups and optgroup labels",
  filters = list(
    queryFilter("Species", type = "character", optgroup = "text_fields", values = levels(iris$Species)),
    queryFilter("Sepal.Length", type = "numeric", optgroup = "num_fields", values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  ),
  optgroups = list(basic = "Basic", text_fields = "Text Fields", num_fields = "Numerical fields")
)
run_app(ui, server)


lang_file <- tempfile(fileext = ".json")
utils::download.file(
  "https://raw.githubusercontent.com/mistic100/jQuery-QueryBuilder/dev/src/i18n/pl.json", 
  destfile = lang_file
)

ui <- ui_out(
  title = "Modifying language works",
  filters = list(
    queryFilter("Species", type = "character", values = levels(iris$Species)),
    queryFilter("Sepal.Length", type = "numeric", values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  ),
  lang = jsonlite::fromJSON(lang_file)
)
run_app(ui, server)

ui <- ui_out(
  title = "Argument 'flags' for queryRule",
  filters = list(
    queryFilter("Species", type = "character", values = levels(iris$Species)),
    queryFilter("Sepal.Length", type = "numeric", values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule(
      "Species", "equal", "setosa", 
      flags = list(filter_readonly = TRUE, operator_readonly = TRUE, value_readonly = TRUE, no_delete = TRUE)
    ),
    queryRule("Sepal.Length", "between", c(5, 7))
  )
)
run_app(ui, server)

ui <- ui_out(
  title = "Argument 'flags' for queryGroup",
  filters = list(
    queryFilter("Species", type = "character", values = levels(iris$Species)),
    queryFilter("Sepal.Length", type = "numeric", values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7)),
    flags = list(condition_readonly = TRUE, no_add_rule = TRUE, no_add_group = TRUE, no_delete = TRUE)
  )
)
run_app(ui, server)

ui <- ui_out(
  title = "For text input `value_separator` allows to provide multiple values",
  filters = list(
    queryFilter(
      "Species", type = "character", input = "text", values = levels(iris$Species), 
      value_separator = ";", multiple = TRUE
    )
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "in", c("setosa", "versicolor"))
  )
)
run_app(ui, server)

# Validation

ui <- ui_out(
  title = "Filters with all the operators",
  filters = list(
    queryFilter(
      "BirthDate", type = "Date", validation = list(format = "YYYY-MM-DD")
    ),
    queryFilter(
      "digit", type = "numeric", 
      validation = list(
        min = 0, max = 9, step = 1,
        messages = list(
          min = "Minimum value allowed is 0", 
          max = "Maximum value allowed is 9", 
          step = "Only integers accepted"
        )
      )
    ),
    queryFilter(
      "Name", type = "character", input = "text", 
      validation = list(format = "[A-Z][a-z]+", messages = list(format = "Name should be capitalized"))
    ),
    queryFilter(
      "weekday_abbr", type = "character", input = "textarea", validation = list(min = 3, max = 3)
    ),
    queryFilter(
      "fav_letter", 
      type = "character", 
      input = "text", 
      validation = list(
        callback = js(paste0(
          "function(value, Rule) {",
          "var result = true;", 
          "is_valid = value.length == 1 && value.toUpperCase() == value;",
          "if (!is_valid) {result = 'Single capital letter allowed only'};",
          "return result;",
          "}",
          collapse = ""
        ))
      )
    )
  )
)
run_app(ui, server)

# Plugins

ui <- ui_out(
  title = "Unique filters (needs to have `plugins = c(\"unique-filter\")` set)",
  filters = list(
    queryFilter(
      "Species", type = "character", operators = c("in", "equal"), values = levels(iris$Species),
      unique = TRUE
    ),
    queryFilter("Sepal.Length", type = "numeric", operators = c("between", "less"), values = range(iris$Sepal.Length))
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  ),
  plugins = c("unique-filter")
)
run_app(ui, server)

# Auto generated filters

ui <- ui_out(
  title = "Auto generated filters based on data frame with custom settings",
  filters = genQueryFilters(
    iris,
    list(
      Species = list(operators = c("equal", "not_equal"))
    )
  ),
  rules = queryGroup(
    condition = "AND", 
    queryRule("Species", "equal", "setosa"),
    queryRule("Sepal.Length", "between", c(5, 7))
  ),
  plugins = c("unique-filter")
)
run_app(ui, server)
