% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_builder_input.R
\name{queryBuilderInput}
\alias{queryBuilderInput}
\alias{updateQueryBuilderInput}
\title{Generate Shiny Query Widget}
\usage{
queryBuilderInput(
  inputId,
  filters,
  rules = list(),
  operators = NULL,
  optgroups,
  default_filter,
  sort_filters = FALSE,
  allow_groups = TRUE,
  allow_rm_groups = TRUE,
  allow_empty = TRUE,
  display_errors = TRUE,
  conditions = c("AND", "OR"),
  default_condition = "AND",
  inputs_separator = " , ",
  display_empty_filter = TRUE,
  select_placeholder = "------",
  lang,
  plugins,
  allow_add_rules = TRUE,
  allow_rm_rules = TRUE,
  .queryBuilderConfig = queryBuilder::queryBuilderConfig
)

updateQueryBuilderInput(
  session,
  inputId,
  rules,
  filters,
  allow_add_rules,
  allow_rm_rules,
  allow_groups,
  allow_rm_groups
)
}
\arguments{
\item{inputId}{The input slot that will be used to access the value.}

\item{filters}{(required) List of available filters in the builder. See \link{queryFilter}.}

\item{rules}{Initial set of rules set with `queryBuilder` package.
See \link[queryBuilder]{queryGroup} and \link[queryBuilder]{queryRule}.
For `queryRule`, `shinyQueryBuilder` accepts an extra argument `flags`, that consists of four logical elements:
`filter_readonly`, `operator_readonly`, `value_readonly` and `no_delete`.
These options prevent from changing the rule inputs and removing the rule in the controller.
For `queryGroup`, `shinyQueryBuilder` accepts an extra argument `flags`, that consists of four logical elements:
`condition_readonly`, `no_add_rule`, `no_add_group` and `no_delete`.
These options allow to disable corresponding group management options.}

\item{operators}{Vector of operator names that should be limited in the input.
Leave \code{NULL} to allow all of the configured filters.}

\item{optgroups}{Named list. Configuration of labels for filters and operators.
List names should consists of 'optgroup' ids, whereas values, the desired labels to be displayed.}

\item{default_filter}{Character string. The id of the default filter chosen for any new rule.}

\item{sort_filters}{Set to `TRUE` to sort filters alphabetically.}

\item{allow_groups}{Logical or integer. Number of allowed nested groups. TRUE for no limit.}

\item{allow_rm_groups}{Logical. Should removing groups be enabled.}

\item{allow_empty}{Logical. No error will be thrown if the builder is entirely empty.}

\item{display_errors}{Logical (`TRUE`, default). When an error occurs on a rule, display an icon with a tooltip
explaining the error.}

\item{conditions}{Character vector. Array of available group conditions.
In order to create custom condition check \link[queryBuilder]{setQueryConditions}.}

\item{default_condition}{Character string. Default active condition selected for each new group.}

\item{inputs_separator}{Character string that will be used to separate multiple input controllers for operator
values (for operators with `nb_inputs > 1`). Default is ','.}

\item{display_empty_filter}{Logical. Add an empty option with `select_placeholder` string to the filter dropdowns.
If the empty filter is disabled and no default_filter is defined, the first filter will be loaded when
adding a rule.}

\item{select_placeholder}{Character string. An option that can be chosen to select empty filter.}

\item{lang}{Nested named list providing language translations for selected controller labels.
See \url{https://github.com/mistic100/jQuery-QueryBuilder/blob/dev/src/i18n/en.json} for the required structure,
or load one of the existing files included at
\url{https://github.com/mistic100/jQuery-QueryBuilder/tree/dev/src/i18n}.}

\item{plugins}{List of plugins names used for the widget. See \url{https://querybuilder.js.org/plugins.html}.}

\item{allow_add_rules}{Logical. Should adding new rules be enabled.}

\item{allow_rm_rules}{Logical. Should removing rules be enabled.}

\item{.queryBuilderConfig}{R6 object of class 'queryBuilderConfig' storing queryOperators.
See \link[queryBuilder]{query-operator}.}

\item{session}{Shiny session object.}
}
\value{
Nested list of `shiny.tag` objects, defining html structure of the input,
or no value in case of usage of `updateQueryBuilderInput` method.
}
\description{
Generate Shiny Query Widget
}
\examples{

ui <- shiny::fluidPage(
  queryBuilderInput(
    "qb",
    filters = list(
      queryFilter(
        "Species", type = "character", operators = c("in", "equal"),
        values = levels(iris$Species), multiple = TRUE,
        optgroup = "char_fields"
      ),
      queryFilter(
        "Sepal.Length", type = "numeric", 
        values = range(iris$Sepal.Length), optgroup = "num_fields"
      )
    ),
    rules = queryGroup(
      condition = "AND",
      queryRule("Species", "equal", "setosa", flags = list(no_delete = TRUE)),
      queryRule("Sepal.Length", "between", c(5, 7))
    ),
    optgroups = list(num_fields = "Numerical fields", char_fields = "Character fields")
  ),
  shiny::verbatimTextOutput("expr")
)
server <- function(input, output, session) {}

if (interactive()) {
  shiny::runApp(ui, server)
}

}
