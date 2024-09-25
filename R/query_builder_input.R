#' Generate Shiny Query Widget
#'
#' @param inputId The input slot that will be used to access the value.
#' @param filters (required) List of available filters in the builder. See \link{queryFilter}.
#' @param plugins Character vector storing plugins to use. Only 'unique-filter' is supported.
#'   See \url{https://querybuilder.js.org/plugins.html}.
#' @param rules Initial set of rules set with `queryBuilder` package.
#'   See \link[queryBuilder]{queryGroup} and \link[queryBuilder]{queryRule}.
#'   For `queryRule`, `shinyQueryBuilder` accepts an extra argument `flags`, that consists of four logical elements:
#'   `filter_readonly`, `operator_readonly`, `value_readonly` and `no_delete`.
#'   These options prevent from changing the rule inputs and removing the rule in the controller.
#'   For `queryGroup`, `shinyQueryBuilder` accepts an extra argument `flags`, that consists of four logical elements:
#'   `condition_readonly`, `no_add_rule`, `no_add_group` and `no_delete`.
#'   These options allow to disable corresponding group management options.
#' @param operators Vector of operator names that should be limited in the input.
#'   Leave \code{NULL} to allow all of the configured filters.
#' @param optgroups Named list. Configuration of labels for filters and operators.
#'   List names should consists of 'optgroup' ids, whereas values, the desired labels to be displayed.
#' @param default_filter Character string. The id of the default filter chosen for any new rule.
#' @param sort_filters Set to `TRUE` to sort filters alphabetically.
#' @param allow_add_rules Logical. Should adding new rules be enabled.
#' @param allow_rm_rules Logical. Should removing rules be enabled.
#' @param allow_groups Logical or integer. Number of allowed nested groups. TRUE for no limit.
#' @param allow_rm_groups Logical. Should removing groups be enabled.
#' @param allow_empty Logical. No error will be thrown if the builder is entirely empty.
#' @param display_errors Logical (`TRUE`, default). When an error occurs on a rule, display an icon with a tooltip
#'   explaining the error.
#' @param conditions Character vector. Array of available group conditions.
#'   In order to create custom condition check \link[queryBuilder]{setQueryConditions}.
#' @param default_condition Character string. Default active condition selected for each new group.
#' @param inputs_separator Character string that will be used to separate multiple input controllers for operator
#'   values (for operators with `nb_inputs > 1`). Default is ','.
#' @param display_empty_filter Logical. Add an empty option with `select_placeholder` string to the filter dropdowns.
#'   If the empty filter is disabled and no default_filter is defined, the first filter will be loaded when
#'   adding a rule.
#' @param select_placeholder Character string. An option that can be chosen to select empty filter.
#' @param lang Nested named list providing language translations for selected controller labels.
#'   See \url{https://github.com/mistic100/jQuery-QueryBuilder/blob/dev/src/i18n/en.json} for the required structure,
#'   or load one of the existing files included at
#'   \url{https://github.com/mistic100/jQuery-QueryBuilder/tree/dev/src/i18n}.
#' @param plugins List of plugins names used for the widget. See \url{https://querybuilder.js.org/plugins.html}.
#' @param .queryBuilderConfig R6 object of class 'queryBuilderConfig' storing queryOperators.
#'   See \link[queryBuilder]{query-operator}.
#'
#' @examples
#'
#' ui <- shiny::fluidPage(
#'   queryBuilderInput(
#'     "qb",
#'     filters = list(
#'       queryFilter(
#'         "Species", type = "character", operators = c("in", "equal"),
#'         values = levels(iris$Species), multiple = TRUE,
#'         optgroup = "char_fields"
#'       ),
#'       queryFilter(
#'         "Sepal.Length", type = "numeric", 
#'         values = range(iris$Sepal.Length), optgroup = "num_fields"
#'       )
#'     ),
#'     rules = queryGroup(
#'       condition = "AND",
#'       queryRule("Species", "equal", "setosa", flags = list(no_delete = TRUE)),
#'       queryRule("Sepal.Length", "between", c(5, 7))
#'     ),
#'     optgroups = list(num_fields = "Numerical fields", char_fields = "Character fields")
#'   ),
#'   shiny::verbatimTextOutput("expr")
#' )
#' server <- function(input, output, session) {}
#'
#' if (interactive()) {
#'   shiny::runApp(ui, server)
#' }
#'
#' @return Nested list of `shiny.tag` objects, defining html structure of the input,
#' or no value in case of usage of `updateQueryBuilderInput` method.
#' @export
queryBuilderInput <- function(inputId,
                              filters, rules = list(), operators = NULL, optgroups, default_filter,
                              sort_filters = FALSE, allow_groups = TRUE, allow_rm_groups = TRUE, allow_empty = TRUE,
                              display_errors = TRUE, conditions = c("AND", "OR"), default_condition = "AND",
                              inputs_separator = " , ", display_empty_filter = TRUE,
                              select_placeholder = "------", lang, plugins,
                              allow_add_rules = TRUE, allow_rm_rules = TRUE,
                              .queryBuilderConfig = queryBuilder::queryBuilderConfig) {

  x <- environment() %>%
    as.list() %>%
    purrr::keep(~ !is.symbol(.x))

  if (length(x$plugins) > 0) {
    x$plugins <- as.list(x$plugins)
  }

  if (length(x$rules) == 0) {
    x$rules <- list()
  }
  operators_defs <- .queryBuilderConfig$get_from_private("gui_operators")
  if (!is.null(operators)) {
    operators_defs <- operators_defs[operators]
  }
  x$operators <- operators_defs %>%
    purrr::map(function(x) x[c("type", "optgroup", "nb_inputs", "multiple", "apply_to")]) %>%
    unname()
  x$inputId <- NULL
  x$new_rules <- NULL
  if (is.logical(x$allow_groups)) {
    x$allow_groups <- NULL # disabling/enabling solved by custom class as setOptions method seems to not work
  }
  empty_filters <- FALSE

  if (length(x$filters) == 0) {
    if (length(rules) > 0) {
      stop("Rules might be passed only when filters are defined.")
    }
    # JS framework doesn't allow to initialize builder with no filters, this is a workaround
    empty_filters <- TRUE
    no_filter_operator <- "in"
    if (!"in" %in% names(operators_defs)) {
      no_filter_operator <- names(operators_defs)[1]
    }
    x$filters <- list(
      queryFilter(
        id = "no_data", label = "no_data", type = "string", values = c("no_data", "data_no"),
        placeholder = "no_data", operators = no_filter_operator, input = "select",
        value_separator = " <-> ", multiple = TRUE
      )
    )
    x$rules <- queryBuilder::queryGroup(
      condition = names(.queryBuilderConfig$get_from_private("conditions"))[1]
    )
  }
  manage_classes <- c("disable-rm-rules", "disable-add-rules", "disable-rm-groups", "disable-add-groups")[
    c(!allow_rm_rules, !allow_add_rules, !allow_rm_groups, !allow_groups)
  ]
  empty_filters_class <- ""
  if (empty_filters) {
    empty_filters_class <- "no-filters"
  }

  if (!missing(lang)) {
    x$regional <- lang
  }

  htmltools::attachDependencies(
    shiny::div(
      id = inputId,
      class = paste(c("shiny-querybuilder", manage_classes, empty_filters_class)),
      `data-config` = jsonlite::toJSON(x, auto_unbox = TRUE, json_verbatim = TRUE)
    ),
    list(
      htmltools::htmlDependency(
        name = "shinyQueryBuilder",
        version = utils::packageVersion("shinyQueryBuilder"),
        package = "shinyQueryBuilder",
        src = "assets",
        script = "binding.js",
        stylesheet = "styles.css"
      ),
      htmltools::htmlDependency(
        name = "jQuery-QueryBuilder",
        version = "3.0.0",
        package = "shinyQueryBuilder",
        src = "assets/lib/jQuery-QueryBuilder",
        script = "query-builder.standalone.js",
        stylesheet = "query-builder.default.css"
      ),
      htmltools::htmlDependency(
        name = "moment",
        version = "2.29.1",
        package = "shinyQueryBuilder",
        src = "assets/lib/moment",
        script = "moment.js"
      ),
      `%:::%`("shiny", "selectizeDependency")()
    )
  )
}

#' @param session Shiny session object.
#'
#' @rdname queryBuilderInput
#' @export
updateQueryBuilderInput <- function(session, inputId, rules, filters,
                                    allow_add_rules, allow_rm_rules, allow_groups, allow_rm_groups) {
  x <- environment() %>%
    as.list() %>%
    purrr::keep(~ !is.symbol(.x))
  x$session <- NULL
  x$inputId <- NULL

  session$sendInputMessage(inputId, x)
}

rule_to_num <- function(rule) {
  if (rule$type %in% c("integer", "double")) {
    rule$value <- as.numeric(rule$value)
  }
  return(rule$value)
}

convert_numeric <- function(rules) {

  if (!is.null(rules$condition)) {
    rules$rules <- purrr::modify(rules$rules, convert_numeric)
  } else {
    rules$value <- rule_to_num(rules)
  }

  return(rules)
}

input_handler <- function(x, shinysession, name) {
  if (length(x) == 0) return(NULL)
  x <- convert_numeric(x)
  return(x)
}

#' Store JS definition as character string
#' @param x Character string containing valid JS object e.g. function
#' @return An object of class 'json' storing the provided character string.
#' @export
js <- function(x) {
  class(x) <- "json"
  return(x)
}
