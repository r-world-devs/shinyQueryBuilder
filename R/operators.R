#' Configure available user interface operators
#'
#' @param name Name of the operator to be mapped.
#' @param optgroup Character string ("basic" default).
#'   Operators with the same `optgroup` will be presented within a separate group in the operators dropdown.
#' @param apply_to Precise what field types (classes) should the operator be available to.
#'   When \code{operators} is not defined for \link{queryFilter}, all of the operators matching `queryFilter`
#'   type will be available in the operators dropdown.
#'   Possible values are 'character', 'factor', 'integer', 'numeric', 'POSIXct', 'Date' and 'logical'.
#' @param nb_inputs  Integer. The number of inputs displayed. See 'Details' for more information.
#' @param multiple  Logical. Inform the builder if operator can accept multiple values for associated inputs.
#'   In order to enable multiple values for specific input, set `multiple = TRUE` when creating \link{queryFilter}s.
#' @param .queryBuilderConfig R6 object of class 'queryBuilderConfig' storing queryOperators.
#'   See \link[queryBuilder]{query-operator}.
#'
#' @details
#' When configuring a single query rule, user needs to precise three values in 'queryBuilderInput' interface:
#' \itemize{
#'   \item{1. field - Name of the field that can be interpreted as a filtered column name. Selected with dropdown.}
#'   \item{2. operator -
#'     Name of the operator to be applied to the field. Selected with dropdown.
#'   }
#'   \item{3. operator value(s) -
#'     Value(s) that narrows down the operator definition.
#'     Depending on the chosen operator, such input can be take through various kind of **input controllers**.
#'   }
#' }
#'
#' \if{html}{\figure{sqb_inputs.png}{Inputs explained}}
#'
#' More detailed configuration for operators linked to specific fields as long as **input controllers** for taking
#' operator values should be set with \link{queryFilter}.
#'
#' \code{mapOperator} is responsible to establish connection between user interface operators
#' and \link[queryBuilder]{queryOperator}, that are responsible to convert user input to a valid R-expression.
#' The provided configuration allows to shape what **input controllers** should be used to allow users providing
#' operators' value(s).
#'
#' Parameter `multiple` precises whether \link{queryBuilderInput} should allow to provide multiple values for each
#' input controller.
#' When input controller accepts more than one value and user provides them, in case of `multiple = FALSE`,
#' 'queryBuilderInput' will alert about it and won't send any values to application server.
#'
#' \if{html}{\figure{sqb_violated.png}{Validation}}
#'
#' Please remember `multiple = TRUE`, doesn't mean the associated input controller will automatically accept
#' multiple values, this needs to be separately set for each \link{queryFilter}, that is responsible for input
#' controllers configuration.
#'
#' Parameter `nb_inputs` informs how many input controllers should be rendered to take operator value(s).
#'
#' A good practice is to configure your operators the following way:
#' \itemize{
#'   \item{\code{nb_inputs = 0} - 
#'     Operator associated function doesn't require any value, e.g. 'is_null' or 'is_empty' that only require
#'     'field' name.
#'    }
#'   \item{\code{nb_inputs = n, multiple = FALSE} - 
#'     Operator associated function requires exactly `n` values, e.g. `n=2` for 'between' that requires
#'     lower and upper bound to precise it.
#'     As a result `n` separate input controllers will be rendered, each taking a single value.
#'   }
#'   \item{\code{nb_inputs = 1, multiple = TRUE} - 
#'     Operator associated function accepts dynamic number of values, e.g. 'in'.
#'     As a result one single input controller will be rendered, and operator will allow it to have multiple values set.
#'   }
#' }
#'
#' @examples
#'
#' # Set backend operator
#' in_closed_range <- function(field, bounds) {
#'   field >= bounds[1] & field <= bounds[2]
#' }
#' queryBuilder::setQueryOperators(
#'   within = queryBuilder::queryOperator(in_closed_range)
#' )
#'
#' queryBuilder::listQueryOperators()
#'
#' # Map backend operator to the user interface one
#'
#' mapOperator(
#'   name = "within",
#'   nb_inputs = 2, # take value with 2 input controllers
#'   multiple = FALSE, # verify if only single value per controller is set
#'   apply_to = c("numeric", "Date", "logical") # apply operator to selected field types
#' )
#'
#' listMappedOperators()
#'
#' filters = list(
#'   queryFilter(
#'     "Sepal.Length", operators = c("within", "less"), 
#'     type = "numeric", values = range(iris$Sepal.Length)
#'   ),
#'   # no operators set, means take all for "character"
#'   queryFilter("Species", type = "character", values = levels(iris$Species))
#' )
#'
#' ui <- shiny::fluidPage(
#'   title = title,
#'   queryBuilderInput(
#'     "qb",
#'     filters = filters
#'   ),
#'   shiny::verbatimTextOutput("expr")
#' )
#'
#' server <- function(input, output, session) {
#'   output$expr <- shiny::renderPrint({
#'     print(queryToExpr(input$qb))
#'   })
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
#'
#' @name query-operators
#' @return No return value, called for side effects.
#' @export
mapOperator <- function(name, apply_to, optgroup = "basic", nb_inputs = 1, multiple = FALSE,
                        .queryBuilderConfig = queryBuilder::queryBuilderConfig) {
  operators <- .queryBuilderConfig$get_from_private("operators")
  if (!name %in% names(operators)) {
    stop(glue::glue(
      "Operator {sQuote(name)} is not defined.",
      "Use {sQuote('queryBuilder::setQueryOperator')} to create it."
    ))
  }
  gui_operators <- .queryBuilderConfig$get_from_private("gui_operators")
  if (is.null(gui_operators)) {
    gui_operators <- list()
  }
  new_operator <- stats::setNames(
    list(
      list(
        type = name,
        optgroup = optgroup,
        nb_inputs = nb_inputs,
        multiple = multiple,
        apply_to_r_class = apply_to,
        apply_to = r_to_js_opt_type(apply_to)
      )
    ),
    name
  )
  .queryBuilderConfig$set_to_private(
    "gui_operators",
    utils::modifyList(gui_operators, new_operator)
  )
  return(invisible(NULL))
}

#' Convert R class to a valid operator JS type
#'
#' @param apply_to Character value - R class to be converted.
r_to_js_opt_type <- function(apply_to) {
  unique(
    purrr::map_chr(input_type_mappers[apply_to], "opt_type")
  )
}

input_type_mappers <- list(
  character = list(filter_type = "string", input = "select", opt_type = "string"),
  factor = list(filter_type = "string", input = "select", opt_type = "string"),
  integer = list(filter_type = "integer", input = "number", opt_type = "number"),
  numeric = list(filter_type = "double", input = "number", opt_type = "number"),
  POSIXct = list(filter_type = "datetime", input = "text", opt_type = "datetime"),
  Date = list(filter_type = "date", input = "text", opt_type = "datetime"),
  logical = list(filter_type = "boolean", input = "checkbox", opt_type = "boolean")
)

#' @export
#' @param r_class Optional R class to list operators assigned to it.
#'   When skipped all the mapped operators will be summed up.
#' @param print Should the operators summary be printed?
#' @return List of operators registered within \code{.queryBuilderConfig}.
#' @rdname query-operators
listMappedOperators <- function(r_class, print = TRUE, .queryBuilderConfig = queryBuilder::queryBuilderConfig) {
  mapped_operators <- .queryBuilderConfig$get_from_private("gui_operators")
  backend_operators <- .queryBuilderConfig$get_from_private("operators")
  force(backend_operators)
  if (!missing(r_class)) {
    mapped_operators <- mapped_operators %>% purrr::keep(~ r_class %in% .x$apply_to_r_class)
  }
  output_fun <- I
  if (print) {
    output_fun <- invisible
    mapped_operators %>%
      purrr::imap(
        ~cat(
          .y, ": \n",
          "  optgroup: ", .x$optgroup, "\n",
          "  nb_inputs: ", .x$nb_inputs, "\n",
          "  multiple: ", .x$multiple, "\n",
          "  apply_to: ", paste(.x$apply_to_r_class, collapse = ", "), "\n",
          "  R method: ", deparse(backend_operators[[.y]]), "\n",
          sep = ""
        )
      )
  }

  return(output_fun(mapped_operators))
}
