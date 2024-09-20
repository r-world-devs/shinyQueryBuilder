#' Shiny Wrapper for jQuery-QueryBuilder
#'
#' @name shinyQueryBuilder-package
#' @importFrom magrittr %>%
#' @import queryBuilder
NULL

.onLoad <- function(...) {
  shiny::registerInputHandler("shinyQueryBuilder.querybuilder", input_handler, force = TRUE)
  mapOperator("equal", "basic", 1, FALSE, apply_to = c("factor", "character", "numeric", "Date", "logical"))
  mapOperator("not_equal", "basic", 1, FALSE, apply_to = c("factor", "character", "numeric", "Date", "logical"))
  mapOperator("in", "basic", 1, TRUE, apply_to = c("factor", "character", "numeric", "Date"))
  mapOperator("not_in", "basic", 1, TRUE, apply_to = c("factor", "character", "numeric", "Date"))
  mapOperator("less", "basic", 1, FALSE, apply_to = c("numeric", "Date"))
  mapOperator("less_or_equal", "basic", 1, FALSE, apply_to = c("numeric", "Date"))
  mapOperator("greater", "basic", 1, FALSE, apply_to = c("numeric", "Date"))
  mapOperator("greater_or_equal", "basic", 1, FALSE, apply_to = c("numeric", "Date"))
  mapOperator("between", "basic", 2, FALSE, apply_to = c("numeric", "Date"))
  mapOperator("not_between", "basic", 2, FALSE, apply_to = c("numeric", "Date"))
  mapOperator("begins_with", "basic", 1, FALSE, apply_to = c("factor", "character"))
  mapOperator("not_begins_with", "basic", 1, FALSE, apply_to = c("factor", "character"))
  mapOperator("contains", "basic", 1, FALSE, apply_to = c("factor", "character"))
  mapOperator("not_contains", "basic", 1, FALSE, apply_to = c("factor", "character"))
  mapOperator("ends_with", "basic", 1, FALSE, apply_to = c("factor", "character"))
  mapOperator("not_ends_with", "basic", 1, FALSE, apply_to = c("factor", "character"))
  mapOperator("is_empty", "basic", 0, FALSE, apply_to = c("factor", "character"))
  mapOperator("not_is_empty", "basic", 0, FALSE, apply_to = c("factor", "character"))
  mapOperator("is_null", "basic", 0, FALSE, apply_to = c("factor", "character", "numeric", "Date", "logical"))
  mapOperator("not_is_null", "basic", 0, FALSE, apply_to = c("factor", "character", "numeric", "Date", "logical"))
}

.onUnLoad <- function(...) {
  queryBuilder::queryBuilderConfig$set_to_private("gui_operators", NULL)
}

`%:::%` <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}

force_register <- function() {
  R6::R6Class
}
