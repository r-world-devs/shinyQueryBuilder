null_default <- function(value, default) {
  if (is.null(value)) {
    return(default)
  }
  return(value)
}

modify_list <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  return(
    utils::modifyList(x, y, keep.null = TRUE)
  )
}

setting_from_stat <- function(stat, column_name, ..., .queryBuilderConfig) {
  col_class <- stat$class
  stat$class <- NULL
  extra_args <- rlang::dots_list(...)
  modify_list(
    list(
      id = column_name,
      field = column_name,
      type = col_class,
      input = input_type_mappers[[col_class]]$input,
      placeholder = glue::glue("Choose {column_name}"),
      operators = names(listMappedOperators(col_class, FALSE, .queryBuilderConfig))
    ),
    modify_list(extra_args, stat)
  )
}

filter_from_column <- function(column, column_name, settings, default_settings) {
  col_settings <- null_default(settings[[column_name]], list())
  default_col_settings <- default_settings[[column_name]]
  do.call(
    queryFilter,
    utils::modifyList(default_col_settings, col_settings)
  )
}

get_data_setting <- function(data, .queryBuilderConfig) {
  
  stat_from_column <- `%:::%`("queryBuilder", "stat_from_column")
  
  purrr::imap(
    data,
    stat_from_column
  ) %>%
    purrr::imap(
      setting_from_stat,
      .queryBuilderConfig = .queryBuilderConfig
    )
}

#' Generate filters definition
#'
#' @param data Dataset from which filters should extracted.
#' @param settings Named list. Column-specific filter configuration.
#'   For each variable the provided settings will overwrite the default ones.
#' @param .queryBuilderConfig R6 object of class 'queryBuilderConfig' storing queryOperators.
#'   See \link[queryBuilder]{query-operator}.
#'
#' @examples
#'
#' genQueryFilters(
#'   iris,
#'   list(
#'     Species = list(operators = c("equal", "not_equal"))
#'   )
#' )
#'
#' @return Nested list object storing generated filters configuration.
#' @export
genQueryFilters <- function(data, settings = list(), .queryBuilderConfig = queryBuilder::queryBuilderConfig) {
  purrr::imap(
    data,
    filter_from_column,
    settings = settings,
    default_settings = get_data_setting(data, .queryBuilderConfig)
  ) %>% unname()
}
