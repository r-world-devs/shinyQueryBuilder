% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filters.R
\name{queryFilter}
\alias{queryFilter}
\title{Define query filter.}
\usage{
queryFilter(
  id,
  field,
  label,
  optgroup,
  type,
  input,
  values,
  value_separator,
  default_value,
  input_event,
  size,
  rows,
  multiple,
  placeholder,
  vertical,
  validation,
  operators,
  default_operator,
  plugin,
  plugin_config,
  data,
  valueSetter,
  valueGetter,
  unique
)
}
\arguments{
\item{id}{Character string    (required). Unique identifier of the filter.}

\item{field}{Character string (equals `id` when missing).
Field used by the filter, multiple filters can use the same field.
The provided field wiil be used in the returned query.}

\item{label}{Character string (equals `field` when missing). Label used to display the field.}

\item{optgroup}{Fields with the same `optgroup` will be presented within a separate group in the fields dropdown.
If skipped, the field will be not listed in any of the groups, but presented independently.}

\item{type}{Character string (required). Type of the field being an R class.
The argument determines default configuration for the field input controllers.
Available types are 'character', 'factor', 'integer', 'numeric', 'POSIXct', 'Date' and 'logical'.}

\item{input}{Character string or JS function. Type of input used.
  Available types are 'text', 'number', 'textarea', 'radio', 'checkbox' and 'select'.
  It can also be a JS function which returns the HTML of the said input, this function takes 2 parameters:
 \itemize{
   \item{\code{rule} - the Rule object}
   \item{\code{input_name} - the name of the input}
 }
In order to define it, create the function definition as character string and pass it to \link{js}.
When skipped, the default input will be used based on the provided \code{type}.}

\item{values}{Vector of possible values.
Required for limited selection inputs (e.g. 'radio', 'checkbox', 'select').}

\item{value_separator}{Character string. Used the split the provided value when a 'text' input is used with an
operator allowing multiple values ('in' for example).
When skipped, the provided input will be used as a bare value.
Needs to be set, when multiple values needs to be provided for 'text' and 'textarea' inputs.}

\item{default_value}{Default operator value.}

\item{input_event}{(advanced) Character string ('change' by default).
Space separated list of DOM events which the builder should listen to detect value changes.}

\item{size}{Integer. Only for 'text' and 'textarea' inputs: horizontal size of the input.}

\item{rows}{Integer. Only for 'textarea' inputs: vertical size of the input.}

\item{multiple}{Logical (`FALSE` default). Set to `TRUE` if value input controller should accept multiple values.
Please make sure the corresponding operators allow to take multiple values to make it work, see \link{mapOperator}.}

\item{placeholder}{Character string  Only for 'text' and 'textarea' inputs: placeholder to display inside the input.}

\item{vertical}{Logical (FALSE default). Only for 'radio' and 'checkbox' inputs: display inputs vertically
not horizontally.}

\item{validation}{List of options for rule validation. See \code{vignette("validation")}.}

\item{operators}{Character vector of operators types to use for this filter.
When skipped the filter will use all applicable operators.
See \link{listMappedOperators}.}

\item{default_operator}{Character string. Name of the operator that should be used by default
when defining new rules.
When skipped the first value from \code{operators} is used.}

\item{plugin, plugin_config}{(advanced) Name of a jQuery plugin to apply on the input and plugin configuration.
See \url{https://querybuilder.js.org/demo.html#widgets}.}

\item{data}{List. Additional data that will be added to the returned query - `input`
element returned by \link{queryBuilderInput}. Use this to store any functional data you need.}

\item{valueSetter}{(advanced) JS function used to set the input(s) value.
  If provided the default function is not run.
 The function takes 2 parameters:
 \itemize{
   \item{\code{rule} - the Rule object}
   \item{\code{value}}
 }
In order to define it, create the function definition as character string and pass it to \link{js}.}

\item{valueGetter}{(advanced) function  Function used to get the input(s) value.
  If provided the default function is not run.
  It takes 1 parameter:
 \itemize{
   \item{\code{rule} - the Rule object}
 }
In order to define it, create the function definition as character string and pass it to \link{js}.}

\item{unique}{Allow to use the filter only once. Can be `FALSE/TRUE` or 'group' to allow using filter once per group.
In order to make it work please enable 'unique-filter' plugin (`plugin = list("unique-filter")`)
for \link{queryBuilderInput}.}
}
\value{
A list object storing the filter created filter configuration.
}
\description{
Filters are responsible for defining available options for providing field-rules in the interface.
With filters you may decide what operators should be available for the field,
what possible operator-values can be chosen or even customize what kind of input controllers
should be used for that goal.
}
\examples{
ui <- shiny::fluidPage(
  queryBuilderInput(
    "qb",
    filters = list(
      queryFilter(
        "Species", type = "character", operators = c("in", "equal"),
        values = levels(iris$Species), multiple = TRUE, input = "text", value_separator = ";"
      ),
      queryFilter(
        "Sepal.Length", type = "numeric", values = range(iris$Sepal.Length),
        validation = list(min = 4.3, max = 7.9, step = 0.1)
      )
    )
  ),
  shiny::verbatimTextOutput("expr")
)
server <- function(input, output, session) {}

if (interactive()) {
  shiny::runApp(ui, server)
}

}
\seealso{
\link[queryBuilder]{queryRule}
}
