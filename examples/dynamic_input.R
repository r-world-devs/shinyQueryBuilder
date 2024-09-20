# The example shows how to implement various operator inputs depending on the operator type

pkgload::load_all()
library(magrittr)
library(bslib)

input_rule <- js("function(rule, name) {
  var $value_container = rule.$el.find('.rule-value-container');
  var $operator = rule.$el.find('.rule-operator-container');

  var hh = '<select class=\"form-control\" name=\"' + name +'\" >'

  var $letter_select = '';
  var $number_select = '';

  $letter_select += '<option value=\"' + rule.filter.data.letter_vals[0] + '\" disabled selected>' + rule.filter.data.letter_label + '</option>'
  for (i = 0; i < rule.filter.data.letter_vals.length; i++) {
    $letter_select += '<option value=\"' + rule.filter.data.letter_vals[i] + '\">' + rule.filter.data.letter_vals[i] + '</option>';
  }

  $number_select += '<option value=\"' + rule.filter.data.number_vals[0] + '\" disabled selected>' + rule.filter.data.number_label + '</option>'
  for (i = 0; i < rule.filter.data.number_vals.length; i++) {
    $number_select += '<option value=\"' + rule.filter.data.number_vals[i] + '\">' + rule.filter.data.number_vals[i] + '</option>';
  }

  $operator.on('change', 'select', function(){
    var h = '';

    switch ($(this).val()) {
      case 'is_letter':
        h = $letter_select;
        default_val = rule.filter.data.letter_vals[0];
        break;
      case 'is_number':
        h = $number_select;
        default_val = rule.filter.data.number_vals[0];
        break;
    }

    $value_container.find('select').html(h);
    $value_container.find('select').val(default_val).trigger('change');
  });

  if (rule.operator.type == 'is_number') {
    return hh + $number_select + '</select>';
  }

  return hh + $letter_select + '</select>';

}")

val_getter <- js("function(rule) {
  return rule.$el.find('.rule-value-container select').val();
}")

val_setter <- js("function(rule, value) {
  rule.$el.find('.rule-value-container select').val(value).trigger('change');
}")

setQueryOperators(
  is_number = queryOperator(method = `==`),
  is_letter = queryOperator(method = `==`)
)

mapOperator("is_letter", optgroup = "basic", nb_inputs = 1, multiple = FALSE, apply_to = c('character'))
mapOperator("is_number", optgroup = "basic", nb_inputs = 1, multiple = FALSE, apply_to = c('character'))

query_input <- queryBuilderInput(
  inputId = "query",
  filters = list(
    queryFilter(
      id = "val", label = "Value", type = "character", values = c("1", "2", "3", "A", "B", "C"),
      placeholder = "Choose value", operators = c("is_number", "is_letter"),
      value_separator = " , ", input = input_rule,
      data = list(
        letter_vals = c("A", "B", "C"), letter_label = "Letter equals",
        number_vals = c("1", "2", "3"), number_label = "Number equals"
      ),
      valueSetter = val_setter, valueGetter = val_getter,
      validation = list(callback = js("function(value, rule) {if (value === null) return 'Empty'; return true;}"))
    )
  ),
  allow_empty = TRUE,
  rules = queryGroup(
    condition = "OR",
    queryRule("val", "is_number", "1")
  ),
  conditions = c("AND", "OR"),
  select_placeholder = "- - - - -",
  display_empty_filter = FALSE,
  default_filter = "val"
)



ui <- page_fluid(
  title = "Data Based Filters Demo",
  card(
    card_header("queryBuilder"),
    query_input,
    shiny::verbatimTextOutput("expr")
  )
)

server <- function(input, output, session) {
  output$expr <- renderPrint({
    print(queryToExpr(input$query))
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
