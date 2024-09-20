var queryBuilderInput = new Shiny.InputBinding();

function isValidQuery(el) {
  try {
    var valid = $(el).queryBuilder('validate');
    return valid;
  } catch {
    return false;
  }
}

function reconfigClass(el, data, rule, rule_class) {
  if (data.hasOwnProperty(rule)) {
    if (data[rule]) {
      $(el).removeClass(rule_class);
    } else {
      $(el).addClass(rule_class);
    }
  }
}

$.extend(queryBuilderInput, {
  find: function(scope) {
    return $(scope).find(".shiny-querybuilder");
  },
  initialize: function(el) {
    config = $(el).data('config');
    if (typeof config != 'object') {
      config = eval('(' + config + ')');
    }
    /*
    for (var idx in config.filters) {
      filter = config.filters[idx]; 
      if (filter.hasOwnProperty('validation')) {
        if (filter.validation.hasOwnProperty('callback')) {
          config.filters[idx]['validation']['callback'] = new Function("value", "Rule", filter['validation']['callback']);
        }
      }
    }
    */
    $(el).queryBuilder(config);
    $(el).data('update_mode', false);
  },
  getValue: function(el) {
    return $(el).queryBuilder('getRules');
  },
  setValue: function(el, value) {
    $(el).queryBuilder('setRules', value);
  },
  receiveMessage: function(el, data) {
    $(el).data('update_mode', true);
    reconfigClass(el, data, "allow_add_rules", "disable-add-rules");
    reconfigClass(el, data, "allow_groups", "disable-add-groups");
    reconfigClass(el, data, "allow_rm_rules", "disable-rm-rules");
    reconfigClass(el, data, "allow_rm_groups", "disable-rm-groups");
    if (data.hasOwnProperty("filters")) {
      if ($(el).hasClass("no-filters")) {
        $(el).removeClass("no-filters");
      }
      $(el).queryBuilder('setFilters', true, data.filters);
    }
    if (data.hasOwnProperty("rules")) {
      this.setValue(el, data.rules);
    }
    
  },
  getType: function(el) {
    return "shinyQueryBuilder.querybuilder";
  },
  subscribe: function (el, callback) {
    $(el).on('rulesChanged.queryBuilder',
      function(e, rule, error, value) {
        if (isValidQuery($(el)) & !$(el).data('update_mode')) {
          callback(true)
        }
      }
    );
    $(el).on('afterSetRules.queryBuilder',
      function(e, value) {
        $(el).data('update_mode', false);
        if (isValidQuery($(el))) {
          callback(true)
        }
      }
    );
    $(el).on('afterSetFilters.queryBuilder',
      function(e, value) {
        $(el).data('update_mode', false);
        if (isValidQuery($(el))) {
          callback(true)
        }
      }
    );
  },
  unsubscribe: function(el) {
    $(el).off(".queryBuilder");
  },
  getRatePolicy: function() {
    return {
      policy: 'debounce',
      delay: 500
    }
  }
});

Shiny.inputBindings.register(queryBuilderInput, 'shiny.queryBuilderBinding');
