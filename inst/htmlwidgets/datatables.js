HTMLWidgets.widget({
  name: "datatables",
  type: "output",
  renderValue: function(el, data) {
    var $el = $(el), cells = data.data;
    $el.empty();
    if (data.isDF === true) cells = HTMLWidgets.transposeArray2D(cells);
    $el.append(data.container);
    if (data.caption) $el.find('table').prepend(data.caption);
    var options = {};
    if (cells !== null) options = {
      data: cells
    };
    var table = $el.find('table').DataTable($.extend(options, data.options || {}));
    // initialize extensions
    for (var ext in data.extOptions) {
      new $.fn.dataTable[ext](table, data.extOptions[ext] || {});
    }
    // run the callback function on the table instance
    if (typeof data.callback === 'string') {
      var callback = eval('(' + data.callback + ')');
      if (typeof callback === 'function') callback(table);
    }
    // interaction with shiny
    if (!window.Shiny) return;
    var selected = [];
    table.$('input[type="checkbox"].DT.checkboxRows')
      .each(function(i) {
        if (this.checked) selected.push($(this).data('row'));
      })
      .on('change', function() {
        var $this = $(this), value = $this.data('row');
        if (this.checked) {
          selected.push(value);
        } else {
          selected.splice($.inArray(value, selected), 1);
        }
        Shiny.onInputChange(el.id + '_selected', selected);
      });
    Shiny.onInputChange(el.id + '_selected', selected);
  }
})
