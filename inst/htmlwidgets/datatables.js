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
    if (typeof data.callback === 'string') {
      var callback = eval('(' + data.callback + ')');
      if (typeof callback === 'function') callback(table);
    }
    if (!window.Shiny) return;
    var selectedRows = [];
    table.$('input[type="checkbox"].DT.checkboxRows').on('change', function() {
      var $this = $(this), value = $this.data('row');
      if (this.checked) {
        selectedRows.push(value);
      } else {
        selectedRows.splice($.inArray(value, selectedRows), 1);
      }
      Shiny.onInputChange(el.id + '_selected', selectedRows);
    });
    Shiny.onInputChange(el.id + '_selected', selectedRows);
  }
})
