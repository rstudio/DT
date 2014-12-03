HTMLWidgets.widget({
  name: "datatables",
  type: "output",
  renderValue: function(el, data) {
    var $el = $(el), cells = data.data, thiz = this;
    if (data.isDF === true) cells = HTMLWidgets.transposeArray2D(cells);
    $el.append(data.table);
    $el.find('table').DataTable($.extend({
      data: cells
    }, data.options || {}))
  }
})
