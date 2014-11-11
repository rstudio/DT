HTMLWidgets.widget({
  name: "datatables",
  type: "output",
  renderValue: function(el, data) {
    var $el = $(el), cells = data.data, thiz = this;
    if (data.isDF === true) cells = HTMLWidgets.transposeArray2D(cells);
    $el.append(data.table);
    data.evals.map(function(member) {
      thiz.evaluateStringMember(data.options, member);
    });
    $el.find('table').DataTable($.extend({
      data: cells
    }, data.options || {}))
  },
  evaluateStringMember: function(o, member) {
    var parts = member.split('.');
    for(var i = 0, l = parts.length; i < l; i++) {
      var part = parts[i];
      // part may be a character or 'numeric' member name
      if (o !== null && typeof o === "object" && part in o) {
        if (i == (l - 1)) { // if we are at the end of the line then evalulate
          if (typeof o[part] === "string")
            o[part] = eval("(" + o[part] + ")")
        } else { // otherwise continue to next embedded object
          o = o[part];
        }
      }
    }
  }
})
