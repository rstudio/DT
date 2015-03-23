HTMLWidgets.widget({
  name: "datatables",
  type: "output",
  renderValue: function(el, data) {
    var $el = $(el), cells = data.data;
    $el.empty();

    if (cells instanceof Array) cells = HTMLWidgets.transposeArray2D(cells);

    $el.append(data.container);
    if (data.caption) $el.find('table').prepend(data.caption);

    var options = {};
    if (cells !== null) options = {
      data: cells
    };

    var table = $el.find('table').DataTable($.extend(options, data.options || {}));

    // server-side processing?
    var server = data.options.serverSide === true;
    var throttle = $.fn.dataTable.util.throttle;

    // throttle searching in the server mode (perhaps debounce is better, but it
    // is not available in DataTables)
    if (server) {
      $el.find('label input').first().unbind('keyup')
         .keyup(throttle(function() {
           table.search(this.value).draw();
         }, 1000));
    }

    if (data.filter !== 'none') {
      $(table.columns().footer()).each(function(i, td) {

        var $td = $(td), type = $td.data('type'), filter;
        if (type === 'category') {
          filter = $td.children('select').selectize({
            plugins: ['remove_button'],
            onChange: function(value) {
              if (server) {
                table.column(i).search(value || '').draw();
                return;
              }
              // turn off filter if nothing selected
              $td.data('filter', value !== null && value.length > 0);
              table.draw();  // redraw table, and filters will be applied
            }
          });
        } else if (type === 'number' || type === 'date') {
          var $x = $td.children('div');
          var r1 = +$x.data('min'), r2 = +$x.data('max');
          filter = $x.noUiSlider({
            start: [r1, r2],
            range: {min: r1, max: r2},
            connect: true
          }).on({
            change: function() {
              var val = $(this).val();
              if (server) {
                table.column(i).search(val.join(',')).draw();
                return;
              }
              // turn off filter if in full range
              $td.data('filter', val[0] != r1 || val[1] != r2);
              table.draw();
            }
          });
        }

        // server-side processing will be handled by R (or whatever server
        // language you use); the following code is only needed for client-side
        // processing
        if (server) return;

        var customFilter = function(settings, data, dataIndex) {
          // there is no way to attach a search function to a specific table,
          // and we need to make sure a global search function is not applied to
          // all tables (i.e. a range filter in a previous table should not be
          // applied to the current table); we use the settings object to
          // determine if we want to perform searching on the current table,
          // since settings.sTableId will be different to different tables
          if (table.settings()[0] !== settings) return true;
          // no filter on this column or no need to filter this column
          if (typeof filter === 'undefined' || !$td.data('filter')) return true;

          var r = filter.val(), v, r0, r1;
          if (type === 'number') {
            v = parseFloat(data[i]);
            // how to handle NaN? currently exclude these rows
            if (isNaN(v)) return(false);
            r0 = parseFloat(r[0]); r1 = parseFloat(r[1]);
            if (v >= r0 && v <= r1) return true;
          } else if (type === 'date' || type === 'time') {
            v = new Date(data[i]);
            r0 = new Date(+r[0]); r1 = new Date(+r[1]);
            if (v >= r0 && v <= r1) return true;
          } else if (type === 'category') {
            // TODO: enable partial matching?
            if (r.length === 0 || $.inArray(data[i], r) > -1) return true;
          }
          return false;
        };

        $.fn.dataTable.ext.search.push(customFilter);

      });
    }

    // initialize extensions
    for (var ext in data.extOptions) {
      new $.fn.dataTable[ext](table, data.extOptions[ext] || {});
    }

    // run the callback function on the table instance
    if (typeof data.callback === 'function') data.callback(table);

    // interaction with shiny
    if (!window.Shiny) return;

    var changeInput = function(id, data) {
      Shiny.onInputChange(el.id + '_' + id, data);
    };

    // selected rows (checkboxes added via DT::checkboxRows())
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
        changeInput('selected', selected);
      });
    changeInput('selected', selected);

    // expose some table info to Shiny
    var updateTableInfo = function(e, settings) {
      // TODO: is anyone interested in the page info?
      // changeInput('page_info', table.page.info());
      var updateRowInfo = function(id, modifier) {
        changeInput('rows' + '_' + id, table.rows($.extend({
          search: 'applied',
          page: 'all'
        }, modifier)).indexes().map(function(i) {
          return 1 + i;
        }).toArray());
      };
      updateRowInfo('current', {page: 'current'});
      updateRowInfo('all', {});
    };
    table.on('draw.dt', updateTableInfo);
    updateTableInfo();
  }
});
