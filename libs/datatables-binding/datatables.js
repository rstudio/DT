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

      var filterRow;
      if (data.filter === 'top') {
        filterRow = $(table.table().node()).find('thead tr:last td');
      } else {
        filterRow = $(table.columns().footer());
      }

      filterRow.each(function(i, td) {

        var $td = $(td), type = $td.data('type'), filter;
        var $input = $td.children('div').first().children('input');
        $input.on('input blur', function() {
          $input.next('span').toggle(Boolean($input.val()));
        });
        // Bootstrap sets pointer-events to none and we won't be able to click
        // the clear button
        $input.next('span').css('pointer-events', 'auto').hide().click(function() {
          $(this).hide().prev('input').val('').trigger('input').focus();
        });
        var $x = $td.children('div').last();

        if (type === 'factor') {
          $input.on({
            click: function() {
              $input.parent().hide(); $x.show(); filter[0].selectize.focus();
            },
            input: function() {
              if ($input.val() === '') filter[0].selectize.setValue([]);
            }
          });
          filter = $x.children('select').selectize({
            plugins: ['remove_button'],
            hideSelected: true,
            onChange: function(value) {
              $input.val(value === null ? '' : JSON.stringify(value));
              if (value) $input.trigger('input');
              $input.attr('title', $input.val());
              if (server) {
                table.column(i).search(value ? JSON.stringify(value) : '').draw();
                return;
              }
              // turn off filter if nothing selected
              $td.data('filter', value !== null && value.length > 0);
              table.draw();  // redraw table, and filters will be applied
            },
            onBlur: function() {
              $x.hide(); $input.parent().show();
            }
          });
          filter.next('div').css('margin-bottom', 'auto');
        } else if (type === 'character') {
          var fun = function() {
            table.column(i).search($input.val()).draw();
          };
          if (server) {
            fun = throttle(fun, 1000);
          }
          $input.on('input', fun);
        } else if (type === 'number' || type === 'date' || type === 'time') {
          var $x0 = $x;
          $x = $x0.children('div').first();
          $x0.css({
            'background-color': '#fff',
            'border': '1px #ddd solid',
            'border-radius': '4px',
            'padding': '20px 20px 10px 20px'
          });
          var $spans = $x0.children('span').css('margin-top', '10px');
          var $span1 = $spans.first(), $span2 = $spans.last();
          var r1 = +$x.data('min'), r2 = +$x.data('max');
          $input.on({
            focus: function() {
              $x0.show();
              $x0.outerWidth(Math.max(
                160, $span1.outerWidth() + $span2.outerWidth() + 20, $input.outerWidth()
              ));
            },
            blur: function() {
              $x0.hide();
            },
            input: function() {
              if ($input.val() === '') filter.val([r1, r2]);
            },
            change: function() {
              var v = $input.val();
              if (v === '') return;
              v = v.split('...');
              if (v.length !== 2) {
                $input.parent().addClass('has-error');
                return;
              }
              $input.parent().removeClass('has-error');
              // treat date as UTC time at midnight
              var strTime = function(x) {
                var s = type === 'date' ? 'T00:00:00Z' : '';
                var t = new Date(x.replace(/\s/g, '') + s).getTime();
                // add 10 minutes to date since it does not hurt the date, and
                // it helps avoid the tricky floating point arithmetic problems,
                // e.g. sometimes the date may be a few milliseconds earlier
                // than the midnight due to precision problems in noUiSlider
                return type === 'date' ? t + 3600000 : t;
              };
              if (type !== 'number') {
                v[0] = strTime(v[0]);
                v[1] = strTime(v[1]);
              }
              filter.val([+v[0], +v[1]]);
            }
          });
          var formatDate = function(d) {
            if (type === 'number') return d;
            var x = new Date(+d);
            if (type === 'date') {
              var pad0 = function(x) {
                return ('0' + x).substr(-2, 2);
              };
              return x.getUTCFullYear() + '-' + pad0(1 + x.getUTCMonth())
                      + '-' + pad0(x.getUTCDate());
            } else {
              return x.toISOString();
            }
          };
          var opts = type === 'date' ? { step: 60 * 60 * 1000 } : {};
          filter = $x.noUiSlider($.extend({
            start: [r1, r2],
            range: {min: r1, max: r2},
            connect: true
          }, opts));
          $span1.text(formatDate(r1)); $span2.text(formatDate(r2));
          var updateSlider = function(e) {
            var val = filter.val();
            // turn off filter if in full range
            $td.data('filter', val[0] != r1 || val[1] != r2);
            var v1 = formatDate(val[0]), v2 = formatDate(val[1]);
            if ($td.data('filter')) {
              var ival = v1 + ' ... ' + v2;
              $input.attr('title', ival).val(ival).trigger('input');
            } else {
              $input.attr('title', '').val('');
            }
            $span1.text(v1); $span2.text(v2);
            if (e.type === 'slide') return;  // no searching when sliding only
            if (server) {
              table.column(i).search($td.data('filter') ? val.join(',') : '').draw();
              return;
            }
            table.draw();
          };
          filter.on({
            set: updateSlider,
            slide: updateSlider
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
          } else if (type === 'factor') {
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
