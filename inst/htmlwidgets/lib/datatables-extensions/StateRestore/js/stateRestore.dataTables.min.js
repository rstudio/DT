/*!
 Bootstrap integration for DataTables' StateRestore
 ©2016 SpryMedia Ltd - datatables.net/license
*/
(function(c){"function"===typeof define&&define.amd?define(["jquery","datatables.net-dt","datatables.net-staterestore"],function(b){return c(b)}):"object"===typeof exports?module.exports=function(b,a){b||(b=window);a&&a.fn.dataTable||(a=require("datatables.net-dt")(b,a).$);a.fn.dataTable.StateRestore||require("datatables.net-staterestore")(b,a);return c(a)}:c(jQuery)})(function(c){return c.fn.dataTable.stateRestore});
