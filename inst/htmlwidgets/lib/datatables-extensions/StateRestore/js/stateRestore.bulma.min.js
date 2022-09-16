/*!
 Bootstrap integration for DataTables' StateRestore
 ©2016 SpryMedia Ltd - datatables.net/license
*/
(function(b){"function"===typeof define&&define.amd?define(["jquery","datatables.net-bm","datatables.net-staterestore"],function(a){return b(a)}):"object"===typeof exports?module.exports=function(a,c){a||(a=window);c&&c.fn.dataTable||(c=require("datatables.net-bm")(a,c).$);c.fn.dataTable.StateRestore||require("datatables.net-staterestore")(a,c);return b(c)}:b(jQuery)})(function(b){var a=b.fn.dataTable;b.extend(!0,a.StateRestoreCollection.classes,{checkRow:"dtsr-check-row checkbox",creationButton:"dtsr-creation-button button",
creationForm:"dtsr-creation-form modal-content",creationText:"dtsr-creation-text modal-header",creationTitle:"dtsr-creation-title modal-card-title",nameInput:"dtsr-name-input input"});b.extend(!0,a.StateRestore.classes,{confirmationButton:"dtsr-confirmation-button button",confirmationTitle:"dtsr-confirmation-title modal-card-title",input:"dtsr-input input"});return a.stateRestore});
