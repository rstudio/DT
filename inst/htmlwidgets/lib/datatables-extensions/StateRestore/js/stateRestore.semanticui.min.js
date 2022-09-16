/*!
 Bootstrap integration for DataTables' StateRestore
 ©2016 SpryMedia Ltd - datatables.net/license
*/
(function(b){"function"===typeof define&&define.amd?define(["jquery","datatables.net-se","datatables.net-staterestore"],function(a){return b(a)}):"object"===typeof exports?module.exports=function(a,c){a||(a=window);c&&c.fn.dataTable||(c=require("datatables.net-se")(a,c).$);c.fn.dataTable.StateRestore||require("datatables.net-staterestore")(a,c);return b(c)}:b(jQuery)})(function(b){var a=b.fn.dataTable;b.extend(!0,a.StateRestoreCollection.classes,{checkBox:"dtsr-check-box form-check-input",checkLabel:"dtsr-check-label form-check-label",
checkRow:"dtsr-check-row form",creationButton:"dtsr-creation-button ui button primary",creationForm:"dtsr-creation-form modal-body",creationText:"dtsr-creation-text modal-header",creationTitle:"dtsr-creation-title modal-title",nameInput:"dtsr-name-input form-control",nameLabel:"dtsr-name-label form-label",nameRow:"dtsr-name-row ui input"});b.extend(!0,a.StateRestore.classes,{confirmation:"dtsr-confirmation modal",confirmationButton:"dtsr-confirmation-button ui button primary",confirmationText:"dtsr-confirmation-text modal-body",
renameModal:"dtsr-rename-modal ui input"});return a.stateRestore});
