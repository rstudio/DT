/*!
 Bootstrap integration for DataTables' StateRestore
 ©2016 SpryMedia Ltd - datatables.net/license
*/
(function(b){"function"===typeof define&&define.amd?define(["jquery","datatables.net-bs5","datatables.net-staterestore"],function(a){return b(a)}):"object"===typeof exports?module.exports=function(a,c){a||(a=window);c&&c.fn.dataTable||(c=require("datatables.net-bs5")(a,c).$);c.fn.dataTable.StateRestore||require("datatables.net-staterestore")(a,c);return b(c)}:b(jQuery)})(function(b){var a=b.fn.dataTable;b.extend(!0,a.StateRestoreCollection.classes,{checkBox:"dtsr-check-box form-check-input",checkLabel:"dtsr-check-label form-check-label",
checkRow:"dtsr-check-row form-check",creationButton:"dtsr-creation-button btn btn-secondary",creationForm:"dtsr-creation-form modal-body",creationText:"dtsr-creation-text modal-header",creationTitle:"dtsr-creation-title modal-title",nameInput:"dtsr-name-input form-control",nameLabel:"dtsr-name-label form-label"});b.extend(!0,a.StateRestore.classes,{confirmationButton:"dtsr-confirmation-button btn btn-secondary",input:"dtsr-input form-control"});return a.stateRestore});
