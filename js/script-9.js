// manage active state of menu based on current page
$(document).ready(function () {
    // active menu
    var href = window.location.pathname;
    href = href.substr(href.lastIndexOf('/') + 1);
    if (href === '') href = './';
    $('a[href="' + href + '"]').parent().addClass('active');
});
