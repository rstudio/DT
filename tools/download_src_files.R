# Update the html_text variable with the latest DataTables CDN links
# To update:
# 1. Visit https://datatables.net/download/index
# 2. Select all default options except "Extensions"
# 3. At the bottom of the page, click the "CDN" panel
# 4. Choose "Minified" but not "Concatenated"
# 5. Copy the entire CDN panel content
# 6. Replace the html_text variable below with the copied content
#
# Note: This ensures we always have the most up-to-date DataTables resources
# while maintaining a consistent structure for our download process.

version = "2.1.4" # this must be the same as the version in the html_text
html_text = r"{
<link href="https://cdn.datatables.net/2.1.4/css/dataTables.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/autofill/2.7.0/css/autoFill.dataTables.css" rel="stylesheet">
<link href="https://cdn.datatables.net/buttons/3.1.1/css/buttons.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/colreorder/2.0.4/css/colReorder.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/datetime/1.5.3/css/dataTables.dateTime.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/fixedcolumns/5.0.1/css/fixedColumns.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/fixedheader/4.0.1/css/fixedHeader.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/keytable/2.12.1/css/keyTable.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/responsive/3.0.2/css/responsive.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/rowgroup/1.5.0/css/rowGroup.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/rowreorder/1.5.0/css/rowReorder.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/scroller/2.4.3/css/scroller.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/searchbuilder/1.8.0/css/searchBuilder.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/searchpanes/2.3.2/css/searchPanes.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/select/2.0.5/css/select.dataTables.min.css" rel="stylesheet">
<link href="https://cdn.datatables.net/staterestore/1.4.1/css/stateRestore.dataTables.min.css" rel="stylesheet">

<script src="https://cdnjs.cloudflare.com/ajax/libs/jszip/3.10.1/jszip.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.2.7/pdfmake.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.2.7/vfs_fonts.js"></script>
<script src="https://cdn.datatables.net/2.1.4/js/dataTables.min.js"></script>
<script src="https://cdn.datatables.net/autofill/2.7.0/js/dataTables.autoFill.min.js"></script>
<script src="https://cdn.datatables.net/buttons/3.1.1/js/dataTables.buttons.min.js"></script>
<script src="https://cdn.datatables.net/buttons/3.1.1/js/buttons.colVis.min.js"></script>
<script src="https://cdn.datatables.net/buttons/3.1.1/js/buttons.html5.min.js"></script>
<script src="https://cdn.datatables.net/buttons/3.1.1/js/buttons.print.min.js"></script>
<script src="https://cdn.datatables.net/colreorder/2.0.4/js/dataTables.colReorder.min.js"></script>
<script src="https://cdn.datatables.net/datetime/1.5.3/js/dataTables.dateTime.min.js"></script>
<script src="https://cdn.datatables.net/fixedcolumns/5.0.1/js/dataTables.fixedColumns.min.js"></script>
<script src="https://cdn.datatables.net/fixedheader/4.0.1/js/dataTables.fixedHeader.min.js"></script>
<script src="https://cdn.datatables.net/keytable/2.12.1/js/dataTables.keyTable.min.js"></script>
<script src="https://cdn.datatables.net/responsive/3.0.2/js/dataTables.responsive.min.js"></script>
<script src="https://cdn.datatables.net/rowgroup/1.5.0/js/dataTables.rowGroup.min.js"></script>
<script src="https://cdn.datatables.net/rowreorder/1.5.0/js/dataTables.rowReorder.min.js"></script>
<script src="https://cdn.datatables.net/scroller/2.4.3/js/dataTables.scroller.min.js"></script>
<script src="https://cdn.datatables.net/searchbuilder/1.8.0/js/dataTables.searchBuilder.min.js"></script>
<script src="https://cdn.datatables.net/searchpanes/2.3.2/js/dataTables.searchPanes.min.js"></script>
<script src="https://cdn.datatables.net/select/2.0.5/js/dataTables.select.min.js"></script>
<script src="https://cdn.datatables.net/staterestore/1.4.1/js/dataTables.stateRestore.min.js"></script>
}"
download_datatables_files = function(html_text) {
    # Define the base directory as a variable
    base_dir = "download/DataTables"
    # Create the base directory and ensure it's empty
    if (dir.exists(base_dir)) {
        unlink(base_dir, recursive = TRUE)
    }
    dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

    # Extract URLs from the HTML text
    urls = unlist(regmatches(html_text, gregexpr("https://[^\"]+", html_text)))

    # replace all the xxx.js to xxx.min.js, except xxx.min.js
    # Replace xxx.js with xxx.min.js, except for already minified files
    urls = gsub("(?<!min)\\.js$", ".min.js", urls, perl = TRUE)
    # Use non-minified versions for 'pdfmake.js' and 'vfs_fonts.js'
    # This is due to compatibility issues with pdfmake.min.js on Windows for self-contained HTML pages
    # Reference: https://github.com/rstudio/DT/issues/774#issuecomment-595277726
    urls = gsub("pdfmake.min.js", "pdfmake.js", urls, fixed = TRUE)
    urls = gsub("vfs_fonts.min.js", "vfs_fonts.js", urls, fixed = TRUE)

    # Download and save each file
    for (url in urls) {
        # Extract the relative path from the URL
        rel_path = sub("^https://[^/]+/", "", url)

        # Remove version part from the path
        rel_path = gsub("/[0-9.]+/", "/", rel_path)

        # Create the full local path
        local_path = file.path(base_dir, rel_path)

        # Ensure the directory exists
        dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)

        # Download the file
        tryCatch(
            {
                utils::download.file(url, local_path, mode = "wb")
                cat("Downloaded:", url, "to", local_path, "\n")
            },
            error = function(e) {
                cat("Error downloading:", url, "-", e$message, "\n")
            }
        )
    }
    # should change the download/$version$/* to download/datatables/*
    file.rename(
        from = sprintf("%s/%s", base_dir, version),
        to = sprintf("%s/datatables", base_dir)
    )
}

# Run
download_datatables_files(html_text)
