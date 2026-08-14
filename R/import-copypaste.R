@@
-      fread_args$tex <- input$data_pasted
-      imported <- try(rlang::exec(data.table::fread, !!!fread_args), silent = TRUE)
+      # try to read pasted text using readr; fall back to TSV/semicolon if needed
+      fread_args$tex <- input$data_pasted
+      text <- input$data_pasted
+      imported <- tryCatch({
+        # first try CSV
+        readr::read_csv(I(text), col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
+      }, error = function(e1) {
+        tryCatch({
+          # then try TSV
+          readr::read_tsv(I(text), col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
+        }, error = function(e2) {
+          tryCatch({
+            # then try semicolon-delimited
+            readr::read_delim(I(text), delim = ";", col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
+          }, error = function(e3) {
+            structure(list(error = TRUE, message = conditionMessage(e3)), class = "try-error")
+          })
+        })
+      })
 
-      if (inherits(imported, "try-error") || NROW(imported) < 1) {
+      if (inherits(imported, "try-error") || nrow(imported) < 1) {
         toggle_widget(inputId = "confirm", enable = FALSE)
-        insert_error(mssg = i18n(attr(imported, "condition")$message))
+        insert_error(mssg = i18n(attr(imported, "condition")$message))
         temporary_rv$status <- "error"
         temporary_rv$data <- NULL
         temporary_rv$name <- NULL
       } else {
         toggle_widget(inputId = "confirm", enable = TRUE)
         insert_alert(
           selector = ns("import"),
           status = "success",
           make_success_alert(
             imported,
             trigger_return = trigger_return,
             btn_show_data = btn_show_data
           )
         )
         temporary_rv$status <- "success"
         temporary_rv$data <- imported
       }
     }, ignoreInit = TRUE)
