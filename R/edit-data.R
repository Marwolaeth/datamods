#' @title Shiny module to interactively edit a `data.frame`
#'
#' @description The module generates different options to edit a `data.frame`: adding, deleting and modifying rows, exporting data (csv and excel), choosing editable columns, choosing mandatory col[...] 
#' This module returns the edited table with the user modifications.
#'
#' @param id Module ID
#'
#' @importFrom shiny uiOutput
#' @importFrom htmltools tagList tags
#' @importFrom reactable reactableOutput
#' @importFrom utils getFromNamespace
#'
#' @export
#'
#' @name edit-data
#'
#' @example examples/edit_data.R
edit_data_ui <- function(id) {
  ns <- NS(id)

  notify_dep <- getFromNamespace("html_dependency_notify", "shinybusy")

  tagList(

    notify_dep(),

    # Download data in Excel format --
    uiOutput(outputId = ns("download_excel"), style = "display: inline;"),

    # Download data in csv format --
    uiOutput(outputId = ns("download_csv"), style = "display: inline;"),

    # Add a row --
    uiOutput(outputId = ns("add_button"), style = "display: inline;"),

    tags$div(class = "clearfix mb-2"),

    # Table --
    reactableOutput(outputId = ns("table"))
  )
}

#' @title Shiny module to interactively edit a `data.frame`
#' ...
#' @export
edit_data_server <- function(id,
                             data_r = reactive(NULL),
                             add = TRUE,
                             update = TRUE,
                             delete = TRUE,
                             download_csv = TRUE,
                             download_excel = TRUE,
                             file_name_export = "data",
                             var_edit = NULL,
                             var_mandatory = NULL,
                             var_multiline = NULL,
                             var_labels = NULL,
                             add_default_values = list(),
                             n_column = 1,
                             return_class = c("data.frame", "data.table", "tbl_df", "raw"),
                             reactable_options = NULL,
                             modal_size = c("m", "s", "l", "xl"),
                             modal_easy_close = TRUE,
                             callback_add = NULL,
                             callback_update = NULL,
                             callback_delete = NULL,
                             only_callback = FALSE,
                             use_notify = TRUE) {
  return_class <- match.arg(return_class)
  modal_size <- match.arg(modal_size)
  callback_default <- function(...) return(TRUE)
  if (!is_function(callback_add))
    callback_add <- callback_default
  if (!is_function(callback_update))
    callback_update <- callback_default
  if (!is_function(callback_delete))
    callback_delete <- callback_default
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      data_rv <- reactiveValues(
        data = NULL,
        colnames = NULL,
        mandatory = NULL,
        multiline = NULL,
        edit = NULL,
        internal_colnames = NULL,
        labels = NULL
      )

      # Data data_r() with added columns ".datamods_edit_update" et ".datamods_edit_delete" ---
      data_init_r <- eventReactive(data_r(), {
        req(data_r())
        data <- as.data.frame(data_r())

        # resolve reactive args
        if (is.reactive(var_mandatory))
          var_mandatory <- var_mandatory()
        if (is.reactive(var_multiline))
          var_multiline <- var_multiline()
        if (is.reactive(var_labels))
          var_labels <- var_labels()
        if (is.null(var_labels))
          var_labels <- setNames(as.list(names(data)), names(data))
        if (is.reactive(var_edit))
          var_edit <- var_edit()
        if (is.null(var_edit))
          var_edit <- names(data)

        # store original colnames for display
        orig_colnames <- colnames(data)
        data_rv$colnames <- orig_colnames

        # work on tibble/data.frame but create internal names for consistent UI mapping
        data <- dplyr::as_tibble(data)
        if (ncol(data) > 0) {
          names(data) <- paste0("col_", seq_along(data))
          data_rv$internal_colnames <- names(data)
        } else {
          data_rv$internal_colnames <- character(0)
        }

        # map mandatory/edit/multiline from provided original names to internal names
        data_rv$mandatory <- data_rv$internal_colnames[which(data_rv$colnames %in% var_mandatory)]
        data_rv$edit <- data_rv$internal_colnames[which(data_rv$colnames %in% var_edit)]

        # determine which of var_multiline are actual character columns using original data
        if (!is.null(var_multiline)) {
          var_multiline <- intersect(var_multiline, var_edit)
          # use original data types
          original_df <- as.data.frame(data_r())
          var_multiline <- var_multiline[sapply(original_df[var_multiline], is.character)]
        }
        data_rv$multiline <- data_rv$internal_colnames[which(data_rv$colnames %in% var_multiline)]

        # build labels mapping (internal names -> labels)
        data_rv$labels <- get_variables_labels(var_labels, data_rv$colnames, data_rv$internal_colnames)

        # add internal helper columns
        n <- nrow(data)
        data$.datamods_id <- seq_len(n)

        if (is.reactive(update)) {
          update <- update()
        }

        if (isTRUE(update)) {
          data$.datamods_edit_update <- lapply(seq_len(n), function(i) btn_update(ns("update"))(i))
        } else {
          data$.datamods_edit_update <- rep(NA_character_, n)
        }

        if (is.reactive(delete)) {
          delete <- delete()
        }

        if (isTRUE(delete)) {
          data$.datamods_edit_delete <- lapply(seq_len(n), function(i) btn_delete(ns("delete"))(i))
        } else {
          data$.datamods_edit_delete <- rep(NA_character_, n)
        }

        data_rv$data <- data
        return(data)
      })


      # Table ---
      output$table <- renderReactable({
        data <- req(data_init_r())
        if (is.reactive(reactable_options))
          reactable_options <- reactable_options()
        table_display(
          data = data,
          colnames = data_rv$colnames,
          reactable_options = reactable_options
        )
      })

      # Retrieve selected row(s)
      selected_r <- reactive({
        getReactableState("table", "selected")
      })


      # Add a row ---
      output$add_button <- renderUI({
        if (is.reactive(add)) {
          add <- add()
        }
        if (isTRUE(add)) {
          actionButton(
            inputId = ns("add"),
            label = tagList(ph("plus"), i18n("Add a row")),
            class = "btn-outline-primary float-end"
          )
        }
      })

      observeEvent(input$add, {
        req(data_r())
        edit_modal(
          default = get_variables_default(
            add_default_values,
            data_rv$colnames,
            data_rv$internal_colnames
          ),
          id_validate = "add_row",
          data = data_rv$data,
          var_edit = data_rv$edit,
          var_mandatory = data_rv$mandatory,
          var_multiline = data_rv$multiline,
          var_labels = data_rv$labels,
          modal_size = modal_size,
          modal_easy_close = modal_easy_close,
          n_column = n_column
        )
      })

      observeEvent(input$add_row, {
        req(data_r())
        data <- data_rv$data

        for (var in data_rv$mandatory) {
          if (!isTruthy(input[[var]])) {
            notification_warning(
              title = i18n("Required field"),
              text = i18n("Please fill in the required fields"),
              use_notify = use_notify
            )
            return(NULL)
          }
        }

        results_add <- try({
          results_inputs <- lapply(
            X = setNames(data_rv$edit, data_rv$edit),
            FUN = function(x) {
              input[[x]] %||% NA
            }
          )

          # build helper columns
          id <- if (nrow(data) == 0) 1L else max(data$.datamods_id, na.rm = TRUE) + 1L
          results_inputs[[".datamods_id"]] <- id
          results_inputs[[".datamods_edit_update"]] <- if (update) list(btn_update(ns("update"))(id)) else NA_character_
          results_inputs[[".datamods_edit_delete"]] <- if (delete) list(btn_delete(ns("delete"))(id)) else NA_character_

          new <- tibble::as_tibble(results_inputs)

          res_callback <- callback_add(
            format_edit_data(data, data_rv$colnames),
            format_edit_data(new, data_rv$colnames, data_rv$internal_colnames)
          )

          if (isTruthy(res_callback) & !isTRUE(only_callback)) {
            data <- dplyr::bind_rows(data, new)
            data_rv$data <- data
            removeModal()
            update_table(data, data_rv$colnames)
          } else {
            NULL
          }
        })

        if (is.null(results_add)) {
          notification_warning(
            title = i18n("Warning"),
            text = i18n("The row wasn't added to the data"),
            use_notify = use_notify
          )
        } else if (inherits(results_add, "try-error")) {
          notification_failure(
            title = i18n("Error"),
            text = i18n("Unable to add the row, contact the platform administrator"),
            use_notify = use_notify
          )
        } else {
          notification_success(
            title = i18n("Registered"),
            text = i18n("Row has been saved"),
            use_notify = use_notify
          )
        }
      })


      # Update a row ---
      observeEvent(input$update, {
        data <- data_rv$data
        idx_row <- which(data$.datamods_id == input$update)
        row <- if (length(idx_row) == 1) data[idx_row, , drop = FALSE] else NULL
        edit_modal(
          default = row,
          title = i18n("Update row"),
          id_validate = "update_row",
          data = data,
          var_edit = data_rv$edit,
          var_mandatory = data_rv$mandatory,
          var_multiline = data_rv$multiline,
          var_labels = data_rv$labels,
          modal_size = modal_size,
          modal_easy_close = modal_easy_close,
          n_column = n_column
        )
      })

      observeEvent(input$update_row, {
        req(data_r())
        data <- data_rv$data

        for (var in data_rv$mandatory) {
          if (!isTruthy(input[[var]])) {
            notification_failure(
              title = i18n("Required field"),
              text = i18n("Please fill in the required fields"),
              use_notify = use_notify
            )
            return(NULL)
          }
        }

        results_update <- try({
          id <- input$update

          data_updated <- data
          idx <- which(data_updated$.datamods_id == id)
          if (length(idx) == 1) {
            for (col in data_rv$edit) {
              val <- input[[col]] %||% NA
              # assign value to the cell; preserve column type if possible
              data_updated[idx, col] <- list(val)
            }
          }

          res_callback <- callback_update(
            format_edit_data(data, data_rv$colnames),
            format_edit_data(
              if (length(idx) == 1) data_updated[idx, , drop = FALSE] else tibble::tibble(),
              data_rv$colnames,
              data_rv$internal_colnames
            )
          )
          if (isTruthy(res_callback) & !isTRUE(only_callback)) {
            data_updated <- dplyr::arrange(data_updated, .datamods_id)
            data_rv$data <- data_updated
            removeModal()
            update_table(data_updated, data_rv$colnames)
          } else {
            NULL
          }
        })
        if (is.null(results_update)) {
          notification_warning(
            title = i18n("Warning"),
            text = i18n("Data wasn't updated"),
            use_notify = use_notify
          )
        } else if (inherits(results_update, "try-error")) {
          notification_failure(
            title = i18n("Error"),
            text = i18n("Unable to modify the item, contact the platform administrator"),
            use_notify = use_notify
          )
        } else {
          notification_success(
            title = i18n("Registered"),
            text = i18n("Item has been modified"),
            use_notify = use_notify
          )
        }
      })


      # Delete a row ---
      observeEvent(input$delete, {
        req(data_r())
        data <- data_rv$data
        row <- data[data$.datamods_id == input$delete, , drop = FALSE]
        removeModal()
        showModal(confirmation_window(
          inputId = ns("confirmation_delete_row"),
          title = i18n("Delete"),
          i18n("Do you want to delete the selected row ?")
        ))
      })
      observeEvent(input$confirmation_delete_row_yes, {
        req(data_r())
        data <- data_rv$data

        results_delete <- try({

          res_callback <- callback_delete(
            format_edit_data(data, data_rv$colnames),
            format_edit_data(
              data[data$.datamods_id == input$delete, , drop = FALSE],
              data_rv$colnames,
              data_rv$internal_colnames
            )
          )

          if (isTruthy(res_callback) & !isTRUE(only_callback)) {
            data <- dplyr::filter(data, .data$.datamods_id != input$delete)
            data <- dplyr::arrange(data, .data$.datamods_id)
            data_rv$data <- data
            removeModal()
            update_table(data, data_rv$colnames)
          } else {
            NULL
          }
        })
        if (is.null(results_delete)) {
          notification_warning(
            title = i18n("Warning"),
            text = i18n("Data wasn't deleted"),
            use_notify = use_notify
          )
        } else if (inherits(results_delete, "try-error")) {
          notification_failure(
            title = i18n("Error"),
            text = i18n("Unable to delete the row, contact platform administrator"),
            use_notify = use_notify
          )
        } else {
          notification_success(
            title = i18n("Registered"),
            text = i18n("The row has been deleted"),
            use_notify = use_notify
          )
        }
      })
      observeEvent(input$confirmation_delete_row_no, {
        notification_info(
          title = i18n("Information"),
          text = i18n("Row was not deleted"),
          use_notify = use_notify
        )
        removeModal()
      })


      # Download data in Excel format ---
      output$download_excel <- renderUI({
        if (is.reactive(download_excel)) {
          download_excel <- download_excel()
        }
        if (isTRUE(download_excel)) {
          downloadButton(
            outputId = ns("export_excel"),
            label = tagList(ph("download"), "Excel"),
            icon = NULL,
            class = "btn-datamods-export"
          )
        }
      })

      output$export_excel <- downloadHandler(
        filename = function() {
          file_name <- file_name_export
          paste0(file_name, ".xlsx")
        },
        content = function(file) {
          data <- format_edit_data(data_rv$data, data_rv$colnames)
          write_xlsx(
            x = list(data = data),
            path = file
          )
        }
      )

      # Download data in csv format ---
      output$download_csv <- renderUI({
        if (is.reactive(download_csv)) {
          download_csv <- download_csv()
        }
        if (isTRUE(download_csv)) {
          downloadButton(
            outputId = ns("export_csv"),
            label = tagList(ph("download"), "CSV"),
            icon = NULL,
            class = "btn-datamods-export"
          )
        }
      })
      output$export_csv <- downloadHandler(
        filename = function() {
          file_name <- file_name_export
          paste0(file_name, ".csv")
        },
        content = function(file) {
          data <- format_edit_data(data_rv$data, data_rv$colnames)
          write.csv(
            x = data,
            file = file
          )
        }
      )


      return(
        reactive({
          req(data_rv$data)
          data <- format_edit_data(data_rv$data, data_rv$colnames)
          setattr(data, "selected", selected_r())
          as_out(data, return_class)
        })
      )

    }
  )
}
