## Function sample_n()

#' @title Sample rows
#'
#' @description The `sample_n` function returns a sample of rows from a dataset.
#'
#' @param data `data.frame` or tibble
#' @param n integer number of rows to sample
#'
#' @return a tibble (tbl_df) with the sampled rows
#'
#' @noRd
#'
#' @examples
#' sample_n(iris, 25)
sample_n <- function(data, n) {
  data <- as.data.frame(data)
  total <- nrow(data)
  if (is.null(total) || total == 0) return(as_out(data, "tbl_df"))
  # guard: if n is greater than available rows, take up to available rows
  n <- min(as.integer(n), total)
  sampled <- dplyr::slice_sample(dplyr::as_tibble(data), n = n)
  as_out(sampled, "tbl_df")
}


## Function sample_prop()

#' @title Sample percentage
#'
#' @description The `sample_prop` function returns a sample of rows from a
#'  dataset based on the percentage chosen by the user.
#'
#' @param data `data.frame` or tibble
#' @param prop numeric percentage (0-100)
#'
#' @return a tibble (tbl_df) with the sampled rows
#'
#' @noRd
#'
sample_prop <- function(data, prop) {
  data <- as.data.frame(data)
  total <- nrow(data)
  if (is.null(total) || total == 0) return(as_out(data, "tbl_df"))
  # ensure prop is between 0 and 100
  prop <- as.numeric(prop)
  prop <- max(0, min(100, prop))
  size <- floor(total * (prop / 100))
  # ensure at least 1 row if prop > 0 and total > 0
  if (prop > 0 && size < 1) size <- 1
  sampled <- dplyr::slice_sample(dplyr::as_tibble(data), n = size)
  as_out(sampled, "tbl_df")
}


## Function sample_ui()

#' @title Shiny module to interactively sample a `data.frame`
#'
#' @description Allow to take a sample of `data.frame` for a given number or proportion of rows to keep.
#'
#' @param id Module id. See [shiny::moduleServer()].
#'
#' @return
#' * UI: HTML tags that can be included in shiny's UI
#' * Server: a `reactive` fgunction with the sampled data.
#'
#' @export
#'
#' @name module-sample
#'
#' @importFrom htmltools tagList
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shiny NS conditionalPanel sliderInput uiOutput
#'
#' @example examples/sample.R
sample_ui <- function(id) {
  ns <- NS(id)

  tagList(
    radioGroupButtons(
      inputId = ns("choice"),
      label = i18n("Sample data by :"),
      choiceNames = c(i18n("number of rows"), i18n("proportion of rows")),
      choiceValues = c("number of rows", "proportion of rows"),
      justified = TRUE,
      size = "xs",
      width = "100%"
    ),

    conditionalPanel(
      condition = "input.choice == `proportion of rows`",
      ns = ns,
      sliderInput(
        inputId = ns("proportion_rows"),
        label = i18n("Choose a percentage :"),
        min = 0,
        max = 100,
        value = 100,
        post = " %",
        width = "100%"
      ),
      uiOutput(outputId = ns("feedback_proportion_rows"))
    ),

    conditionalPanel(
      condition = "input.choice == `number of rows`",
      ns = ns,
      sliderInput(
        inputId = ns("number_rows"),
        label = i18n("Choose a number of rows :"),
        min = 0,
        max = 10,
        value = 10,
        width = "100%"
      ),
      uiOutput(outputId = ns("feedback_number_rows"))
    )
  )
}


## Function sample_server()

#' @param data_r `reactive` containing a `data.frame` to use in the module.
#'
#' @export
#'
#' @rdname module-sample
#'
#' @importFrom shiny moduleServer observeEvent updateSliderInput renderUI reactive
#' @importFrom htmltools tags div
#'
sample_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(data_r(), {
        req(data_r())
        updateSliderInput(
          session,
          inputId = "number_rows",
          min = 0,
          max = nrow(data_r()),
          value = nrow(data_r())
        )
      })

      output$feedback_proportion_rows <- renderUI({
        sample <- req(sample_r())
        tags$div(
          paste(input$proportion_rows, i18n("% of the total, i.e."), nrow(sample), i18n("rows"))
        )
      })

      output$feedback_number_rows <- renderUI({
        data <- req(data_r())
        tags$div(
          paste(input$number_rows, i18n("lines, i.e."), round(input$number_rows / nrow(data) * 100, 1), i18n("% of the total"))
        )
      })

      sample_r <- reactive({
        req(data_r())
        if (input$choice == "proportion of rows") {
          table_sample <- sample_prop(data = data_r(), prop = input$proportion_rows)
        } else {
          table_sample <- sample_n(data = data_r(), n = input$number_rows)
        }
        return(table_sample)
      })

      return(sample_r)
    }
  )
}
