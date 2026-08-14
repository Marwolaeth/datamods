test_that("edit_data_ui works", {
  ui <- edit_data_ui("ID")
  # ensure ui contains a reactableOutput placeholder with expected id
  expect_true(grepl("table", as.character(ui)))
})


test_that("table_display works", {
  mydata_df <- as.data.frame(iris)

  mydata_df$.datamods_edit_update <- as.character(seq_len(nrow(mydata_df)))
  mydata_df$.datamods_edit_delete <- as.character(seq_len(nrow(mydata_df)))
  mydata_df$.datamods_id <- seq_len(nrow(mydata_df))
  widget <- table_display(mydata_df, colnames = NULL)

  cols <- widget$x$tag$attribs$columns
  # number of column definitions should match input columns
  expect_equal(length(cols), ncol(mydata_df))
  # and the column names should match (order may differ in reactable internals)
  expect_setequal(names(cols), names(mydata_df))
})


test_that("col_def_update works", {
  col_def_update <- col_def_update()
  expect_equal(col_def_update$name, "Update")
  expect_named(col_def_update, c('name', 'sortable', 'filterable', 'html', 'width'))
})


test_that("col_def_delete works", {
  col_def_delete <- col_def_delete()
  expect_equal(col_def_delete$name, "Delete")
  expect_named(col_def_delete, c('name', 'sortable', 'filterable', 'html', 'width'))
})


test_that("btn_update works", {
  f <- btn_update("input")
  expect_is(f, "function")
  rendered <- f(1)
  # should contain the Shiny.setInputValue call and the provided input id
  expect_true(grepl("Shiny.setInputValue", rendered))
  expect_true(grepl("input", rendered))
})


test_that("btn_delete works", {
  f <- btn_delete("input")
  expect_is(f, "function")
  rendered <- f(1)
  expect_true(grepl("Shiny.setInputValue", rendered))
  expect_true(grepl("input", rendered))
})


test_that("confirmation_window works", {
  win <- confirmation_window(inputId = "input", title = "titre")
  # render as character and check the title is present
  rendered <- as.character(win)
  expect_true(grepl("titre", rendered))
})
