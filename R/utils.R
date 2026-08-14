%||% <- function(x, y) {
  if (is.null(x))
    y
  else x
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}

nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0 || x == ""
}

#' Remove list columns from a data.frame / tibble
#'
#' This used to handle data.table specially; in the tidy rewrite we operate
#' on data.frame/tibble inputs only. Columns of type "list" are dropped.
#'
#' @param x data.frame or tibble
#' @return data.frame / tibble without list columns
#' @noRd
#' @examples
#' dropListColumns(tibble::tibble(a = 1:3, b = list(1,2,3)))
dropListColumns <- function(x) {
  type_col <- vapply(
    X = x,
    FUN = typeof,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  x[, type_col != "list", drop = FALSE]
}


#' Search for object with specific class in an environment
#'
#' @param what a class to look for
#' @param env An environment
#'
#' @return Character vector of the names of objects, NULL if none
#' @noRd
#'
#' @examples
#'
#' # NULL if no data.frame
#' search_obj("data.frame")
#'
#' library(ggplot2)
#' data("mpg")
#' search_obj("data.frame")
#'
#'
#' gg <- ggplot()
#' search_obj("ggplot")
#'
search_obj <- function(what = "data.frame", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if (inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    }
  )
  objs <- unlist(objs)
  if (length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}




#' Convert outputs to requested class with a compatibility wrapper
#'
#' The package now works with tibbles by default. A temporary compatibility
#' option `return_class = "data.table"` is accepted for callers but will
#' only produce a data.table if the data.table package is installed; otherwise
#' a data.frame is returned and a warning is emitted. Prefer `tbl_df`.
#'
#' @param x object to coerce
#' @param return_class one of "data.frame", "data.table", "tbl_df", "raw"
#' @noRd
#' @examples
#' as_out(iris, "tbl_df")
#' as_out(iris, "data.table")
as_out <- function(x, return_class = c("data.frame", "data.table", "tbl_df", "raw")) {
  if (is.null(x))
    return(NULL)
  return_class <- match.arg(return_class)
  if (identical(return_class, "raw"))
    return(x)
  is_sf <- inherits(x, "sf")

  out <- NULL
  if (identical(return_class, "data.frame")) {
    out <- as.data.frame(x)
  } else if (identical(return_class, "data.table")) {
    # we do not import data.table; if it's installed use it, otherwise
    # fall back to data.frame and warn the user
    if (requireNamespace("data.table", quietly = TRUE)) {
      out <- data.table::as.data.table(x)
    } else {
      warning("data.table is not available; returning a data.frame instead", call. = FALSE)
      out <- as.data.frame(x)
    }
  } else {
    out <- tibble::as_tibble(x)
  }

  if (is_sf)
    class(out) <- c("sf", class(out))
  return(out)
}


genId <- function(bytes = 12) {
  paste(format(as.hexmode(sample(256, bytes, replace = TRUE) - 1), width = 2), collapse = "")
}

makeId <- function(x) {
  if (length(x) < 1)
    return(NULL)
  x <- as.character(x)
  x <- lapply(X = x, FUN = function(y) {
    paste(as.character(charToRaw(y)), collapse = "")
  })
  x <- unlist(x, use.names = FALSE)
  make.unique(x, sep = "_")
}


%inT% <- function(x, table) {
  if (!is.null(table) && ! "" %in% table) {
    x %in% table
  } else {
    rep_len(TRUE, length(x))
  }
}



%inF% <- function(x, table) {
  if (!is.null(table) && ! "" %in% table) {
    x %in% table
  } else {
    rep_len(FALSE, length(x))
  }
}

#' @importFrom utils hasName
header_with_classes <- function(data) {
  function(value) {
    if (!hasName(data, value))
      return("")
    classes <- tags$div(
      style = "font-style: italic; font-weight: normal; font-size: small;",
      get_classes(data[, value, drop = FALSE])
    )
    tags$div(title = value, value, classes)
  }
}


split_char <- function(x, split = ",") {
  if (is.null(x))
    return(NULL)
  unlist(strsplit(x, split = split))
}



apply_grid_theme <- function() {
  toastui::set_grid_theme(
    cell.normal.background = "#FFF",
    cell.normal.border = "#D8DEE9",
    cell.normal.showVerticalBorder = TRUE,
    cell.normal.showHorizontalBorder = TRUE,
    cell.header.border = "#D8DEE9",
    area.header.border = "#4C566A",
    cell.summary.border = "#D8DEE9",
    cell.summary.showVerticalBorder = TRUE,
    cell.summary.showHorizontalBorder = TRUE
  )
}
