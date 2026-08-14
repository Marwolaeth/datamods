#' Extract labels from R files and update i18n CSVs
#'
#' Utilities to manage translation CSV files stored in inst/i18n.
#'
#' @examples extract_labels(folder = "R")
extract_labels <- function(folder = "R") {
  files <- list.files(folder)
  list_extractions <- lapply(
    X = files,
    FUN = function(file) {
      read_file <- readLines(file.path(folder, file))
      extraction <- stringr::str_extract_all(
        string = stringr::str_subset(read_file, "i18n"),
        pattern = 'i18n\\(".*?"\\)'
      ) |>
        unlist()
      extraction
    }
  )
  extract_labels <- unlist(list_extractions, recursive = TRUE)
  stringr::str_remove_all(unique(extract_labels), paste(c("i18n", "\"", "\\)", "\\("), collapse = "|"))
}


#' Update all csvs that are in inst/i18n
#'
#' @param labels results of label extractions
#' @param lang the language that you want to translate the text into. See polyglotr::google_supported_languages for the Table with the codes of available languages
#' @param lang_csv the name of the csv file
#' @param translation TRUE or FALSE if you want to translate the language
#' @param ... other arguments passed to datamods::translate_labels
#'
#' @return all csvs updated
#' @export
#'
#' @examples update_csv(labels = extract_labels(folder = "R"))
#' new_csv_fr <- readr::read_csv("inst/i18n/fr.csv")
update_csv <- function(labels,
                       lang,
                       lang_csv,
                       translation = TRUE,
                       ...) {
  old_file <- sprintf("inst/i18n/%s.csv", lang_csv)
  old <- tryCatch(
    readr::read_csv(old_file, locale = readr::locale(encoding = "UTF-8"), col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE),
    error = function(e) tibble::tibble(label = character(0), translation = character(0))
  )

  new <- tibble::tibble(label = unique(labels)) %>%
    dplyr::left_join(old, by = "label")

  if (isTRUE(translation)) {
    missing_labels <- new %>% dplyr::filter(is.na(translation)) %>% dplyr::pull(label)
    translated <- if (length(missing_labels) > 0) {
      translate_labels(labels = missing_labels, target_language = lang, ...)
    } else {
      tibble::tibble(label = character(0), translation = character(0), comment = character(0))
    }

    final <- dplyr::bind_rows(
      new %>% dplyr::filter(!is.na(translation)),
      translated,
      .id = NULL
    )
  } else {
    final <- new
  }

  # write CSV
  readr::write_csv(final, file = old_file, na = "")
}


#' Translate labels
#'
#' @param labels labels to translate
#' @param source_language the language that you want to translate the text into. See polyglotr::google_supported_languages for the Table with the codes of available languages
#' @param target_language the language of the text that you want to translate. See polyglotr::google_supported_languages for the Table with the codes of available languages
#' @param encoding Name of encoding. See stringi::stri_enc_list() for a complete list
#'
#' @return a tibble with translated labels
#' @export
#'
#' @examples translate_labels(labels = extract_labels(folder = "R"))
translate_labels <- function(labels,
                             source_language = "en",
                             target_language = "fr",
                             encoding = "UTF-8") {

  translation <- polyglotr::google_translate(
    text = labels,
    target_language = target_language,
    source_language = source_language
  )
  tibble::tibble(
    label = labels,
    translation = unlist(translation) %>% stringi::stri_conv(encoding),
    comment = "Automatically translated"
  )
}
