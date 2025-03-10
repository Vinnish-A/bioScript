#' load_pkgs
#'
#' @export
load_pkgs = function(pkgs_) {

  for (pkg_ in pkgs_) {
    failed_ = suppressMessages(
      tryCatch(
        expr = {
          library(pkg_, character.only = T)
          NULL
        },
        error = function(e) {
          pkg_
        }
      )
    )
  }

  loaded_ = setdiff(pkgs_, failed_)

  if (length(failed_) > 0) {
    cat(c('Loaded: ', loaded_, 'Failed: ', failed_), sep = '\n')
  } else {
    cat(c('Loaded: ', loaded_), sep = '\n')
  }

}

#' capitalize
#'
#' Uppercase the first letter of each word in a sentence.
#'
#' @export
capitalize = function(sentence_) {
  gsub("\\b([a-z])", "\\U\\1", sentence_, perl = TRUE)
}


#' findFile
#'
#' Find the files containing the keywords and print out their paths.
#'
#' @param keyWord_
#' @param path_
#' @param pattern_
#'
#' @importFrom purrr map walk
#' @importFrom cli cli_text
#'
#' @export
findFile = function(keyWord_, path_ = ".", pattern_ = ".R$") {

  files_ = list.files(path_, pattern = pattern_, recursive = T, full.names = T)

  where_ = files_ |>
    map(~ paste0(readLines(.x), collapse = "")) |>
    map(~ grepl(pattern = keyWord_, .x)) |>
    unlist() |>
    which() |>
    suppressWarnings()

  files_ = files_[where_]
  walk(files_, ~ cli_text("{.href [{.x}](file://{.x})}."))

}

#' is_dev_package
#'
#' @export
is_dev_package = function(pkg_) {

  return(pkgload::is_dev_package(pkg_))

}
