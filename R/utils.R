
#' cat_red
#'
#' @description
#' Work like `cat`.
#'
#' @export
cat_red = function(..., sep = ' ') {

  text_ = paste(..., sep = sep)
  cat(paste0("\033[31m", text_, "\033[0m\n"))

}

#' cat_green
#'
#' @description
#' Work like `cat`.
#'
#' @export
cat_green = function(..., sep = ' ') {

  text_ = paste(..., sep = sep)
  cat(paste0("\033[32m", text_, "\033[0m\n"))

}

#' cat_yellow
#'
#' @description
#' Work like `cat`.
#'
#' @export
cat_yellow = function(..., sep = ' ') {

  text_ = paste(..., sep = sep)
  cat(paste0("\033[33m", text_, "\033[0m\n"))

}

#' Generic select_lst function
#'
#' This is a generic function for selecting elements from a list.
#'
#' @param .data The input data, typically a list.
#' @param ... Additional arguments passed to methods.
#'
#' @export
select_lst = function (.data, ...) {
  UseMethod("select_lst")
}

#' Method for select_lst with list
#'
#' This method applies a filtering function to a list and returns a subset of the list.
#'
#' @param .data A list to be filtered.
#' @param fun_ A function that returns a single logical value for each element of the list.
#'
#' @return A subset of the list where `fun_` evaluates to `TRUE`.
#'
#' @export
#' @method select_lst list
select_lst.list = function(.data, fun_) {

  is_logic_ = \(x_) is.logical(x_) & length(x_) == 1

  logics_ = lapply(.data, fun_)

  if (all(sapply(logics_, is_logic_))) return(.data[unlist(logics_)])

  stop('fun_ not suitable')

}

#' vec2copy
#'
#' @export
vec2copy = function(vec_) {

  if (is.numeric(vec_)) {
    cat(sprintf('c(%s)', paste(vec_, collapse = ', ')), '\n')
  } else if (is.character(vec_)) {
    cat(sprintf('c(\'%s\')', paste(vec_, collapse = "', '")), '\n')
  }

}

#' docal
#'
#' @export
docal = function(x_, fun_, ...) {

  params_ = lapply(as.list(match.call())[-c(1:3)], \(x__) eval(x__, envir = x_))
  res_ = do.call(fun_, params_)

  return(res_)

}


#' findGit
#'
#' @keywords internal
findGit = function() {

  path_ = getwd()
  while (!file.exists(file.path(path_, '.git'))) {
    parent_ = dirname(path_)
    if (parent_ == path_) {
      stop('No .git directory found')
    }
    path_ = parent_
  }

  return(path_)

}

#' fileWhenTest
#'
#' @importFrom pkgload is_dev_package
#'
#' @export
fileWhenTest = function(file_, project_ = 'bioScript') {

  if (is_dev_package(project_)) {
    filename_ = file.path(findGit(), 'inst/extdata', file_)
  } else {
    filename_ = system.file('extdata', file_, package = project_)
  }

  return(filename_)

}



