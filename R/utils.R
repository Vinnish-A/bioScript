
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

#' fxx
#'
#' @noRd
#'
#' @export
ifeltor = function(test, yes, no) {

  fcall = match.call()
  lst_param = as.list(as.list(fcall)[-1])
  vec_level = c(yes, no)
  env = parent.frame()
  vec_res = do.call(ifelse, lst_param, envir = env)

  res = droplevels(factor(vec_res, levels = vec_level))

  return(res)

}

#' %s%
#'
#' @noRd
#'
#' @export
`%s%` = function(a, b) {
  paste0(a, b)
}

#' %S%
#'
#' @noRd
#'
#' @export
`%S%` = `%s%`

#' %p%
#'
#' @noRd
#'
#' @export
`%p%` = function(a, b) {

  normalizePath(file.path(a, b), mustWork = F)

}

#' %P%
#'
#' @noRd
#'
#' @export
`%P%` = `%p%`

#' @export
`%In%` = `%in%`

#' @export
`%IN%` = `%in%`

#' @export
`%||%` = rlang::`%||%`

#' p2sig
#'
#' @noRd
#'
#' @export
p2sig = function(vec, label = 'text') {

  match.arg(label, c('text', 'significance'))

  if (label == 'significance') {

    case_when(
      vec < 0.001 ~ '***',
      vec < 0.01 ~ '**',
      vec < 0.05 ~ '*',
      T ~ '-'
    )

  } else {

    case_when(
      vec < 0.001 ~ '< 0.001',
      vec < 0.01 ~ '< 0.01',
      vec < 0.05 ~ '< 0.05',
      T ~ as.character(signif(vec, 3))
    )

  }

}

#' select_iterable
#'
#' @noRd
#'
#' @export
select_iterable = function(iterable, f) {

  iterable[map_lgl(iterable, f)]

}

#' appendWithName
#'
#' @noRd
#'
#' @export
appendWithName = function(lst_, ...) {

  lst_appending_ = list(...)
  for (i_ in seq_along(lst_appending_)) {

    name_ = names(lst_appending_)[[i_]]
    value_ = lst_appending_[[i_]]

    lst_[[name_]] = value_

  }

  return(lst_)

}

