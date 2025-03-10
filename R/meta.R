
# meta --------------------------------------------------------------------



#' unpackx
#'
#' @noRd
#'
#' @export
unpackx = function(x_, expr_, ...) {

  expr_ = substitute(expr_)

  withIn(expr_, list2env(x_), ...)

}

#' fxx
#'
#' @noRd
#'
#' @export
fxx = function(x, f) {

  f = rlang::as_function(f)
  f(x)

}

#' reparse_expr
#'
#' @noRd
#'
#' @export
reparse_expr = function(expr) {

  if (length(expr) == 1) {
    expr = as.character(expr)
    expr = sprintf('x |> %s()', expr)
  } else {
    name_par = names(expr)[[2]]
    name_par = ifelse(nchar(name_par) > 0, name_par %s% '=', NULL)
    expr = as.character(expr)
    expr = sprintf('x |> %s(%s)', expr[[1]], name_par %s% expr[[2]])
  }

  return(parse(text = expr))

}

#' hang
#'
#' @noRd
#'
#' @export
hang = function(x, expr, ...) {

  expr = substitute(expr)
  expr = reparse_expr(expr)

  feas = as.character(substitute(list(...)))[-1]
  for (fea in feas) {

    assign(fea, x[[fea]])
    x[[fea]] = NULL

  }

  eval(expr)

}

#' branch
#'
#' @noRd
#'
#' @export
branch = function(x, expr, ...) {

  expr = substitute(expr)
  expr = reparse_expr(expr)

  lst_res = list(...) |> map(rlang::as_function) |> map(~ .x(x))

  env_param = list2env(lst_res)
  parent.env(env_param) = environment()

  eval(expr, envir = env_param)

}
