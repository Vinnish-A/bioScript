
# import ------------------------------------------------------------------

#' buildModule
#'
#' @keywords internal
buildModule = function(builded_, env_ = NULL) {

  if (is.list(builded_)) {

    env_node_ = new.env()
    if (is.environment(env_)) {
      parent.env(env_node_) = env_
    }

    text_ = builded_[['head']]
    if (is.null(text_)) text_ = ''
    eval(parse(text = text_), envir = env_node_)
    builded_[['head']] = NULL

    for (i_ in seq_along(builded_)) {

      env_node_[[names(builded_)[[i_]]]] = buildEnv(builded_[[i_]], env_node_)

    }

  } else {

    env_leaf_ = new.env()
    if (is.environment(env_)) parent.env(env_leaf_) = env_
    eval(parse(text = builded_), envir = env_leaf_)

    return(as.list(env_leaf_))

  }

  return(as.list(env_node_))

}

#' Import an R File or Module
#'
#' This function imports an R script file into the current session, optionally converting it into a module structure.
#'
#' @param file A character string specifying the path to the R script file to be imported.
#' @param modulize A logical value indicating whether to convert the file into a module structure.
#' If `TRUE`, the function uses `buildModule` to process the imported file.
#' Defaults to `FALSE`.
#'
#' @return A list representation of the imported file's environment. If `modulize = TRUE`, the list
#' represents the module structure created by `buildModule`.
#'
#' @examples
#' # Import an R script without modulizing
#' env = import(fileWhenTest('import.R'))
#'
#' # Import an R script and convert it into a module
#' module = import(fileWhenTest('import.R'), modulize = TRUE)
#'
#' @export
import = function(file, modulize = F) {

  if (modulize) {

    module = buildModule(buildFile(file))

    return(module)


  } else {

    env = new.env()
    source(file, local = env)

    return(as.list(env))

  }

}

