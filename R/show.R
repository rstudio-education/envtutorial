#' show_env
#'
#' show_env returns a list that contains two environments: calling, which is the
#' calling environment when you run show_env, and execution, which is the
#' execution environment when you run show_env.
#'
#' @export
show_env <- function() {
  call <- parent.frame()
  exec <- environment()
  list(calling = call, execution = exec)
}

#' show_envs
#'
#' show_envs returns a list that contains three environments: calling, which is
#' the calling environment when you run show_env; execution, which is the
#' execution environment when you run show_env; and enclosing, which is the
#' enclosing environment of show_envs.
#'
#' @export
show_envs <- function() {
  call <- parent.frame()
  exec <- environment()
  encl <- parent.env(exec)
  list(calling = call, execution = exec, enclosing = encl)
}

#' show_stack
#'
#' @param e
#'
#' show_stack returns an environment list that contains the labels of each
#' environment that is associated with the call stack that led to show_stack
#' being run. In other words, show_stack returns the execution environment of
#' show_stack as well as the execution environment of the function that called
#' show_stack, the function that called that function and so on.
#'
#' @export
show_stack <- function (e = parent.frame()) {
  if (!is.environment(e))
    e <- environment(e)
  if (is.null(e))
    return(NULL)
  envs <- list(environment(), e)
  n <- 2
  while (TRUE) {
    if (identical(e, globalenv()))
      break
    e <- parent.frame(n)
    n <- n + 1
    envs <- c(envs, e)
  }
  pryr::as.envlist(envs)
}

#' show_paths
#'
#' show_paths returns a list of two environment lists for comparison.
#'
#' call_stack contains an environment list that contains the labels of each
#' environment that is associated with the call stack that led to show_stack
#' being run. In other words, call_stack shows the execution environment of
#' show_paths as well as the execution environment of the function that called
#' show_paths, the function that called that function and so on.
#'
#' search_path contains a list of every envrironment on the search path that
#' begins with the execution environment of show_paths. This is the path that R
#' would use to find objects called by show_path.
#'
#' @export
show_paths <- function() {
  stack <- show_stack()
  search <- pryr::parenvs(all = TRUE)
  list(call_stack = stack, search_path = search)
}
