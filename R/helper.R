
.get_first_arg_name <- function(fun) {
  call <- sys.call(which = -1)
  if (length(call) > 1 && is.symbol(call[[2]])) {
    rlang::as_string(call[[2]])
  } else {
    names(formals(fun))[1]
  }
}
