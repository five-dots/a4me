
#' Assert input is a character scalar (string)
#'
#' @param string A character scalar.
#' @param allow_null A logical scalar. Is NULL allowed ?
#' @return same as input
#' @export
a_str <- function(string = NULL, allow_null = FALSE) {
  allow_null <- a_flag(allow_null)
  if (allow_null && is.null(string)) return(NULL)
  arg_name <- .get_first_arg_name(a_str)
  if (is.null(string))
    stop(arg_name, " must not be NULL.", call. = FALSE)
  if (!rlang::is_string(string))
    stop(arg_name, " must be a character scalar.", call. = FALSE)
  string
}

#' Assert input is a positive integer scalar (count)
#'
#' @param count A positive integer scalar.
#' @inheritParams a_str
#' @return same as input
#' @export
a_count <- function(count = NULL, allow_null = FALSE) {
  allow_null <- a_flag(allow_null)
  if (allow_null && is.null(count)) return(NULL)
  arg_name <- .get_first_arg_name(a_count)
  if (is.null(count))
    stop(arg_name, " must not be NULL.", call. = FALSE)
  if (anyNA(count) || !rlang::is_scalar_integerish(count) || count < 1)
    stop(arg_name, " must be a positive integer scalar.", call. = FALSE)
  count
}

#' Assert input is a logical scalar (flag)
#'
#' @param flag A logical scalar.
#' @return same as input
#' @export
a_flag <- function(flag = NULL) {
  arg_name <- .get_first_arg_name(a_flag)
  if (is.null(flag))
    stop(arg_name, " must not be NULL.", call. = FALSE)
  if (any(is.na(flag)) || !rlang::is_bool(flag))
    stop(arg_name, " must be a logical scalar.", call. = FALSE)
  flag
}

#' Assert input is a writable directory
#'
#' @param dir A charactor scalar of directory.
#' @inheritParams a_str
#' @return same as input
#' @export
a_dir <- function(dir = NULL, allow_null = FALSE) {
  allow_null <- a_flag(allow_null)
  if (allow_null && is.null(dir)) return(NULL)
  arg_name <- .get_first_arg_name(a_dir)
  if (is.null(dir))
    stop(arg_name, " must not be NULL.", call. = FALSE)
  a_str(dir)
  if (!fs::is_dir(dir))
    stop(dir, " is not direcotry.", call. = FALSE)
  if (!assertthat::is.writeable(dir))
    stop(dir, " is not writeable", call. = FALSE)
  dir
}

#' Assert input object class
#'
#' @param object Any object to be tested for.
#' @param class A character scalar.
#' @inheritParams a_str
#' @return same as input
#' @export
a_class <- function(object = NULL, class = NULL, allow_null = FALSE) {
  class <- a_str(class)
  allow_null <- a_flag(allow_null)
  if (allow_null && is.null(object)) return(NULL)
  arg_name <- .get_first_arg_name(a_class)
  if (!inherits(object, class))
    stop(arg_name, " must be a ", class, " class.", call. = FALSE)
  object
}
