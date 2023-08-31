#' @export
alligator <- function(x) {
  structure(rlang::enquo(x), class = c("alligator", "quosure", "formula"))
}

#' @export
is_alligator <- function(x) {
  inherits(x, "alligator")
}

#' @export
collect.alligator <- function(x) {
  rlang::eval_tidy(x)
}

#' @export
format.alligator <- function(x) {
  if (rlang::is_quosure(x)) {
    code <- sprintf(
      "%s # %s",
      rlang::expr_deparse(rlang::quo_squash(x)),
      rlang::env_label(rlang::quo_get_env(x))
    )
    return(code)
  }
  if (!is_alligator(x)) return(format(x))
  alligators <- sapply(x, is_alligator)
  if (length(x) > 2 || any(alligators)) {
    code <- sapply(x, format.alligator)
    nms <- rlang::names2(x)
    nms[[2]] <- ""
    code <- ifelse(nms == "", code, paste(nms, "=", code))
    first <- code[[2]]
    if (grepl("#[^\"'#\n]+$", first)) {
      first <- sub("(.*)(#[^\"'#\n]+)$", "\\1 |> \\2\n", first)
    } else {
      first <- paste0(first, " |>\n")
    }
    args <- code[-(1:2)]
    args[-length(args)] <- paste0(args[-length(args)], ",")
    args <- sub("(.*)(#[^\"'#\n]+),$", "\\1, \\2", args)
    code <- paste0(
      first, code[[1]], "(\n",
      paste(args, collapse = "\n"),
      "\n)"
    )
    code
  } else {
    code <- paste(deparse(x), collapse = "\n")
  }
  if (length(code) > 1) browser()
  code
}

#' @export
print.alligator <- function(x, ...) {
  print(styler::style_text(format(x)))
}
