#' @export
collect.quosure <- function(x) {
  rlang::eval_tidy(x, parent.frame())
}

format_query <- function(x) {
  if (!rlang::is_quosure(x)) {
    #message("do we even get there?")
    return(rlang::expr_deparse(x))
  }
  expr <- rlang::quo_squash(x)
  env <- rlang::quo_get_env(x)
  is_call_to_qplyr <-
    is.call(expr) &&
    rlang::expr_deparse(expr[[1]]) %in% generics

  if (!is_call_to_qplyr) {
    code <- sprintf(
      "%s # %s",
      rlang::expr_deparse(expr),
      rlang::env_label(env)
    )
    return(code)
  }
  x <- rlang::quo_get_expr(x)
  code <- sapply(x, format_query)
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
    first, code[[1]], "( # ", rlang::env_label(env), "\n",
    paste(args, collapse = "\n"),
    "\n)"
  )

  code
}

#' @export
show_query.quosure <- function(x, ...) {
  print(styler::style_text(format_query(x)))
}
