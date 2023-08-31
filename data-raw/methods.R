## code to prepare `methods` dataset goes here

methods <- ls(asNamespace("dbplyr"), pattern = "\\.tbl_lazy$|\\.tbl_sql$")
generics <- sub("\\.tbl_lazy$|\\.tbl_sql$", "", methods)

# generics from various packages -----------------------------------------------
# we probably don't care about those
dbplyr_generics <- sort(intersect(getNamespaceExports("dbplyr"), generics))
dbplyr_generics


dplyr_generics <- sort(intersect(getNamespaceExports("dplyr"), generics))
dplyr_generics




tidyr_generics <- sort(intersect(getNamespaceExports("tidyr"), generics))
tidyr_generics



usethis::use_data(methods, overwrite = TRUE)


collapse_formals_transformer <- function(text, envir) {
  regex <- "[*]$"
  collapse <- grepl(regex, text)
  if (!collapse) return(get(text, envir))
  text <- sub(regex, "", text)
  browser()
  code <- deparse(get(text, envir))
  args <- sub("as.pairlist\(alist\(.data = , ... = \)\)")
  text <- lapply( deparse)
  glue_collapse(text, sep = ", ")
}



template_dots <- '
{
  mc <- match.call(expand.dots = FALSE)
  mc[[1]] <- quote(<<generic>>)
  mc[[2]] <- <<arg1>>
  for (arg in setdiff(names(mc[-(1:2)]), "...")) {
    mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
  }
  if ("..." %in% names(mc)) {
    pos <- match("...", names(mc))
    mc <- as.call(append(as.list(mc), rlang::enquos(...), after = pos))
    mc$... <- NULL
  }
  structure(mc, class = "alligator")
}'

template_no_dots <- '
{
  mc <- match.call(expand.dots = FALSE)
  mc[[1]] <- quote(<<generic>>)
  mc[[2]] <- <<arg1>>
  for (arg in names(mc[-(1:2)])) {
    mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
  }
  structure(mc, class = "alligator")
}'
library(tidyverse)
library(glue)

for (generic in setdiff(c(dplyr_generics, tidyr_generics), "collect")) {
  fmls <- formals(get(generic))
  obj <- get(generic)
  fml_names <- names(formals(obj))

  body_chr <- glue(
    if ("..." %in% fml_names) template_dots else template_no_dots,
    .envir = list(
      generic = generic,
      arg1 = fml_names[[1]]
    ),
    .open = "<<",
    .close = ">>"
  )
  body(obj) <- parse(text = body_chr)
  code <- deparse(obj)
  code[[1]] <- sprintf("%s.alligator <- %s", generic, code[[1]])
  code <- c("#' @export", code)
  cat(c(code, "", ""), sep = "\n", file = "R/methods.R", append = TRUE)
}
glue(template)
