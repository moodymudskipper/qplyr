generics <- sub("\\.quosure$", "", ls(pattern = "\\.quosure$"))

.onLoad <- function(...) {
  # to avoid warning we need to do this onload and not @export our filter method
  registerS3method("filter", "quosure", filter.quosure)
}
