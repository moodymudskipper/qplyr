#' @export
add_count.alligator <- function (x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = deprecated())
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(add_count)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
anti_join.alligator <- function (x, y, by = NULL, copy = FALSE, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(anti_join)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
arrange.alligator <- function (.data, ..., .by_group = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(arrange)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
auto_copy.alligator <- function (x, y, copy = FALSE, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(auto_copy)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
collapse.alligator <- function (x, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(collapse)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
compute.alligator <- function (x, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(compute)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
count.alligator <- function (x, ..., wt = NULL, sort = FALSE, name = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(count)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
cross_join.alligator <- function (x, y, ..., copy = FALSE, suffix = c(".x", ".y"))
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(cross_join)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
distinct.alligator <- function (.data, ..., .keep_all = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(distinct)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
do.alligator <- function (.data, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(do)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
explain.alligator <- function (x, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(explain)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
filter.alligator <- function (.data, ..., .by = NULL, .preserve = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(filter)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
full_join.alligator <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),
    ..., keep = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(full_join)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
group_by.alligator <- function (.data, ..., .add = FALSE, .drop = group_by_drop_default(.data))
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(group_by)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
group_by_drop_default.alligator <- function (.tbl)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(group_by_drop_default)
    mc[[2]] <- .tbl
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    structure(mc, class = "alligator")
}


#' @export
group_size.alligator <- function (x)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(group_size)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    structure(mc, class = "alligator")
}


#' @export
group_vars.alligator <- function (x)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(group_vars)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    structure(mc, class = "alligator")
}


#' @export
groups.alligator <- function (x)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(groups)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    structure(mc, class = "alligator")
}


#' @export
inner_join.alligator <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),
    ..., keep = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(inner_join)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
intersect.alligator <- function (x, y, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(intersect)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
left_join.alligator <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),
    ..., keep = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(left_join)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
mutate.alligator <- function (.data, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(mutate)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
n_groups.alligator <- function (x)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(n_groups)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    structure(mc, class = "alligator")
}


#' @export
pull.alligator <- function (.data, var = -1, name = NULL, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(pull)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
relocate.alligator <- function (.data, ..., .before = NULL, .after = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(relocate)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
rename.alligator <- function (.data, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(rename)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
rename_with.alligator <- function (.data, .fn, .cols = everything(), ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(rename_with)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
right_join.alligator <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),
    ..., keep = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(right_join)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
rows_append.alligator <- function (x, y, ..., copy = FALSE, in_place = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(rows_append)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
rows_delete.alligator <- function (x, y, by = NULL, ..., unmatched = c("error", "ignore"),
    copy = FALSE, in_place = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(rows_delete)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
rows_insert.alligator <- function (x, y, by = NULL, ..., conflict = c("error", "ignore"),
    copy = FALSE, in_place = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(rows_insert)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
rows_patch.alligator <- function (x, y, by = NULL, ..., unmatched = c("error", "ignore"),
    copy = FALSE, in_place = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(rows_patch)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
rows_update.alligator <- function (x, y, by = NULL, ..., unmatched = c("error", "ignore"),
    copy = FALSE, in_place = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(rows_update)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
rows_upsert.alligator <- function (x, y, by = NULL, ..., copy = FALSE, in_place = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(rows_upsert)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
same_src.alligator <- function (x, y)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(same_src)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    structure(mc, class = "alligator")
}


#' @export
select.alligator <- function (.data, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(select)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
semi_join.alligator <- function (x, y, by = NULL, copy = FALSE, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(semi_join)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
setdiff.alligator <- function (x, y, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(setdiff)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
show_query.alligator <- function (x, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(show_query)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
slice.alligator <- function (.data, ..., .by = NULL, .preserve = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(slice)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
slice_head.alligator <- function (.data, ..., n, prop, by = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(slice_head)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
slice_max.alligator <- function (.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE,
    na_rm = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(slice_max)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
slice_min.alligator <- function (.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE,
    na_rm = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(slice_min)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
slice_sample.alligator <- function (.data, ..., n, prop, by = NULL, weight_by = NULL, replace = FALSE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(slice_sample)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
slice_tail.alligator <- function (.data, ..., n, prop, by = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(slice_tail)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
summarise.alligator <- function (.data, ..., .by = NULL, .groups = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(summarise)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
tally.alligator <- function (x, wt = NULL, sort = FALSE, name = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(tally)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    structure(mc, class = "alligator")
}


#' @export
tbl_vars.alligator <- function (x)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(tbl_vars)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    structure(mc, class = "alligator")
}


#' @export
transmute.alligator <- function (.data, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(transmute)
    mc[[2]] <- .data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
ungroup.alligator <- function (x, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(ungroup)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
union.alligator <- function (x, y, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(union)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
union_all.alligator <- function (x, y, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(union_all)
    mc[[2]] <- x
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
complete.alligator <- function (data, ..., fill = list(), explicit = TRUE)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(complete)
    mc[[2]] <- data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
expand.alligator <- function (data, ..., .name_repair = "check_unique")
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(expand)
    mc[[2]] <- data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
fill.alligator <- function (data, ..., .direction = c("down", "up", "downup", "updown"))
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(fill)
    mc[[2]] <- data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
pivot_longer.alligator <- function (data, cols, ..., cols_vary = "fastest", names_to = "name",
    names_prefix = NULL, names_sep = NULL, names_pattern = NULL,
    names_ptypes = NULL, names_transform = NULL, names_repair = "check_unique",
    values_to = "value", values_drop_na = FALSE, values_ptypes = NULL,
    values_transform = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(pivot_longer)
    mc[[2]] <- data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
pivot_wider.alligator <- function (data, ..., id_cols = NULL, id_expand = FALSE, names_from = name,
    names_prefix = "", names_sep = "_", names_glue = NULL, names_sort = FALSE,
    names_vary = "fastest", names_expand = FALSE, names_repair = "check_unique",
    values_from = value, values_fill = NULL, values_fn = NULL,
    unused_fn = NULL)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(pivot_wider)
    mc[[2]] <- data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


#' @export
replace_na.alligator <- function (data, replace, ...)
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(replace_na)
    mc[[2]] <- data
    for (arg in setdiff(names(mc[-(1:2)]), "...")) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    if ("..." %in% names(mc)) {
        pos <- match("...", names(mc))
        mc <- as.call(append(as.list(mc), rlang::enquos(...),
            after = pos))
        mc$... <- NULL
    }
    structure(mc, class = "alligator")
}


