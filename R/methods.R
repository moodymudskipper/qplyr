#' @export
add_count.quosure <- function (x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = deprecated()) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
anti_join.quosure <- function (x, y, by = NULL, copy = FALSE, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
arrange.quosure <- function (.data, ..., .by_group = FALSE) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
auto_copy.quosure <- function (x, y, copy = FALSE, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
collapse.quosure <- function (x, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
compute.quosure <- function (x, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
count.quosure <- function (x, ..., wt = NULL, sort = FALSE, name = NULL) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
cross_join.quosure <- function (x, y, ..., copy = FALSE, suffix = c(".x", ".y")) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
distinct.quosure <- function (.data, ..., .keep_all = FALSE) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
do.quosure <- function (.data, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
explain.quosure <- function (x, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


filter.quosure <- function (.data, ..., .by = NULL, .preserve = FALSE) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
full_join.quosure <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
group_by.quosure <- function (.data, ..., .add = FALSE, .drop = group_by_drop_default(.data)) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
group_by_drop_default.quosure <- function (.tbl) 
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(group_by_drop_default)
    mc[[2]] <- .tbl
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    rlang::new_quosure(mc, parent.frame())
}


#' @export
group_size.quosure <- function (x) 
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(group_size)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    rlang::new_quosure(mc, parent.frame())
}


#' @export
group_vars.quosure <- function (x) 
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(group_vars)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    rlang::new_quosure(mc, parent.frame())
}


#' @export
groups.quosure <- function (x) 
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(groups)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    rlang::new_quosure(mc, parent.frame())
}


#' @export
inner_join.quosure <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
intersect.quosure <- function (x, y, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
left_join.quosure <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
mutate.quosure <- function (.data, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
n_groups.quosure <- function (x) 
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(n_groups)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    rlang::new_quosure(mc, parent.frame())
}


#' @export
pull.quosure <- function (.data, var = -1, name = NULL, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
relocate.quosure <- function (.data, ..., .before = NULL, .after = NULL) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
rename.quosure <- function (.data, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
rename_with.quosure <- function (.data, .fn, .cols = everything(), ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
right_join.quosure <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
rows_append.quosure <- function (x, y, ..., copy = FALSE, in_place = FALSE) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
rows_delete.quosure <- function (x, y, by = NULL, ..., unmatched = c("error", "ignore"), 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
rows_insert.quosure <- function (x, y, by = NULL, ..., conflict = c("error", "ignore"), 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
rows_patch.quosure <- function (x, y, by = NULL, ..., unmatched = c("error", "ignore"), 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
rows_update.quosure <- function (x, y, by = NULL, ..., unmatched = c("error", "ignore"), 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
rows_upsert.quosure <- function (x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
same_src.quosure <- function (x, y) 
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(same_src)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    rlang::new_quosure(mc, parent.frame())
}


#' @export
select.quosure <- function (.data, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
semi_join.quosure <- function (x, y, by = NULL, copy = FALSE, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
setdiff.quosure <- function (x, y, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
slice.quosure <- function (.data, ..., .by = NULL, .preserve = FALSE) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
slice_head.quosure <- function (.data, ..., n, prop, by = NULL) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
slice_max.quosure <- function (.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
slice_min.quosure <- function (.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
slice_sample.quosure <- function (.data, ..., n, prop, by = NULL, weight_by = NULL, replace = FALSE) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
slice_tail.quosure <- function (.data, ..., n, prop, by = NULL) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
summarise.quosure <- function (.data, ..., .by = NULL, .groups = NULL) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
tally.quosure <- function (x, wt = NULL, sort = FALSE, name = NULL) 
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(tally)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    rlang::new_quosure(mc, parent.frame())
}


#' @export
tbl_vars.quosure <- function (x) 
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(tbl_vars)
    mc[[2]] <- x
    for (arg in names(mc[-(1:2)])) {
        mc[[arg]] <- do.call(rlang::enquo, list(rlang::sym(arg)))
    }
    rlang::new_quosure(mc, parent.frame())
}


#' @export
transmute.quosure <- function (.data, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
ungroup.quosure <- function (x, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
union.quosure <- function (x, y, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
union_all.quosure <- function (x, y, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
complete.quosure <- function (data, ..., fill = list(), explicit = TRUE) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
expand.quosure <- function (data, ..., .name_repair = "check_unique") 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
fill.quosure <- function (data, ..., .direction = c("down", "up", "downup", "updown")) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
pivot_longer.quosure <- function (data, cols, ..., cols_vary = "fastest", names_to = "name", 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
pivot_wider.quosure <- function (data, ..., id_cols = NULL, id_expand = FALSE, names_from = name, 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
replace_na.quosure <- function (data, replace, ...) 
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
head.quosure <- function (x, ...) 
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(head)
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
    rlang::new_quosure(mc, parent.frame())
}


#' @export
tail.quosure <- function (x, ...) 
{
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(tail)
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
    rlang::new_quosure(mc, parent.frame())
}


