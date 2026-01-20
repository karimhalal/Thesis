#' @param x the list to append to
#' @param item the item to append
#' @return The new list
append_to_list <- function(x, item) {
  x[[length(x) + 1]] <- item
  return(x)
}