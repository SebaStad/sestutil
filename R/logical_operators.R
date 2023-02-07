#' %and% operator similar to python
#'
#' @param lhs Left hand side
#' @param rhs Right hand side
#'
#' @return Bool.
#' @export
#'
#' @examples
#' a <- 2 > 0
#' b <- TRUE
#' a %and% b
`%and%` <- function(lhs, rhs) {
  lhs && rhs
}


#' %or% operator similar to python
#'
#' @param lhs Left hand side
#' @param rhs Right hand side
#'
#' @return Bool.
#' @export
#'
#' @examples
#' a <- 2 < 0
#' b <- TRUE
#' a %or% b
`%or%` <- function(lhs, rhs) {
  lhs || rhs
}


#' %nor% operator, just to have it
#'
#' @param lhs Left hand side
#' @param rhs Right hand side
#'
#' @return Bool.
#' @export
#'
#' @examples
#' a <- 2 < 0
#' b <- FALSE
#' a %nor% b
`%nor%` <- function(lhs, rhs) {
  !lhs %% !rhs
}
