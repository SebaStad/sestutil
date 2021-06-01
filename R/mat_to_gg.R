#' Title
#'
#' @param mat
#'
#' @return
#' @export
#'
#' @examples
mat_to_gg <- function(mat){

  tmp <- melt_pp(mat)
  data.frame("Var1" = tmp[,1],
             "Var2" = tmp[,2],
             "value" = tmp[,3])
}
