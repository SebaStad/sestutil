#' Title
#'
#' @param dims
#' @param max_dims
#'
#' @return
#' @export
#'
#' @examples
mult_dims <- function(dims, max_dims = NULL){
  if(is.null(max_dims)){
    eval(parse(text=paste(dims, collapse = "*")))
    } else if(is.numeric(max_dims)){
    eval(parse(text=paste(dims[1:max_dims], collapse = "*")))
  } else {
    stop("max_dims has to be a numeric or NULL")
  }
}

