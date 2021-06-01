#' Title
#'
#' @param base_size 
#' @param base_family 
#' @param base_line_size 
#' @param base_rect_size 
#' @param x_pos_leg 
#' @param y_pos_leg 
#' @param aspect 
#'
#' @return
#' @export
#'
#' @examples
theme_python2 <- function(base_size = 11,
                          base_family = "",
                          base_line_size = base_size / 170,
                          base_rect_size = base_size / 170,
                          x_pos_leg = 0.1, y_pos_leg = 0.85,
                          aspect = 5.5 / 8) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          axis.ticks = element_line(size = 0.5),
          axis.ticks.length = unit(3, "pt"),
          panel.grid.major = element_line(colour = "darkgrey"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"),
          aspect.ratio = aspect,
          complete = TRUE
    )
}
