#' Default NRES theme
#'
#' @param base_size base font size
#' @param base_family base font family (default = sans)
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#'
#' @return a ggplot2 theme object
#' @export
#'
theme_nres <- function(base_size = 16, base_family = "sans",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22){
  ggplot2::theme_bw(base_size, base_family, base_line_size, base_rect_size) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
}
