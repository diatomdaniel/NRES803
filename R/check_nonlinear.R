if(getRversion() >= "2.15.1")  
  
#' Plot model parameters against residuals of a fitted model
#'
#'
#' @param x a fitted model object 
#' @param ... not used at present
#'
#' @return NULL called for its side effect, making a plot
#' @export
#'
#' @examples
#' lm1 <- lm(Sepal.Width ~ Petal.Length, data = iris)
#' check_nonlinear(lm1)
check_nonlinear <- function(x,...){
  
  # Plotting aesthetics.
  my_theme <- ggplot2::theme_bw() +
    ggplot2::theme(panel.grid  = ggplot2::element_blank(), 
                   axis.text = ggplot2::element_text(color = "black"))
  
  # Get model data. 
  x_dat <- model.frame(x)
  
  # Get model residuals. 
  x_dat$residuals <- residuals(x, type = "pearson")
  
  # Pivot longer using tidyverse. 
  x_dat_gather <- tidyr::gather(x_dat, "Variables", "values", -residuals)
  
  # Plot data. 
  ggplot2::ggplot(x_dat_gather, ggplot2::aes(values, residuals)) + 
    ggplot2::geom_hline(yintercept = 0, col = "red") +
    ggplot2::geom_point() + 
    ggplot2::geom_smooth(method = "loess", formula = y~x, se = FALSE) + 
    ggplot2::facet_wrap(Variables~., scales = "free") + 
    ggplot2::labs(x = "Variables", y = "Pearson residuals") + 
    my_theme

}

