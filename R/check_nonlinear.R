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
  
  # Find the response variable. 
  response <- attr(terms(x), "response")
  
  # Drop it from data frame. 
  x_dat <- x_dat[,-response, drop = FALSE]
  
  # Get class information. 
  x_dat_class <- sapply(x_dat, class)
  
  # Get model residuals. 
  x_dat$residuals <- residuals(x, type = "pearson")
  
  # Get variables names. 
  x_dat_names <- names(x_dat)
  
  # For loop for plots.
  # Storage list. 
  plotlist <- list()
  # For loop for columns in df. 
  for(i in seq_along(x_dat_class)){
    # Treat numeric and factor seperately. 
    if(x_dat_class[i] == "numeric"){
      
      # Continuous variable. 
      plot_ob <- ggplot2::ggplot(x_dat, ggplot2::aes_string(x_dat_names[i], "residuals")) + 
        ggplot2::geom_hline(yintercept = 0, col = "red") +
        ggplot2::geom_point() + 
        ggplot2::geom_smooth(method = "loess", formula = y~x, se = FALSE) + 
        ggplot2::labs(x = colnames(x_dat)[i], y = "Pearson Residuals") + 
        my_theme
      
      # Append plot. 
      plotlist[[i]] <- plot_ob
      
    } else{ # Factor. 
      plot_ob <- ggplot2::ggplot(x_dat, ggplot2::aes_string(x_dat_names[i], "residuals")) + 
        ggplot2::geom_hline(yintercept = 0, col = "red") +
        ggplot2::geom_boxplot() + 
        ggplot2::labs(x = colnames(x_dat)[i], y = "Pearson Residuals") + 
        my_theme
      
      # Append plot. 
      plotlist[[i]] <- plot_ob
      
      
      
    }
    
    
  }
  
  ggpubr::ggarrange(plotlist =plotlist)
  

}

# Reproducible example. 
#data("iris")
#x <- lm(Sepal.Length ~ ., data = iris)
#check_nonlinear(x)
