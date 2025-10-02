  # Interpolating to plot contours: https://stackoverflow.com/questions/65873211/empty-contour-plot-in-ggplot
  plot3D <- function(data3D, dupes = "mean") {
    
    data3Dclean <- dplyr::rename(tidyr::drop_na(data3D),
                                 "x" = 1, "y" = 2, "z" = 3)
    
    suppressWarnings(grid <- akima::interp(dplyr::pull(data3Dclean, 1), 
                                           dplyr::pull(data3Dclean, 2),
                                           dplyr::pull(data3Dclean, 3),
                                           duplicate = dupes))
    
    griddf <- data.frame(x = rep(grid$x, ncol(grid$z)), 
                         y = rep(grid$y, each = nrow(grid$z)), 
                         z = as.numeric(grid$z))
    
    myVars <- colnames(data3D)
    
      ggplot2::ggplot(griddf, ggplot2::aes(x = x, y = y, z = z)) +
      ggplot2::geom_contour_filled(aes(x = x, 
                                       y = y, 
                                       z = z)) + 
      ggplot2::geom_point(data = data3Dclean, colour="white", pch = 21, 
                          fill = "black", size = 1.5) +
      ggplot2::labs(x = myVars[1], y = myVars[2],
                    fill = myVars[3],
                    title = "Three-dimensional distribution") +
      ggplot2::theme_bw() + 
      ggplot2::theme(text = ggplot2::element_text(face = "bold"), 
                     panel.grid = ggplot2::element_blank(),
                     legend.position = "bottom")
  }
