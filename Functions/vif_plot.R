# Function for plotting variance inflation factor (VIF) values 
vif_plot <- function(modFit) {
  
  vifs <- car::vif(modFit)
  
  if("GVIF^(1/(2*Df))" %in% colnames(vifs)) {
    vifs <- vifs[, "GVIF^(1/(2*Df))"]
    ggTitle <- "Generalized variance inflation factors"
    xLab <- "GVIF"
  } else {
    ggTitle <- "Variance inflation factors"
    xLab <- "VIF"
    }
  
  preds <- names(vifs)
  
  vifGG <- tibble(Predictor = preds,
       VIF = vifs) %>% 
    dplyr::mutate(Predictor = fct_reorder(Predictor, -VIF)) %>% 
    ggplot(aes(x = VIF, y = Predictor)) + 
    geom_segment(aes(xend = VIF, x = 0, 
                     yend = Predictor, y = Predictor)) +
    geom_vline(xintercept = 5, linetype = "dotted") +
    geom_vline(xintercept = 10, linetype = "dotted") +
    scale_x_continuous(limits = c(0, max(vifs)*1.1),
                       expand = expansion(mult = c(0, 0.10))) +
    geom_point(size = 3, color = "steelblue") +
    labs(title = ggTitle, x = xLab,
         caption = stringr::str_wrap(paste0(xLab, " values calculated via the car package: https://search.r-project.org/CRAN/refmans/car/html/vif.html")))
  
return(suppressWarnings(print(vifGG)))
}
