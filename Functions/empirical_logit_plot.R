# Create function to plot empirical logits for a quantitative predictor
empirical_logit_plot <- function(modelFit,
                                 nbins = 4) {
  
  library(tidyverse)
  
  myData <- modelFit$model
  response <- colnames(myData)[1]
  
  # Reference category for the response variable
  refCat <- modelFit$data[[response]][as.logical(modelFit$y)][1]
  
  # Making response a dummy variable
  myData[[response]] <- modelFit[["y"]]
  
  quantPredictors <- colnames(myData)[-c(1)][which(sapply(myData, FUN = is.numeric)[-c(1)])]
  catPredictors <- colnames(myData)[-c(1)][which(sapply(myData, FUN = function(x){!is.numeric(x)})[-c(1)])]
  
  # Check for complete separation
  if(length(unique(myData[[response]])) == 1) {
    stop("Complete separation in data. Cannot create empirical logit plot.")
  }
  
  # Instantiating list for empirical logit plots
  empPlots <- list()
  
  # Creating empirical logit plots for quantitative predictors
  for(predictor in quantPredictors) {
    
  # Create a dataframe with binned predictor values and the proportion of positive responses in each bin
  aggData <- myData %>%
    dplyr::group_by(bin = ggplot2::cut_interval(.data[[predictor]], n = nbins)) %>%
    dplyr::summarize(prop_response = mean(.data[[response]])) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(logodds = log(prop_response / (1 - prop_response)))
  
  nans <- any(is.infinite(aggData$logodds))
  nbinsNew <- nbins
  
  # Display warning if NaN values
  if(nans) {
    while(nans & nbinsNew >= 1) {
      # Create a dataframe with binned predictor values and the proportion of positive responses in each bin
      aggData <- myData %>%
        dplyr::group_by(bin = cut_interval(.data[[predictor]], n = nbinsNew - 1)) %>%
        dplyr::summarize(prop_response = mean(.data[[response]])) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(logodds = log(prop_response / (1 - prop_response)))
      
      # Check for NaN values
      nans <- any(is.infinite(aggData$logodds))
      
      # Reduce number of bins by 1
      nbinsNew <- nbinsNew - 1
    }
    warning(paste0("Complete separation for ", predictor, 
                   " based on ",
                   nbins, " bins."))
  }
  
  usedBins <- nrow(aggData)
  
  if(nbins > usedBins) {
    message(paste0("Number of bins reduced to ", 
                   usedBins, " instead of ", nbins, " for ",
                   predictor, "."))
  }
  
  # Create the empirical logit plot
  empPlots[[predictor]] <- ggplot2::ggplot(aggData, ggplot2::aes(x = bin, y = logodds)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.1))) +
    ggplot2::labs(x = predictor,
         y = paste0("Log odds of ", response, " = ", refCat),
         title = "Empirical Logit Plot") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
  
  # Creating empirical logit plots for quantitative predictors
  for(predictor in catPredictors) {
    aggData <- myData %>% 
      dplyr::group_by(!!as.name(predictor)) %>% 
      dplyr::summarize(prop_response  = mean(!!as.name(response)),
                       logodds = log(prop_response / (1 - prop_response)))
      
      if(!any(is.infinite(aggData$logodds))) {
      empPlots[[predictor]] <- aggData %>% 
        ggplot2::ggplot(ggplot2::aes(x = !!as.name(predictor), y = logodds)) +
        ggplot2::geom_point() +
        ggplot2::labs(x = predictor,
             y = paste0("Log odds of ", response, " = ", refCat),
             title = "Empirical Logit Plot") +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
      } else {
        warning(paste0("Complete separation for levels of ", predictor, 
                       "."))
      }
  }
  
  if(length(empPlots) > 1) {
    return(empPlots[colnames(myData)[-c(1)]])
  } else {
    return(empPlots[[1]])
  }
}
