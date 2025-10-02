# Function for making flextable
make_flex <- function(myData, ndigits = 2, caption = NULL) {
  
  if("p.value" %in% colnames(myData)) {
    myData$p.value <- format.pval(myData$p.value, digits = ndigits)
  }
  
  myFlex <- myData %>% 
    flextable::flextable() %>% 
    flextable::colformat_double(digits = ndigits) %>% 
    flextable::autofit() %>%
    flextable::fit_to_width(7) 
  
    if(!exists("tbl_counter")) {
  assign("tbl_counter", 
         value = 1, envir = .GlobalEnv)
  }
  
  if(!is.null(caption)) {
        myFlex <- myFlex %>% 
      flextable::set_caption(paste0("Table ", tbl_counter, ": ", caption))
  }
  
  assign("tbl_counter", 
         value = tbl_counter + 1, envir = .GlobalEnv)
  
   return(myFlex) 
}
