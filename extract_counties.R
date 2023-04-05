# Create function to extract county / parish, city info
# Arguments
# address_info: a tibble or data frame with a variable called address_full
extract_counties <- function(address_info) {
  
  # Calculating maximum number of commas in any address
  maxCommas <- address_info %>% 
    dplyr::pull(address_full) %>% 
    stringr::str_count(pattern = ",") %>% 
    max(na.rm = TRUE)
  
  matchWords <- "County|county|Parish|parish|Municipio|municipio|Municipality|municipality|District of Columbia|City of Baltimore|City of Saint Louis|City of Richmond|City of Norfolk|City of Newport News|City of Virginia Beach|City of Hampton|City of Chesapeake|City of Suffolk|City of Roanoke|City of Portsmouth|City of Alexandria|City of Lynchburg"
  
  # Fixing NYC boroughs and other metro names
  address_info <- address_info %>% 
    dplyr::mutate(
      address_full = stringr::str_replace_all(address_full,
                                              pattern = "Bronx, City of New York", replacement = "Bronx County,"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = "Brooklyn, City of New York", replacement = "Kings County,"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = "Manhattan, City of New York", replacement = "New York County,"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = "Queens, City of New York", replacement = "Queens County,"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = "Staten Island, City of New York", replacement = "Richmond County,"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Baltimore, Maryland", replacement = ", City of Baltimore, Maryland"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", St. Louis, Missouri", replacement = ", City of Saint Louis, Missouri"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Richmond, Virginia", replacement = ", City of Richmond, Virginia"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Norfolk, Virginia", replacement = ", City of Norfolk, Virginia"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Newport News, Virginia", replacement = ", City of Newport News, Virginia"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Virginia Beach, Virginia", replacement = ", City of Virginia Beach, Virginia"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Hampton, Virginia", replacement = ", City of Hampton, Virginia"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Chesapeake, Virginia", replacement = ", City of Chesapeake, Virginia"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Suffolk, Virginia", replacement = ", City of Suffolk, Virginia"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Roanoke, Virginia", replacement = ", City of Roanoke, Virginia"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Portsmouth, Virginia", replacement = ", City of Portsmouth, Virginia"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Alexandria, Virginia", replacement = ", City of Alexandria, Virginia"),
      address_full = stringr::str_replace_all(address_full,
                                              pattern = ", Lynchburg, Virginia", replacement = ", City of Lynchburg, Virginia"))
  
  # Extracting county info from addresses (not an easy task!)
  suppressWarnings({
    countyParishInfo <- address_info %>% 
      tidyr::separate(col = address_full, remove = FALSE, sep = ",", 
                      into = paste0("addr_info_", 1:(maxCommas + 1))) %>%
      dplyr::select(address_full, tidyselect::starts_with("addr_info_")) %>% 
      dplyr::mutate(ID = 1:n()) %>% 
      tidyr::pivot_longer(cols = tidyselect::starts_with("addr_info_"), names_to = "Component", values_to = "county_parish_city") %>%
      dplyr::filter(stringr::str_detect(county_parish_city, pattern = matchWords)) %>% 
      dplyr::group_by(ID) %>% 
      dplyr::slice_tail(n = 1) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(address_full, county_parish_city) %>% 
      dplyr::mutate(county_parish_city = stringr::str_remove_all(county_parish_city, pattern = "City of "))
  })
  
  # Adding state names
  suppressMessages({
    stateCommas <- paste0(", ", state.name, ",")
    countyParishInfo$state <- purrr::map(.x = stateCommas,
                                         .f = function(state) {
                                           stringr::str_detect(countyParishInfo$address_full, pattern = state)
                                         }) %>% 
      dplyr::bind_cols() %>% 
      as.matrix() %>% 
      t() %>% 
      apply(MARGIN = 2, FUN = function(x){state.name[x][1]}, simplify = TRUE)
  })
  
  # Fixing issue with Washington DC
  countyParishInfo <- countyParishInfo %>% 
    dplyr::mutate(county_parish_city = stringr::str_squish(county_parish_city),
                  state = dplyr::case_when(county_parish_city == "District of Columbia" ~ "District of Columbia",
                                           TRUE ~ state))
  
  return(countyParishInfo)
}