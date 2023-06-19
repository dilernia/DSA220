# Functioning for preparing date values to create calendar
tidy_for_calendar <- function(dateData) {
  counts <- dateData %>% 
    dplyr::mutate(date = lubridate::make_date(year = year, month = month, 
                                       day = day)) %>% 
    dplyr::count(date) %>% 
    dplyr::mutate(day_num = lubridate::mday(date),
      month_name = lubridate::month(date, label = TRUE, abbr = FALSE),
      week_day = lubridate::wday(date, label = TRUE)) %>% 
    dplyr::group_by(month_name) %>% 
    dplyr::mutate(week = cumsum(week_day == "Sun" & day_num != 1) + 1) %>% 
    dplyr::ungroup()
  
  return(counts)
}

# Function for creating calendar using ggplot2. 
create_calendar <- function(dateCounts,
                            myTitle = "An overall title for the plot",
                            mySubtitle = "A subtitle for the plot",
                            myLegendTitle = "A legend title for the plot",
                            myCaption = "A caption for the plot",
                            myFont = 'Calibri',
                            textColor = 'black',
                            barColor = "dodgerblue",
                            barBreaks = NULL) {
  
  # If breaks not specified, find nice breaks
  if(is.null(barBreaks)) {
    barBreaks <- pretty(dateCounts$n, n = 5)
  }
  
  myCalendar <- dateCounts %>% 
    ggplot2::ggplot(ggplot2::aes(x = week_day, y = 5 - week)) +
    ggplot2::geom_tile(ggplot2::aes(fill = n), col = textColor) +
    ggplot2::geom_text(ggplot2::aes(label = day_num),
      nudge_x = 0.25, nudge_y = 0.25, col = textColor,
      size = 3, family = myFont) +
    ggplot2::geom_text(data = dateCounts %>% dplyr::filter(month_name %in% c('September', 'October', 'November', 'December')),
                       ggplot2::aes(y = -0.85*(1 + (max(week) == 6)), label = week_day),
      col = textColor, size = 3, family = myFont) +
    ggplot2::facet_wrap(ggplot2::vars(month_name), ncol = 4) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::scale_x_discrete(expand=c(0.3, 0.3)) +
    ggplot2::scale_fill_gradient(high = barColor, low = "white",
      breaks = barBreaks) +
    ggplot2::theme_void() +
    ggplot2::theme(text = ggplot2::element_text(color = textColor, family = myFont),
      plot.title = ggplot2::element_text(size = 24, margin = ggplot2::margin(t = 0.25, b = 0.25, unit = 'cm')),
      plot.subtitle = ggplot2::element_text(size = 16, margin = ggplot2::margin(b = 0.5, unit = 'cm')),
      plot.caption = ggplot2::element_text(size = 10, margin = ggplot2::margin(b = 0.25, unit = 'cm')),
      plot.background = element_rect(fill = 'white', color = NA),
      legend.position = 'top',
      legend.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 14),
      strip.text = ggplot2::element_text(size = 12, margin = ggplot2::margin(b = 0.25, unit = 'cm')),
      panel.spacing=unit(0, "lines"))  +
    ggplot2::guides(fill = ggplot2::guide_colorbar(
        barwidth = unit(15, 'cm'),
        barheight = unit(0.3, 'cm'),
        title.position = 'top',
        frame.color = textColor)) +
    ggplot2::labs(title = myTitle,
      subtitle = mySubtitle,
      fill = myLegendTitle,
      caption = myCaption)
  
  return(myCalendar)
}
