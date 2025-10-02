# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting observed/expected bar charts

# ------------------------------------------------------------------------------
# Observed vs. expected charts ----
# ------------------------------------------------------------------------------
f_graph_expected <- function(data, variable, y_title = "Number of people", y_breaks, y_expand) {
  
  variable <- rlang::enquo(variable)
  
  data <- data %>%
    tidyr::pivot_longer(c(aboriginal_n, aboriginal_exp_non),
                        names_to  = "comparison",
                        values_to = "n") %>%
    #
    dplyr::mutate(
      comparison = factor(dplyr::case_when(
        comparison == "aboriginal_n"       ~ "Observed number identifying as Aboriginal and/or Torres Strait Islander",
        comparison == "aboriginal_exp_non" ~ "Expected number based on NEPHU non-Indigenous population",
        TRUE ~ NA_character_),
        #
        levels = c("Observed number identifying as Aboriginal and/or Torres Strait Islander", 
                   "Expected number based on NEPHU non-Indigenous population")))
  
  figure <- ggplot(data) +
    geom_col(aes(x = !!variable, y = n, group = comparison, fill = comparison),
             col      = colour_black,
             position = "dodge") +
    #
    scale_x_discrete(guide  = guide_axis(angle = 90)) +
    #
    scale_y_continuous(breaks = scales::breaks_width(y_breaks),
                       expand = expansion(add = c(0, y_expand))) +
    #
    scale_fill_manual(values = c(colour_lightblue, colour_burgundy),
                      labels = c("Observed number:\nIdentified as Aboriginal and/or Torres Strait Islander",
                                 "Expected number:\nBased on NEPHU non-Indigenous population"),
                      name   = NULL) +    
    #
    labs(x = NULL,
         y = y_title) +
    #
    theme_classic() +
    #
    theme(legend.title       = element_text(size = 9),
          legend.location    = "plot",
          legend.position    = "bottom",
          legend.box.spacing = unit(0, "pt"),
          legend.margin      = margin(c(5, 0, 0, 0)),
          #
          axis.title   = element_text(size = 9),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)))
  
  return(figure)
  
}

