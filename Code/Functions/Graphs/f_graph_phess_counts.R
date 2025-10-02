# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting stacked bar charts

# ------------------------------------------------------------------------------
# Stacked bar charts - notifiable diseases counts
# ------------------------------------------------------------------------------
f_graph_phess_counts <- function(data, condition_name, y_max, y_breaks, y_expand) {
  
  data <- data %>% 
    dplyr::filter(condition == condition_name & indigenous_status == "aboriginal") %>%
    #
    dplyr::select(year,
                  nephu_n,
                  notnephu_n) %>% 
    #
    tidyr::pivot_longer(cols      = ends_with("_n"),
                        names_to  = "comparison",
                        values_to = "aboriginal_n") %>%
    #
    dplyr::mutate(comparison = factor(comparison,
                                      levels = c("notnephu_n", "nephu_n"))) 
  
  figure <- ggplot(data) +
    geom_col(aes(x = year, y = aboriginal_n, fill = comparison),
             col = colour_gray) +
    #
    scale_y_continuous(limits = c(0, y_max),
                       breaks = scales::breaks_width(y_breaks),
                       expand = expansion(add = c(0, y_expand)),
                       labels = scales::comma_format(big.mark = ",")) +
    #
    scale_fill_manual(values = c(colour_lightblue, colour_green),
                      labels = c("Rest of Victoria", "NEPHU"),
                      name   = NULL) +
    #
    labs(x = NULL,
         y = "Number of notifications") +
    #
    theme_classic() +
    #
    theme(legend.title       = element_text(size = 9),
          legend.location    = "plot",
          legend.position    = "bottom",
          legend.box.spacing = unit(5, "pt"),
          legend.margin      = margin(c(0, 0, 0, 0)),
          #
          plot.margin = margin(5, 5, 0, 5),
          #
          axis.title   = element_text(size = 9),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0))) +
    #
    guides(fill = guide_legend(reverse = TRUE))

  return(figure)

}

