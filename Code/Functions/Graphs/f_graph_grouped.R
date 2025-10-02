# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting stacked bar charts

# ------------------------------------------------------------------------------
# Stacked bar charts - multiple group categories
# ------------------------------------------------------------------------------
f_graph_grouped <- function(data, x_variable, factor_levels, factor_colours,
                            factor_labels, y_breaks, y_expand, y_title = "Number of people") {

  x_variable <- rlang::enquo(x_variable)

  data <- data %>%
    tidyr::pivot_longer(cols      = ends_with("_n"),
                        names_to  = "comparison",
                        values_to = "aboriginal_n") %>%
    #
    dplyr::mutate(comparison = factor(comparison,
                                      levels = factor_levels))

  figure <- ggplot(data) +
    geom_col(aes(x = !!x_variable, y = aboriginal_n, fill = comparison),
             col = colour_gray) +
    #
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    #
    scale_y_continuous(breaks = scales::breaks_width(y_breaks),
                       expand = expansion(add = c(0, y_expand)),
                       labels = scales::comma_format(big.mark = ",")) +
    #
    scale_fill_manual(values = factor_colours,
                      labels = factor_labels,
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
          legend.box.spacing = unit(5, "pt"),
          legend.margin      = margin(c(0, 0, 0, 0)),
          #
          axis.title   = element_text(size = 9),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0))) +
    #
    guides(fill = guide_legend(reverse = TRUE))

  return(figure)

}

