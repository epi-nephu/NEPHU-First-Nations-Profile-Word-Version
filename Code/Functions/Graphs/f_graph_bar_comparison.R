# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting grouped bar charts

# ------------------------------------------------------------------------------
# Grouped bar charts - comparison by indigenous status ----
# ------------------------------------------------------------------------------
f_graph_bar_comparison <- function(data, x_variable, y_variable, x_labels, y_max, y_breaks, y_expand, y_label) {
  
  x_variable <- rlang::enquo(x_variable)
  y_variable <- rlang::enquo(y_variable)
  
  figure <- ggplot(data) +
    geom_col(aes(x = !!x_variable, y = !!y_variable, group = indigenous_status, fill = indigenous_status),
             position = "dodge",
             col      = colour_gray) +
    #
    scale_x_discrete(labels = x_labels) +
    #
    scale_y_continuous(limits = c(0, y_max),
                       breaks = scales::breaks_width(y_breaks),
                       expand = expansion(add = c(0, y_expand)),
                       labels = scales::comma_format(big.mark = ",")) +
    #
    scale_fill_manual(values = c(colour_lightblue, colour_dodgerblue),
                      labels = c("Yes", "No"),
                      name   = "Identifies as Aboriginal and/or Torres Strait Islander:") +
    #
    labs(x = NULL,
         y = y_label) +
    #
    theme_classic() +
    #
    theme(legend.position    = "bottom",
          legend.title       = element_text(size = 9),
          legend.location    = "plot",
          legend.box.spacing = unit(10, "pt"),
          legend.margin      = margin(c(0, 0, 0, 0)),
          #
          plot.margin = margin(5, 5, 0, 5),
          plot.tag    = element_text(size = 12,
                                     face = "bold"),
          #
          axis.title   = element_text(size = 9),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)))

  return(figure)

}

