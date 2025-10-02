# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting line charts

# ------------------------------------------------------------------------------
# Grouped line chart for notifiable disease rates, by indigenous status ----
# ------------------------------------------------------------------------------
f_graph_phess_rates <- function(data, plot_tag, y_variable, y_max, y_breaks, y_expand) {

  y_variable <- rlang::enquo(y_variable)

  figure <- ggplot(data) +
    geom_line(aes(x = year, y = !!y_variable, group = indigenous_status, col = indigenous_status),
              linewidth = 1.5) +
    #
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    #
    scale_y_continuous(limits = c(0, y_max),
                       breaks = scales::breaks_width(y_breaks),
                       expand = expansion(add = c(0, y_expand)),
                       labels = scales::comma_format(big.mark = ",")) +
    #
    scale_color_manual(values = c(colour_lightblue, colour_burgundy),
                       labels = c("Yes", "No"),
                       name   = "Identified as Aboriginal and/or Torres Strait Islander:") +
    #
    labs(x   = NULL,
         y   = "Rate per 100,000 population",
         tag = plot_tag) +
    #
    theme_classic() +
    #
    theme(legend.title       = element_text(size = 9),
          legend.position    = "bottom",
          legend.location    = "plot",
          legend.box.spacing = unit(10, "pt"),
          legend.margin      = margin(c(0, 10, 0, 0)),
          #
          plot.margin = margin(5, 5, 0, 5),
          plot.tag    = element_text(size = 12,
                                     face = "bold"),
          #
          axis.title   = element_text(size = 9),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)))

  return(figure)

}

