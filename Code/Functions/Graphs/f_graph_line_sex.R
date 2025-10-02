# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting line charts

# ------------------------------------------------------------------------------
# Grouped line chart, by sex
# ------------------------------------------------------------------------------
f_graph_line_sex <- function(data, x_variable, y_variable, y_max, y_breaks, y_expand, y_title) {
  
  x_variable <- rlang::enquo(x_variable)
  y_variable <- rlang::enquo(y_variable)

  figure <- ggplot(data) +
    geom_line(aes(x = !!x_variable, y = !!y_variable, group = sex, col = sex),
              linewidth = 1.5) +
    #
    scale_y_continuous(limits = c(0, y_max),
                       breaks = scales::breaks_width(y_breaks),
                       expand = expansion(add = c(0, y_expand)),
                       labels = scales::comma_format(big.mark = ",")) +
    #
    scale_color_manual(values = c(colour_orange, colour_dodgerblue),
                       labels = c("Female", "Male"),
                       name   = NULL) +
    #
    labs(x = NULL,
         y = y_title) +
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
          #
          axis.title   = element_text(size = 9),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)))

  return(figure)

}

