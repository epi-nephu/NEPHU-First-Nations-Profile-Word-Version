# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting simple (univariate) bar charts

# ------------------------------------------------------------------------------
# Simple bar charts - LGA ----
# ------------------------------------------------------------------------------
f_graph_lga <- function(data, y_variable, y_max, y_breaks, y_expand, y_title, plot_tag) {
  
  y_variable <- rlang::enquo(y_variable)
  
  figure <- ggplot(data) +
    geom_col(aes(x = lga_name, y = !!y_variable),
             col  = colour_black,
             fill = colour_lightblue) +
    #
    scale_x_discrete(name   = "", 
                     labels = c("NEPHU" = expression(bold(NEPHU)), 
                                parse   = TRUE),
                     guide  = guide_axis(angle = 90)) +
    #
    scale_y_continuous(limits = c(0, y_max),
                       breaks = scales::breaks_width(y_breaks),
                       expand = expansion(add = c(0, y_expand)),
                       labels = scales::comma_format(big.mark = ",")) +
    #
    labs(x = NULL,
         y = y_title,
         #
         tag = plot_tag) +
    #
    theme_classic() +
    #
    theme(plot.tag = element_text(size = 12,
                                  face = "bold"),
          #
          axis.title   = element_text(size = 9),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          #
          plot.margin = margin(5, 5, 0, 5))
  
  return(figure)

}

