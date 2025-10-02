# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting horizontal bar charts

# ------------------------------------------------------------------------------
# Horizontal bar charts
# ------------------------------------------------------------------------------
f_graph_bar_horizontal <- function(data, x_variable, y_variable, y_max, y_breaks, y_expand, y_title) {
  
  x_variable <- rlang::enquo(x_variable)
  y_variable <- rlang::enquo(y_variable)
  
  figure <- ggplot(data) + 
    geom_col(aes(x = forcats::fct_reorder(factor(!!x_variable), !!y_variable, .desc = TRUE), y = !!y_variable),
             col  = colour_gray,
             fill = colour_lightblue) +
    #
    scale_y_continuous(limits = c(0, y_max),
                       breaks = scales::breaks_width(y_breaks),
                       expand = expansion(add = c(0, y_expand)),
                       labels = scales::comma_format(big.mark = ",")) +
    #
    labs(x = NULL, 
         y = y_title) +
    #
    theme_classic() +
    #
    theme(plot.margin = margin(5, 5, 0, 5),
          #
          axis.title   = element_text(size = 9),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0))) +
    #
    coord_flip()

  return(figure)

}

