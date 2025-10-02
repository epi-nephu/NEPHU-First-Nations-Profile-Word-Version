# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting simple (univariate) bar charts

# ------------------------------------------------------------------------------
# Simple bar charts - age and sex ----
# ------------------------------------------------------------------------------
f_graph_agesex <- function(data, sex, y_variable, y_max, y_breaks, y_expand, 
                           y_title, plot_title, plot_tag) {
  
  sex        <- rlang::enquo(sex)
  y_variable <- rlang::enquo(y_variable)
  
  data <- data %>%
    dplyr::filter(age_group %in% age_group_lvl & sex == !!sex) %>%
    #
    dplyr::mutate(age_group = factor(age_group, levels = age_group_lvl))
  
  figure <- ggplot(data) +
    geom_col(aes(x = age_group, y = !!y_variable),
             col  = colour_black,
             fill = colour_lightblue) +
    #
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    #
    scale_y_continuous(limits = c(0, y_max),
                       breaks = scales::breaks_width(y_breaks),
                       expand = expansion(add = c(0, y_expand)),
                       labels = scales::comma_format(big.mark = ",")) +
    #
    labs(x = NULL,
         y = y_title,
         #
         title    = plot_title,
         subtitle = plot_tag) +
    #
    theme_classic() +
    #
    theme(plot.title.position = "plot",
          #
          plot.title = element_text(size = 12,
                                    face = "bold"),
          #
          plot.subtitle = element_text(size = 12,
                                       face = "bold"),
          #
          axis.title   = element_text(size = 10),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          #
          plot.margin = margin(5, 5, 5, 5))

  return(figure)

}

