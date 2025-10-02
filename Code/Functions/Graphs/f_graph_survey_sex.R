# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting grouped bar charts

# ------------------------------------------------------------------------------
# Grouped bar charts - health/social survey data ----
# ------------------------------------------------------------------------------
f_graph_survey_sex <- function(data, plot_tag, x_levels, x_labels, y_variable, 
                               y_max, y_breaks, y_expand, y_title) {

  y_variable        <- rlang::enquo(y_variable)
  y_variable_string <- rlang::as_name(y_variable)
  
  data <- data %>% 
    dplyr::select(sex,
                  ends_with(paste0("_", y_variable_string)),
                  -starts_with("answered")) %>%
    #
    dplyr::filter(sex != "Total") %>%
    #
    tidyr::pivot_longer(cols      = !sex,
                        names_to  = "comparison",
                        values_to = y_variable_string) %>%
    #
    dplyr::mutate(comparison = factor(comparison,
                                      levels = x_levels))

  figure <- ggplot(data) +
    geom_col(aes(x = comparison, y = !!y_variable, group = sex, fill = sex),
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
    scale_fill_manual(values = c(colour_lightblue, colour_dodgerblue)) +
    #
    labs(x = NULL,
         y = y_title,
         #
         tag = plot_tag) +
    #
    theme_classic() +
    #
    theme(legend.position    = "bottom",
          legend.title       = element_blank(),
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

