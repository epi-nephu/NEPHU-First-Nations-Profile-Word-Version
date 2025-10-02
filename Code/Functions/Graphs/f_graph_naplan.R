# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting stacked bar charts

# ------------------------------------------------------------------------------
# Stacked bar charts - NAPLAN data ----
# ------------------------------------------------------------------------------
f_graph_naplan <- function(data) {

  figure <- ggplot(data) +
    geom_col(aes(x = grade, y = vic, fill = forcats::fct_rev(achievement)),
             col = colour_gray) +
    #
    scale_y_continuous(breaks = scales::breaks_width(10),
                       expand = expansion(add = c(0, 1)),
                       labels = scales::comma_format(big.mark = ",")) +
    #
    scale_fill_manual(values = c(colour_gray, colour_yellow, colour_orange,
                                 colour_green, colour_lightblue),
                      labels = c("Exempt", "Needs additional support", "Developing",
                                 "Strong", "Exceeding"),
                      name   = NULL) +
    #
    labs(x = NULL,
         y = "Percentage of children") +
    #
    theme_classic() +
    #
    theme(legend.position    = "bottom",
          legend.title       = element_text(size = 9),
          legend.location    = "plot",
          legend.box.spacing = unit(0, "pt"),
          #
          axis.title = element_text(size = 9)) +
    #
    guides(fill = guide_legend(reverse = TRUE))

  return(figure)

}

