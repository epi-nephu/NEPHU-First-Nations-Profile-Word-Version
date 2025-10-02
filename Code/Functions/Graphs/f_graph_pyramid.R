# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting population pyramid charts

# ------------------------------------------------------------------------------
# Population pyramids - females ----
# ------------------------------------------------------------------------------
f_graph_pyramid_female <- function(data, prop_variable, fill_variable) {
  
  prop_variable <- rlang::enquo(prop_variable)
  fill_variable <- rlang::enquo(fill_variable)
  
  data <- data %>% 
    dplyr::filter(sex == "Female")
  
  figure <- ggplot(data) +
    geom_col(aes(x = age_group, y = !!prop_variable, fill = !!fill_variable),
             position = position_dodge(),
             col      = colour_black) +
    #
    geom_hline(yintercept = 0) +
    #
    scale_x_discrete(labels = NULL,
                     breaks = NULL) +
    #
    scale_y_continuous(breaks = seq(from = -25, to = 0, by = 5),
                       labels = abs(seq(from = -25, to = 0, by = 5)),
                       expand = expansion(add = c(5, 0))) +
    #
    scale_fill_manual(labels = c("Identified as Aboriginal\nand/or Torres Strait Islander",
                                 "Did not identify as Aboriginal\nand/or Torres Strait Islander"),
                      values = c(colour_lightblue, colour_burgundy)) +
    #
    labs(x    = NULL, 
         y    = "Percentage of total population",
         fill = NULL,
         tag  = "Females") +
    #
    coord_flip() +
    #
    theme_classic() +
    #
    theme(plot.tag = element_text(size  = 12,
                                  face  = "bold",
                                  hjust = 0.5),
          #
          axis.text  = element_text(size = 9),
          axis.title = element_text(size = 10),
          #
          legend.position    = "bottom",
          legend.location    = "plot",
          legend.box.spacing = unit(10, "pt"),
          legend.margin      = margin(c(0, 0, 0, 0)),
          legend.title       = element_text(size  = 9,
                                            hjust = 0.5))
  
  return(figure)

}

# ------------------------------------------------------------------------------
# Population pyramids - males ----
# ------------------------------------------------------------------------------
f_graph_pyramid_male <- function(data, prop_variable, fill_variable) {
  
  prop_variable <- rlang::enquo(prop_variable)
  fill_variable <- rlang::enquo(fill_variable)
  
  data <- data %>% 
    dplyr::filter(sex == "Male")
  
  figure <- ggplot(data) +
    geom_col(aes(x = age_group, y = !!prop_variable, fill = !!fill_variable),
             position = position_dodge(),
             col      = colour_black) +
    #
    geom_hline(yintercept = 0) +
    #
    scale_x_discrete(labels = NULL,
                     breaks = NULL) +
    #
    scale_y_continuous(breaks = seq(from = 0, to = 25, by = 5),
                       labels = seq(from = 0, to = 25, by = 5),
                       expand = expansion(add = c(0, 5))) +
    #
    scale_fill_manual(labels = c("Identified as Aboriginal\nand/or Torres Strait Islander",
                                 "Did not identify as Aboriginal\nand/or Torres Strait Islander"),
                      values = c(colour_lightblue, colour_burgundy)) +
    #
    labs(x    = NULL, 
         y    = "Percentage of total population",
         fill = NULL,
         tag  = "Males") +
    #
    coord_flip() +
    #
    theme_classic() +
    #
    theme(plot.tag = element_text(size  = 12,
                                  face  = "bold",
                                  hjust = 0.5),
          #
          axis.text  = element_text(size = 9),
          axis.title = element_text(size = 10),
          #
          legend.position    = "bottom",
          legend.location    = "plot",
          legend.box.spacing = unit(10, "pt"),
          legend.margin      = margin(c(0, 0, 0, 0)),
          legend.title       = element_text(size  = 9,
                                            hjust = 0.5))
  
  return(figure)

}

