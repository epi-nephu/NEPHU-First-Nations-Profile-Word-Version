# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting simple (univariate) bar charts

# ------------------------------------------------------------------------------
# Simple bar charts - IARE ----
# ------------------------------------------------------------------------------
f_graph_iare <- function(data, suppress, y_variable, text_pos = NULL, y_label = NULL,
                         y_max, y_breaks, y_expand, y_title, plot_tag) {
  
  y_variable <- rlang::enquo(y_variable)
  y_label    <- rlang::enquo(y_label)

  data_suppress_yes <- data %>%
    dplyr::filter(is.na(!!y_variable))
  
  if (suppress == "Yes") {
    
    figure <- ggplot() +
      geom_col(data = data,
               aes(x = iare_name_short,
                   y = !!y_variable),
               col  = colour_black,
               fill = colour_lightblue) +
      #
      geom_text(data = data_suppress_yes,
                aes(x = iare_name_short,
                    y = text_pos,
                    #
                    label = formatC(!!y_label, big.mark = ",")),
                size  = 4,
                vjust = 0.25,
                col   = colour_black) +
      #
      
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      #
      scale_y_continuous(limits = c(0, y_max),
                         breaks = scales::breaks_width(y_breaks),
                         expand = expansion(add = c(0, y_expand)),
                         labels = scales::comma_format(big.mark = ",")) +
      #
      labs(x   = NULL,
           y   = y_title,
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
            plot.margin = margin(5, 5, 5, 5))

    } else if (suppress == "No") {
      
      figure <- ggplot(data) +
        geom_col(aes(x = iare_name_short,
                     y = !!y_variable),
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
        labs(x   = NULL,
             y   = y_title,
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
              plot.margin = margin(5, 5, 5, 5))

    }
  
  return(figure)

}

