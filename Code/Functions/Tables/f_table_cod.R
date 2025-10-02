# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting tables for cause of death rankings

# ------------------------------------------------------------------------------
# Leading causes of death
# ------------------------------------------------------------------------------
f_table_cod <- function(data, sex_name) {
  
  data <- data %>% 
    dplyr::filter(sex == sex_name) %>%
    #
    dplyr::select(-sex)
  
  table <- flextable::flextable(data) %>%
    flextable::set_header_labels(values = c("Rank", "Cause of death", "Deaths")) %>%
    #
    flextable::add_header_row(values    = c(sex_name),
                              colwidths = c(3)) %>%
    #
    flextable::valign(valign = "bottom",
                      part   = "header") %>%
    #
    flextable::align(align = "left",
                     part  = "header") %>% 
    #
    flextable::align(j = 1:2,
                     align = "left",
                     part  = "all") %>%
    #
    flextable::align(j = 3,
                     align = "right",
                     part  = "all") %>%
    #
    flextable::fontsize(size = 10,
                        part = "all") %>%
    #
    flextable::bold(part = "header") %>%
    #
    flextable::width(j = c(1, 3),
                     width = 1) %>%
    #
    flextable::width(j = 2,
                     width = 1.75) %>%
    #
    flextable::line_spacing(space = 0.5,
                            part  = "body")

  return(table)

}

