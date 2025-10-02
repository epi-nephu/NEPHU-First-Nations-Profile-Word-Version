# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting observed/expected tables

# ------------------------------------------------------------------------------
# Observed vs. expected tables
# ------------------------------------------------------------------------------
f_table_expected <- function(data, variable, variable_label, group_label, total_row, 
                             digits_yes = 1, digits_no = 1) {
  
  variable <- rlang::enquo(variable)
  
  data <- data %>% 
    dplyr::select(!!variable,
                  aboriginal_n,
                  aboriginal_prop,
                  aboriginal_exp_non,
                  non_aboriginal_prop,
                  aboriginal_diff_label) %>% 
    #
    dplyr::mutate(aboriginal_prop     = round(aboriginal_prop, digits_yes),
                  non_aboriginal_prop = round(non_aboriginal_prop, digits_no))
  
  table <- flextable::flextable(data) %>%
    flextable::set_header_labels(values = c(variable_label,
                                            "n", "%",
                                            "n", "%",
                                            "")) %>% 
    #
    flextable::add_header_row(values = c(group_label, 
                                         "Observed values\n(identified as Aboriginal\nand/or Torres Strait Islander)", 
                                         "Expected values\n(based on NEPHU\nnon-Indigenous population)",
                                         "Difference\nbetween observed\nand expected"),
                              #
                              colwidths = c(1, 2, 2, 1)) %>%
    #
    flextable::valign(valign = "bottom",
                      part   = "header") %>% 
    #
    flextable::valign(i = 1,
                      valign = "bottom",
                      part   = "body") %>% 
    #
    flextable::align(i = 1,
                     align = "right",
                     part  = "header") %>% 
    #
    flextable::align(j = 1,
                     align = "left",
                     part  = "all") %>%
    #
    flextable::align(j = c(2:6),
                     align = "right",
                     part  = "body") %>%
    #
    flextable::fontsize(size = 10,
                        part = "all") %>% 
    #
    flextable::bold(part = "header") %>% 
    #
    flextable::bold(i = total_row) %>% 
    #
    flextable::hline(i = total_row - 1,
                     border = fp_border_default(width = 2)) %>%
    #
    flextable::width(j = 1,
                     width = 1.0) %>%
    #
    flextable::width(j = 2:5,
                     width = 1.125) %>%
    #
    flextable::width(j = 6,
                     width = 1.5) %>% 
    #
    flextable::line_spacing(space = 0.75,
                            part  = "body")
  
  return(table)
  
}

