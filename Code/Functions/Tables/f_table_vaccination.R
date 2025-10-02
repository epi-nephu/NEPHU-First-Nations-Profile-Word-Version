# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting simple (univariate) tables

# ------------------------------------------------------------------------------
# Vaccination coverage tables ----
# ------------------------------------------------------------------------------
f_table_vaccination <- function(data, x_labels, fully_row) {
  
  data <- data %>% 
    tidyr::pivot_wider(names_from  = "indigenous_status",
                       values_from = "percent") %>% 
    #
    dplyr::mutate(vaccine = dplyr::recode(vaccine, !!!x_labels),
                  vaccine = factor(vaccine, levels = x_labels)) %>% 
    #
    dplyr::arrange(vaccine)

  table <- data %>%
    flextable::flextable() %>%
    #
    flextable::set_header_labels(values = c("Vaccine", "Yes\n(%)", "No\n(%)")) %>%
    #
    flextable::add_header_row(values    = c(" ", "Identified as Aboriginal and/or\nTorres Strait Islander"),
                              colwidths = c(1, 2)) %>%
    #
    flextable::valign(valign = "bottom",
                      part   = "header") %>%
    #
    flextable::align(j = 1,
                     align = "left",
                     part  = "body") %>%
    #
    flextable::align(j = c(2:3),
                     align = "right",
                     part  = "body") %>%
    #
    flextable::fontsize(size = 10,
                        part = "all") %>%
    #
    flextable::bold(part = "header") %>%
    #
    flextable::bold(i = fully_row) %>% 
    #
    flextable::hline(i = fully_row - 1,
                     border = fp_border_default(width = 2)) %>% 
    #
    flextable::width(j = 1,
                     width = 1.25) %>%
    #
    flextable::width(j = 2:3,
                     width = 1.5) %>%
    #
    flextable::line_spacing(space = 0.75,
                            part  = "body")

  return(table)

}

