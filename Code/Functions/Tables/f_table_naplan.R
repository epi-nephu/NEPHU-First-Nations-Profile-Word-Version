# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting grouped tables

# ------------------------------------------------------------------------------
# Grouped tables - NAPLAN data ----
# ------------------------------------------------------------------------------
f_table_naplan <- function(data) {

  data <- data %>%
    tidyr::pivot_wider(names_from  = grade,
                       values_from = vic) %>%
    #
    janitor::clean_names() %>%
    #
    dplyr::select(-year) %>%
    #
    janitor::adorn_totals()

  table <- data %>%
    flextable::flextable() %>%
    #
    flextable::set_header_labels(values = c("Proficiency level",
                                            "Year 3", "Year 5", "Year 7", "Year 9")) %>%
    #
    flextable::valign(valign = "bottom",
                      part   = "header") %>%
    #
    flextable::align(j = 1,
                     align = "left",
                     part  = "body") %>%
    #
    flextable::align(j = c(2:5),
                     align = "right",
                     part  = "body") %>%
    #
    flextable::fontsize(size = 10,
                        part = "all") %>%
    #
    flextable::bold(part = "header") %>%
    #
    flextable::bold(i = 6) %>%
    #
    flextable::hline(i = 5,
                     border = fp_border_default(width = 2)) %>%
    #
    flextable::width(j = 1,
                     width = 2) %>%
    #
    flextable::width(j = 2:5,
                     width = 1) %>%
    #
    flextable::line_spacing(space = 0.75,
                            part  = "body")

  return(table)

}

