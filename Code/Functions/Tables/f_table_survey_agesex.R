# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting grouped tables

# ------------------------------------------------------------------------------
# Grouped tables - health/social survey data, by age group and sex ----
# ------------------------------------------------------------------------------
f_table_survey_agesex <- function(data, table_header, divide_row) {

  data <- data %>%
    tidyr::pivot_wider(names_from  = "sex",
                       values_from = c("estimate", "proportion"),
                       names_vary  = "slowest") %>%
    #
    janitor::clean_names()

  table <- data %>%
    flextable::flextable() %>%
    #
    flextable::set_header_labels(values = c("Age group",
                                            table_header,
                                            "Estimate", "Percent",
                                            "Estimate", "Percent",
                                            "Estimate", "Percent")) %>%
    #
    flextable::add_header_row(values    = c(" ", "Male", "Female", "Total"),
                              colwidths = c(2, 2, 2, 2)) %>%
    #
    flextable::merge_v(j = 1) %>%
    #
    flextable::align(i = 1,
                     align = "center",
                     part  = "header") %>%
    #
    flextable::align(j = 1,
                     align = "left",
                     part  = "body") %>%
    #
    flextable::align(j = c(3:8),
                     align = "right",
                     part  = "body") %>%
    #
    flextable::fontsize(size = 10,
                        part = "all") %>%
    #
    flextable::bold(part = "header") %>%
    #
    flextable::hline(i = divide_row,
                     border = fp_border_default(width = 1)) %>%
    #
    flextable::autofit() %>%
    #
    flextable::line_spacing(space = 0.75,
                            part  = "body")

  return(table)

}

