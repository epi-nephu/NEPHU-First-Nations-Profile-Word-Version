# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting grouped tables

# ------------------------------------------------------------------------------
# Grouped tables - multiple group categories - by LGA
# ------------------------------------------------------------------------------
f_table_grouped_lga <- function(data, table_headers, group_headers, group_widths, last_column) {

  table <- data %>%
    flextable::flextable() %>%
    #
    flextable::set_header_labels(values = c("LGA", table_headers, "Total")) %>%
    #
    flextable::add_header_row(values    = c(" ", group_headers, " "),
                              colwidths = c(1, group_widths, 1)) %>%
    #
    flextable::align(i = 1,
                     align = "right",
                     part  = "header") %>%
    #
    flextable::align(j = 1,
                     align = "left",
                     part  = "body") %>%
    #
    flextable::align(j = c(2:last_column),
                     align = "right",
                     part  = "body") %>%
    #
    flextable::fontsize(size = 10,
                        part = "all") %>%
    #
    flextable::bold(part = "header") %>%
    #
    flextable::bold(i = 13) %>%
    #
    flextable::hline(i = 12,
                     border = fp_border_default(width = 2)) %>%
    #
    flextable::width(j = 1,
                     width = 1.5) %>%
    #
    flextable::width(j = 2:last_column,
                     width = 0.75) %>%
    #
    flextable::line_spacing(space = 0.75,
                            part  = "body")

  return(table)

}

