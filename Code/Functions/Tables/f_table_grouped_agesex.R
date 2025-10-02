# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting grouped tables

# ------------------------------------------------------------------------------
# Grouped tables - multiple group categories - by age and sex ----
# ------------------------------------------------------------------------------
f_table_grouped_agesex <- function(data, sex_name, table_headers, group_headers,
                                   group_widths, last_column) {

   data <- data %>%
     dplyr::filter(sex == sex_name) %>%
     #
     dplyr::select(-sex)

   table <- flextable::flextable(data) %>%
     flextable::set_header_labels(values = c("Age group", table_headers, "Total")) %>%
     #
     flextable::add_header_row(values    = c(sex_name, group_headers, " "),
                               colwidths = c(1, group_widths, 1)) %>%
     #
     flextable::align(i = 1,
                      align = "right",
                      part  = "header") %>%
     #
     flextable::align(j = 1,
                      align = "left",
                      part  = "all") %>%
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
     flextable::bold(i = 12) %>%
     #
     flextable::hline(i = 11,
                      border = fp_border_default(width = 2)) %>%
     #
     flextable::width(j = 1,
                      width = 1.0) %>%
     #
     flextable::width(j = 2:last_column,
                      width = 0.75) %>%
     #
     flextable::line_spacing(space = 0.75,
                             part  = "body")

   return(table)

}

