# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting simple (univariate) tables

# ------------------------------------------------------------------------------
# Univariate tables - IARE ----
# ------------------------------------------------------------------------------
f_table_iare <- function(data, table_headers, last_column) {

  table <- data %>%
    flextable::flextable() %>%
    #
    flextable::set_header_labels(values = c("IARE", table_headers)) %>%
    #
    flextable::valign(valign = "bottom",
                      part   = "header") %>%
    #
    flextable::align(j = 1,
                     align = "left",
                     part  = "all") %>%
    #
    flextable::align(j = 2:last_column,
                     align = "right",
                     part  = "all") %>%
    #
    flextable::fontsize(size = 10,
                        part = "all") %>%
    #
    flextable::bold(part = "header") %>%
    #
    flextable::italic(i = 11:12) %>%
    #
    flextable::bold(i = 13) %>%
    #
    flextable::hline(i = c(10, 12),
                     border = fp_border_default(width = 2)) %>%
    #
    flextable::width(j = 1,
                     width = 2.5) %>%
    #
    flextable::width(j = 2:last_column,
                     width = 1.5) %>%
    #
    flextable::line_spacing(space = 0.75,
                            part  = "body")

  return(table)

}

