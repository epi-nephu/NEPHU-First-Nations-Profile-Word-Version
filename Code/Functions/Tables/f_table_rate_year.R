# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting simple (univariate) tables

# ------------------------------------------------------------------------------
# Rates per year tables ----
# ------------------------------------------------------------------------------
f_table_rate_year <- function(data) {

  table <- data %>%
    flextable::flextable() %>%
    #
    flextable::set_header_labels(values = c("Year", "Yes\n(rate)", "No\n(rate)")) %>%
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
    flextable::width(j = 1,
                     width = 0.75) %>%
    #
    flextable::width(j = 2:3,
                     width = 1.5) %>%
    #
    flextable::line_spacing(space = 0.75,
                            part  = "body")

  return(table)

}

