# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting simple (univariate) tables

# ------------------------------------------------------------------------------
# Population tables ----
# ------------------------------------------------------------------------------
f_table_population <- function(data, total_row, table_headers) {
  
  table <- flextable::flextable(data) %>%
    flextable::set_header_labels(values = table_headers) %>%
    #
    flextable::valign(valign = "bottom",
                      part   = "header") %>% 
    #
    flextable::align(j = 1,
                     align = "left",
                     part  = "body") %>%
    #
    flextable::align(j = c(2:4),
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
                     width = 1.5) %>%
    #
    flextable::width(j = 2:4,
                     width = 1.75) %>% 
    #
    flextable::line_spacing(space = 0.75,
                            part  = "body")
  
  return(table)
  
}

