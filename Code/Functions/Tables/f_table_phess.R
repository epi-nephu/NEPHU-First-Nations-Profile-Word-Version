# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting tables of counts and rates per year from PHESS

# ------------------------------------------------------------------------------
# Counts and rates per year, NEPHU and Victoria
# ------------------------------------------------------------------------------
f_table_phess <- function(data, condition_name) {
  
  table <- data %>% 
    dplyr::filter(condition == condition_name) %>% 
    #
    dplyr::select(-condition,
                  -notnephu_n) %>% 
    #
    tidyr::pivot_wider(names_from  = "indigenous_status",
                       names_vary  = "slowest",
                       values_from = c("nephu_n", "nephu_rate", "vic_n", "vic_rate")) %>%
    #
    flextable::flextable() %>%
    #
    flextable::set_header_labels(values = c("Year",
                                            "NEPHU\n(n)", "NEPHU\n(rate)", "VIC\n(n)", "VIC\n(rate)",
                                            "NEPHU\n(n)", "NEPHU\n(rate)", "VIC\n(n)", "VIC\n(rate)")) %>%
    #
    flextable::add_header_row(values = c(" ",
                                         "Identified as Aboriginal and/or Torres Strait Islander",
                                         "Did not identify as Aboriginal and/or Torres Strait Islander"),
                              #
                              colwidths = c(1, 4, 4)) %>% 
    #
    flextable::colformat_double(j = c(3, 5, 7, 9),
                                digits = 1) %>% 
    #
    flextable::align(j = 1,
                     align = "left",
                     part  = "body") %>%
    #
    flextable::align(j = c(2:9),
                     align = "right",
                     part  = "body") %>%
    #
    flextable::fontsize(size = 10,
                        part = "all") %>%
    #
    flextable::bold(part = "header") %>%
    #
    flextable::line_spacing(space = 0.75,
                            part  = "body")

  return(table)

}

