# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for creating and formatting grouped tables

# ------------------------------------------------------------------------------
# Grouped tables - health/social survey data, by sex ----
# ------------------------------------------------------------------------------
f_table_survey_sex <- function(data, digits = 1, x_labels, variable_label, total_row) {
  
  data_number <- data %>% 
    dplyr::select(sex,
                  ends_with(paste0("_", "n"))) %>%
    #
    tidyr::pivot_longer(cols      = !sex,
                        names_to  = "comparison",
                        values_to = "n") %>%
    #
    dplyr::mutate(comparison = stringr::str_remove(comparison, "\\_.*"),
                  comparison = dplyr::recode(comparison, !!!x_labels),
                  comparison = factor(comparison, levels = x_labels))
  
  data_percent <- data %>%
    dplyr::select(sex,
                  ends_with(paste0("_", "prop"))) %>%
    #
    tidyr::pivot_longer(cols      = !sex,
                        names_to  = "comparison",
                        values_to = "prop") %>%
    #
    dplyr::mutate(comparison = stringr::str_remove(comparison, "\\_.*"),
                  comparison = dplyr::recode(comparison, !!!x_labels),
                  comparison = factor(comparison, levels = x_labels),
                  #
                  prop = round(prop, digits = digits))
  
  table <- data_number %>%
    dplyr::left_join(data_percent, by = c("sex", "comparison")) %>%
    #
    tidyr::pivot_wider(names_from  = sex,
                       values_from = c("n", "prop"),
                       names_vary  = "slowest") %>%
    #
    dplyr::arrange(comparison) %>%
    #
    flextable::flextable() %>%
    #
    flextable::set_header_labels(values = c(variable_label,
                                            "Estimate", "Percent",
                                            "Estimate", "Percent",
                                            "Estimate", "Percent")) %>%
    #
    flextable::add_header_row(values    = c(" ", "Male", "Female", "Total"),
                              colwidths = c(1, 2, 2, 2)) %>%
    #
    flextable::merge_v(j = 1) %>%
    #
    flextable::colformat_double(j = c(3, 5, 7),
                                digits = 1) %>% 
    #
    flextable::align(i = 1,
                     align = "center",
                     part  = "header") %>%
    #
    flextable::align(j = 1,
                     align = "left",
                     part  = "body") %>%
    #
    flextable::align(j = c(2:7),
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
    flextable::hline(i = (total_row - 1),
                     border = fp_border_default(width = 2)) %>%
    #
    flextable::autofit() %>%
    #
    flextable::line_spacing(space = 0.75,
                            part  = "body")

  return(table)

}

