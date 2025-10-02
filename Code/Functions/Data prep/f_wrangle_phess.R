# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for importing and wrangling data
# Data source: PHESS

# ------------------------------------------------------------------------------
# Wrangle PHESS data
# ------------------------------------------------------------------------------
f_wrangle_phess <- function(data, conditions_list) {
  
  vic_cases <- data %>% 
    dplyr::filter(condition %in% conditions_list) %>% 
    #
    dplyr::group_by(condition, year, indigenous_status) %>% 
    dplyr::summarise(vic_n = n()) %>% 
    dplyr::ungroup() %>% 
    #
    tidyr::complete(condition, year, indigenous_status,
                    fill = list(vic_n = 0)) %>%
    #
    dplyr::mutate(vic_rate = dplyr::case_when(
      indigenous_status == "aboriginal"     ~ (vic_n / population_vic_aboriginal) * 100000,
      indigenous_status == "non_aboriginal" ~ (vic_n / population_vic_nonaboriginal) * 100000,
      TRUE ~ NA_integer_))
  
  notnephu_cases <- data %>% 
    dplyr::filter(condition %in% conditions_list & nephu_case == "Not NEPHU") %>% 
    #
    dplyr::group_by(condition, year, indigenous_status) %>% 
    dplyr::summarise(notnephu_n = n()) %>% 
    dplyr::ungroup() %>% 
    #
    tidyr::complete(condition, year, indigenous_status,
                    fill = list(notnephu_n = 0)) 
  
  nephu_cases <- data %>% 
    dplyr::filter(condition %in% conditions_list & nephu_case == "NEPHU") %>% 
    #
    dplyr::group_by(condition, year, indigenous_status) %>% 
    dplyr::summarise(nephu_n = n()) %>% 
    dplyr::ungroup() %>% 
    #
    tidyr::complete(condition, year, indigenous_status,
                    fill = list(nephu_n = 0)) %>%
    #
    dplyr::mutate(nephu_rate = dplyr::case_when(
      indigenous_status == "aboriginal"     ~ (nephu_n / population_nephu_aboriginal) * 100000,
      indigenous_status == "non_aboriginal" ~ (nephu_n / population_nephu_nonaboriginal) * 100000,
      TRUE ~ NA_integer_)) 
  
  cases_combined <- nephu_cases %>%
    dplyr::left_join(notnephu_cases, by = c("condition", "year", "indigenous_status")) %>% 
    dplyr::left_join(vic_cases, by = c("condition", "year", "indigenous_status"))

  return(cases_combined)

}

