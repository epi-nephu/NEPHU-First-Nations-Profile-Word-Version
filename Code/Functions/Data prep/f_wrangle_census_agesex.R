# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for importing and wrangling data
# Data source: ABS Census 2021
# https://www.abs.gov.au/census

# ------------------------------------------------------------------------------
# Wrangle Census TableBuilder data - by age and sex ----
# ------------------------------------------------------------------------------
f_wrangle_census_agesex <- function(data, variable, dichot) {
  
  variable <- rlang::enquo(variable)
  
  data <- data %>% 
    dplyr::mutate(
      aboriginal_n     = aboriginal + torres_strait_islander + both_aboriginal_and_torres_strait_islander,
      non_aboriginal_n = non_indigenous + not_stated,
      #
      age_group = dplyr::case_when(
        age_group == "0-9 years"   ~ "0-9 yrs",
        age_group == "10-19 years" ~ "10-19 yrs",
        age_group == "20-29 years" ~ "20-29 yrs",
        age_group == "30-39 years" ~ "30-39 yrs",
        age_group == "40-49 years" ~ "40-49 yrs",
        age_group == "50-59 years" ~ "50-59 yrs",
        age_group == "60-69 years" ~ "60-69 yrs",
        age_group == "70-79 years" ~ "70-79 yrs",
        age_group == "80-89 years" ~ "80-89 yrs",
        age_group == "90-99 years" ~ "90-99 yrs",
        age_group == "100 years and over" ~ "100+ yrs",
        TRUE ~ NA_character_)) %>%
    #
    dplyr::group_by(lga_name,
                    age_group,
                    sex,
                    !!variable) %>%
    #
    dplyr::summarise(aboriginal_n     = sum(aboriginal_n),
                     non_aboriginal_n = sum(non_aboriginal_n)) %>%
    #
    dplyr::ungroup()
  
  data <- data %>% 
    dplyr::filter(lga_name %in% nephu_lga) %>% 
    #
    dplyr::group_by(age_group,
                    sex,
                    !!variable) %>%
    #
    dplyr::summarise(aboriginal_n     = sum(aboriginal_n),
                     non_aboriginal_n = sum(non_aboriginal_n)) %>%
    #
    dplyr::ungroup() %>%
    #
    dplyr::bind_rows(data %>%
                       dplyr::filter(lga_name %in% nephu_lga) %>%
                       #
                       dplyr::group_by(sex,
                                       !!variable) %>%
                       #
                       dplyr::summarise(aboriginal_n     = sum(aboriginal_n),
                                        non_aboriginal_n = sum(non_aboriginal_n)) %>% 
                       #
                       dplyr::mutate(age_group = "Total") %>% 
                       #
                       dplyr::ungroup()) %>% 
    #
    tidyr::pivot_wider(names_from  = !!variable,
                       values_from = c("aboriginal_n", "non_aboriginal_n")) %>%
    #
    janitor::clean_names() %>% 
    #
    dplyr::mutate(age_group = factor(age_group, levels = c(age_group_lvl, "Total"))) %>% 
    #
    dplyr::arrange(age_group)

  if (dichot == "yes") {

    data <- data %>%
      dplyr::mutate(aboriginal_total     = aboriginal_n_yes + aboriginal_n_no,
                    non_aboriginal_total = non_aboriginal_n_yes + non_aboriginal_n_no,
                    aboriginal_prop      = aboriginal_n_yes / aboriginal_total * 100,
                    non_aboriginal_prop  = non_aboriginal_n_yes / non_aboriginal_total * 100)

    data <- data %>%
      dplyr::mutate(across(where(is.numeric), ~replace(., is.na(.), 0)),
                    across(where(is.numeric), ~replace(., is.nan(.), 0))) %>%
      #
      dplyr::mutate(aboriginal_exp_non    = round((non_aboriginal_prop / 100) * aboriginal_total, digits = 0),
                    aboriginal_difference = aboriginal_n_yes - aboriginal_exp_non,
                    #
                    aboriginal_diff_format = formatC(abs(aboriginal_difference), big.mark = ","),
                    #
                    aboriginal_diff_label = dplyr::case_when(
                      aboriginal_difference > 0 ~ paste0(aboriginal_diff_format, " more"),
                      aboriginal_difference < 0 ~ paste0(aboriginal_diff_format, " fewer"),
                      TRUE ~ "None")) %>%
      #
      dplyr::select(age_group,
                    sex,
                    aboriginal_n = aboriginal_n_yes,
                    aboriginal_total,
                    aboriginal_prop,
                    aboriginal_exp_non,
                    non_aboriginal_n = non_aboriginal_n_yes,
                    non_aboriginal_total,
                    non_aboriginal_prop,
                    aboriginal_difference,
                    aboriginal_diff_label)

  } else if (dichot == "no") {

    data <- data

    }
  
  return(data)
  
}

