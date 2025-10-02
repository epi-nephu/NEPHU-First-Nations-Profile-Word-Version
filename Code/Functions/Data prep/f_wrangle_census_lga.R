# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for importing and wrangling data
# Data source: ABS Census 2021
# https://www.abs.gov.au/census

# ------------------------------------------------------------------------------
# Wrangle Census TableBuilder data - by LGA ----
# ------------------------------------------------------------------------------
f_wrangle_census_lga <- function(data, variable, level, dichot) {

  variable <- rlang::enquo(variable)

  if (level == "person") {

    data <- data %>%
      dplyr::group_by(lga_name, !!variable) %>%
      dplyr::summarise(across(non_indigenous:total, sum)) %>%
      dplyr::ungroup() %>%
      #
      dplyr::mutate(aboriginal_n     = aboriginal + torres_strait_islander + both_aboriginal_and_torres_strait_islander,
                    non_aboriginal_n = non_indigenous + not_stated)

  } else if (level == "household") {

    data <- data %>%
      dplyr::mutate(aboriginal_n     = household_with_aboriginal_and_or_torres_strait_islander_person_s,
                    non_aboriginal_n = other_households + not_applicable)

    }

  data <- data %>%
    dplyr::group_by(lga_name,
                    !!variable) %>%
    #
    dplyr::summarise(aboriginal_n     = sum(aboriginal_n),
                     non_aboriginal_n = sum(non_aboriginal_n)) %>%
    #
    dplyr::ungroup()

  data <- data %>%
    tidyr::pivot_wider(id_cols     = "lga_name",
                       names_from  = !!variable,
                       values_from = c(aboriginal_n, non_aboriginal_n)) %>%
    #
    janitor::clean_names() %>%
    #
    dplyr::mutate(aboriginal_total     = rowSums(select(., starts_with("aboriginal_"))),
                  non_aboriginal_total = rowSums(select(., starts_with("non_aboriginal_"))))

  data <- data %>%
    dplyr::bind_rows(data %>%
                       dplyr::filter(lga_name %in% nephu_lga) %>%
                       #
                       janitor::adorn_totals(name = "NEPHU") %>%
                       #
                       dplyr::filter(lga_name == "NEPHU")) %>%
    #
    dplyr::mutate(lga_name = factor(lga_name, levels = c(nephu_lga, "NEPHU", "Other Metro", "VIC"))) %>%
    #
    dplyr::arrange(lga_name)

  if (dichot == "yes") {

    data <- data %>%
      dplyr::mutate(aboriginal_prop     = aboriginal_n_yes / aboriginal_total * 100,
                    non_aboriginal_prop = non_aboriginal_n_yes / non_aboriginal_total * 100)

    data <- data %>%
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
      dplyr::mutate(across(where(is.numeric), ~replace(., is.na(.), 0)),
                    across(where(is.numeric), ~replace(., is.nan(.), 0))) %>% 
      #
      dplyr::select(lga_name,
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

