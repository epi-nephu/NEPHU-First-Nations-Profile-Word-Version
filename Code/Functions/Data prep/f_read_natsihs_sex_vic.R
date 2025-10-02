# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for importing and wrangling data
# Data source: ABS National Aboriginal and Torres Strait Islander Health Survey
# https://www.abs.gov.au/statistics/people/aboriginal-and-torres-strait-islander-peoples/national-aboriginal-and-torres-strait-islander-health-survey/latest-release

# ---------------------------------------------------------------------------------------
# National Aboriginal and Torres Strait Islander Health Survey data - by sex ----
# ---------------------------------------------------------------------------------------
f_read_natsihs_sex_vic <- function(data) {

  data <- read.csv(file.path(root_folder, subfolder_natsihs, data),
                   skip        = 9,
                   strip.white = TRUE) %>%
    #
    dplyr::rename(iare_name = colnames(.)[1],
                  sex       = colnames(.)[2]) %>%
    #
    janitor::clean_names() %>%
    #
    dplyr::mutate(iare_name = if_else(iare_name == "", NA_character_, iare_name),
                  sex       = if_else(sex == "", NA_character_, sex)) %>%
    #
    tidyr::fill(iare_name, sex) %>%
    #
    dplyr::filter(iare_name == "Total" & sex %in% c("Male", "Female")) %>%
    #
    dplyr::mutate(iare_name = dplyr::case_when(
      iare_name == "Total" ~ "Victoria",
      TRUE ~ NA_character_)) %>%
    #
    dplyr::select(iare_name,
                  everything(),
                  -ends_with("_rse"),
                  -ends_with("_annotations"),
                  -x_1) %>%
    #
    dplyr::rename_with(.fn = ~ paste0(., "_n"), .cols = 3:ncol(.)) %>% 
    #
    dplyr::mutate(across(!c(iare_name, sex), as.numeric)) %>%
    #
    dplyr::mutate_if(is.numeric, ~ . * 1000)

  return(data)

}

