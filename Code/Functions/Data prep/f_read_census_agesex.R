# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for importing and wrangling data
# Data source: ABS Census 2021
# https://www.abs.gov.au/census

# ------------------------------------------------------------------------------
# Read in Census TableBuilder data - by age and sex ----
# ------------------------------------------------------------------------------
f_read_census_agesex <- function(file, variable) {
  
  data <- read.csv(file.path(root_folder, tablebuilder_subfolder, file), 
                   skip        = 9, 
                   strip.white = TRUE) %>% 
    #
    janitor::clean_names()
    
  data <- data[2:(grep("Data source", data[, 1]) -1), ] %>% 
    dplyr::rename(lga_name    = colnames(data)[1],
                  age_group   = colnames(data)[2],
                  sex         = colnames(data)[3],
                  !!variable := colnames(data)[4]) %>%
    #
    dplyr::mutate(lga_name  = if_else(lga_name == "Total", "VIC", lga_name),
                  lga_name  = if_else(lga_name == "", NA_character_, lga_name),
                  age_group = if_else(age_group == "", NA_character_, age_group),
                  sex       = if_else(sex == "", NA_character_, sex)) %>% 
    #
    tidyr::fill(lga_name, age_group, sex)
  
  data <- data %>% 
    dplyr::left_join(lphu_assign %>% 
                       dplyr::select(lga_name,
                                     lphu_long,
                                     lphu_short), 
                     by = "lga_name") %>% 
    #
    dplyr::mutate(
      lphu_long = dplyr::case_when(
        lga_name == "VIC" ~ "Victoria",
        TRUE ~ as.character(lphu_long)),
      #
      lphu_short = dplyr::case_when(
        lga_name == "VIC" ~ "VIC",
        TRUE ~ as.character(lphu_short)),
      #
      lga_name = dplyr::case_when(
        lphu_short %in% c("SEPHU", "WPHU") ~ "Other Metro",
        TRUE ~ as.character(lga_name))) %>% 
    #
    dplyr::filter(lga_name %in% c(nephu_lga, "Other Metro", "VIC"))
  
  return(data)
  
}

