# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for importing and wrangling data
# Data source: ABS National Aboriginal and Torres Strait Islander Health Survey
# https://www.abs.gov.au/statistics/people/aboriginal-and-torres-strait-islander-peoples/national-aboriginal-and-torres-strait-islander-health-survey/latest-release

# ----------------------------------------------------------------------------------
# National Aboriginal and Torres Strait Islander Health Survey data - statewide ----
# ----------------------------------------------------------------------------------
f_read_natsihs <- function(data_sheet, variable, row_range) {
  
  variable <- rlang::enquo(variable)
  
  data <- readxl::read_excel(file.path(root_folder, subfolder_natsihs, "Data_Tables_Victoria_Clean.xlsx"),
                             sheet = data_sheet) %>%
    #
    janitor::clean_names() %>%
    #
    dplyr::rename(!!variable := x1) %>% 
    #
    dplyr::filter(row_number() %in% row_range)
  
  data <- data %>%
    tidyr::pivot_longer(cols = male_estimate:total_proportion,
                        #
                        names_to  = c("sex", "measure"),
                        names_sep = "_",
                        values_to = c("value")) %>% 
    #
    tidyr::pivot_wider(names_from  = "measure",
                       values_from = "value") %>%
    #
    dplyr::mutate(sex = stringr::str_to_title(sex),
                  #
                  estimate = estimate * 1000)
  
  return(data)
  
}

