# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for importing and wrangling data
# Data source: PHIDU Social Health Atlas
# https://phidu.torrens.edu.au/social-health-atlases

# ------------------------------------------------------------------------------
# PHIDU Social Atlas hospital admissions data - by IARE ----
# ------------------------------------------------------------------------------
f_read_phidu_iare_hospital <- function(data_sheet, column_select) {

  data <- readxl::read_xls(file.path(root_folder, subfolder_phidu, "phidu_atsi_data_ia_aust.xls"),
                           sheet        = data_sheet,
                           trim_ws      = TRUE,
                           .name_repair = "minimal") %>%
    #
    janitor::clean_names() %>%
    #
    dplyr::select(iare_name = dplyr::all_of(names(.)[2]),
                  hospital_n = which(names(.) == column_select),
                  hospital_rate = which(names(.) == column_select) + 1) %>% 
    #
    dplyr::filter(iare_name %in% c("Craigieburn - Sunbury",
                                   "Knox",
                                   "Maroondah",
                                   "Melbourne - East",
                                   "Melbourne - North-East",
                                   "Melbourne - Port Phillip",
                                   "Moreland - Broadmeadows",
                                   "Northcote - Preston - Whittlesea",
                                   "Whitehorse",
                                   "Yarra Ranges",
                                   #
                                   "Greater Melbourne",
                                   "Rest of Victoria",
                                   "Victoria")) %>%
    #
    dplyr::mutate(
      iare_name = dplyr::case_when(
        iare_name == "Craigieburn - Sunbury"            ~ "Craigieburn/Sunbury",
        iare_name == "Melbourne - East"                 ~ "Melbourne East",
        iare_name == "Melbourne - North-East"           ~ "Melbourne North-East",
        iare_name == "Melbourne - Port Phillip"         ~ "Melbourne/Port Phillip",
        iare_name == "Moreland - Broadmeadows"          ~ "Moreland/Broadmeadows",
        iare_name == "Northcote - Preston - Whittlesea" ~ "Northcote/Preston/Whittlesea",
        TRUE ~ iare_name),
      #
      iare_name_short = dplyr::case_when(
        iare_name == "Craigieburn/Sunbury"          ~ "Craigieburn*",
        iare_name == "Melbourne East"               ~ "Melbourne (E)*",
        iare_name == "Melbourne North-East"         ~ "Melbourne (NE)*",
        iare_name == "Melbourne/Port Phillip"       ~ "Port Phillip*",
        iare_name == "Moreland/Broadmeadows"        ~ "Broadmeadows*",
        iare_name == "Northcote/Preston/Whittlesea" ~ "Northcote*",
        TRUE ~ iare_name))
  
  data <- data %>%
    dplyr::mutate(across(where(is.character), ~na_if(., "#")),
                  across(where(is.character), ~na_if(., "..")),
                  across(where(is.character), ~na_if(., "n.a."))) %>%
    #
    dplyr::mutate(across(!c(iare_name, iare_name_short), as.numeric)) %>%
    #
    dplyr::mutate(hospital_ratio = hospital_rate / hospital_rate[12] * 100) %>% 
    #
    dplyr::mutate(across(ends_with("_n"), ~ round(.x, digits = 0))) %>% 
    #
    dplyr::select(iare_name,
                  iare_name_short,
                  hospital_n,
                  hospital_rate,
                  hospital_ratio)

  return(data)

}

