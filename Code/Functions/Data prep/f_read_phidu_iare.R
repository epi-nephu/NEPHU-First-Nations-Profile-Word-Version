# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for importing and wrangling data
# Data source: PHIDU Social Health Atlas
# https://phidu.torrens.edu.au/social-health-atlases

# ------------------------------------------------------------------------------
# PHIDU Social Atlas data - by IARE ----
# ------------------------------------------------------------------------------
f_read_phidu_iare <- function(data_sheet, column_select) {

  data <- readxl::read_xls(file.path(root_folder, subfolder_phidu, "phidu_atsi_data_ia_aust.xls"),
                           sheet        = data_sheet,
                           skip         = 4,
                           trim_ws      = TRUE,
                           .name_repair = "universal_quiet") %>%
    #
    janitor::clean_names() %>%
    #
    dplyr::select(!!!column_select) %>%
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
    dplyr::mutate(iare_name_short = dplyr::case_when(
      iare_name == "Craigieburn - Sunbury"            ~ "Craigieburn*",
      iare_name == "Melbourne - East"                 ~ "Melbourne (E)*",
      iare_name == "Melbourne - North-East"           ~ "Melbourne (NE)*",
      iare_name == "Melbourne - Port Phillip"         ~ "Port Phillip*",
      iare_name == "Moreland - Broadmeadows"          ~ "Broadmeadows*",
      iare_name == "Northcote - Preston - Whittlesea" ~ "Northcote*",
      TRUE ~ iare_name)) %>% 
    #
    dplyr::mutate(across(where(is.character), ~na_if(., "#")),
                  across(where(is.character), ~na_if(., "..")),
                  across(where(is.character), ~na_if(., "n.a."))) %>%
    #
    dplyr::mutate(across(!c(iare_name, iare_name_short), as.numeric)) %>%
    #
    dplyr::mutate(across(ends_with("_n"), ~ round(.x, digits = 0)),
                  across(ends_with("_total"), ~ round(.x, digits = 0)))

  return(data)

}

