# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for importing and wrangling data
# Data source: ABS Census 2021
# https://www.abs.gov.au/census

# ------------------------------------------------------------------------------
# Read in Census population data - by LGA or SA2
# ------------------------------------------------------------------------------
f_read_census_population <- function(table, level) {
  
  if(level=="lga") {
    file   = paste0("2021Census_", table, "_VIC_LGA.csv")
    output = read.csv(file.path(root_folder, census_lga_data_subfolder, file))
  }
  
  if(level=="poa") {
    file   = paste0("2021Census_", table, "_VIC_POA.csv")
    output = read.csv(file.path(root_folder, census_poa_data_subfolder, file))
  } 
  
  if(level=="sa2") {
    file   = paste0("2021Census_", table, "_VIC_SA2.csv")
    output = read.csv(file.path(root_folder, census_sa2_data_subfolder, file))
  } 
  
  return(output)

}

