# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025

# Extract PHESS data from PHAR for Health Consequences - Infectious Diseases

if (!require("pacman")) install.packages("pacman")

pacman::p_load(here,
               tidyverse,
               janitor,
               writexl,
               glue,
               odbc,
               DBI)

# Connect to PHAR and extract data ---------------------------------------------
start_date <- lubridate::ymd("2015-01-01")
end_date   <- lubridate::ymd("2024-12-31")

con <- DBI::dbConnect(odbc::odbc(), "PHAR", UseProxy = 1)

cases_enteric <- DBI::dbGetQuery(con,
                                 glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                             WHERE CONDITION_TYPE = 'Enteric Diseases'
                                             AND EVENT_CLASSIFICATION IN ('Confirmed', 'Probable', 'Suspected')
                                             AND EVENT_DATE >= DATE '{start_date}'
                                             AND EVENT_DATE <= DATE '{end_date}'")) %>% 
  #
  janitor::clean_names()

cases_bbv <- DBI::dbGetQuery(con,
                             glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                         WHERE CONDITION_TYPE = 'Blood Borne Viruses'
                                         AND EVENT_CLASSIFICATION IN ('Confirmed', 'Probable', 'Suspected')
                                         AND EVENT_DATE >= DATE '{start_date}'
                                         AND EVENT_DATE <= DATE '{end_date}'")) %>% 
  #
  janitor::clean_names()

cases_sti <- DBI::dbGetQuery(con,
                             glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                         WHERE CONDITION_TYPE = 'Sexually Transmissible Infections'
                                         AND EVENT_CLASSIFICATION IN ('Confirmed', 'Probable', 'Suspected')
                                         AND EVENT_DATE >= DATE '{start_date}'
                                         AND EVENT_DATE <= DATE '{end_date}'")) %>% 
  #
  janitor::clean_names()

cases_vaccine <- DBI::dbGetQuery(con,
                                 glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                             WHERE CONDITION_TYPE = 'Vaccine Preventable Diseases'
                                             AND EVENT_CLASSIFICATION IN ('Confirmed', 'Probable', 'Suspected')
                                             AND EVENT_DATE >= DATE '{start_date}'
                                             AND EVENT_DATE <= DATE '{end_date}'")) %>% 
  #
  janitor::clean_names()

cases_vector <- DBI::dbGetQuery(con,
                                glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                            WHERE CONDITION_TYPE = 'Vector Borne Diseases'
                                            AND EVENT_CLASSIFICATION IN ('Confirmed', 'Probable', 'Suspected')
                                            AND EVENT_DATE >= DATE '{start_date}'
                                            AND EVENT_DATE <= DATE '{end_date}'")) %>% 
  #
  janitor::clean_names()

cases_zoo <- DBI::dbGetQuery(con,
                             glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                         WHERE CONDITION_TYPE = 'Zoonotic'
                                         AND EVENT_CLASSIFICATION IN ('Confirmed', 'Probable', 'Suspected')
                                         AND EVENT_DATE >= DATE '{start_date}'
                                         AND EVENT_DATE <= DATE '{end_date}'")) %>% 
  #
  janitor::clean_names()

cases_other <- DBI::dbGetQuery(con,
                               glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                           WHERE CONDITION_TYPE = 'Other Conditions'
                                           AND EVENT_CLASSIFICATION IN ('Confirmed', 'Probable', 'Suspected')
                                           AND EVENT_DATE >= DATE '{start_date}'
                                           AND EVENT_DATE <= DATE '{end_date}'")) %>% 
  #
  janitor::clean_names()

DBI::dbDisconnect(con)

# Data wrangling - enterics ----------------------------------------------------
conditions <- cases_enteric %>% 
  dplyr::arrange(condition) %>% 
  dplyr::distinct(condition) %>% 
  pull(condition)

results <- list()

for (i in seq_along(conditions)) {
  
  results[[i]] <- cases_enteric %>% 
    dplyr::filter(condition == conditions[i]) %>% 
    dplyr::group_by(indigenous_status) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() 
  
}

names(results) <- conditions

results

cases_enteric <- cases_enteric %>% 
  dplyr::filter(condition %in% c("Campylobacter infection",
                                 "Cryptosporidiosis",
                                 "Hepatitis A",
                                 "Salmonellosis",
                                 "Shiga-toxin and Vero-toxin producing Escherichia coli",
                                 "Shigellosis"),
                #
                event_type == "Case") %>% 
  #
  dplyr::select(phess_id = event_id,
                condition_type,
                condition,
                event_date,
                classification = event_classification,
                lga,
                lphu,
                age_years,
                sex,
                indigenous_status,
                meets_investigation_criteria,
                epi_ticked_for_follow_up_unit_last,
                enhanced_surveillance_request,
                enhanced_surveillance_collected,
                discharge_summary_requested,
                investigation_status,
                assigned_lphu)

# Data wrangling - blood-borne viruses -----------------------------------------
conditions <- cases_bbv %>% 
  dplyr::arrange(condition) %>% 
  dplyr::distinct(condition) %>% 
  pull(condition)

results <- list()

for (i in seq_along(conditions)) {
  
  results[[i]] <- cases_bbv %>% 
    dplyr::filter(condition == conditions[i]) %>% 
    dplyr::group_by(indigenous_status) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() 
  
}

names(results) <- conditions

results

cases_bbv <- cases_bbv %>% 
  dplyr::filter(condition %in% c("Hepatitis B - Newly acquired",
                                 "Hepatitis B - Unspecified",
                                 "Hepatitis C - <24 months of age",
                                 "Hepatitis C - Newly acquired",
                                 "Hepatitis C - Unspecified"),
                #
                event_type == "Case") %>% 
  #
  dplyr::select(phess_id = event_id,
                condition_type,
                condition,
                event_date,
                classification = event_classification,
                lga,
                lphu,
                age_years,
                sex,
                indigenous_status,
                meets_investigation_criteria,
                epi_ticked_for_follow_up_unit_last,
                enhanced_surveillance_request,
                enhanced_surveillance_collected,
                discharge_summary_requested,
                investigation_status,
                assigned_lphu)

# Data wrangling - sexually transmissible infections ---------------------------
conditions <- cases_sti %>% 
  dplyr::arrange(condition) %>% 
  dplyr::distinct(condition) %>% 
  pull(condition)

results <- list()

for (i in seq_along(conditions)) {
  
  results[[i]] <- cases_sti %>% 
    dplyr::filter(condition == conditions[i]) %>% 
    dplyr::group_by(indigenous_status) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() 
  
}

names(results) <- conditions

results

cases_sti <- cases_sti %>% 
  dplyr::filter(condition %in% c("Chlamydia trachomatis infection",
                                 "Gonococcal infection",
                                 "Human Immunodeficiency Virus Infection - Child aged less than 18 months",
                                 "Human Immunodeficiency Virus Infection - Individual aged 18 months or older",
                                 "Syphilis - Congenital",
                                 "Syphilis - Infectious",
                                 "Syphilis - Late"),
                #
                event_type == "Case") %>% 
  #
  dplyr::select(phess_id = event_id,
                condition_type,
                condition,
                event_date,
                classification = event_classification,
                lga,
                lphu,
                age_years,
                sex,
                indigenous_status,
                meets_investigation_criteria,
                epi_ticked_for_follow_up_unit_last,
                enhanced_surveillance_request,
                enhanced_surveillance_collected,
                discharge_summary_requested,
                investigation_status,
                assigned_lphu)

# Data wrangling - vaccine preventable infections ------------------------------
conditions <- cases_vaccine %>% 
  dplyr::arrange(condition) %>% 
  dplyr::distinct(condition) %>% 
  pull(condition)

results <- list()

for (i in seq_along(conditions)) {
  
  results[[i]] <- cases_vaccine %>% 
    dplyr::filter(condition == conditions[i]) %>% 
    dplyr::group_by(indigenous_status) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() 
  
}

names(results) <- conditions

results

cases_vaccine <- cases_vaccine %>% 
  dplyr::filter(condition %in% c("Acute Rheumatic Fever",
                                 "Diphtheria",
                                 "Haemophilus influenzae type B infection",
                                 "Influenza",
                                 "Invasive Group A Streptococcus",
                                 "Measles",
                                 "Meningococcal infection",
                                 "Mumps",
                                 "Pertussis",
                                 "Pneumococcal infection (IPD)",
                                 "Respiratory Syncytial virus",
                                 "Rheumatic Heart Disease",
                                 "Rotavirus infection",
                                 "Varicella zoster infection (Chickenpox)",
                                 "Varicella zoster infection (Shingles)",
                                 "Varicella zoster infection (Unspecified)"),
                #
                event_type == "Case") %>% 
  #
  dplyr::select(phess_id = event_id,
                condition_type,
                condition,
                event_date,
                classification = event_classification,
                lga,
                lphu,
                age_years,
                sex,
                indigenous_status,
                meets_investigation_criteria,
                epi_ticked_for_follow_up_unit_last,
                enhanced_surveillance_request,
                enhanced_surveillance_collected,
                discharge_summary_requested,
                investigation_status,
                assigned_lphu)

# Data wrangling - vectorborne diseases ----------------------------------------
conditions <- cases_vector %>% 
  dplyr::arrange(condition) %>% 
  dplyr::distinct(condition) %>% 
  pull(condition)

results <- list()

for (i in seq_along(conditions)) {
  
  results[[i]] <- cases_vector %>% 
    dplyr::filter(condition == conditions[i]) %>% 
    dplyr::group_by(indigenous_status) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() 
  
}

names(results) <- conditions

results

cases_vector <- cases_vector %>% 
  dplyr::filter(condition %in% c("Barmah Forest virus infection",
                                 "Murray Valley encephalitis virus infection",
                                 "Mycobacterium ulcerans",
                                 "Ross River virus infection"),
                #
                event_type == "Case") %>% 
  #
  dplyr::select(phess_id = event_id,
                condition_type,
                condition,
                event_date,
                classification = event_classification,
                lga,
                lphu,
                age_years,
                sex,
                indigenous_status,
                meets_investigation_criteria,
                epi_ticked_for_follow_up_unit_last,
                enhanced_surveillance_request,
                enhanced_surveillance_collected,
                discharge_summary_requested,
                investigation_status,
                assigned_lphu)

# Data wrangling - zoonotic diseases -------------------------------------------
conditions <- cases_zoo %>% 
  dplyr::arrange(condition) %>% 
  dplyr::distinct(condition) %>% 
  pull(condition)

results <- list()

for (i in seq_along(conditions)) {
  
  results[[i]] <- cases_zoo %>% 
    dplyr::filter(condition == conditions[i]) %>% 
    dplyr::group_by(indigenous_status) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() 
  
}

names(results) <- conditions

results

cases_zoo <- cases_zoo %>% 
  dplyr::filter(condition %in% c("Leptospirosis",
                                 "Mpox",
                                 "Psittacosis",
                                 "Q Fever"),
                #
                event_type == "Case") %>% 
  #
  dplyr::select(phess_id = event_id,
                condition_type,
                condition,
                event_date,
                classification = event_classification,
                lga,
                lphu,
                age_years,
                sex,
                indigenous_status,
                meets_investigation_criteria,
                epi_ticked_for_follow_up_unit_last,
                enhanced_surveillance_request,
                enhanced_surveillance_collected,
                discharge_summary_requested,
                investigation_status,
                assigned_lphu)

# Data wrangling - other conditions --------------------------------------------
conditions <- cases_other %>% 
  dplyr::arrange(condition) %>% 
  dplyr::distinct(condition) %>% 
  pull(condition)

results <- list()

for (i in seq_along(conditions)) {
  
  results[[i]] <- cases_other %>% 
    dplyr::filter(condition == conditions[i]) %>% 
    dplyr::group_by(indigenous_status) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() 
  
}

names(results) <- conditions

results

cases_other <- cases_other %>% 
  dplyr::filter(condition %in% c("Legionellosis",
                                 "Mycobacterium ulcerans"),
                #
                event_type == "Case") %>% 
  #
  dplyr::select(phess_id = event_id,
                condition_type,
                condition,
                event_date,
                classification = event_classification,
                lga,
                lphu,
                age_years,
                sex,
                indigenous_status,
                meets_investigation_criteria,
                epi_ticked_for_follow_up_unit_last,
                enhanced_surveillance_request,
                enhanced_surveillance_collected,
                discharge_summary_requested,
                investigation_status,
                assigned_lphu)

# Bind datasets and save .csv for further analyses ----------------------------
cases_all <- cases_enteric %>% 
  dplyr::bind_rows(cases_bbv) %>% 
  dplyr::bind_rows(cases_sti) %>% 
  dplyr::bind_rows(cases_vaccine) %>% 
  dplyr::bind_rows(cases_vector) %>% 
  dplyr::bind_rows(cases_zoo) %>% 
  dplyr::bind_rows(cases_other)

write.csv(cases_all, paste0(here::here(), "/Data", "/PHAR_Extract_2015_2024.csv"), row.names = FALSE)

