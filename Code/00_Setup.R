# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025

# General code that applies to all indicators
# - Data folders
# - LGA reference data
# - Constants
# - Colours
# - Standard footnotes

# ------------------------------------------------------------------------------
# Data folders ----
# ------------------------------------------------------------------------------
root_folder <- str_extract(getwd(), "^.+Epidemiology")

# NEPHU map objects
load(file.path(root_folder, "Population Health Data/NEPHU Maps", "NEPHU_basemaps_sf.RData"))

# ABS Census 2021
census_lga_data_subfolder <- "Population Health Data/ABS Census Population Housing 2021 (GCP)/Data/LGA/VIC"
census_sa2_data_subfolder <- "Population Health Data/ABS Census Population Housing 2021 (GCP)/Data/SA2/VIC"
tablebuilder_subfolder    <- "Population Health Data/ABS Census Population Housing 2021 (GCP)/Data/TableBuilder/Aboriginal"

# Other data sources
subfolder_birth       <- "Population Health Data/ABS Births and Mortality/Data/Raw data"
subfolder_cancer      <- "Population Health Data/VIC Cancer Registry/Data"
subfolder_coroner     <- "Population Health Data/VIC Coroners Court/Data"
subfolder_corrective  <- "Population Health Data/Corrective Services/Data"
subfolder_crime       <- "Population Health Data/Crime Statistics Agency/Data"
subfolder_ctg         <- "Population Health Data/Closing the Gap/Data"
subfolder_death       <- "Population Health Data/ABS Births and Mortality/Data/Raw data"
subfolder_framework   <- "Population Health Data/AIHW Aboriginal Health Performance Framework/Data"
subfolder_natsihs     <- "Population Health Data/ABS National Aboriginal Health Survey 2022-23/Data"
subfolder_phidu       <- "Population Health Data/PHIDU/Data"
subfolder_projections <- "Population Health Data/Population Projections/Data"
subfolder_protection  <- "Population Health Data/Child Protection/Data"
subfolder_screening   <- "Population Health Data/Cancer Screening Participation Data/Data"
subfolder_vcams       <- "Population Health Data/VCAMS/Data"

# ------------------------------------------------------------------------------
# LGA reference data ----
# ------------------------------------------------------------------------------
lphu_assign <- readxl::read_xlsx(file.path(root_folder, "Population Health Data/LPHU", "LPHU Classification.xlsx")) %>%
  dplyr::rename(lga_name_long = lga_name,
                lga_name      = lga_name_2) %>%
  #
  dplyr::mutate(lga_name      = stringr::str_to_title(lga_name),
                LGA_CODE_2021 = paste0("LGA", lga_id))

# ------------------------------------------------------------------------------
# Constants ----
# ------------------------------------------------------------------------------
# Age groups
age_group_lvl <- c("0-9 yrs", "10-19 yrs", "20-29 yrs", "30-39 yrs",
                   "40-49 yrs", "50-59 yrs", "60-69 yrs", "70-79 yrs",
                   "80-89 yrs", "90-99 yrs", "100+ yrs")

# LPHUs
lphu_names_short <- c("NEPHU", "SEPHU", "WPHU", "BSWPHU", "LMPHU",
                      "GPHU", "GWSMPHU", "GVPHU", "OMPHU")
 
lphu_names_long <- c("North Eastern", "South Eastern", "Western",
                     "Barwon South West", "Loddon Mallee",
                     "Gippsland", "Grampians Wimmera Southern Mallee",
                     "Goulburn Valley", "Ovens Murray")

# NEPHU geographies
nephu_lga <- nephu_lga.sf$lga_name
nephu_poa <- nephu_poa.sf$poa_code
nephu_sa2 <- nephu_sa2.sf$sa2_code
nephu_sal <- nephu_sal.sf$sal_name

# NEPHU LGAs
lga_nephu <- c("Banyule", "Boroondara", "Darebin", 
               "Hume", "Knox", "Manningham", 
               "Maroondah", "Nillumbik", "Whitehorse", 
               "Whittlesea", "Yarra", "Yarra Ranges")

# LGA labels to 'nudge' out of the way in maps
nudge_lga <- c("Boroondara", "Darebin", "Yarra", "Maroondah", "Banyule")

# IAREs
iare_names <- c("Craigieburn/Sunbury", "Knox", "Maroondah", "Melbourne East",
                "Melbourne North-East", "Melbourne/Port Phillip", 
                "Moreland/Broadmeadows", "Northcote/Preston/Whittlesea",
                "Whitehorse", "Yarra Ranges")

iare_short <- c("Craigieburn*", "Knox", "Maroondah", "East Melbourne",
                "North East Melbourne", "Port Phillip*", 
                "Broadmeadows*", "Northcote*",
                "Whitehorse", "Yarra Ranges")

iare_inc_vic_lvl <- c("Craigieburn/Sunbury", "Knox", "Maroondah", "Melbourne East",
                      "Melbourne North-East", "Melbourne/Port Phillip", 
                      "Moreland/Broadmeadows", "Northcote/Preston/Whittlesea",
                      "Whitehorse", "Yarra Ranges",
                      #
                      "Greater Melbourne", "Rest of Victoria", "Victoria")

# Notifiable disease groupings
conditions_enteric <- c("Campylobacter infection", 
                        "Cryptosporidiosis", 
                        "Hepatitis A", 
                        "Rotavirus infection", 
                        "Salmonellosis", 
                        "Shiga-toxin and Vero-toxin producing Escherichia coli", 
                        "Shigellosis")

conditions_bbv_sti <- c("Chlamydia trachomatis infection", 
                        "Gonococcal infection", 
                        "Hepatitis B - Newly acquired", 
                        "Hepatitis B - Unspecified",
                        "Hepatitis C - <24 months of age", 
                        "Hepatitis C - Newly acquired", 
                        "Hepatitis C - Unspecified", 
                        "Human Immunodeficiency Virus Infection - Child aged less than 18 months",
                        "Human Immunodeficiency Virus Infection - Individual aged 18 months or older", 
                        "Mpox", 
                        "Syphilis - Congenital",
                        "Syphilis - Infectious", 
                        "Syphilis - Late")

conditions_vax_resp <- c("Diphtheria", 
                         "Haemophilus influenzae type B infection", 
                         "Influenza", 
                         "Measles", 
                         "Meningococcal infection", 
                         "Mumps",
                         "Pertussis", 
                         "Pneumococcal infection (IPD)", 
                         "Respiratory Syncytial virus", 
                         "Varicella zoster infection (Chickenpox)",
                         "Varicella zoster infection (Shingles)", 
                         "Varicella zoster infection (Unspecified)")

conditions_vbd_zoo <- c("Barmah Forest virus infection", 
                        "Leptospirosis", 
                        "Murray Valley encephalitis virus infection",
                        "Mycobacterium ulcerans",
                        "Psittacosis", 
                        "Q Fever", 
                        "Ross River virus infection")

conditions_other <- c("Acute Rheumatic Fever", 
                      "Invasive Group A Streptococcus", 
                      "Legionellosis", 
                      "Rheumatic Heart Disease")

# Population counts for rate calculations
# Based on ABS Census 2021
population_vic_aboriginal    <- 65639
population_vic_nonaboriginal <- 6482401

population_nephu_aboriginal    <- 12065
population_nephu_nonaboriginal <- 1781207

# ------------------------------------------------------------------------------
# Colours ----
# ------------------------------------------------------------------------------
colour_burgundy   <- "#5C1441"
colour_darkblue   <- "#191D43"
colour_lightblue  <- "#374091"
colour_dodgerblue <- "#1C86EE"
colour_orange     <- "#D55E00"
colour_green      <- "#5BC788"
colour_yellow     <- "#F0E442"

colour_white      <- "white"
colour_black      <- "black"
colour_gray       <- "gray30"

colour_gradient_low  <- "#FFFF99"
colour_gradient_mid  <- "#41B6C4"
colour_gradient_high <- "#253494"

colour_table_light <- "#B5CEFA"
colour_table_mid   <- "#8FB6F5"
colour_table_dark  <- "#1C86EE"

# ------------------------------------------------------------------------------
# Standard footnotes: data sources ----
# ------------------------------------------------------------------------------
note_data_cancer_screen <- glue::glue("
- Data were obtained from the [Victorian Cancer Screening Framework (VCSF)](https://acpcc.org.au/services/population-health/), maintained by the Australian Centre for the Prevention of Cervical Cancer (ACPCC).

  - The VCSF collates data from the National Cancer Screening Register (cervical and bowel screening) and BreastScreen Victoria in order to monitor and improve prevention and early detection of breast, bowel, and cervical cancers in Victoria, particularly among priority communities.
  
  - Aboriginal and/or Torres Strait Islander peoples are defined based on the recording of Aboriginal and/or Torres Strait Islander status in the individual datasets that make up the VCSF.

  - Data broken down by Aboriginal and/or Torres Strait Islander status are not publicly available on the ACPCC website but can be requested from the ACPCC.")

note_data_census <- glue::glue("
- Data were obtained from the [ABS Census 2021](https://www.abs.gov.au/census) and are publicly available at Local Government Area (LGA), ABS Statistical Area Level 2 (SA2), and Indigenous Area of Residence (IARE) level.
  
  - The 2021 Census was held on the evening of August 10th 2021.
                                     
  - Aboriginal and/or Torres Strait Islander peoples were defined as those who identified themselves as Aboriginal and/or Torres Strait Islander in the Census.")

note_data_ctg_suicide <- glue::glue("
- Data were obtained from the [Closing the Gap Information Repository](https://www.pc.gov.au/closing-the-gap-data), compiled from the [ABS Causes of Death Data](https://www.abs.gov.au/statistics/health/causes-death).

  - The Closing the Gap Information Repository brings together data from a wide range of data sources to provide up to date for on each Closing the Gap target, indicator, and contextual factors.
  
  - The ABS processes information about all deaths that are registered in Australia and apply coding rules to determine the underlying and contributing causes of death for each death.
  
  - 'Australia' includes NSW, Victoria, Queensland, Western Australia, South Australia, and the Northern Territory.")

note_data_crime <- glue::glue("
- Data were obtained from the [Victorian Crime Statistics Agency](https://www.crimestatistics.vic.gov.au/crime-statistics/latest-aboriginal-crime-data/family-incidents-by-aboriginal-and-torres-strait).

  - The Victorian Crime Statistics Agency collates information about crimes reported to Victoria Police and recorded in the Law Enforcement Assistance Program database.
 
  - Aboriginal and/or Torres Strait Islander status for affected family members and other parties is based on the most frequent recording of the Indigenous status for each person in the LEAP database. 
                              
  - If a person appears in the database multiple times, the most frequently recorded response is taken as being correct.")

note_data_child_protection <- glue::glue("
- Data were obtained from the [AIHW Child Protection National Minimum Dataset](https://www.aihw.gov.au/about-our-data/our-data-collections/child-protection-national-minimum-data-set).

  - The AIHW Child Protection National Minimum Dataset includes information about children who come into contact with State and Territory departments responsible for child protection.
 
  - Aboriginal and/or Torres Strait Islander peoples are defined as those who are identified as such in the individual state and territory data collections that make up the AIHW Child Protection National Minimum Dataset.")

note_data_framework_air <- glue::glue("
- Data were obtained from the [AIHW Aboriginal and Torres Strait Islander Performance Framework](https://www.indigenoushpf.gov.au/), compiled from the [Australian Immunisation Register (AIR)](https://www.servicesaustralia.gov.au/australian-immunisation-register).

  - The AIHW Aboriginal and Torres Strait Islander Framework brings together information from a wide range of national, jurisdictional, and regional data sources to report on determinants of health, health outcomes, and health system performance for Aboriginal and/or Torres Strait Islander peoples.

  - The AIR records all vaccines given to people in Australia and includes vaccines given under the National Immunisation Program, through school programs, and privately (e.g. travel-related vaccines).
  
  - Aboriginal and/or Torres Strait Islander peoples were defined as those recorded as Aboriginal and/or Torres Strait Islander in the AIR based on their Medicare records or as recorded by an immunisation provider.")

note_data_framework_breastscreen <- glue::glue("
- Data were obtained from the [AIHW Aboriginal and Torres Strait Islander Performance Framework](https://www.indigenoushpf.gov.au/), compiled from [BreastScreen Australia](https://www.health.gov.au/our-work/breastscreen-australia-program).

  - The AIHW Aboriginal and Torres Strait Islander Framework brings together information from a wide range of national, jurisdictional, and regional data sources to report on determinants of health, health outcomes, and health system performance for Aboriginal and/or Torres Strait Islander peoples.

  - BreastScreen Australia is the national free breast cancer screening program. 

  - For Aboriginal and/or Torres Strait Islander women and people with breasts, screening is recommended every two years from 40-74 years of age. For non-Indigenous women and people with breasts, screening is recommended every two years from 50-74 years of age.

  - Aboriginal and/or Torres Strait Islander women were defined as those who identified themselves as Aboriginal and/or Torres Strait Islander at the time of their breast screening appointment.")

note_data_justice_adult <- glue::glue("
- Data were obtained from the [ABS Prisoners in Australia Dataset](https://www.abs.gov.au/statistics/people/crime-and-justice/prisoners-australia/latest-release).

  - The ABS Prisoners in Australia Dataset includes all persons remanded or sentenced to adult custodial corrective services agencies in each Australian state and territory.

  - Aboriginal and/or Torres Strait Islander peoples are defined as those who identified themselves as Aboriginal and/or Torres Strait Islander at the time of their contact with a corrective service agency.")

note_data_justice_youth <- glue::glue("
- Data were obtained from the [AIHW Youth Justice National Minimum Dataset](https://www.aihw.gov.au/about-our-data/our-data-collections/youth-justice).

  - The AIHW Youth Justice National Minimum Dataset includes information about young people in Australia who were under youth justice supervision in each Australian state and territory.
 
  - Aboriginal and/or Torres Strait Islander peoples are defined as those who identified themselves as Aboriginal and/or Torres Strait Islander at the time of their contact with the youth justice system.")

note_data_mortality_death <- glue::glue("
- Data were obtained from the ABS Technical Note [*The impact of using multiple sources for deriving the Indigenous status of deaths in 2023*](https://www.abs.gov.au/articles/technical-note-impact-using-multiple-sources-deriving-indigenous-status-deaths-2023-changes-victoria-and-coroner-referred-deaths).

  - The ABS has implemented an enhanced process for ascertaining Aboriginal and/or Torres Strait Islander status for deaths registered from 2023 onwards in Victoria.
  
  - This enhanced process assigns Aboriginal and/or Torres Strait Islander status based on data from three sources: the Death Registration Statement (completed by the person who notified the death, usually the funeral director or a family member), the Medical Certificate Cause of Death (for deaths that were certified by a doctor), and the National Coronial Information System (for deaths that were referred to the coroner).
  
  - If these data sources do not agree, identification on any source is usually given preference over recording the deceased as non-Indigenous.
  
  - Prior to 2023, Aboriginal and/or Torres Strait Islander status was assigned based on the Death Registration Statement only in Victoria. Data for 2023 onwards cannot be reliably compared to data from previous years.")

note_data_mortality_life <- glue::glue("
- Data were obtained from the [ABS Aboriginal and Torres Strait Islander Life Expectancy Report](https://www.abs.gov.au/statistics/people/aboriginal-and-torres-strait-islander-peoples/aboriginal-and-torres-strait-islander-life-expectancy/2020-2022).

  - Life expectancy estimates are only available for the populations of New South Wales, Queensland, Western Australia, and the Northern Territory. These four jurisdictions have numbers of deaths sufficient to construct separate reliable life expectancy estimates. However, life expectancy estimates for Australia include deaths from all states and territories.
  
  - Aboriginal and/or Torres Strait Islander peoples are defined based on the recording of Aboriginal and/or Torres Strait Islander status in state and territory death registers.
  
  - Life expectancy estimates are calculated taking age-specific identification rates into account.")

note_data_mortality_suicide <- glue::glue("
- Data were sourced from the Coroners Court of Victoria [Suicides of Aboriginal and Torres Strait Islander People Report (2018-2023)](https://www.coronerscourt.vic.gov.au/suicides-aboriginal-and-torres-strait-islander-people-victoria-2018-2023).
  
  - This report is updated annually and provides an overview of suicides during the preceding five-year period. Data include in this report were compiled from information recorded in the Victorian Suicide Register.
                                          
  - The Victorian Suicide Register contains detailed information on suicides in Victoria, including information on demographics and potential contributing factors and stressors.")

note_data_natsihs <- glue::glue("
- Data were obtained from the [2022-23 ABS National Aboriginal and Torres Strait Islander Health Survey](https://www.abs.gov.au/statistics/people/aboriginal-and-torres-strait-islander-peoples/national-aboriginal-and-torres-strait-islander-health-survey/latest-release).

  - The 2022-23 ABS National Aboriginal and Torres Strait Islander Health Survey collected information from Aboriginal and/or Torres Strait Islander people of all ages in non-remote and remote areas of Australia, including discrete Aboriginal and/or Torres Strait Islander communities.
                                
  - Data were collected through face-to-face interviews with survey respondents or their parent/guardian.")

note_data_natsihs_2019 <- glue::glue("
- Data were obtained from the [2018-19 ABS National Aboriginal and Torres Strait Islander Health Survey](https://www.abs.gov.au/statistics/people/aboriginal-and-torres-strait-islander-peoples/national-aboriginal-and-torres-strait-islander-health-survey/latest-release).

  - The 2018-19 ABS National Aboriginal and Torres Strait Islander Health Survey collected information from Aboriginal and/or Torres Strait Islander people of all ages in non-remote and remote areas of Australia, including discrete Aboriginal and/or Torres Strait Islander communities.
                                
  - Data were collected through face-to-face interviews with survey respondents or their parent/guardian.")

note_data_phess <- glue::glue("
- Data were obtained from the [Victorian Public Health Event Surveillance System (PHESS)](https://www.health.vic.gov.au/infectious-diseases/infectious-diseases-surveillance-in-victoria).

  - PHESS contains data on conditions that are legally required to be notified to the Victorian Department of Health by medical practitioners and laboratories.
  
  - Aboriginal and/or Torres Strait Islander peoples are defined based on the recording of Aboriginal and/or Torres Strait Islander status at the time of notification by a laboratory or medical practitioner or during case management and follow-up by Local Public Health Units or the Victorian Department of Health.

  - Data presented here are for notifications where Aboriginal and/or Torres Strait Islander status was known. The completeness of data on Aboriginal and/or Torres Strait Islander status varies across conditions and over time, as the percentage of notifications signed out for case management and follow-up varies across conditions and has increased since the transition of case management and follow-up to Local Public Health Units in 2022-2023.
  
  - Data broken down by Aboriginal and/or Torres Strait Islander status are publicly available for a [selection of conditions at statewide level](https://www.health.vic.gov.au/infectious-diseases/aboriginal-and-torres-strait-islander-summary-state-wide-victoria). Additional data may be made available upon request to the Victorian Department of Health.")                              

note_data_phidu_vaed <- glue::glue("
- Data were obtained from the [PHIDU Social Health Atlas](https://phidu.torrens.edu.au/social-health-atlases), compiled from the [Victorian Admitted Episodes Dataset (VAED)](https://www.health.vic.gov.au/data-reporting/victorian-admitted-episodes-dataset).

  - The PHIDU Social Health Atlas is a compendium of demographic, social, health status, and health service utilisation indicators compiled by Torrens University Australia from a wide range of national, jurisdictional, and regional data sources.
  
  - The VAED includes data from all Victorian public and private hospitals, including rehabilitation centres, extended care facilities, and day procedure centres.
  
  - Aboriginal and/or Torres Strait Islander peoples are defined as those who identified themselves as Aboriginal and/or Torres Strait Islander at the time of their contact with the health service.

  - Data were aggregated for a four-year period (July 2017 to June 2021).")

note_data_phidu_vemd <- glue::glue("
- Data were obtained from the [PHIDU Social Health Atlas](https://phidu.torrens.edu.au/social-health-atlases), compiled from the [AIHW National Non-Admitted Patient Emergency Department Care Database (NNAPEDCD)](https://www.aihw.gov.au/reports/hospitals/non-admitted-patient-emergency-dept-care-nmds/summary).

  - The PHIDU Social Health Atlas is a compendium of demographic, social, health status, and health service utilisation indicators compiled by Torrens University Australia from a wide range of national, jurisdictional, and regional data sources.
  
  - The NNAPEDCD provides information on care provided to non-admitted patients in public hospital emergency departments.
                                   
  - Aboriginal and/or Torres Strait Islander peoples are defined as those who identified themselves as Aboriginal and/or Torres Strait Islander at the time of their contact with the health service.")

note_data_reconciliation <- glue::glue("
- Data were obtained from the [Australian Reconciliation Barometer](https://www.reconciliation.org.au/reconciliation/australian-reconciliation-barometer/).

  - The Australian Reconciliation Barometer is a national survey undertaken every two years and aims to measure the progress of reconciliation between Aboriginal and/or Torres Strait Islander and non-Indigenous peoples.

  - Aboriginal and/or Torres Strait Islander peoples are defined as those who self-identified as Aboriginal and/or Torres Strait Islander when completing the survey.")

note_data_viccr <- glue::glue("
- Data are obtained from the [Victorian Cancer Registry](https://www.cancervic.org.au/cancer-information/statistics) and are publicly available at statewide level.

  - Benign neoplasms and recurrent cancers are excluded.

  - Aboriginal and/or Torres Strait Islander peoples are defined as those who are recorded as Aboriginal and/or Torres Strait Islander in the Victorian Cancer Registry.")

# ------------------------------------------------------------------------------
# Standard footnotes: caveats ----
# ------------------------------------------------------------------------------
note_caveat_admissions <- glue::glue("
- Data were based on the number of separations, or completions of episodes of care for a patient in hospital.

  - The completion of an episode of care can be due to discharge from hospital, death, transfer to another hospital, or change in the type of care within the same hospital (e.g. from acute care to rehabilitation).

  - The data presented in this report relate to short-term episodes of care, and as a result the number of admissions will be similar to the number of separations in a given time period. The term 'admissions' has been used throughout this report.
  
  - Repeated admissions for the same person and transfers between hospitals are counted as separate admissions.")

note_caveat_ctg_target <- glue::glue("
- The Closing the Gap target is marked with a dashed blue line.")

note_caveat_death_cause <- glue::glue("
- Cause of death data for 2023 are preliminary and are subject to change as additional data become available.

  - The process of coding cause of death data can take up to 2.5 years due to the requirement to wait for coroner cases to be closed and for information from autopsies and toxicology testing become available for inclusion when death data are coded by the ABS. The next revision of 2023 death data is due to occur in April 2026.

  - [ICD-10-AM codes](https://icd.who.int/browse10/2019/en) were assigned to all conditions listed on a Medical Certificate of Cause of Death and the National Coronial Information System and rules applied to select an underlying cause of death. The underlying cause of death is defined as the disease or injury that initiated the train of morbid events leading directly to death.
  
  - Detailed information about the methodology used for cause of death coding in Australia can be found on the [ABS website](https://www.abs.gov.au/methodologies/causes-death-australia-methodology)")

note_caveat_iare_nephu <- glue::glue("
- IAREs that are partly or entirely within the NEPHU catchment include: Craigieburn/Sunbury (Craigieburn), Knox, Maroondah, Melbourne East (Melbourne (E)), Melbourne North-East (Melbourne (NE)), Melbourne/Port Phillip (Port Phillip), Moreland/Broadmeadows (Broadmeadows), Northcote/Preston/Whittlesea (Northcote), Whitehorse, and Yarra Ranges.")

note_caveat_iare_ratio <- glue::glue("
- The standardised ratio compares the age-standardised rate in each IARE (the observed value) with the age-standardised rate for Greater Melbourne (the expected value).

  - A ratio of 100 indicates that there is no difference between the IARE and Greater Melbourne overall.
  
  - A ratio above 100 indicates a higher than expected rate compared with Greater Melbourne.
  
  - A ratio below 100 indicates a lower than expected rate compared with Greater Melbourne.")

note_caveat_iare_suppress <- glue::glue("
- IAREs marked with * have data suppressed due to small numbers.")

note_caveat_multipleoptions <- glue::glue("
- Totals may not sum to 100% as respondents could select multiple options.")

note_caveat_notifiable_decrease <- glue::glue("
- The public health measures implemented during the COVID-19 pandemic impacted the epidemiology of many notifiable conditions, with lower than expected numbers of notifications seen during the pandemic.")

note_caveat_notifiable_rate <- glue::glue("
- Rates are calculated as the crude rate per 100,000 population, based on population data from the ABS Census 2021.")

note_caveat_survey_agegroups <- glue::glue("
- Adults are defined as persons aged 18 years and over, and children are defined as people aged 2 to 17 years.")

note_caveat_survey_estimate <- glue::glue("
- Survey data do not represent exact numbers of people. As only a sample of people were surveyed, their results were converted into estimates for the entire population and/or reported as percentages.")

note_caveat_underidentification <- glue::glue("
- Reported counts and percentages are a likely underestimate due to under-identification.")

note_caveat_unknown_agesex <- glue::glue("
- People with an unknown or not stated age or sex have been excluded from age/sex breakdowns.")

note_caveat_unknown_survey <- glue::glue("
- Unknown, not applicable, and not stated survey responses have been excluded.")

