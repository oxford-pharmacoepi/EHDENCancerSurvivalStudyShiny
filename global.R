#### PACKAGES -----
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(ggalt)
library(bslib)
library(PatientProfiles)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(CDMConnector)
library(CirceR)
library(rjson)
library(rclipboard)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#605ca8"
  ),
  adminlte_sidebar(
    dark_bg = "#78B7C5", #  "#D8DEE9",
    dark_hover_bg = "#3B9AB2", #"#81A1C1",
    dark_color ="white" ,
    dark_submenu_bg = "#605ca8"
  ),
  adminlte_global(
    content_bg = "#eaebea"
  ),
  adminlte_vars(
    border_color = "black",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446",
    table_border_color = "black"
    
  )
)

# format markdown
formatMarkdown <- function(x) {
  lines <- strsplit(x, "\r\n\r\n") |> unlist()
  getFormat <- function(line) {
    if (grepl("###", line)) {return(h3(gsub("###", "", line)))} 
    else {h4(line)} 
  }
  purrr::map(lines, ~ getFormat(.))
}

# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# printing numbers with 4 decimal place and commas 
nice.num4<-function(x) {
  trimws(format(round(x,4),
                big.mark=",", nsmall = 4, digits=4, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}

#### Load and extract data -----
results <-list.files(here("data"), full.names = TRUE,
                     recursive = TRUE,
                     include.dirs = TRUE,
                     pattern = ".zip")

#unzip data
for (i in (1:length(results))) {
  utils::unzip(zipfile = results[[i]],
               exdir = here("data"))
}

#grab the results from the folders
results <- list.files(
  path = here("data"),
  pattern = ".csv",
  full.names = TRUE,
  recursive = TRUE,
  include.dirs = TRUE
)

# database details
database_details <- read_csv(here::here("www", "database_details.csv"), show_col_types = FALSE)

# clinical code lists
cohort_set <- CDMConnector::read_cohort_set(here::here(
  "www", "cohorts" ))

cohort_set$markdown <- ""

for (n in  row_number(cohort_set) ) {
  
  cohort <- cohort_set$cohort_name[n]  
  json <- paste0(cohort_set$json[n]  )
  cohortExpresion <- CirceR::cohortExpressionFromJson(json)
  markdown <- CirceR::cohortPrintFriendly(cohortExpresion)
  cohort_set$markdown[n] <-  markdown
  
} 


# Get concept ids from a provided path to cohort json files
# in dataframe
# Get a list of JSON files in the directory
json_files <- list.files(path = here("www", "cohorts"), pattern = "\\.json$", full.names = TRUE)
concept_lists_temp <- list()
concept_lists <- list()
concept_sets <- list()

if(length(json_files > 0)){
  
  for(i in seq_along(json_files)){
    concept_lists_temp[[i]] <- fromJSON(file = json_files[[i]]) 
    
  } 
  
  for(i in 1:length(concept_lists_temp)){
    
    for(k in 1:length(concept_lists_temp[[i]]$ConceptSets[[1]]$expression$items)){  
      
      concept_sets[[k]] <- bind_rows(concept_lists_temp[[i]]$ConceptSets[[1]]$expression$items[[k]]$concept)  
      
    }
    
    concept_lists[[i]] <- bind_rows(concept_sets) %>% 
      mutate(name = concept_lists_temp[[i]]$ConceptSets[[1]]$name)
    
    
  }
  
  
  concept_sets_final <- bind_rows(concept_lists) %>% 
    mutate(name = case_when(
      name == "Breast" ~ "incidentbreastcancer",
      name == "Colorectal" ~    "incidentcolorectalcancer" ,
      name == "Head_and_neck" ~  "incidentheadneckcancer"  ,
      name == "Liver" ~   "incidentlivercancer"  ,
      name == "Lung" ~  "incidentlungcancer"    ,
      name == "Pancreas" ~  "incidentpancreaticcancer" ,
      name == "Prostate" ~  "incidentprostatecancer" ,
      name == "Stomach" ~  "incidentstomachcancer" ,
      TRUE ~ name
    ))
  
}

rm(concept_lists)
rm(concept_lists_temp)
rm(concept_sets)

# survival estimates
survival_estimates_files <- results[stringr::str_detect(results, ".csv")]
survival_estimates_files <- results[stringr::str_detect(results, "survival_estimates")]

survival_estimates <- list()
for(i in seq_along(survival_estimates_files)){
  survival_estimates[[i]]<-readr::read_csv(survival_estimates_files[[i]],
                                           show_col_types = FALSE)
}
survival_estimates <- dplyr::bind_rows(survival_estimates) %>% 

  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Head_and_neck", "Head and Neck")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Pancreatic", "Pancreas")) %>%
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "HUS2000wtrunc", "HUS")) %>% 
  left_join(database_details %>% select(Database, database_type), by = "Database") %>% 
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD (UK)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "CRN", "CRN (Norway)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "ECI", "ECI (Scotland)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "GCR", "GCR (Switzerland)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "HUVM", "HUVM (Spain)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "IMASIS", "IMASIS (Spain)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "IPCI", "IPCI (Netherlands)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "NCR", "NCR (Netherlands)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "SIDIAP", "SIDIAP (Spain)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "ULSM", "ULSM (Portugal)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "ULSGE", "ULSGE (Portugal)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "ULSEDV", "ULSEDV (Portugal)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "ULSRA", "ULSRA (Portugal)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "UTARTU", "UTARTU (Estonia)")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "HUS", "HUS (Finland)")) 

survival_estimates_prostate <- survival_estimates %>% 
  filter(Cancer == "Prostate") %>% 
  mutate(Sex = "Both")

survival_estimates_ECI <- survival_estimates %>% 
  filter(Database == "ECI") %>% 
  dplyr::mutate(Sex = replace(Sex, Sex == "Both", "Female"))

survival_estimates <- bind_rows(survival_estimates,
                                survival_estimates_ECI,
                                survival_estimates_prostate) 
                
                
rm(survival_estimates_prostate,
   survival_estimates_ECI)


# risk tables ----------
survival_risk_table_files <- results[stringr::str_detect(results, ".csv")]
survival_risk_table_files <- results[stringr::str_detect(results, "risk_table")]

survival_risk_table <- list()
for(i in seq_along(survival_risk_table_files)){
  survival_risk_table[[i]]<-readr::read_csv(survival_risk_table_files[[i]],
                                            show_col_types = FALSE) %>%
    mutate_if(is.double, as.character)

}

survival_risk_table <- dplyr::bind_rows(survival_risk_table) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Head_and_neck", "Head and Neck")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Pancreatic", "Pancreas")) %>%
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "HUS2000wtrunc", "HUS")) %>% 
  select(-c("Method", "Stratification", "Adjustment" )) %>% 
  relocate(Database, .before = 1) %>% 
  filter(details != "n.censor")

survival_risk_table_ECI <- survival_risk_table %>% 
  filter(Database == "ECI") %>% 
  dplyr::mutate(Sex = replace(Sex, Sex == "Both", "Female"))

survival_risk_table_prostate <- survival_risk_table %>% 
  filter(Cancer == "Prostate") %>% 
  mutate(Sex = "Both")

survival_risk_table <- bind_rows(survival_risk_table,
                                 survival_risk_table_ECI,
                                survival_risk_table_prostate) %>% 
  mutate_all(~ ifelse(is.na(.), "-", .)) 

rm(survival_risk_table_prostate,
   survival_risk_table_ECI
   )


# median and survival probabilities ------
survival_median_files <- results[stringr::str_detect(results, ".csv")]
survival_median_files <- results[stringr::str_detect(results, "median_mean")]
  
survival_median_table <- list()
for(i in seq_along(survival_median_files)){
  suppressWarnings(
  survival_median_table[[i]]<-readr::read_csv(survival_median_files[[i]],
                                              show_col_types = FALSE) %>% 
    mutate(n = as.character(n),
           events = as.character(events))
  )
}


survival_median_table <- dplyr::bind_rows(survival_median_table) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Pancreatic", "Pancreas")) %>%
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>%
  dplyr::mutate(Database = replace(Database, Database == "HUS2000wtrunc", "HUS")) %>% 
  filter(Truncated != "Yes", Method == "Kaplan-Meier") %>% 
  relocate(Database, .before = 1) %>% 
  mutate(
    "1-year Survival (95% CI)"= ifelse(!is.na(`surv year 1`),
                                       paste0(paste0(nice.num(`surv year 1`)), " (",
                                              paste0(nice.num(`lower year 1`)),"-",
                                              paste0(nice.num(`upper year 1`)), ")"),
                                       NA),
    
    "5-year Survival (95% CI)"= ifelse(!is.na(`surv year 5`),
                                       paste0(paste0(nice.num(`surv year 5`)), " (",
                                              paste0(nice.num(`lower year 5`)),"-",
                                              paste0(nice.num(`upper year 5`)), ")"),
                                       NA) ,
    "10-year Survival (95% CI)"= ifelse(!is.na(`surv year 10`),
                                        paste0(paste0(nice.num(`surv year 10`)), " (",
                                               paste0(nice.num(`lower year 10`)),"-",
                                               paste0(nice.num(`upper year 10`)), ")"),
                                        NA) ,
    
    "Median Survival (95% CI)" = ifelse(!is.na(median),
                                        paste0(paste0(nice.num(median)), " (",
                                               paste0(nice.num(lower_median)),"-",
                                               paste0(nice.num(upper_median)), ")"),
                                        NA) ,
    
    "Mean Survival (SE)" = ifelse(!is.na(rmean),
                                  paste0(paste0(nice.num2(rmean)), " (",
                                         paste0(nice.num2(se)), ")"),
                                  NA),
    
    "Mean Survival 5 years (SE)" = ifelse(!is.na(rmean5yr),
                                          paste0(paste0(nice.num2(rmean5yr)), " (",
                                                 paste0(nice.num2(se5yr)), ")"),
                                          NA),
    
    "Mean Survival 10 years (SE)" = ifelse(!is.na(rmean10yr),
                                           paste0(paste0(nice.num2(rmean10yr)), " (",
                                                  paste0(nice.num2(se10yr)), ")"),
                                           NA)
    
    
    
  ) %>% 

  
  select(!c(Adjustment, 
            Stratification, Truncated
  )) 

survival_median_table_prostate <- survival_median_table %>% 
  filter(Cancer == "Prostate") %>% 
  mutate(Sex = "Both")

survival_median_table <- bind_rows(survival_median_table,
                                   survival_median_table_prostate) 

rm(survival_median_table_prostate)


# table one ------
tableone_whole_files <- results[stringr::str_detect(results, ".csv")]
tableone_whole_files <- results[stringr::str_detect(results, "tableone")]
tableone_whole <- list()
for(i in seq_along(tableone_whole_files)){
  tableone_whole[[i]] <- readr::read_csv(tableone_whole_files[[i]],
                                         show_col_types = FALSE)
}
tableone_whole <- bind_rows(tableone_whole) %>% 
dplyr::mutate(cdm_name = replace(cdm_name, cdm_name == "HUS2000", "HUS")) %>% 
  dplyr::mutate(Cancer = replace(group_level, group_level == "Pancreatic", "Pancreas")) %>%
  dplyr::mutate(cdm_name = replace(cdm_name, cdm_name == "CPRD_GOLD", "CPRD GOLD")) %>% 
  filter(estimate_type != "q05",
         estimate_type != "q95",
         estimate_type != "mean",
         estimate_type != "sd"
         ) %>% 
  dplyr::mutate(variable_level = if_else(variable_level == "Obesitycharybdis",
                                         "Obesity", variable_level)) %>% 
  dplyr::mutate(variable = if_else(variable == "age",
                                         "Age", variable))  %>%
  dplyr::mutate(variable = if_else(variable == "age_gr",
                                   "Age group", variable))  %>%
  dplyr::mutate(variable = if_else(variable == "cohort_end_date",
                                   "Cohort end date", variable))  %>%
  dplyr::mutate(variable = if_else(variable == "cohort_start_date",
                                   "Cohort start date", variable))  %>%
  dplyr::mutate(variable = if_else(variable == "future_observation",
                                   "Future observation", variable))  %>%
  dplyr::mutate(variable = if_else(variable == "number records",
                                   "Number records", variable))  %>%
  dplyr::mutate(variable = if_else(variable == "number subjects",
                                   "Number subjects", variable))  %>%
  dplyr::mutate(variable_level = if_else(variable_level == "18 To 39",
                                   "18 to 39", variable_level))  %>%
  dplyr::mutate(variable_level = if_else(variable_level == "40 To 49",
                                   "40 to 49", variable_level))  %>%
  dplyr::mutate(variable_level = if_else(variable_level == "50 To 59",
                                   "50 to 59", variable_level))  %>%
  dplyr::mutate(variable_level = if_else(variable_level == "60 To 69",
                                   "60 to 69", variable_level))  %>%
  dplyr::mutate(variable_level = if_else(variable_level == "70 To 79",
                                   "70 to 79", variable_level))  %>%
  dplyr::mutate(variable = if_else(variable == "sex",
                                   "Sex", variable))  %>%
  dplyr::mutate(variable = if_else(variable == "Outcome flag from 0 to 0",
                                   "outcome", variable))  %>%
  dplyr::mutate(group_level = if_else(group_level == "Overall",
                                         "cohort_name", group_level))  %>%
  dplyr::mutate(group_name = if_else(group_name == "cohort_name",
                                      "Overall", group_name)) %>%
  filter(!(variable == "Sex" & variable_level == "None")) %>% 
  mutate_all(~ str_replace_all(., "Head_and_neck", "Head and neck")) 
  
# cdm snapshot ------
snapshot_files <- results[stringr::str_detect(results, ".csv")]
snapshot_files <- results[stringr::str_detect(results, "cdm_snapshot")]
snapshotcdm <- list()
for(i in seq_along(snapshot_files)){
  snapshotcdm[[i]] <- readr::read_csv(snapshot_files[[i]],
                                         show_col_types = FALSE) %>% 
    mutate_all(as.character)

}
snapshotcdm <- bind_rows(snapshotcdm) %>% 
  select("cdm_name", "person_count", "observation_period_count" ,
         "vocabulary_version", "cdm_version", "cdm_description", "StudyPeriodStartDate", "earliest_observation_period_start_date" ,) %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  dplyr::mutate(cdm_name = replace(cdm_name, cdm_name == "CPRD_GOLD", "CPRD GOLD")) %>%
  dplyr::mutate(cdm_name = replace(cdm_name, cdm_name == "HUS2000", "HUS")) %>% 
  rename("Database" = "cdm_name",
         "Persons in the cancer cohorts" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version",
         "Database CDM Version" = "cdm_version",
         "Database Description" = "cdm_description",
         "Study Start Date" = "StudyPeriodStartDate",
         "Database Start Date" = "earliest_observation_period_start_date" ) 

snapshotcdm <- full_join(snapshotcdm, database_details, by = "Database" ) %>% 
  relocate("Database Description", .after = last_col()) %>% 
  relocate("Full name", .after = `Database`)

snapshotcdm <- snapshotcdm %>%
  mutate(`Database Description` = ifelse(`Database` == "HUVM", 
                                         "Virgen Macarena University Hospital provides hospital and community care services to 480,000 people. The hospital belongs to the Andalusian Public Health System as 3erd level hospital in Seville and Huelva areas (Spain). The hospital includes 37 medical specialties provided with state of the art technology for complex and advanced healthcare treatments. Its infrastructure includes 800 beds, 25 surgical theather distributed in 7 buildings. The hospital has 6000 professionals and its budget is more than €398 Million.  The hospital currently participates in more than 435 in phase I, II and III clinical trials and produced scientific publications with 1187 impact factor points during last year. Our EHR system has been in use for more than a decade and it contains more than 10 million episodes and 1 million discharge summaries.", 
                                         `Database Description`))

snapshotcdm <- snapshotcdm %>%
  distinct()

# attrition ----------
attrition_files <- results[stringr::str_detect(results, ".csv")]
attrition_files <- results[stringr::str_detect(results, "attrition")]
attritioncdm <- list()
for(i in seq_along(attrition_files)){
  attritioncdm [[i]] <- readr::read_csv(attrition_files[[i]],
                                      show_col_types = FALSE)
}
attritioncdm <- bind_rows(attritioncdm) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Head_and_neck", "Head and Neck")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Pancreatic", "Pancreas")) %>%
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "HUS2000", "HUS")) %>% 
  select(!c(cohort_definition_id))

# only keep results for ECI breast
attritioncdm <- attritioncdm %>% 
  dplyr::filter(!(Database == "ECI" & Cancer != "Breast"))
       
# filter results for just km results
survival_km <- survival_estimates %>% 
  filter(Method == "Kaplan-Meier")

# age standization for survival

# read in ICCS_1 values (for all cancers in this study apart from prostate)
ICSS_1 <- readr::read_csv(here("www", "ICSS_1.csv"), 
                          show_col_types = FALSE) 

# read in ICCS values (for prostate)
ICSS_prostate <- readr::read_csv(here("www", "ICSS_prostate.csv"), 
                                 show_col_types = FALSE) 



# Function to calculate age-standardized survival
calculate_age_standardized_survival <- function(data, standard_population) {
  
  # Filter and summarize the survival rates by age groups
  age_stds <- standard_population %>%
    mutate(Age = case_when(
      Age %in% c("0-14", "15-19") ~ "0 to 19",
      Age %in% c("20-24", "25-29", "30-34", "35-39") ~ "18 to 39",
      Age %in% c("40-44", "45-49") ~ "40 to 49",
      Age %in% c("50-54", "55-59") ~ "50 to 59",
      Age %in% c("60-64", "65-69") ~ "60 to 69",
      Age %in% c("70-74", "75-79") ~ "70 to 79",
      Age %in% c("80-84", "85+") ~ "80 +"
    )) %>%
    group_by(Age) %>%
    filter(Age != "0 to 19") %>% 
    summarise(ICSS = sum(ICSS)/100000)
  
  # Merge age-standardized survival to the original data
  data <- data %>%
    left_join(age_stds, by = c("Age")) %>%
    mutate(age_standard_1year = `surv year 1` * ICSS,
           age_standard_lower_1year = (`lower year 1`) * ICSS,
           age_standard_upper_1year = (`upper year 1`) * ICSS,
           
           age_standard_5year = `surv year 5` * ICSS,
           age_standard_lower_5year = (`lower year 5`) * ICSS,
           age_standard_upper_5year = (`upper year 5`) * ICSS,
           
           age_standard_10year = `surv year 10` * ICSS,
           age_standard_lower_10year = (`lower year 10`) * ICSS,
           age_standard_upper_10year = (`upper year 10`) * ICSS,
           
           age_standard_median = ifelse(!is.na(median) & !is.na(lower_median) & !is.na(upper_median), median * ICSS, NA),
           age_standard_lower_median = ifelse(!is.na(median) & !is.na(lower_median) & !is.na(upper_median), lower_median * ICSS, NA),
           age_standard_upper_median = ifelse(!is.na(median) & !is.na(lower_median) & !is.na(upper_median), upper_median * ICSS, NA),
           
           # age_standard_median = median * ICSS,
           # age_standard_lower_median = lower_median * ICSS,
           # age_standard_upper_median = upper_median * ICSS,
           
           age_standard_rmean = rmean * ICSS,
           age_standard_se = se * ICSS,
           
           age_standard_rmean5year = rmean5yr * ICSS,
           age_standard_se5year = se5yr * ICSS,
           
           age_standard_rmean10year = rmean10yr * ICSS,
           age_standard_se10year = se10yr * ICSS
           
           
    )  # Calculate age-standardized survival
  
  
  # Summarize to get total age-standardized survival
  total_age_standard <- data %>%
    summarise(Age = "Age Standardized", 
              `1-year Survival (95% CI)` = 
                paste0(round(sum(age_standard_1year, na.rm = TRUE), 1),
                       " (",
                       round(sum(age_standard_lower_1year, na.rm = TRUE),1),
                       "-",
                       round(sum(age_standard_upper_1year, na.rm = TRUE), 1),
                       ")"),
              
              `5-year Survival (95% CI)` = 
                paste0(round(sum(age_standard_5year, na.rm = TRUE), 1),
                       " (",
                       round(sum(age_standard_lower_5year, na.rm = TRUE),1),
                       "-",
                       round(sum(age_standard_upper_5year, na.rm = TRUE), 1),
                       ")"),
              
              
              `10-year Survival (95% CI)` = 
                paste0(round(sum(age_standard_10year, na.rm = TRUE), 1),
                       " (",
                       round(sum(age_standard_lower_10year, na.rm = TRUE),1),
                       "-",
                       round(sum(age_standard_upper_10year, na.rm = TRUE), 1),
                       ")"),
              
              `surv year 1` = sum(age_standard_1year, na.rm = TRUE) ,
              `surv year 5` = sum(age_standard_5year, na.rm = TRUE) ,          
              `surv year 10` = sum(age_standard_10year, na.rm = TRUE)  ,          
              `lower year 1` =  sum(age_standard_lower_1year, na.rm = TRUE),    
              `lower year 5` =  sum(age_standard_lower_5year, na.rm = TRUE),
              `lower year 10` = sum(age_standard_lower_10year, na.rm = TRUE)      ,     
              `upper year 1`  = sum(age_standard_upper_1year, na.rm = TRUE),
              `upper year 5`   = sum(age_standard_upper_5year, na.rm = TRUE), 
              `upper year 10`= sum(age_standard_upper_10year, na.rm = TRUE) ,
              
              
              `Median Survival (95% CI)` = 
                paste0(round(sum(age_standard_median, na.rm = TRUE), 1),
                       " (",
                       round(sum(age_standard_lower_median, na.rm = TRUE),1),
                       "-",
                       round(sum(age_standard_upper_median, na.rm = TRUE), 1),
                       ")"),
              
              
              `Mean Survival (SE)` = 
                paste0(round(sum(age_standard_rmean, na.rm = TRUE), 1),
                       " (",
                       round(
                         (sum(age_standard_rmean, na.rm = TRUE)) -
                           (sum(age_standard_se, na.rm = TRUE)) , 1 ) ,
                       
                       "-",
                       round(
                         (sum(age_standard_rmean, na.rm = TRUE)) +
                           (sum(age_standard_se, na.rm = TRUE)) , 1 ) ,
                       
                       
                       ")"),
              
              
              `Mean Survival 5 years (SE)` = 
                paste0(round(sum(age_standard_rmean5year, na.rm = TRUE), 1),
                       " (",
                       round(
                         (sum(age_standard_rmean5year, na.rm = TRUE)) -
                           (sum(age_standard_se5year, na.rm = TRUE)) , 1 ) ,
                       
                       "-",
                       round(
                         (sum(age_standard_rmean5year, na.rm = TRUE)) +
                           (sum(age_standard_se5year, na.rm = TRUE)) , 1 ) ,
                       
                       
                       ")"),
              
              
              `Mean Survival 10 years (SE)` = 
                paste0(round(sum(age_standard_rmean10year, na.rm = TRUE), 1),
                       " (",
                       round(
                         (sum(age_standard_rmean10year, na.rm = TRUE)) -
                           (sum(age_standard_se10year, na.rm = TRUE)) , 1 ) ,
                       
                       "-",
                       round(
                         (sum(age_standard_rmean10year, na.rm = TRUE)) +
                           (sum(age_standard_se10year, na.rm = TRUE)) , 1 ) ,
                       
                       
                       ")"),
              
              
              
              rmean = sum(age_standard_rmean, na.rm = TRUE),  
              se     =      sum(age_standard_se, na.rm = TRUE),                
              median    =  sum(age_standard_median, na.rm = TRUE),              
              lower_median = sum(age_standard_lower_median, na.rm = TRUE),
              upper_median = sum(age_standard_upper_median, na.rm = TRUE),
              rmean5yr      =     sum(age_standard_rmean5year, na.rm = TRUE)   ,     
              se5yr = sum(age_standard_se5year, na.rm = TRUE) ,
              rmean10yr = sum(age_standard_rmean10year, na.rm = TRUE),
              se10yr = sum(age_standard_se10year, na.rm = TRUE)
              
              
              
    )  # Calculate sum of age_standard
  
  # Combine with the original data
  data <- bind_rows(data, total_age_standard)
  
  # Fill down Database and Sex columns
  data <- data %>%
    tidyr::fill(Database, Sex, Cancer, Method, study_period) 
  #%>% 
   # select(-matches("age_standard_")) %>% 
    #select(-c(ICSS))
  
  return(data)
}


results_database <- list()
results_cancer <- list()
# now for loops

for(db in 1:length(table(survival_median_table$Database ))){
  
  # filter to one database
  survival_median_table_temp <- survival_median_table %>%
    filter(Database == names(table(survival_median_table$Database ))[db] )
  
  
  for(cancer in 1:length(table(survival_median_table_temp$Cancer ))){
    
    
    survival_median_table_temp_temp <- survival_median_table_temp %>%
      filter(Cancer == names(table(survival_median_table_temp$Cancer))[cancer])
    
    results_cancer[[cancer]] <- survival_median_table_temp_temp %>%
      filter(Sex == "Both" & Age != "All") %>%
      calculate_age_standardized_survival(standard_population = ICSS_1) %>%
      filter(Age == "Age Standardized")
    
    #print(names(table(survival_median_table_temp$Cancer ))[cancer])
    
    
  }
  
  results_database[[db]] <- bind_rows(results_cancer)
  
  results_cancer <- list()
  
  #print(names(table(survival_median_table$Database))[db])
  
}

final_results_age_std <- bind_rows(results_database)

survival_median_table <- bind_rows(survival_median_table,
                                   final_results_age_std)

rm(final_results_age_std,
   results_cancer,
   results_database,
   survival_median_table_temp,
   survival_median_table_temp_temp)

med_surv_km <- survival_median_table %>% 
  select(c(Cancer, 
           n,
           events,
           Sex,
           Age,
           `1-year Survival (95% CI)`,
           `5-year Survival (95% CI)`,
           `10-year Survival (95% CI)`  ,
           `Median Survival (95% CI)`   ,
           `Mean Survival (SE)` ,
           Database
  )) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Head_and_neck", "Head and Neck")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Pancreatic", "Pancreas")) %>%
  dplyr::mutate(Database = replace(Database, Database == "HUS2000", "HUS")) %>% 
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>% 
  mutate(across(everything(), ~replace(., . == "0 (0-0)", NA)))


med_surv_km_sex_age <- survival_median_table %>% 
  filter(Method == "Kaplan-Meier") %>% 
  mutate(upper_rmean = rmean + se,
         lower_rmean = rmean - se) %>% 
  select(!c(`Median Survival (95% CI)`,
            `Mean Survival (SE)`,
            `Mean Survival 5 years (SE)` ,
            `Mean Survival 10 years (SE)`,
            `1-year Survival (95% CI)`,
            `5-year Survival (95% CI)`,
            `10-year Survival (95% CI)`,
            rmean10yr   ,
            se10yr,
            rmean5yr   ,
            se5yr,
            n,
            events,
            se,
            Method                
            
  )) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Head_and_neck", "Head and Neck")) %>%
  dplyr::mutate(Database = replace(Database, Database == "HUS2000", "HUS")) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Pancreatic", "Pancreas")) %>%
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>% 
  pivot_longer(
    cols = c(rmean, median, `surv year 1`, `surv year 5`,`surv year 10` ),
    names_to = "Variable",
    values_to = "Value"
  ) %>% 
  dplyr::mutate(Variable = replace(Variable, Variable == "median", "Median")) %>%
  dplyr::mutate(Variable = replace(Variable, Variable == "rmean", "Restricted Mean")) %>%
  dplyr::mutate(Variable = replace(Variable, Variable == "surv year 1", "One Year Survival")) %>%
  dplyr::mutate(Variable = replace(Variable, Variable == "surv year 5", "Five Year Survival")) %>%
  dplyr::mutate(Variable = replace(Variable, Variable == "surv year 10", "Ten Year Survival"))

rm(survival_estimates)
rm(survival_median_table)


# attrition functions ----
attritionChart <- function(x) {
  formatNum <- function(col) {
    col <- round(as.numeric(col))
    if_else(
      !is.na(col),
      gsub(" ", "", format(as.integer(col), big.mark=",")),
      as.character(col)
    )
  }
  
  xn <- x %>%
    arrange(reason_id) %>%
    mutate(
      number_subjects = formatNum(number_subjects),
      number_records = formatNum(number_records),
      excluded_subjects = formatNum(excluded_subjects),
      excluded_records = formatNum(excluded_records),
      label = paste0(
        "N subjects = ", number_subjects, "\nN records = ", number_records
      )
    )
  if (nrow(xn) == 1) {
    xn <- xn %>%
      mutate(label = paste0("Qualifying events", "\n", label)) %>%
      select(label)
  } else {
    att <- xn %>%
      filter(reason_id > min(reason_id)) %>%
      mutate(
        label = paste0(
          "N subjects = ", excluded_subjects, "\nN records = ", excluded_records
        )
      ) %>%
      select(reason, label)
    xn <- xn %>%
      mutate(
        label = if_else(
          reason_id == min(reason_id),
          paste0("Initial events", "\n", label),
          if_else(
            reason_id == max(reason_id),
            paste0("Final events", "\n", label),
            label
          )
        )
      ) %>%
      select(label)
  }
  n <- nrow(x)
  xg <- create_graph()
  
  for (k in seq_len(n)) {
    xg <- xg %>%
      add_node(
        label = xn$label[k],
        node_aes = node_aes(
          shape = "box",
          x = 1,
          width = 1.4,
          y = n + 1 - k + ifelse(k == 1, 0.1, 0) + ifelse(k == n, -0.1, 0),
          height = ifelse(k == 1 | k == n, 0.6, 0.4),
          fontsize = 10, fontcolor = "black", penwidth = ifelse(k == 1 | k == n, 2, 1), color = "black"
        )
      )
    if (k > 1) {
      xg <- xg %>%
        add_edge(from = k - 1, to = k, edge_aes = edge_aes(color = "black"))
    }
  }
  salt <- function(x) {
    s <- 50
    x <- strsplit(x = x, split = " ") |> unlist()
    nn <- (nchar(x) + c(0, rep(1, length(x)-1))) |> cumsum()
    id <- which(nn > s)
    if (length(id) > 0) {
      id <- id[1] - 1
      x <- paste0(paste0(x[1:id], collapse = " "), "\n", paste0(x[-(1:id)], collapse = " "))
    } else {
      x <- paste0(x, collapse = " ")
    }
    return(x)
  }
  if (n > 1) {
    for (k in seq_len(nrow(att))) {
      res <- att$reason[k]
      res <- salt(res)
      xg <- xg %>%
        add_node(
          label = att$label[k],
          node_aes = node_aes(
            shape = "box", x = 3.5, width = 1.2, y = n + 0.5 - k, height = 0.4,
            fontsize = 8, fillcolor = "grey", fontcolor = "black", color = "black"
          )
        ) %>%
        add_node(
          label = res,
          node_aes = node_aes(
            shape = "box", x = 1, width = 3.2, y = n + 0.5 - k, height = 0.35, fillcolor = "white", color = "black", fontcolor = "back"
          )
        ) %>%
        add_edge(
          from = 2*k + n, to = 2*k + n -1, edge_aes = edge_aes(color = "black")
        )
    }
  }
  
  return(xg)
}


