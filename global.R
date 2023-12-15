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
concepts_lists <- read_csv(here::here("www", "concept_list.csv"), show_col_types = FALSE)

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
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD"))

survival_estimates_prostate <- survival_estimates %>% 
  filter(Cancer == "Prostate") %>% 
  mutate(Sex = "Both")

survival_estimates <- bind_rows(survival_estimates,
                                survival_estimates_prostate)
rm(survival_estimates_prostate)

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
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>% 
  select(-c("Method", "Stratification", "Adjustment" )) %>% 
  relocate(Database, .before = 1)

survival_risk_table_prostate <- survival_risk_table %>% 
  filter(Cancer == "Prostate") %>% 
  mutate(Sex = "Both")

survival_risk_table <- bind_rows(survival_risk_table,
                                survival_risk_table_prostate) %>% 
  mutate_all(~ ifelse(is.na(.), "-", .))
rm(survival_risk_table_prostate)


# median and survival probabilities ------
survival_median_files <- results[stringr::str_detect(results, ".csv")]
survival_median_files <- results[stringr::str_detect(results, "median_mean")]

survival_median_table <- list()
for(i in seq_along(survival_median_files)){
  survival_median_table[[i]]<-readr::read_csv(survival_median_files[[i]],
                                              show_col_types = FALSE) %>% 
    mutate(n = as.character(n))
}
survival_median_table <- dplyr::bind_rows(survival_median_table) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Head_and_neck", "Head and Neck")) %>%
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>% 
  relocate(Database, .before = 1) %>% 
  relocate(`rmean in years (SE)`, .after = `Median Survival in Years (95% CI)` ) %>% 
  #select(!c(study_period)) %>% 
  rename(
    "1-year Survival (95% CI)" = `Survival Rate % (95% CI) year 1`,
    "5-year Survival (95% CI)" = `Survival Rate % (95% CI) year 5`,
    "10-year Survival (95% CI)" = `Survival Rate % (95% CI) year 10`,
     "5-year RMST (SE)" = `rmean 5yrs in years (SE)`,
    "10-year RMST (SE)" = `rmean 10yrs in years (SE)`,
    "Mean Survival (SE)" = `rmean in years (SE)`,
    "Median Survival (95% CI)" = `Median Survival in Years (95% CI)`
  ) %>% 
  select(!c("5-year RMST (SE)",
            "10-year RMST (SE)",
            "rmean5yr",
            "se5yr",
            "rmean10yr",
            "se10yr"))

survival_median_table_prostate <- survival_median_table %>% 
  filter(Cancer == "Prostate") %>% 
  mutate(Sex = "Both")

survival_median_table <- bind_rows(survival_median_table,
                                   survival_median_table_prostate)
rm(survival_median_table_prostate)

# extract upper and low confidence intervals from survival estimates
# suppressing warnings as it NA values are bring up an error
suppressWarnings(
survival_median_table <- survival_median_table %>%
  filter(Method == "Kaplan-Meier") %>% 
  # mutate(across(c(rmean5yr, se5yr), ~ifelse(study_period < 5, NA, .))) %>%
  # mutate(across(c(rmean10yr, se10yr), ~ifelse(study_period < 10, NA, .))) %>%
  mutate(across(where(is.character) | where(is.numeric), ~ifelse(n == "<10" & events == "0", NA, .))) %>%
  mutate(across(c(`1-year Survival (95% CI)`, `5-year Survival (95% CI)`, `10-year Survival (95% CI)`), 
                ~ifelse(grepl("0.0 \\(0.0-0.0\\)", .), NA, .))) %>% 
  mutate(across(c(`1-year Survival (95% CI)`, `5-year Survival (95% CI)`, `10-year Survival (95% CI)`), 
                ~ifelse(grepl("0.0 \\(NA-NA\\)", .), NA, .))) %>% 
  # mutate(across(c(`1-year Survival (95% CI)`, `5-year Survival (95% CI)`, `10-year Survival (95% CI)`), 
  #               ~ifelse(grepl("100.0 \\(100.0-100.0\\)", .), NA, .))) %>% 
  mutate(lower_upper_1yrsurv = stringr::str_extract(`1-year Survival (95% CI)`, "\\((.*?)\\)")) %>%
  separate(lower_upper_1yrsurv, into = c("lower_1yrsurv", "upper_1yrsurv"), sep = "-") %>%
  mutate(lower_upper_5yrsurv = stringr::str_extract(`5-year Survival (95% CI)`, "\\((.*?)\\)")) %>%
  separate(lower_upper_5yrsurv, into = c("lower_5yrsurv", "upper_5yrsurv"), sep = "-") %>%
  mutate(lower_upper_10yrsurv = stringr::str_extract(`10-year Survival (95% CI)`, "\\((.*?)\\)")) %>%
  separate(lower_upper_10yrsurv, into = c("lower_10yrsurv", "upper_10yrsurv"), sep = "-") %>%
  mutate(lower_upper_median = stringr::str_extract(`Median Survival (95% CI)`, "\\((.*?)\\)")) %>%
  separate(lower_upper_median, into = c("lower_medsurv", "upper_medsurv"), sep = "-") %>%
  mutate(across(c(lower_1yrsurv, upper_1yrsurv,
                  lower_5yrsurv, upper_5yrsurv ,
                  lower_10yrsurv, upper_10yrsurv,
                  lower_medsurv , upper_medsurv), ~as.numeric(stringr::str_remove_all(.,"[()]")))) %>%
  mutate(lower_rmean = rmean - se,
         upper_rmean = rmean + se) %>% 
  mutate(across(c(lower_1yrsurv, upper_1yrsurv,
           lower_5yrsurv, upper_5yrsurv ,
           lower_10yrsurv, upper_10yrsurv,
           lower_medsurv , upper_medsurv,
           lower_rmean, upper_rmean
           ), ~ifelse(is.na(.), NA, .))) %>% 
  filter(complete.cases(Database)) %>% 
  mutate(`surv year 1` = ifelse(is.na(`1-year Survival (95% CI)`), NA, `surv year 1`),
         `surv year 5` = ifelse(is.na(`5-year Survival (95% CI)`), NA, `surv year 5`),
         `surv year 10` = ifelse(is.na(`10-year Survival (95% CI)`), NA, `surv year 10`) ) 

)


# table one ------
tableone_whole_files <- results[stringr::str_detect(results, ".csv")]
tableone_whole_files <- results[stringr::str_detect(results, "tableone")]
tableone_whole <- list()
for(i in seq_along(tableone_whole_files)){
  tableone_whole[[i]] <- readr::read_csv(tableone_whole_files[[i]],
                                         show_col_types = FALSE)
}
tableone_whole <- bind_rows(tableone_whole) %>% 
  dplyr::mutate(group_level = replace(group_level, group_level == "Head_and_neck", "Head and Neck")) %>%
  dplyr::mutate(cdm_name = replace(cdm_name, cdm_name == "CPRD_GOLD", "CPRD GOLD"))

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
         "vocabulary_version", "cdm_version", "cdm_description",) %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  dplyr::mutate(cdm_name = replace(cdm_name, cdm_name == "CPRD_GOLD", "CPRD GOLD")) %>% 
  rename("Database name" = "cdm_name",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version",
         "Database CDM Version" = "cdm_version",
         "Database Description" = "cdm_description" ) 

snapshotcdm <- full_join(snapshotcdm, database_details, by = "Database name" ) %>% 
  relocate("Database Description", .after = last_col())

snapshotcdm <- snapshotcdm %>%
  mutate(`Database Description` = ifelse(`Database name` == "MAITT", 
                                         "MAITT is a dataset specifically composed for RITA1/02-96-11 project (https://www.etis.ee/Portal/Projects/Display/7c765be1-d8a7-44e3-8789-1760ccbf00e3?lang=ENG, ethics committee approval number 268/T-12) from three national health databases in Estonia: digital prescription, claims, and EHR. It contains 10% random sample from Estonian population. For each individual in the sample, it contains all records from these three databases from 2012-2019", 
                                         `Database Description`))

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
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>% 
  select(!c(cohort_definition_id))

# filter results for just km results
survival_km <- survival_estimates %>% 
  filter(Method == "Kaplan-Meier")

med_surv_km <- survival_median_table %>% 
  filter(Method == "Kaplan-Meier",
         Adjustment == "None") %>% 
  select(!c(Adjustment, 
            rmean,
            se,
            median,
            `surv year 1`,
            `surv year 5`,
            `surv year 10`,
            Method,
            Stratification
            )) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Head_and_neck", "Head and Neck")) %>%
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) 

med_surv_km_sex_age <- survival_median_table %>% 
  filter(Method == "Kaplan-Meier",
         Adjustment == "None") %>% 
  select(!c(Adjustment, 
            `Median Survival (95% CI)`,
            `Mean Survival (SE)`,
            `1-year Survival (95% CI)`,
            `5-year Survival (95% CI)`,
            `10-year Survival (95% CI)`,
            n,
            events,
            se,
            Method,
            Stratification
  )) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Head_and_neck", "Head and Neck")) %>%
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
