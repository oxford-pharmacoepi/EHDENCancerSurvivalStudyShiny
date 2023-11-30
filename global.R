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
  select(c("Database", "details", "0", "0.5", "1", "2", "4", "6", "8", "10" , "12", "14", "16", "18" ,"20",
           "Cancer", "Age", "Sex")) %>% 
  relocate(Database, .before = 1)

survival_risk_table_prostate <- survival_risk_table %>% 
  filter(Cancer == "Prostate") %>% 
  mutate(Sex = "Both")

survival_risk_table <- bind_rows(survival_risk_table,
                                survival_risk_table_prostate)


# median and survival probabilities ------
survival_median_files <- results[stringr::str_detect(results, ".csv")]
survival_median_files <- results[stringr::str_detect(results, "median_mean")]

survival_median_table <- list()
for(i in seq_along(survival_median_files)){
  survival_median_table[[i]]<-readr::read_csv(survival_median_files[[i]],
                                              show_col_types = FALSE)
}
survival_median_table <- dplyr::bind_rows(survival_median_table) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Head_and_neck", "Head and Neck")) %>%
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>% 
  relocate(Database, .before = 1) %>% 
  select(!c(study_period))

survival_median_table_prostate <- survival_median_table %>% 
  filter(Cancer == "Prostate") %>% 
  mutate(Sex = "Both")

survival_median_table <- bind_rows(survival_median_table,
                                   survival_median_table_prostate)


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
  dplyr::mutate(cdm_name = replace(cdm_name, cdm_name == "CPRDGold", "CPRD GOLD"))

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
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) 

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
            rmean10yr,
            se10yr,
            rmean5yr,
            se5yr,
            `surv year 1`,
            `surv year 5`,
            `surv year 10`,
            Method,
            Stratification
            )) %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "Head_and_neck", "Head and Neck")) %>%
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) 
