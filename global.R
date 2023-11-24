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
#data
study_results <- readRDS(here("data","Results.rds"))
# extract each element from the list to put results into r environment
list2env(study_results,globalenv())
rm(study_results)

# database details
database_details <- read_csv(here::here("www", "database_details.csv"), show_col_types = FALSE)

# clinical code lists
concepts_lists <- read_csv(here::here("www", "concept_list.csv"), show_col_types = FALSE)


# cdm snapshot ------
snapshotcdm <- snapshotcdm %>% 
  select("cdm_name", "person_count", "observation_period_count" ,
         "vocabulary_version") %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  rename("Database name" = "cdm_name",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version")

# tableone ----------
tableone_final <- tableone_final %>%
  pivot_wider(names_from = Database, values_from = Value)

# filter results for just km results
survival_km <- survivalResults %>% 
  filter(Method == "Kaplan-Meier")

med_surv_km <- medianResults %>% 
  filter(Method == "Kaplan-Meier",
         Adjustment == "None") %>% 
  select(!c(Adjustment, 
            rmean,
            se,
            median,
            rmean10yr,
            se10yr,
            `surv year 1`,
            `surv year 5`,
            `surv year 10`,
            Method,
            Stratification
            ))

riskTableResults <- riskTableResults %>% 
  select(c("0", "0.5", "1", "2", "4", "6", "8", "10" , "12", "14", "16", "18" ,"20",
           "Cancer", "Age", "Sex", "Database"))



attritioncdm <- attritioncdm %>% 
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "IncidentBreastCancer", "Breast")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "IncidentColorectalCancer", "Colorectal")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "IncidentHeadNeckCancer", "Head and Neck")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "IncidentLiverCancer", "Liver")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "IncidentLungCancer", "Lung")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "IncidentPancreaticCancer", "Pancreatic")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "IncidentProstateCancer", "Prostate")) %>%
  dplyr::mutate(Cancer = replace(Cancer, Cancer == "IncidentStomachCancer", "Stomach")) 

# # Load, prepare, and merge results -----
# results <-list.files(here("data"), full.names = TRUE,
#                      recursive = TRUE,
#                      include.dirs = TRUE,
#                      pattern = ".zip")
# 
# #unzip data
# for (i in (1:length(results))) {
#   utils::unzip(zipfile = results[[i]],
#                exdir = here("data"))
# }

#grab the results from the folders
results <- list.files(
  path = here("data"),
  pattern = ".csv",
  full.names = TRUE,
  recursive = TRUE,
  include.dirs = TRUE
)
# 
# # merge the survival results together
# survival_estimates_files <- results[stringr::str_detect(results, ".csv")]
# survival_estimates_files <- results[stringr::str_detect(results, "survival_estimates")]
# 
# survival_estimates <- list()
# for(i in seq_along(survival_estimates_files)){
#   survival_estimates[[i]]<-readr::read_csv(survival_estimates_files[[i]],
#                                            show_col_types = FALSE)
# }
# survival_estimates <- dplyr::bind_rows(survival_estimates)
# survival_estimates <- prepare_output_survival(survival_estimates)
# 
# # risk tables ----------
# # merge the risk table (whole dataset)
# survival_risk_table_files<-results[stringr::str_detect(results, ".csv")]
# survival_risk_table_files<-results[stringr::str_detect(results, "risk_table_results")]
# survival_risk_table_files <- survival_risk_table_files[!stringr::str_detect(survival_risk_table_files, "risk_table_results_cy")]
# 
# survival_risk_table <- list()
# for(i in seq_along(survival_risk_table_files)){
#   survival_risk_table[[i]]<-readr::read_csv(survival_risk_table_files[[i]],
#                                             show_col_types = FALSE) %>%
#     mutate_if(is.double, as.character)
#   
# }
# 
# survival_risk_table <- dplyr::bind_rows(survival_risk_table)
# survival_risk_table <- prepare_output_survival(survival_risk_table)
# 
# # median and survival probabilities ------
# # median results whole database
# survival_median_files<-results[stringr::str_detect(results, ".csv")]
# survival_median_files<-results[stringr::str_detect(results, "median_survival")]
# survival_median_files<-survival_median_files[!(stringr::str_detect(survival_median_files, "median_survival_results_cy"))]
# survival_median_table <- list()
# for(i in seq_along(survival_median_files)){
#   survival_median_table[[i]]<-readr::read_csv(survival_median_files[[i]],
#                                               show_col_types = FALSE)  
# }
# survival_median_table <- dplyr::bind_rows(survival_median_table)
# survival_median_table <- prepare_output_survival(survival_median_table)

# survival estimates -----
# whole pop

# table one ------
tableone_whole_files <- results[stringr::str_detect(results, ".csv")]
tableone_whole_files <- results[stringr::str_detect(results, "tableone")]
tableone_whole <- list()
for(i in seq_along(tableone_whole_files)){
  tableone_whole[[i]] <- readr::read_csv(tableone_whole_files[[i]],
                                         show_col_types = FALSE)
}
tableone_whole <- bind_rows(tableone_whole)


