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
  dplyr::mutate(Database = replace(Database, Database == "CPRD_GOLD", "CPRD GOLD")) %>% 
  select(-c("Method", "Stratification", "Adjustment" )) %>% 
  relocate(Database, .before = 1)

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
rm(survival_risk_table_prostate)


# median and survival probabilities ------
survival_median_files <- results[stringr::str_detect(results, ".csv")]
survival_median_files <- results[stringr::str_detect(results, "median_mean")]

survival_median_table <- list()
for(i in seq_along(survival_median_files)){
  survival_median_table[[i]]<-readr::read_csv(survival_median_files[[i]],
                                              show_col_types = FALSE) %>% 
    mutate(n = as.character(n),
           events = as.character(events))
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

survival_median_table_ECI <- survival_median_table %>% 
  filter(Database == "ECI") %>% 
  dplyr::mutate(Sex = replace(Sex, Sex == "Both", "Female"))

survival_median_table <- bind_rows(survival_median_table,
                                   survival_median_table_ECI,
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
  dplyr::mutate(cdm_name = replace(cdm_name, cdm_name == "CPRD_GOLD", "CPRD GOLD")) %>% 
  dplyr::mutate(variable = if_else(variable == "Conditions flag from any time prior to 0",
                            "Conditions flag -inf to 0 days", variable)) %>% 
  dplyr::mutate(variable = if_else(variable == "Medications flag from -365 to 0",
                                   "Medications flag -365 to 0 days", variable)) %>% 
  dplyr::mutate(variable = if_else(variable == "Outcome flag from 0 to 0",
                                   "Outcome flag 0 to 0", variable)) %>% 
  dplyr::mutate(variable = if_else(variable == "Visits count from -365 to 0",
                                   "Visits count -365 to 0 days", variable)) %>% 
  dplyr::mutate(variable = if_else(variable == "Obesity flag from any time prior to 0",
                                   "Conditions flag -inf to 0 days", variable)) %>% 
  dplyr::mutate(variable = if_else(variable == "Obesity flag -inf to 0 days",
                                   "Conditions flag -inf to 0 days", variable)) %>% 
  
  dplyr::mutate(variable = if_else(variable == "Obesity flag -inf to 0 days",
                                   "Conditions flag -inf to 0 days", variable)) %>%  
  
  dplyr::mutate(variable_level = if_else(variable_level == "18 To 39",
                                   "18 to 39", variable_level)) %>% 
  
  dplyr::mutate(variable_level = if_else(variable_level == "40 To 49",
                                         "40 to 49", variable_level)) %>% 
  
  dplyr::mutate(variable_level = if_else(variable_level == "50 To 59",
                                         "50 to 59", variable_level)) %>% 
  
  dplyr::mutate(variable_level = if_else(variable_level == "60 To 69",
                                         "60 to 69", variable_level)) %>% 
  
  dplyr::mutate(variable_level = if_else(variable_level == "70 To 79",
                                         "70 to 79", variable_level)) %>% 
  
  dplyr::mutate(variable_level = if_else(variable_level == "80 To 150",
                                         "80 to 150", variable_level)) %>% 
  
  
  filter(variable_level != "Obesity",
         variable_level != "None",
         estimate_type != "mean",
         estimate_type != "q05",
         estimate_type != "q95",
         estimate_type != "sd"
         ) %>% 
  dplyr::mutate(variable_level = if_else(variable_level == "Obesitycharybdis",
                                         "Obesity", variable_level))


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
  rename("Database name" = "cdm_name",
         "Persons in the cancer cohorts" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version",
         "Database CDM Version" = "cdm_version",
         "Database Description" = "cdm_description",
         "Study Start Date" = "StudyPeriodStartDate",
         "Database Start Date" = "earliest_observation_period_start_date" ) 

snapshotcdm <- full_join(snapshotcdm, database_details, by = "Database name" ) %>% 
  relocate("Database Description", .after = last_col()) %>% 
  relocate("Full name", .after = `Database name`)

snapshotcdm <- snapshotcdm %>%
  mutate(`Database Description` = ifelse(`Database name` == "MAITT", 
                                         "MAITT is a dataset specifically composed for RITA1/02-96-11 project (https://www.etis.ee/Portal/Projects/Display/7c765be1-d8a7-44e3-8789-1760ccbf00e3?lang=ENG, ethics committee approval number 268/T-12) from three national health databases in Estonia: digital prescription, claims, and EHR. It contains 10% random sample from Estonian population. For each individual in the sample, it contains all records from these three databases from 2012-2019", 
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

