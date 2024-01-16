# #### UI -----

# ui shiny ----
ui <- dashboardPage(
  dashboardHeader(
    title = div("Cancer Survival", style = "text-align: left;"),  # Align title to the left
    titleWidth = 250  # Adjust the width as needed
  ),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background",
        icon = shiny::icon("book")
      ),
      
      menuItem(
        text = "Databases",
        tabName = "dbs",
        icon = shiny::icon("database"),
        menuSubItem(
          text = "Snapshot",
          tabName = "snapshotcdm"
        )
      ),
      
      menuItem(
        text = "Cohorts",
        tabName = "cohorts",
        icon = shiny::icon("person"),
        menuSubItem(
          text = "Cohort concepts",
          tabName = "cohort_concepts"
        ),
        menuSubItem(
          text = "Cohort Attrition Table",
          tabName = "cohort_attrition"
        ),
        menuSubItem(
          text = "Cohort Attrition Figures",
          tabName = "cohort_attr_fig"
        )
      ),
      
      menuItem(
        text = "Characteristics",
        tabName = "char",
        icon = shiny::icon("hospital-user"),
        menuSubItem(
          text = "Demographics",
          tabName = "demographics"
        )
      ),

      menuItem(
        text = "Overall Survival",
        tabName = "os",
        icon = shiny::icon("line-chart")
        ,
        menuSubItem(
          text = "Survival Plots",
          tabName = "survival_results"
        ),
        menuSubItem(
          text = "Summary Survival Plots",
          tabName = "summary_plots"
        ),
        menuSubItem(
          text = "Risk Table",
          tabName = "risk_results"
        ),
        menuSubItem(
          text = "Survival Estimates",
          tabName = "stats_results"
        ) 
      ),
      
      # Logo 
      tags$div(
        style = "position: relative; margin-top: -10px; text-align: center; margin-bottom: 0;",
        a(img(
          src = "logoOxford.png",  # Replace with the correct file name and extension
          height = "150px",  # Adjust the height as needed
          width = "auto"     # Let the width adjust proportionally
        ),
        href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
        target = "_blank"
        )
      )
    )
  ),
  
  ## body ----
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      # background  ------
      tabItem(
        tabName = "background",
        h3("Overall survival for common cancers: a multinational cohort study"),
        tags$h4(tags$strong("Please note, the results presented here should be considered as
                                                preliminary and subject to change.")),
        
        tags$h5(
          "This app is a companion to the study focussing on the overall survival for eight different cancers
        (Breast, Colorectal, Lung, Liver, Stomach, Head & Neck, Prostate, and Pancreas) for a variety of different electronic health records and cancer registries across Europe (Spain, Netherlands, Norway, Finland, Portugal, Estonia, Switzerland, and the United Kingdom)."
        ),
        
        
        tags$h5(
          tags$span("Background:", style = "font-weight: bold;"),
          "TBC"
        ),
        
        tags$h5(
          tags$span("Methods:", style = "font-weight: bold;"),
          "TBC"
          
        ),
        tags$h5(
          tags$span("Funding:" , style = "font-weight: bold;"),
          "This research was funded by the European Health Data and Evidence Network (EHDEN) (grant number 806968), the Optimal treatment for patients with solid tumours in Europe through Artificial Intelligence (OPTIMA) initiative (grant number 101034347), and the Oxford NIHR Biomedical Research Centre."
        ),
        
        tags$h5("The results of this study are published in the following journal:"
        ),
        tags$ol(
          tags$li(strong("TBC"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" )),
        
        tags$h5("The analysis code used to generate these results can be found",
                tags$a(href="https://github.com/oxford-pharmacoepi/CancerSurvivalWp2Analysis", "here"),
                ".The cohort diagnostics including the clinical codelists for each of the 8 cancers can be found",
                tags$a(href="https://dpa-pde-oxford.shinyapps.io/CancerExtrapolationDiagnostics/", "here")
                
        ),
        
        tags$h5("Any questions regarding this shiny app or the study please contact",
                tags$a(href="mailto:danielle.newby@ndorms.ox.ac.uk", "Danielle Newby")
                
        ),
        
        tags$hr()
        
      ),
      
      # cdm snapshot ------
      tabItem(
        tags$h5("Snapshot of the cdm from database"),
        tabName = "snapshotcdm",
        htmlOutput('tbl_cdm_snaphot'),
        tags$hr(),
        div(
          style = "display:inline-block",
          downloadButton(
            outputId = "gt_cdm_snaphot_word",
            label = "Download table as word"
          ),
          style = "display:inline-block; float:right"
        )
      ) ,

      tabItem(
        tabName = "demographics",
        tags$h5("The patient characteristics for the study populations are presented below:"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "demographics_database_selector",
            label = "Database",
            choices = unique(tableone_whole$cdm_name),
            selected = unique(tableone_whole$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "demographics_cohort_selector",
            label = "Cancer",
            choices = unique(tableone_whole$group_level),
            selected = unique(tableone_whole$group_level)[9],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "demographics_selector",
            label = "Demographics",
            choices = unique(tableone_whole$strata_level),
            selected = "Overall",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        # tags$hr(),
        gt_output("gt_patient_characteristics") %>% 
          withSpinner() ,
        
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_patient_characteristics_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      ) ,
      
      tabItem(
        tabName = "stats_results",
        tags$h5("Survival results containing one, five and ten year survival, restricted mean survival and median survival for each database are presented below:"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "surv_est_database_name_selector",
            label = "Database",
            choices = unique(survival_km$Database),
            selected = unique(survival_km$Database),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "surv_est_cohort_name_selector",
            label = "Cancer",
            choices = unique(survival_km$Cancer),
            selected = "Breast",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "surv_est_sex_selector",
            label = "Sex",
            choices = unique(survival_km$Sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "surv_est_age_selector",
            label = "Age",
            choices = unique(survival_km$Age),
            selected = "All",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('dt_surv_est'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_surv_est_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),

      tabItem(
        tabName = "risk_results",
        tags$h5("The risk table showing the numbers at risk are presented below:"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "risk_table_database_name_selector",
            label = "Database",
            choices = unique(survival_risk_table$Database),
            selected = unique(survival_risk_table$Database),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "risk_table_cohort_name_selector",
            label = "Cancer",
            choices = unique(survival_risk_table$Cancer),
            selected = "Breast",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "risk_table_sex_selector",
            label = "Sex",
            choices = unique(survival_risk_table$Sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "risk_table_age_selector",
            label = "Age",
            choices = unique(survival_risk_table$Age),
            selected = "All",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('dt_risk_table'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_risk_table_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),
      
      
      tabItem(
        tabName = "cohort_attr_fig",
        tags$h5("Attrition Diagrams for study populations:"),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_database_name_selector1",
            label = "Database",
            choices = unique(attritioncdm$Database),
            selected = unique(attritioncdm$Database)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_cohort_name_selector1",
            label = "Cancer",
            choices = unique(attritioncdm$Cancer),
            selected = "Breast",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        
        
        div(
          style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
          grVizOutput("attrition_diagram", width = "400px", height = "100%") %>%
            withSpinner(),
          h4("Download Figure"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("attrition_download_width", "", 600, width = "50px")
          ),
          div("pixels", style = "display: inline-block; margin-right: 25px;"),
          downloadButton("cohort_attrition_download_figure", "Download plot")
        )
        
      ),
      
      
      tabItem(
        tabName = "tableone",
        tags$h5("The patient characteristics for the study populations are presented below:"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "tableone_database_name_selector",
            label = "Database",
            choices = unique(tableone_whole$cdm_name),
            selected = unique(tableone_whole$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "tableone_cohort_name_selector",
            label = "Cancer",
            choices = unique(tableone_whole$group_level),
            selected = "Breast",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "tableone_sex_selector",
            label = "Sex",
            choices = c("Both", "Female", "Male") ,
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "tableone_age_selector",
            label = "Age",
            choices = unique(survival_km$Age),
            selected = "All",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        
        htmlOutput('dt_tableone'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_tableone_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),
      
      tabItem(
        tags$h5("Description of database details used in study are presented below:"),
        tabName = "database_details",
        htmlOutput('tbl_database_details'),
        tags$hr(),
        div(
          style = "display:inline-block",
          downloadButton(
            outputId = "gt_database_details_word",
            label = "Download table as word"
          ),
          style = "display:inline-block; float:right"
        )
      ) ,
      
      tabItem(
        tags$h5("The clinical codelists for each cancer used in this study are presented below:"),
        tabName = "cohort_concepts",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "codelist_cohort_selector",
            label = "Cancer",
            choices = unique(concepts_lists$Cancer),
            selected = "Breast",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "codelist_vocab_selector",
            label = "Vocabulary",
            choices = unique(concepts_lists$Vocabulary),
            selected = unique(concepts_lists$Vocabulary),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('tbl_codelists'),
        
        tags$hr(),
        
        div(
          style = "display:inline-block",
          downloadButton(
            outputId = "gt_codelists_word",
            label = "Download table as word"
          ),
          style = "display:inline-block; float:right"
        )
      ) , 
      
      tabItem(
        tags$h5("The cohort attrition showing how the final study populations were obtained are presented below:"),
        tabName = "cohort_attrition",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_database_name_selector",
            label = "Database",
            choices = unique(attritioncdm$Database),
            selected = unique(attritioncdm$Database)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_cohort_name_selector",
            label = "Study cohort",
            choices = unique(attritioncdm$Cancer),
            selected = "Breast",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        htmlOutput('dt_attrition'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_attrition_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),
      
      tabItem(
        tabName = "summary_plots",
        tags$h5("Plots of summary survival statistics for the study are presented below:"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survivalsum_database_selector",
            label = "Database",
            choices = unique(med_surv_km_sex_age$Database),
            selected = unique(med_surv_km_sex_age$Database),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survivalsum_cohort_name_selector",
            label = "Cancer",
            choices = unique(med_surv_km_sex_age$Cancer),
            selected = unique(med_surv_km_sex_age$Cancer),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survivalsum_sex_selector",
            label = "Sex",
            choices = unique(med_surv_km_sex_age$Sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survivalsum_age_selector",
            label = "Age",
            choices = unique(med_surv_km_sex_age$Age),
            selected = "All",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survivalsum_variable_selector",
            label = "Summary Statistic",
            choices = unique(med_surv_km_sex_age$Variable),
            selected = "One Year Survival",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "survsum_plot_facet",
                        label = "Facet by",
                        choices = c("Cancer",
                                    "Database",
                                    "Sex",
                                    "Age"
                        ),
                        selected = c("Cancer" ),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = FALSE)
        ),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "survsum_plot_group",
                        label = "Colour by",
                        choices = c("Sex",
                                    "Age",
                                    "Cancer",
                                    "Database"),
                        selected = c("Sex"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = FALSE)
        ),
        # div(
        #   style = "width: 80vh; height: 5vh;",  # Set width to 100% for responsive design
        #   checkboxInput("show_ci", "Show Confidence Intervals", value = TRUE)
        # ),
        
        div(
          style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
          plotOutput("survivalPlotSum",
                     height = "800px"
          ) %>%
            withSpinner(),
          h4("Download Figure"),
          div("Height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("survivalsum_download_height", "", 30, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("survivalsum_download_width", "", 35, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block; margin-right:",
            textInput("survivalsum_download_dpi", "", 600, width = "50px")
          ),
          downloadButton("survivalsum_download_plot", "Download plot")
        )
        ) ,
      
      tabItem(
        tabName = "survival_results",
        tags$h5("The Kaplan-Meier survival plots are presented below:"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survival_database_selector",
            label = "Database",
            choices = unique(survival_km$Database),
            selected = unique(survival_km$Database),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survival_cohort_name_selector",
            label = "Cancer",
            choices = unique(survival_km$Cancer),
            selected = "Breast",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survival_sex_selector",
            label = "Sex",
            choices = unique(survival_km$Sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survival_age_selector",
            label = "Age",
            choices = unique(survival_km$Age),
            selected = "All",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "surv_plot_facet",
                        label = "Facet by",
                        choices = c("Cancer",
                                    "Database",
                                    "Sex",
                                    "Age"
                        ),
                        selected = c("Cancer" ),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "surv_plot_group",
                        label = "Colour by",
                        choices = c("Sex",
                                    "Age",
                                    "Cancer",
                                    "Database"),
                        selected = c("Database"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        div(
          style = "width: 80vh; height: 5vh;",  # Set width to 100% for responsive design
          checkboxInput("show_ci", "Show Confidence Intervals", value = TRUE)
        ),
        
        div(
          style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
          plotOutput("survivalPlot",
                     height = "800px"
          ) %>%
            withSpinner(),
          h4("Download Figure"),
          div("Height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("survival_download_height", "", 10, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("survival_download_width", "", 20, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block; margin-right:",
            textInput("survival_download_dpi", "", 600, width = "50px")
          ),
          downloadButton("survival_download_plot", "Download plot")
        )
      )
    )
  )  
)

