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
        text = "Database Snapshot",
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
      
      menuItem(
        text = "Data Partners",
        tabName = "dp",
        icon = shiny::icon("users-viewfinder"),
        menuSubItem(
          text = "Data Partners",
          tabName = "test"
        )
      ),
      
      # Logo HDS
      tags$div(
        style = "position: relative; margin-top: 20px; text-align: center; margin-bottom: 0;",
        a(img(
          src = "EHDEN_Logo_JPG.jpg",  # Replace with the correct file name and extension
          height = "60px",  # Adjust the height as needed
          width = "auto"     # Let the width adjust proportionally
        ),
        href = "https://www.ehden.eu/",
        target = "_blank"
        )
      ) ,
      
      # Logo HDS
      tags$div(
        style = "position: relative; margin-top: 20px; text-align: center; margin-bottom: 0;",
        a(img(
          src = "Logo_HDS.png",  # Replace with the correct file name and extension
          height = "80px",  # Adjust the height as needed
          width = "auto"     # Let the width adjust proportionally
        ),
        href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
        target = "_blank"
        )
      ) ,
      
      # Logo UOXD
      tags$div(
        style = "position: relative; margin-top: -20px; text-align: center; margin-bottom: 0;",
        a(img(
          src = "logoOxford.png",  # Replace with the correct file name and extension
          height = "100px",  # Adjust the height as needed
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
        h3("Trends in comorbidities, concomitant treatments, and overall survival for eight cancers (2000-2019): A cohort study of 1.7 million patients across 11 European health records and cancer registries"),
        tags$h4(tags$strong("Please note, the results presented here should be considered as
                                                preliminary and subject to change.")),
        
        tags$h5(
          "This app is a companion to the study focussing trends in comorbidities, concomitant treatments, and overall survival for eight cancers (Breast, Colorectal, Lung, Liver, Stomach, Head & Neck, Prostate, and Pancreas) for a variety of different electronic health records and cancer registries across Europe (Spain, Netherlands, Norway, Finland, Portugal, Estonia, Switzerland, and the United Kingdom)."
        ),
        
        
        tags$h5(
          tags$span("Background:", style = "font-weight: bold;"),
          "Real-world evidence offers timely insights into cancer burden, addressing differences between countries in cancer care. This study aims to describe the patient characteristics and estimate overall survival of patients diagnosed with either breast, pancreatic, prostate, colorectal, lung, stomach, liver, or head and neck cancer from 11 electronic health records and cancer registry databases from Estonia, Finland, The Netherlands, Norway, Portugal, Spain, Switzerland, and the United Kingdom."
        ),
        
        tags$h5(
          tags$span("Methods:", style = "font-weight: bold;"),
          "Patients aged 18 years and older and with a primary cancer diagnosis were included in this study. The study period was from the 1st of January 2000 until the 31st of December 2019. Patients were followed up from cancer diagnosis until either death, database exit or end of the study period. Mortality data for each database originated either from National Death Registries, or death records registered within the databases. Patient characteristics at cancer diagnosis, such as sex, age, comorbidities and medication usage, were summarized. Median survival and survival rates at one-, five- and ten-year after diagnosis were calculated using the Kaplan Meier method and age standardized. All results were stratified by cancer type and sex."
          
        ),
        
        tags$h5(
          tags$span("Findings:", style = "font-weight: bold;"),
          "There were 1,796,278 eligible patients included in the study. Baseline patient characteristics showed consistent patterns in age, sex, comorbidities and medication usage across databases with breast, prostate, colorectal, and lung cancer the most prevalent cancers across databases. Patients aged 60-79 years accounted for the highest number of cancer diagnoses across all databases. Common comorbidities prior to cancer diagnosis were hypertension, type 2 diabetes, osteoarthritis, anemia and hyperlipidemia and common medications were those used for acid related disorders, systemic antibacterials and anti-inflammatory/antirheumatic medications. Breast and prostate cancer had the highest one-year, five-year, and ten-year overall survival with pancreatic cancer showing the lowest survival rates."
          
        ),
        
        
        tags$h5(
          tags$span("Interpretation:", style = "font-weight: bold;"),
          "Our study found similar characteristics and survival patterns across different electronic health records databases and cancer registries for a variety of different cancers highlighting the potential benefit of utilizing real world data to enhance our understanding of cancer survival."
          
        ),

        
        tags$h5(
          tags$span("Funding:" , style = "font-weight: bold;"),
          "This research was funded by the European Health Data and Evidence Network (EHDEN) (grant number 806968), and the Oxford NIHR Biomedical Research Centre."
        ),
        
        tags$h5("The results of this study are published in the following preprint and journal:"
        ),
        tags$ol(
          tags$li(strong("Preprint"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" )),
        
        tags$ol(
          tags$li(strong("Journal Article"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" )),
        
        
        tags$h5("The analysis code used to generate these results can be found",
                tags$a(href="https://github.com/oxford-pharmacoepi/CancerSurvivalWp2Analysis", "here"),
                ".The cohort diagnostics including the clinical codelists for each of the eight cancers can be found",
                tags$a(href="https://dpa-pde-oxford.shinyapps.io/CancerExtrapolationDiagnostics/", "here")
                
        ),
        
        tags$h5("Any questions regarding this shiny app or the study please contact",
                tags$a(href="mailto:danielle.newby@ndorms.ox.ac.uk", "Danielle Newby"), "or the corresponding author",
                tags$a(href="mailto:tduarte@idiapjgol.org", "Talita Duarte Salles")
                
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
        tabName = "test",
        h4("Thanks go to all the Data Partners and collaborators who participated in this study"),
        fluidRow(
          column(width = 3, div(class = "logo-container", img(src = "logoOxford.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "idiap.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "nice.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "CPRD.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "sidiap.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "iknl.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "crnorway.svg", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "hus.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "huvm.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "gcr.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "eci.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "uoe.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "imasis.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "imasis1.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "ulsm.jpeg", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "ulsra.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "ulsedv.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "ulsge.jpeg", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "ipci.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "erasmus.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "odysseus.png", height = "100px"))),
          column(width = 3, div(class = "logo-container", img(src = "utartu.png", height = "100px")))
        ),
        tags$style(HTML("
    .logo-container {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 120px; /* Adjusted height to include margin space */
      margin: 10px 0; /* Adds space above and below each logo */
    }
    .logo-container img {
      max-height: 100px;
      max-width: 90%;
    }
  "))
      ),

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
            selected = "cohort_name",
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
        
        div(
          style = "width: 80vh; height: 5vh;",  # Set width to 100% for responsive design
          checkboxInput("show_cond_meds", "Include Comorbidities & Medications", value = FALSE)
        ),

        
        # tags$hr(),
        gt_output("gt_patient_characteristics") %>% 
          withSpinner() ,
        
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_patient_characteristics_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right") ,
        
     # ) ,
      
      div(style="display:inline-block",
          downloadButton(
            outputId = "gt_patient_characteristics_csv",
            label = "Download table as csv"
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
            choices = unique(med_surv_km_sex_age$Database),
            selected = unique(med_surv_km_sex_age$Database),
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
            choices = unique(med_surv_km_sex_age$Age),
            selected = "All",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        DTOutput("dt_surv_est"),
        
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
      
      
      # cohort definition ------
      tabItem(
        tabName = "cohort_concepts",
        
        pickerInput(
          inputId = "cohort_set_input",
          label = "Cohort Set",
          choices = unique(cohort_set$cohort_name),
          selected = unique(cohort_set$cohort_name)[1],
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
          multiple = FALSE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Cohort definition",
            uiOutput("markdown")
          ),
          tabPanel(
            "JSON",
            h4(),
            rclipboardSetup(),
            uiOutput("clip"),
            verbatimTextOutput("verb"),
          ) ,
          tabPanel(
            "Concept sets",
            
            
            htmlOutput('tbl_concept_sets'),
            
            div(style="display:inline-block",
                downloadButton(
                  outputId = "dt_concept_sets_word",
                  label = "Download table as word"
                ), 
                style="display:inline-block; float:right")
            
          ),
          
        )

      ),
      
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
                        multiple = TRUE)
        ),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "survsum_plot_group",
                        label = "Colour by",
                        choices = c("Sex",
                                    "Age",
                                    "Cancer",
                                    "Database"),
                        selected = c("Cancer"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE)
        ),
        
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
        
        tags$h5("The Kaplan-Meier survival plots are presented below. We present the crude and age standardized KM curves. We calculated age-standardised survival estimates for all ages combined using the direct method using",
                tags$a(href = "https://www.sciencedirect.com/science/article/pii/S0959804904005283", "ICSS weightings"),
                
                "We were unable to estimate age survival for some countries due to the small number of cases in some age and cancer-specific strata, so they are not presented as results."),
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
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survival_type_selector",
            label = "Database Type",
            choices = unique(survival_km$database_type),
            selected = unique(survival_km$database_type),
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
                                    "Age",
                                    "database_type"
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
          checkboxInput("show_ci", "Show Confidence Intervals", value = FALSE)
        ),
        
        
          div(
            style = "width: 100%;",  # Use percentage to make it responsive within the column
            numericInput("facet_ncol", "Number of Facets Columns:", value = 2, min = 1, max = 10, step = 1)
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
            textInput("survival_download_height", "", 30, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("survival_download_width", "", 35, width = "50px")
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

