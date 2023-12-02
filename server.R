#### SERVER ------
server <-	function(input, output, session) {
  
  # cdm snapshot------
  output$tbl_cdm_snaphot <- renderText(kable(snapshotcdm) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  output$gt_cdm_snaphot_word <- downloadHandler(
    filename = function() {
      "cdm_snapshot.docx"
    },
    content = function(file) {
      x <- gt(snapshotcdm)
      gtsave(x, file)
    }
  )
  
  #database details
  output$tbl_database_details <- renderText(kable(database_details) %>%
                                              kable_styling("striped", full_width = F) )


  output$gt_database_details_word <- downloadHandler(
    filename = function() {
      "database_description.docx"
    },
    content = function(file) {
      x <- gt(database_details)
      gtsave(x, file)
    }
  )
  
  # patient_characteristics ----
  get_patient_characteristics <- reactive({
    
    validate(
      need(input$demographics_cohort_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$demographics_selector != "", "Please select a demographic")
    )
    
    patient_characteristics <- tableone_whole %>% 
      filter(strata_level %in% input$demographics_selector) %>% 
      filter(group_level %in% input$demographics_cohort_selector) %>% 
      filter(cdm_name %in% input$demographics_database_selector)
      # filter(group_level %in%  
      #          stringr::str_replace_all(
      #            stringr::str_to_sentence(input$demographics_cohort_selector),
      #            "_", " ")
      # )

    patient_characteristics
  })
  
  
  output$gt_patient_characteristics  <- render_gt({
    PatientProfiles::gtCharacteristics(get_patient_characteristics())
  })  
  
  
  output$gt_patient_characteristics_word <- downloadHandler(
    filename = function() {
      "patient_characteristics.docx"
    },
    content = function(file) {
      
      gtsave(PatientProfiles::gtCharacteristics(get_patient_characteristics()), file)
    }
  )
  
  
  # clinical codelists ----------------
  
  get_codelists <- reactive({
    
    table <- concepts_lists %>%
      filter(Vocabulary %in% input$codelist_vocab_selector) %>%
      filter(Cancer %in% input$codelist_cohort_selector)
    
    table
    
  })
  

  output$tbl_codelists <- renderText(kable(get_codelists()) %>%
                                       kable_styling("striped", full_width = F) )


  output$gt_codelists_word <- downloadHandler(
    filename = function() {
      "concept_lists.docx"
    },
    content = function(file) {
      x <- gt(get_codelists())
      gtsave(x, file)
    }
  )
  
  
  # table one --------

  get_table_one <- reactive({

    table <- tableone_final %>%
      filter(Cancer %in% input$tableone_cohort_name_selector) %>%
      filter(Sex %in% input$tableone_sex_selector) %>%
      filter(Age %in% input$tableone_age_selector)

    selected_columns <- c("Description", input$tableone_database_name_selector)
    table <- table[, selected_columns, drop = FALSE]

    table

  })


  output$dt_tableone <- renderText(kable(get_table_one()) %>%
                                     kable_styling("striped", full_width = F) )


  output$gt_tableone_word <- downloadHandler(
    filename = function() {
      "table_one.docx"
    },
    content = function(file) {
      x <- gt(get_table_one())
      gtsave(x, file)
    }
  )
  


  # attrition --------
  get_attrition <- reactive({

    table <- attritioncdm %>%
      filter(Cancer %in% input$attrition_cohort_name_selector) %>%
      filter(Database %in% input$attrition_database_name_selector) 

    table

  })


  output$dt_attrition <- renderText(kable(get_attrition()) %>%
                                kable_styling("striped", full_width = F) )


  output$gt_attrition_word <- downloadHandler(
    filename = function() {
      "cohort_attrition.docx"
    },
    content = function(file) {
      x <- gt(get_attrition())
      gtsave(x, file)
    }
  )

  # surv stats --------
  get_surv_est <- reactive({
    
    table <- med_surv_km %>%
      filter(Cancer %in% input$surv_est_cohort_name_selector) %>%
      filter(Database %in% input$surv_est_database_name_selector) %>% 
      filter(Age %in% input$surv_est_age_selector) %>% 
    filter(Sex %in% input$surv_est_sex_selector)
    
    table
    
  })
  
  
  output$dt_surv_est <- renderText(kable(get_surv_est()) %>%
                                      kable_styling("striped", full_width = F) )
  
  
  output$gt_surv_est_word <- downloadHandler(
    filename = function() {
      "survival_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_surv_est())
      gtsave(x, file)
    }
  )  
  
  
  
  # surv stats --------
  get_risk_table <- reactive({
    
    table <- survival_risk_table %>%
      filter(Cancer %in% input$risk_table_cohort_name_selector) %>%
      filter(Database %in% input$risk_table_database_name_selector) %>% 
      filter(Age %in% input$risk_table_age_selector) %>% 
      filter(Sex %in% input$risk_table_sex_selector)
    
    table
    
  })
  
  
  output$dt_risk_table <- renderText(kable(get_risk_table()) %>%
                                     kable_styling("striped", full_width = F) )
  
  
  output$gt_risk_table_word <- downloadHandler(
    filename = function() {
      "risk_table.docx"
    },
    content = function(file) {
      x <- gt(get_risk_table())
      gtsave(x, file)
    }
  )  
  
  
  
### download survival plot ----
  output$survival_download_plot <- downloadHandler(
    filename = function() {
      "Survival_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_surv_plot(),
        width = as.numeric(input$survival_download_width),
        height = as.numeric(input$survival_download_height),
        dpi = as.numeric(input$survival_download_dpi),
        units = "cm"
      )
    }
  )
  
# survival plots -------
  
  get_surv_plot <- reactive({
    
    validate(
      need(input$survival_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$survival_database_selector != "", "Please select a database")
    )
    validate(
      need(input$survival_sex_selector != "", "Please select a sex")
    )

    validate(
      need(input$surv_plot_group != "", "Please select a group to colour by")
    )

    validate(
      need(input$surv_plot_facet != "", "Please select a group to facet by")
    )
    
      plot_data <- survival_km %>%
        filter(Database %in% input$survival_database_selector) %>%
        filter(Cancer %in% input$survival_cohort_name_selector) %>%
        filter(Age %in% input$survival_age_selector) %>%
        filter(Sex %in% input$survival_sex_selector)
    
    if (input$show_ci) {
      
      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw(base_size = 15) 
        
        
        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15) 
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw(base_size = 15) 
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15) 
        
      }
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot +
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::percent,
                           expand = c(0.02,0.02)) +
        scale_x_continuous(expand = c(0.02,0.02),
                           breaks = pretty_breaks(n = 10)) +
        theme(strip.text = element_text(size = 15, face = "bold"))
      
      plot 
      
    } else {
      
      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw(base_size = 15) 
        
        
        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15) 
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw(base_size = 15) 
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15) 
        
      }
      
      # Move scale_y_continuous outside of ggplot
          plot <- plot +
            scale_y_continuous(limits = c(0, NA),
                               labels = scales::percent,
                               expand = c(0.02,0.02)) +
            scale_x_continuous(expand = c(0.02,0.02),
                               breaks = pretty_breaks(n = 10)) +
            theme(strip.text = element_text(size = 15, face = "bold"))
                                        
                                        
      
      
      plot      
      
      
    }
    
    
    
    
    
  })
  
  output$survivalPlot <- renderPlot(
    get_surv_plot()
  )
  
   
}