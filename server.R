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
  
  # Markdown ----
  output$markdown <- renderUI({
    
    table <- cohort_set %>% 
      filter(cohort_name %in% input$cohort_set_input) %>% 
      pull(markdown) %>% 
      formatMarkdown()
  })
  # JSON ----
  output$verb <- renderPrint({
    
    json_content <- cohort_set %>% 
      filter(cohort_name %in% input$cohort_set_input) %>%
      pull(json) %>%
      unlist()
    
    cat(json_content)
    
  })
  
  output$clip <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy to clipboard",
      clipText = isolate(cohort_set %>%
                           filter(cohort_name %in% input$cohort_set_input) %>%
                           pull(json) %>%
                           unlist()),
      icon = icon("clipboard"),
      placement = "top",
      options = list(delay = list(show = 800, hide = 100), trigger = "hover")
    )
  })
  
  #concepts_sets ----
  get_concepts_sets <- reactive({
    
    validate(
      need(input$cohort_set_input != "", "Please select a cohort")
    )
    
    concept_sets_final <- concept_sets_final %>% 
      filter(name %in% input$cohort_set_input)
      
    
    concept_sets_final
    
  })
  
  
  output$tbl_concept_sets <- renderText(kable(get_concepts_sets()) %>%
                                          kable_styling("striped", full_width = F) )
  
  output$dt_concept_sets_word <- downloadHandler(
    filename = function() {
      "concept_sets.docx"
    },
    content = function(file) {
      x <- gt(get_concepts_sets())
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
      
      # Define the filters
      base_filter <- tableone_whole %>%
        filter(strata_level %in% input$demographics_selector) %>%
        filter(group_level %in% input$demographics_cohort_selector) %>%
        filter(cdm_name %in% input$demographics_database_selector)
      
      if (input$show_cond_meds) {
        patient_characteristics <- base_filter
      } else {
        patient_characteristics <- base_filter %>%
          filter(!variable %in% c(
            "Medications flag from -365 to 0",
            "Visits count from -365 to 0",
            "Conditions flag from any time prior to 0",
            "Prior observation",
            "Obesity flag from any time prior to 0"
          ))
      }
      
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
    
    validate(
      need(input$codelist_cohort_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$codelist_vocab_selector != "", "Please select a vocab")
    )
    
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
  

  # attrition --------
  get_attrition <- reactive({
    
    validate(
      need(input$attrition_cohort_name_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$attrition_database_name_selector != "", "Please select a database")
    )
    
    table <- attritioncdm %>%
      filter(Cancer %in% input$attrition_cohort_name_selector) %>%
      filter(Database %in% input$attrition_database_name_selector) 
    
    table
    
  })
  
  
  get_attrition1 <- reactive({
    
    validate(
      need(input$attrition_cohort_name_selector1 != "", "Please select a cohort")
    )
    
    validate(
      need(input$attrition_database_name_selector1 != "", "Please select a database")
    )

    table <- attritioncdm %>%
      filter(Cancer %in% input$attrition_cohort_name_selector1) %>%
      filter(Database %in% input$attrition_database_name_selector1) 

    table

  })
  
  output$attrition_diagram <- renderGrViz({
    table <- get_attrition1()
    validate(need(nrow(table) > 0, "No results for selected inputs"))
    render_graph(attritionChart(table))
  })
  
  output$cohort_attrition_download_figure <- downloadHandler(
    filename = function() {
      paste0(
        "cohort_attrition_", input$attrition_database_name_selector1, "_", 
        input$attrition_cohort_name_selector1, ".png"
      )
    },
    content = function(file) {
      table <- get_attrition1()
      export_graph(
        graph = attritionChart(table),
        file_name = file,
        file_type = "png",
        width = input$attrition_download_width |> as.numeric()
      )
    }
  )


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
    
    validate(
      need(input$surv_est_cohort_name_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$surv_est_sex_selector != "", "Please select sex group")
    )
    
    validate(
      need(input$surv_est_age_selector != "", "Please select age group")
    )
    
    validate(
      need(input$surv_est_database_name_selector != "", "Please select a database")
    )
    
    table <- med_surv_km %>%
      filter(Cancer %in% input$surv_est_cohort_name_selector) %>%
      filter(Database %in% input$surv_est_database_name_selector) %>% 
      filter(Age %in% input$surv_est_age_selector) %>% 
    filter(Sex %in% input$surv_est_sex_selector) %>% 
      arrange(Cancer)
     
    
    table
    
  })
  
  output$dt_surv_est <- renderDT({
    datatable(get_surv_est(), options = list(pageLength = 100000, order = list(list(0, 'asc'))))
  })

  
  # Capture the table's current state
  table_proxy <- dataTableProxy('dt_surv_est')
  
  # Function to get the sorted and filtered data
  get_table_state <- reactive({
    req(input$dt_surv_est_rows_all)
    get_surv_est()[input$dt_surv_est_rows_all, ]
  })
  
  
  output$gt_surv_est_word <- downloadHandler(
    filename = function() {
      "survival_estimates.docx"
    },
    content = function(file) {
      # Get the filtered and sorted data
      data <- get_table_state() %>%
        select(Cancer, Database, everything(), -`Mean Survival (SE)`) %>%
        arrange(Cancer) %>% 
        mutate(across(everything(), ~ gsub("([0-9])\\.([0-9])", "\\1·\\2", .)))
      
      
      # Conditionally remove the 'Sex' column if 'Both' is selected
      if (input$survival_sex_selector == "Both") {
        data <- data %>% select(-Sex)
      }
      
      # Conditionally remove the 'Age' column if 'All' is selected
      if (input$survival_age_selector == "All") {
        data <- data %>% select(-Age)
      }

      # Create the gt table and save to Word
      x <- gt(data)
      gtsave(x, file)
    }
  )
  
  
  # surv risk table --------
  get_risk_table <- reactive({
    
    validate(
      need(input$risk_table_cohort_name_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$risk_table_sex_selector != "", "Please select sex group")
    )
    
    validate(
      need(input$risk_table_age_selector != "", "Please select age group")
    )
    
    validate(
      need(input$risk_table_database_name_selector != "", "Please select a database")
    )
    
    table <- survival_risk_table %>%
      filter(Cancer %in% input$risk_table_cohort_name_selector) %>%
      filter(Database %in% input$risk_table_database_name_selector) %>% 
      filter(Age %in% input$risk_table_age_selector) %>% 
      filter(Sex %in% input$risk_table_sex_selector) %>% 
      arrange(Cancer)
    
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
          geom_line(size = 1) + 
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group), alpha = 0.2, colour = NA) +
          xlab("Time (Years)") +
          ylab("Survival Function") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw(base_size = 20) 
        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line(size = 1) + 
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group), alpha = 0.2, colour = NA) +
          xlab("Time (Years)") +
          ylab("Survival Function") +
          theme_bw(base_size = 20) 
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line(size = 1) + 
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group), alpha = 0.2, colour = NA) +
          xlab("Time (Years)") +
          ylab("Survival Function") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw(base_size = 20) 
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line(size = 1) + 
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group), alpha = 0.2, colour = NA) +
          xlab("Time (Years)") +
          ylab("Survival Function") +
          theme_bw(base_size = 20) 
        
      }
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot +
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::percent,
                           expand = c(0.02,0.02)) +
        scale_x_continuous(expand = c(0.02,0.02),
                           breaks = pretty_breaks(n = 10)) +
        theme(
          text = element_text(size = 20),
          strip.text = element_text(size = 20, face = "bold"))
      
      plot 
      
    } else {
      
      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line(size = 1) + 
          xlab("Time (Years)") +
          ylab("Survival Function") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw(base_size = 20) 

        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line(size = 1) + 
          xlab("Time (Years)") +
          ylab("Survival Function") +
          theme_bw(base_size = 20) 
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line(size = 1) + 
          xlab("Time (Years)") +
          ylab("Survival Function") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw(base_size = 20) 
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line(size = 1.5) + 
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 20) 
        
      }
      
      # Move scale_y_continuous outside of ggplot
          plot <- plot +
            scale_y_continuous(limits = c(0, NA),
                               labels = scales::percent,
                               expand = c(0.02,0.02)) +
            scale_x_continuous(expand = c(0.02,0.02),
                               breaks = pretty_breaks(n = 10)) +
            theme(
              text = element_text(size = 20),
              strip.text = element_text(size = 20, face = "bold"))
                                        
      
      plot      
      
      
    }
    
    
  })
  
  output$survivalPlot <- renderPlot(
    get_surv_plot()
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
  
  # visualizations for sex and overall stratification
   get_surv_plot_sum <- reactive({
    validate(
      need(input$survivalsum_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$survivalsum_database_selector != "", "Please select a database")
    )
    validate(
      need(input$survivalsum_sex_selector != "", "Please select a sex")
    )
    validate(
      need(input$survsum_plot_group != "", "Please select a group to colour by")
    )
    validate(
      need(input$survsum_plot_facet != "", "Please select a group to facet by")
    )
    validate(
      need(input$survivalsum_variable_selector != "", "Please select a summary variable")
    )


    plot_data <- med_surv_km_sex_age %>%
      filter(Database %in% input$survivalsum_database_selector) %>%
      filter(Cancer %in% input$survivalsum_cohort_name_selector) %>%
      filter(Sex %in% input$survivalsum_sex_selector) %>%
      filter(Age %in% input$survivalsum_age_selector) %>% 
      filter(Variable %in% input$survivalsum_variable_selector) 

    if (!is.null(input$survsum_plot_group) && !is.null(input$survsum_plot_facet)) {
      
      plot <- plot_data %>%
        unite("Group", c(all_of(input$survsum_plot_group)), remove = FALSE, sep = "; ") %>%
        unite("facet_var", c(all_of(input$survsum_plot_facet)), remove = FALSE, sep = "; ") %>%
        ggplot(aes(x = Database, 
                   y = Value,
                   ymin = if (input$survivalsum_variable_selector == "One Year Survival") {
                     `lower year 1`
                   } else if (input$survivalsum_variable_selector == "Five Year Survival") {
                     `lower year 5`
                   } else if (input$survivalsum_variable_selector == "Ten Year Survival") {
                     `lower year 10`  
                   } else if (input$survivalsum_variable_selector == "Restricted Mean") {
                     lower_rmean 
                   } else {
                     lower_median
                   },
                   ymax = if (input$survivalsum_variable_selector == "One Year Survival") {
                     `upper year 1`
                   } else if (input$survivalsum_variable_selector == "Five Year Survival") {
                     `upper year 5`
                   } else if (input$survivalsum_variable_selector == "Ten Year Survival") {
                     `lower year 10`   
                   } else if (input$survivalsum_variable_selector == "Restricted Mean") {
                     upper_rmean 
                   } else {
                     upper_median
                   },
                   group = Group, 
                   colour = Group,
                   fill = Group)) +
        geom_errorbar(position = position_dodge(width = 0.6), width = 0, aes(color = Group), size = 1) +
        geom_point(position = position_dodge(width = 0.6), size = 3, shape = 21, stroke = 0.75, color = "black") +
        xlab("Database") +
        ylab(input$survivalsum_variable_selector) +
        # facet_wrap(vars(facet_var), ncol = 2, scales = "free_x") +
        # theme_bw(base_size = 20) +
        # coord_flip()
      facet_wrap(vars(facet_var), ncol = 2, scales = "free_y") +
        theme_bw(base_size = 20) 
      
    } else if (!is.null(input$survsum_plot_group) && is.null(input$survsum_plot_facet)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$survsum_plot_group)), remove = FALSE, sep = "; ") %>%
        ggplot(aes(x = Database, 
                   y = Value,
                   ymin = if (input$survivalsum_variable_selector == "One Year Survival") {
                     `lower year 1`
                   } else if (input$survivalsum_variable_selector == "Five Year Survival") {
                     `lower year 5`
                   } else if (input$survivalsum_variable_selector == "Ten Year Survival") {
                     `lower year 10`  
                   } else if (input$survivalsum_variable_selector == "Restricted Mean") {
                     lower_rmean 
                   } else {
                     lower_median
                   },
                   ymax = if (input$survivalsum_variable_selector == "One Year Survival") {
                     `upper year 1`
                   } else if (input$survivalsum_variable_selector == "Five Year Survival") {
                     `upper year 5`
                   } else if (input$survivalsum_variable_selector == "Ten Year Survival") {
                     `lower year 10`   
                   } else if (input$survivalsum_variable_selector == "Restricted Mean") {
                     upper_rmean 
                   } else {
                     upper_median
                   },
                   group = Group, 
                   colour = Group,
                   fill = Group)) +
        geom_errorbar(position = position_dodge(width = 0.6), width = 0, aes(color = Group), size = 1) +
        geom_point(position = position_dodge(width = 0.6), size = 2, shape = 21, stroke = 0.75, color = "black") +
        xlab("Database") +
        ylab(input$survivalsum_variable_selector) +
        theme_bw(base_size = 20)
      
    } else if (is.null(input$survsum_plot_group) && !is.null(input$survsum_plot_facet)) {
      plot <- plot_data %>%
        unite("facet_var", c(all_of(input$survsum_plot_facet)), remove = FALSE, sep = "; ") %>%
        ggplot(aes(x = Database, 
                   y = Value,
                   ymin = if (input$survivalsum_variable_selector == "One Year Survival") {
                     `lower year 1`
                   } else if (input$survivalsum_variable_selector == "Five Year Survival") {
                     `lower year 5`
                   } else if (input$survivalsum_variable_selector == "Ten Year Survival") {
                     `lower year 10`  
                   } else if (input$survivalsum_variable_selector == "Restricted Mean") {
                     lower_rmean 
                   } else {
                     lower_median
                   },
                   ymax = if (input$survivalsum_variable_selector == "One Year Survival") {
                     `upper year 1`
                   } else if (input$survivalsum_variable_selector == "Five Year Survival") {
                     `upper year 5`
                   } else if (input$survivalsum_variable_selector == "Ten Year Survival") {
                     `lower year 10`   
                   } else if (input$survivalsum_variable_selector == "Restricted Mean") {
                     upper_rmean 
                   } else {
                     upper_median
                   },
                   group = Group, 
                   colour = Group,
                   fill = Group)) +
        geom_errorbar(position = position_dodge(width = 0.6), width = 0, aes(color = Group), size = 1) +
        geom_point(position = position_dodge(width = 0.6), size = 2, shape = 21, stroke = 0.75, color = "black") +
        xlab("Database") +
        ylab(input$survivalsum_variable_selector) +
        facet_wrap(vars(facet_var), ncol = 2, scales = "free_y") +
        theme_bw(base_size = 20)
      
    } else {
      plot <- plot_data %>%
        ggplot(aes(x = Database, 
                   y = Value,
                   ymin = if (input$survivalsum_variable_selector == "One Year Survival") {
                     `lower year 1`
                   } else if (input$survivalsum_variable_selector == "Five Year Survival") {
                     `lower year 5`
                   } else if (input$survivalsum_variable_selector == "Ten Year Survival") {
                     `lower year 10`  
                   } else if (input$survivalsum_variable_selector == "Restricted Mean") {
                     lower_rmean 
                   } else {
                     lower_median
                   },
                   ymax = if (input$survivalsum_variable_selector == "One Year Survival") {
                     `upper year 1`
                   } else if (input$survivalsum_variable_selector == "Five Year Survival") {
                     `upper year 5`
                   } else if (input$survivalsum_variable_selector == "Ten Year Survival") {
                     `lower year 10`   
                   } else if (input$survivalsum_variable_selector == "Restricted Mean") {
                     upper_rmean 
                   } else {
                     upper_median
                   },
                   group = Group, 
                   colour = Group,
                   fill = Group)) +
        geom_errorbar(position = position_dodge(width = 0.6), width = 0, aes(color = Group), size = 1) +
        geom_point(position = position_dodge(width = 0.6), size = 2, shape = 21, stroke = 0.75, color = "black") +
        xlab("Database") +
        ylab(input$survivalsum_variable_selector) +
        theme_bw(base_size = 20)
    }

    # Move scale_y_continuous outside of ggplot
    plot <- plot +
      theme(strip.text = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))

    plot
  })
  
  
  output$survivalPlotSum <- renderPlot(
    get_surv_plot_sum()
  )
  
  ### download survival plot ----
  output$survivalsum_download_plot <- downloadHandler(
    filename = function() {
      "Survival_summary_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_surv_plot_sum(),
        width = as.numeric(input$survivalsum_download_width),
        height = as.numeric(input$survivalsum_download_height),
        dpi = as.numeric(input$survivalsum_download_dpi),
        units = "cm"
      )
    }
  ) 
  
  
  
   
}