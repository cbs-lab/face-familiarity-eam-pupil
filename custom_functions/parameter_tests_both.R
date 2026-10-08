# 1. Function to create parameter labels
create_param_label <- function(params) {
  paste(names(params), params, sep = "=", collapse = ", ")
}

# 2. Function to apply a parameter combination and create plots for both eyes
apply_param_and_plot_both <- function(input_data, param_set, param_name, processing_function) {
  tryCatch({
    # Special handling for baseline correction
    if (processing_function == "pupil_baselinecorrect") {
      # Use our custom function for baseline correction
      result <- pupil_baselinecorrect_both(
        input_data, 
        bc_onset_message = param_set$bc_onset_message,
        baseline_duration = param_set$baseline_duration,
        type = param_set$type
      )
      
      # Create plot with baseline corrected values
      plot <- create_pupil_plot_both_baseline(
        result,
        paste0(processing_function, ": ", param_name)
      )
    } else {
      # Apply the processing function with these parameters for other functions
      result <- do.call(processing_function, c(list(input_data), param_set))
      
      # Create standard plot with both eyes
      plot <- create_pupil_plot_both(
        result,
        paste0(processing_function, ": ", param_name)
      )
    }
    
    # Return result and plot
    list(data = result, plot = plot, params = param_set)
  }, error = function(e) {
    message(paste("Error with params", param_name, ":", e$message))
    NULL
  })
}

# 3. Function to save parameter test plot
save_param_test_plot <- function(plots, subj, tr, subj_dir, processing_function) {
  combined_plot <- wrap_plots(
    plots, 
    ncol = ceiling(sqrt(length(plots))),
    guides = "collect", 
    axes = "collect"
  )
  
  filename <- paste0("S", subj, "_T", tr, "_", processing_function, "_params.png")
  ggsave(file.path(subj_dir, filename), combined_plot, width = 12, height = 10)
  return(invisible(combined_plot))
}

# 4. Function to process one subject/trial
test_trial_parameters_both <- function(trial_data, trial_key, named_combinations, 
                                       processing_function, input_step, 
                                       plot_dir, output_dir, save_param_data) {
  # Extract subject and trial
  subj <- trial_data$subject
  tr <- trial_data$trial
  
  message(paste("Testing parameters for Subject:", subj, "Trial:", tr))
  
  # Get the data from the specified input step
  input_data <- trial_data[[input_step]]
  if (is.null(input_data)) {
    message(paste("  No data found for step:", input_step))
    return(NULL)
  }
  
  # Create directory for this subject
  subj_dir <- file.path(plot_dir, paste0("S", subj))
  dir.create(subj_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Apply each parameter combination
  param_results <- imap(named_combinations, function(params, param_name) {
    apply_param_and_plot_both(input_data, params, param_name, processing_function)
  }) %>%
    compact()  # Remove NULL results
  
  # Create combined plot of all parameter combinations
  if (length(param_results) > 0) {
    plots <- map(param_results, ~ .x$plot)
    save_param_test_plot(plots, subj, tr, subj_dir, processing_function)
    
    # Save individual parameter results if requested
    if (save_param_data) {
      # Create a folder for parameter data
      param_data_dir <- file.path(output_dir, "param_data", processing_function)
      dir.create(param_data_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Save each parameter result
      iwalk(param_results, function(result, param_name) {
        filename <- paste0("S", subj, "_T", tr, "_", 
                           gsub("[^a-zA-Z0-9]", "_", param_name), ".csv")
        write_csv(result$data, file.path(param_data_dir, filename))
      })
    }
  }
  
  # Enhance return structure to include subject, trial and parameter info
  enhanced_results <- map(param_results, function(result) {
    result$subject <- subj
    result$trial <- tr
    result$input_step <- input_step
    return(result)
  })
  
  # Return results for this subject/trial
  list(
    subject = subj,
    trial = tr,
    param_results = enhanced_results
  )
}

# 5. Function to calculate metrics for each eye and mean
calculate_eye_metrics <- function(data, y_col_base) {
  # Define column names for left, right and mean eyes
  l_col <- paste0("L_", y_col_base)
  r_col <- paste0("R_", y_col_base)
  m_col <- y_col_base  # Mean column without prefix
  
  # Check which columns exist
  has_left <- l_col %in% names(data)
  has_right <- r_col %in% names(data)
  has_mean <- m_col %in% names(data)
  
  # Create metrics for each available eye
  metrics <- tibble()
  
  if (has_left) {
    left_metrics <- tibble(
      Eye = "Left",
      Mean = mean(data[[l_col]], na.rm = TRUE),
      SD = sd(data[[l_col]], na.rm = TRUE),
      Min = min(data[[l_col]], na.rm = TRUE),
      Max = max(data[[l_col]], na.rm = TRUE),
      Missing = sum(is.na(data[[l_col]])) / length(data[[l_col]]) * 100
    )
    metrics <- bind_rows(metrics, left_metrics)
  }
  
  if (has_right) {
    right_metrics <- tibble(
      Eye = "Right",
      Mean = mean(data[[r_col]], na.rm = TRUE),
      SD = sd(data[[r_col]], na.rm = TRUE),
      Min = min(data[[r_col]], na.rm = TRUE),
      Max = max(data[[r_col]], na.rm = TRUE),
      Missing = sum(is.na(data[[r_col]])) / length(data[[r_col]]) * 100
    )
    metrics <- bind_rows(metrics, right_metrics)
  }
  
  if (has_mean || (has_left && has_right)) {
    # If mean column exists, use it; otherwise calculate from left and right
    if (has_mean) {
      mean_data <- data[[m_col]]
    } else {
      mean_data <- rowMeans(data[c(l_col, r_col)], na.rm = TRUE)
    }
    
    mean_metrics <- tibble(
      Eye = "Mean",
      Mean = mean(mean_data, na.rm = TRUE),
      SD = sd(mean_data, na.rm = TRUE),
      Min = min(mean_data, na.rm = TRUE),
      Max = max(mean_data, na.rm = TRUE),
      Missing = sum(is.na(mean_data)) / length(mean_data) * 100
    )
    metrics <- bind_rows(metrics, mean_metrics)
  }
  
  return(metrics)
}

# 6. Main parameter testing function for both eyes
test_parameter_combinations_both <- function(step_data,
                                             param_combinations,
                                             processing_function,
                                             input_step,
                                             plot_dir = here("plots", "parameter_tests_both"),
                                             output_dir = here("data", "parameter_tests_both"),
                                             save_param_data = TRUE) {
  
  # Create directories
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create a named list of parameter combinations
  named_combinations <- set_names(
    param_combinations,
    map_chr(param_combinations, create_param_label)
  )
  
  # Add required eye event columns to data before processing
  step_data <- map(step_data, function(trial_data) {
    # Check if input step data exists
    if (!is.null(trial_data[[input_step]])) {
      # Add eye event columns if they don't exist
      if (!"Eye_Event" %in% names(trial_data[[input_step]])) {
        trial_data[[input_step]] <- trial_data[[input_step]] %>%
          mutate(
            Eye_Event = "Fixation",
            L_Eye_Event = "Fixation",
            R_Eye_Event = "Fixation"
          )
      }
    }
    return(trial_data)
  })
  
  # Process each subject/trial
  results <- imap(step_data, function(trial_data, trial_key) {
    test_trial_parameters_both(
      trial_data, 
      trial_key, 
      named_combinations, 
      processing_function, 
      input_step, 
      plot_dir,
      output_dir,
      save_param_data
    )
  }) %>%
    compact()  # Remove NULL results
  
  # Save combined parameter results if requested
  if (save_param_data && length(results) > 0) {
    # Extract all parameter results
    all_param_results <- map(results, ~ .x$param_results) %>% 
      flatten()
    
    # Group by parameter combination
    param_names <- unique(map_chr(all_param_results, function(result) {
      create_param_label(result$params)
    }))
    
    # Create a combined data file for each parameter combination
    iwalk(param_names, function(param_name, idx) {
      # Filter results for this parameter combination
      param_data <- all_param_results %>%
        keep(~ create_param_label(.x$params) == param_name) %>%
        map_df(function(result) {
          # Add subject and trial columns to the data
          result$data %>%
            mutate(
              Subject = result$subject,
              Trial = result$trial
            )
        })
      
      # Save combined file
      safe_param_name <- gsub("[^a-zA-Z0-9]", "_", param_name)
      filename <- paste0(processing_function, "_", safe_param_name, "_all_subjects.csv")
      write_csv(param_data, file.path(output_dir, filename))
    })
    
    # Create a summary file with key metrics for each parameter combination
    summary_data <- map_df(param_names, function(param_name) {
      # Filter results for this parameter combination
      param_results <- all_param_results %>%
        keep(~ create_param_label(.x$params) == param_name)
      
      # Get parameter values
      param_values <- param_results[[1]]$params
      
      # Extract key metrics from results
      metrics <- map_df(param_results, function(result) {
        data <- result$data
        
        # Determine which column to use based on the processing function
        if (processing_function == "pupil_baselinecorrect") {
          y_col <- "Pupil_Diameter_bc.mm"
        } else {
          y_col <- "Pupil_Diameter.mm"
        }
        
        # Calculate metrics for both eyes and mean
        eye_metrics <- calculate_eye_metrics(data, y_col)
        
        # Add subject and trial info
        eye_metrics %>%
          mutate(
            Subject = result$subject,
            Trial = result$trial
          )
      })
      
      # Calculate averages across trials
      summary_metrics <- metrics %>%
        group_by(Eye) %>%
        summarize(
          Mean_across_trials = mean(Mean, na.rm = TRUE),
          SD_across_trials = sd(Mean, na.rm = TRUE),
          Mean_missing = mean(Missing, na.rm = TRUE)
        )
      
      # Add parameter information
      summary_with_params <- summary_metrics %>%
        mutate(Parameter_Set = param_name) %>%
        cbind(as_tibble(param_values)[rep(1, nrow(summary_metrics)), ])
      
      return(summary_with_params)
    })
    
    # Save summary file
    write_csv(summary_data, file.path(output_dir, 
                                      paste0(processing_function, "_parameter_summary.csv")))
  }
  
  return(results)
}