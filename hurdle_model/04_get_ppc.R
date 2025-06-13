# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, tidybayes)  

# functions
source('hurdle_model/hurdle_functions.R')

# experiment, condition and model info to iterate through
experiments <- c('exp1', 'exp2')
conditions <- c('baserate', 'cue', 'payoff')
models <- c('simple', 'contrast', 'hurdle', 'full')


# Get r-hat values --------------------------------------------------------

# get r-hat of all model
for (e in experiments) {
  for (c in conditions) {
    for (m in models) {
      
      cat(sprintf('ppc_%s_%s_%s\n', e, c, m))
      
      # read model fit
      fit <- read_model(e, c, m)
      # read data
      data <- read_csv(paste0(path_to_data, sprintf('all_data_filtered_%s.csv', e)), col_types = cols())
      # Filter reproduction data and pick important columns
      data <- data %>% filter(trial_type == 'reproduction')
      data <- data %>% mutate(across(c(target_contrast, answer), ~abs(as.double(.x))))
      data <- data %>% select(participant, bias_source, bias_direction, contrast_level, target_contrast, answer) %>% rename(stim=target_contrast, response=answer, cond=bias_direction)
      data <- data %>% filter(bias_source == c)
      
      # Add predicted draws to data
      predicted_data <- 
        data %>%
        select(-response) %>% 
        distinct() %>% 
        add_predicted_draws(fit)
      
      # Get observed data summaries
      data <- 
        # Get non zero reproduction
        data %>% 
        filter(response != 0) %>% 
        group_by(participant, bias_source, cond, contrast_level, stim) %>% 
        reframe(response_non_zero = mean(response, na.rm = TRUE)) %>% 
        # Get proportion of non zero reproductions and overall reproduction
        full_join(
          data %>%
            group_by(participant, bias_source, cond, contrast_level, stim) %>% 
            reframe(prop_zero = mean(response==0, na.rm = TRUE),
                    response = mean(response, na.rm = TRUE)),
          by = join_by(participant, bias_source, cond, contrast_level, stim)
        )
      
      # Get predicted response
      ppc_resp <- predicted_data %>% 
        group_by(participant, bias_source, cond, contrast_level, stim) %>% 
        reframe(response_pred = mean(.prediction, na.rm = TRUE))
      
      # Get predicted prop zero reproductions
      ppc_prop <- predicted_data %>% 
        group_by(participant, bias_source, cond, contrast_level, stim) %>% 
        reframe(hurdle_prob = 1-mean(.prediction > 0, na.rm = TRUE))
      
      # Get predicted non zero reproductions
      ppc_non_zero <- predicted_data %>% 
        filter(.prediction > 0) %>%  # Filter out zero predictions
        group_by(participant, bias_source, cond, contrast_level, stim) %>% 
        reframe(response_pred_non_zero = mean(.prediction, na.rm = TRUE))
      
      # Combine observed and predicted data
      ppc <- data %>% 
        full_join(ppc_resp, by = join_by(participant, bias_source, cond, contrast_level, stim)) %>% 
        full_join(ppc_prop, by = join_by(participant, bias_source, cond, contrast_level, stim)) %>% 
        full_join(ppc_non_zero, by = join_by(participant, bias_source, cond, contrast_level, stim))
      
      # Add model name
      ppc <- ppc %>% mutate(model_name = m)
      
      # Save ppc csv
      csv_file_name <- sprintf('ppc_%s_%s_%s.csv', e, c, m)
      write_csv(ppc, paste0(path_to_ppc, csv_file_name))
      
    }
  }
}




