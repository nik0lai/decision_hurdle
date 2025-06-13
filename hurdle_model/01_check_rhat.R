# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, rstan)

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
      # get r-hat values
      get_rhat_csv(e, c, m)
    }
  }
}

# Check r-hat values ------------------------------------------------------

rhat_threshold <- 1.02

# get r-hat of all model
for (e in experiments) {
  for (c in conditions) {
    for (m in models) {
      
      # read file with r-hat values
      csv_file <- sprintf('rhat_%s_%s_%s.csv', e, c, m)
      rhat_values <- read_csv(paste0(path_to_rhat, csv_file), col_types = cols())
      # get values above threshold
      high_rhat_values <- rhat_values %>% 
        filter(rhat > rhat_threshold)
      
      # Print diagnostic
      cat(sprintf('\n%s_%s_%s\n', e, c, m))
      if (nrow(high_rhat_values)) {
        cat('-- some r-hat values are higher than 1.01\n')
        print(high_rhat_values)
      } else {
        cat(sprintf('-- all values are < %s\n', rhat_threshold))
      }
      
    }
  }
}







