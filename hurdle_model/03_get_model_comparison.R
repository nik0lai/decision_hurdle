# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, bayestestR, furrr)  

# functions
cat('working on: ', getwd())

if (str_detect(getwd(), 'gpfs')) {
  source('functions.R')
} else {
  source('hurdle_model/hurdle_functions.R')
}

# experiment, condition and model info to iterate through
experiments <- c('exp1', 'exp2')
conditions <- c('baserate', 'cue', 'payoff')

# Get bf values -----------------------------------------------------------

get_model_comparison_bf <- function(e, c) {
  cat(sprintf('%s_%s', e, c))
  
  # read models 
  fit_simple <- read_model(e, c, 'simple')
  fit_contrast <- read_model(e, c, 'contrast')
  fit_hurdle <- read_model(e, c, 'hurdle')
  fit_full <- read_model(e, c, 'full')
  
  # compare all models against simple model
  bf_mc_all <- bayesfactor_models(fit_simple, fit_contrast, fit_hurdle, fit_full, denominator = 1)
  # convert bf values to table
  bf_values <- tibble(model = c('simple', 'contrast', 'hurdle', 'full'), denominator = c('simple'), bf_log = bf_mc_all$log_BF)
  
  # compare contrast and hurdle model
  bf_mc_hyp <- bayesfactor_models(fit_contrast, fit_hurdle, denominator = 1)
  # convert bf values to table
  bf_values <- bind_rows(bf_values, tibble(model = c('contrast', 'hurdle'), denominator = c('contrast'), bf_log = bf_mc_hyp$log_BF))
  
  # create df with both comparisons bfs
  bf_values <- bf_values %>% mutate(experiment = e, condition = c)
  
  # save csv
  csv_file_name <- sprintf('%s_%s.csv', e, c)
  write_csv(bf_values, paste0(path_to_bfs, csv_file_name))
  
}

# set parallel plan
plan(multisession)

# get combinations of experiment and conditions
combinations <- expand.grid(experiments, conditions, stringsAsFactors = FALSE)
# get aligned vectors
experiments <- combinations$Var1
conditions <- combinations$Var2

# e <- experiments[1]
# c <- conditions[1]

# run function in parallel
future_map2(experiments, conditions, get_model_comparison_bf)






