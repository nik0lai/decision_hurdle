# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, brms)  


path_to_data <- 'data/processed/'
path_to_fit_models <- 'hurdle_model/fits/'
path_to_rhat <- 'hurdle_model/rhat/'
path_to_bfs <- 'hurdle_model/bfs_model_comparison/'
path_to_ppc <- 'hurdle_model/ppc/'

# Sampling settings -------------------------------------------------------

# This settings are used when run_model is called without arguments. The 
# settings used are in the sh_scripts folder

CHAINS <- 4
ITER <- 8000
CORES <- 4
# pararelization through future takes longer to start for some
# reason. i'm using the argument CORES instead
FUTURE <- FALSE

# # Debugging ---------------------------------------------------------------
# 
# experiment <- 'exp2'
# condition <- 'payoff'
# model_name <- 'simple'

# Read model
read_model <- function(experiment, condition, model_name) {
  
  # Read data
  data_file_name <- sprintf('all_data_filtered_%s.csv', experiment)
  data <- read_csv(paste0(path_to_data, data_file_name), show_col_types = FALSE)
  
  # Filter reproduction data and pick important columns
  data <- data %>% filter(trial_type == 'reproduction')
  data <- data %>% mutate(across(c(target_contrast, answer), ~abs(as.double(.x))))
  data <- data %>% select(participant, bias_source, bias_direction, contrast_level, target_contrast, answer) %>% rename(stim=target_contrast, response=answer, cond=bias_direction)
  data <- data %>% filter(bias_source == condition)
  
  
  # Model formula 
  model_formula <- get_model_formula(model_name = model_name)
  
  # Make fit file name
  cond_name <- str_sub(unique(data$bias_source), 1, 3)
  fit_file_name <- sprintf('%s_%s_%s.rds', experiment, cond_name, model_name)
  
  # Get model fit
  fit <- brm(
    model_formula,
    data = data,
    family = hurdle_gaussian,
    stanvars = stanvars,
    chains = 0,
    file = paste0(path_to_fit_models, fit_file_name)
  )
  
  return(fit)
}

# Run model
run_model <- function(experiment, condition, model_name, compile_only=FALSE, chains=CHAINS, iter=ITER, cores=CORES, future=FUTURE) {
  
  # Print current working directory for debugging
  cat('\nWorking directory: ', getwd())
  
  # Print message on console
  cat('\n\n#######################################\n',
      sprintf('Running "%s" model on "%s" condition (%s)\n', model_name, condition, experiment),
      sprintf('Settings:\n - chains=%s\n - iterations=%s\n - cores=%s\n - future=%s', chains, iter, cores, future),
      '\n#######################################\n\n\n')
  
  # Data file name
  data_file_name <- sprintf('all_data_filtered_%s.csv', experiment)
  # Read data
  data <- read_csv(paste0(path_to_data, data_file_name), show_col_types = FALSE)
  
  # Filter reproduction data and pick important columns
  data <- data %>% filter(trial_type == 'reproduction')
  data <- data %>% mutate(across(c(target_contrast, answer), ~abs(as.double(.x))))
  data <- data %>% select(participant, bias_source, bias_direction, contrast_level, target_contrast, answer) %>% rename(stim=target_contrast, response=answer, cond=bias_direction)
  
  # Filter condition 
  data <- data %>% filter(bias_source == condition)
  
  # Run model
  cond_name <- str_sub(unique(data$bias_source), 1, 3)
  file_name <- paste0(path_to_fit_models, experiment, '_', cond_name, '_', model_name, '.rds')
  
  # Model formula 
  model_formula <- get_model_formula(model_name = model_name)
  
  # If compile only set number of chains to zero
  if (compile_only) {
    CHAINS <- 0
  }
  
  # Run model
  f <- brm(
    model_formula,
    data = data,
    family = hurdle_gaussian,
    stanvars = stanvars,
    chains = chains,
    iter = iter,
    cores = cores,
    future = future,
    save_pars = save_pars(all = TRUE),
    file = file_name
  )
}

# This formula simply returns a brms formula object using a string
get_model_formula <- function(model_name) {
  
  if (model_name == 'simple') {
    return(simple)
  } else if (model_name == 'contrast') {
    return(contrast)
  } else if (model_name == 'hurdle') {
    return(hurdle)
  } else if (model_name == 'full') {
    return(full)
  } 
  
}

get_rhat_csv <- function(experiment, condition, model_name) {
  # make csv file name and check if file already exists
  file_name <- sprintf('rhat_%s_%s_%s.csv', experiment, condition, model_name)
  
  # skip rest of function if file exists
  if (!file_name %in% dir(path_to_rhat)) {
    cat(paste0('creating file with r-hat values for ', sprintf('%s_%s_%s', experiment, condition, model_name), '\n'))
    
    # read model fit
    fit <- read_model(experiment, condition, model_name)
    # get r-hat values
    rhat_values <- rhat(fit)
    # make into tibble 
    rhat_values <- tibble(parameter = names(rhat_values), rhat = rhat_values)
    # add exp, cond and model info
    rhat_values <- rhat_values %>% 
      mutate(experiment = experiment,
             condition = condition,
             model_name = model_name)
    
    # Save csv with r-hat values
    write_csv(rhat_values, paste0(path_to_rhat, file_name))
    
  } else {
    cat(paste0(sprintf('%s_%s_%s', experiment, condition, model_name), 'r-hat file already exists\n'))
  }
}

# to run regression between proportion of absent responses in the detection
# task and the probability of not passing the hurdle in the reproduction task
get_regression <- function(df) {
  
  df <- df %>% mutate(participant = factor(participant))
  fit <- brm(
    formula = hurdle_prob ~ answer  + (1 | participant),
    iter = 10000,
    chains = 4,
    data = df,
    cores = 4,
    save_pars = save_pars(all = TRUE),
    file = sprintf('hurdle_model/fits_regression/reg_%s_%s.rds', e, unique(df$bias_source))
  )
  
  return(fit)
}

get_model_colors <- function() {
  c(
    'baseline' = lighten('#E0D44D', .05),
    'contrast' = lighten('#6933b0', .15),
    'hurdle' = darken('#fc8d45', .01),
    'full' = 'black',
    'empirical' = 'gray80'
  )
}

get_model_shapes <- function() {
  c('full'=0,'contrast'=1, 'hurdle'=2, empirical=16)
}

# Model formulas ----------------------------------------------------------

# base line model, no effect allowed in contrast or hurdle
simple <- bf(response ~ stim + (1 | participant),
             hu       ~ stim + (1 | participant))

# effect on perceived contrast, not in hurdle
contrast <- bf(response ~ stim * cond + (cond | participant),
               hu       ~ stim + (1 | participant))

# effect in hurdle, not in perceived contrast 
hurdle <- bf(response ~ stim + (1 | participant),
             hu       ~ stim * cond + (cond | participant))

# effect both in perceived contrast and in hurdle
full <- bf(response ~ stim * cond + (cond | participant),
           hu       ~ stim * cond + (cond | participant))


# Hurdle-Gaussian ---------------------------------------------------------

# Custom Gaussian hurdle' family
hurdle_gaussian <- 
  # Create a custom family that is logit if y = 0, normal/Gaussian if not
  custom_family("hurdle_gaussian", 
                dpars = c("mu", "sigma", "hu"),
                links = c("identity", "log", "logit"),
                lb = c(NA, 0, NA),
                type = "real")

# Stan code
stan_funs <- "
  real hurdle_gaussian_lpdf(real y, real mu, real sigma, real hu) { 
    if (y == 0) { 
      return bernoulli_lpmf(1 | hu); 
    } else { 
      return bernoulli_lpmf(0 | hu) +  
             normal_lpdf(y | mu, sigma); 
    } 
  }
"

# Prepare Stan code for use in brm()
stanvars <- stanvar(scode = stan_funs, block = "functions")

# predict new responses using fit parameters
posterior_predict_hurdle_gaussian <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  theta <- brms::get_dpar(prep, "hu", i = i)
  
  hu <- runif(prep$ndraws, 0, 1)
  ifelse(hu < theta, 0, rnorm(prep$ndraws, mu,sigma))
}

