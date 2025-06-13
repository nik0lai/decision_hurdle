# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2)  

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
      
      cat('plotting traces of ', sprintf('%s_%s_%s', e, c, m))
      
      # plot file name
      plot_file_name <- sprintf('%s_%s_%s.png', e, c, m)
      # get model fit
      fit <- read_model(e, c, m)
      # make plot
      png(paste0('hurdle_model/traceplots/', plot_file_name), width = 800, height = 1000, units = 'px')
      plot(fit, N = 15)
      dev.off()
      
    }
  }
}
