# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, stringr, colorspace, tibble, ggplot2, ggh4x, patchwork)

# Functions ---------------------------------------------------------------

source('main_analysis/main_functions.R')

# Data --------------------------------------------------------------------

dat <- read_csv('data/processed/all_data_filtered_exp1.csv', col_types = cols())

# Formatting --------------------------------------------------------------

# Create condition column, convert other columns to factor to set 
# plot order of condition
dat <- format_columns(dat, sep='\n')

# Proportion present answers ----------------------------------------------

# Get proportion of present responses
proportion_present_data <- 
  dat %>% 
  filter(trial_type == 'detection') %>% 
  group_by(participant, condition_code, stim_code, bias_source, bias_direction, contrast_level) %>% 
  mutate(answer = recode(answer, 'present'=1, 'absent'=0)) %>% 
  reframe(proportion_present = mean(answer))

# Plot
p_dec_prop_pres <- 
  plot_proportion_present(proportion_present_data)

# Test for difference between conditions
decision_bf <-
  proportion_present_data %>%
  select(-c(condition_code, stim_code)) %>% 
  rename(value = proportion_present) %>% 
  get_bf_d(data=., conditions=c('control', 'present'))  
  
# Add BF and effect sizes to plot
p_dec_prop_pres <- 
  p_dec_prop_pres %>% 
  plot_bf_d(., decision_bf, 
            # Coordinates to place BF and Cohen's d values
            x=rep(c(1.4, 3.8, 5.8), 3), 
            y=rep(c(.9, 0.22, 0.22), 3), 
            # Gat between BF and Cohen's d value
            gap=-.15)

# apply minimal theme
p_dec_prop_pres <- 
  p_dec_prop_pres +
  theme_bw() +
  theme(
    strip.background = element_rect(fill='white', color='black', linewidth = 0.8)
  )

# Reproduction ------------------------------------------------------------

# Get data
reproduction_data <-
  dat %>%
  filter(trial_type == 'reproduction') %>% 
  group_by(participant, condition_code, stim_code, bias_source, bias_direction, contrast_level) %>% 
  mutate(answer = abs(as.double(answer))) %>% 
  reframe(reproduction = mean(answer))

# Plot
p_reproduction <- plot_contrast_reproduction(reproduction_data)

# Test for difference between conditions
reproduction_bf <- 
  reproduction_data %>%
  select(-c(condition_code, stim_code)) %>% 
  rename(value = reproduction) %>% 
  get_bf_d(., c('control', 'present'))

# Add BF and effect size
p_reproduction <- 
  p_reproduction %>% 
  plot_bf_d(., reproduction_bf, 
            # Coordinates to place BF and Cohen's d values
            x=rep(c(1.5, 3.5, 5.5), 3), 
            y=rep(c(.1, .1, .1), 3), 
            # Gap between BF and Cohen's d 
            gap=-.015)

p_reproduction <- 
  p_reproduction +
  scale_y_continuous(
    labels = function(x) sprintf("%.2f", x),
    breaks = seq(min(reproduction_data$reproduction), 
                 max(reproduction_data$reproduction), by = 0.025))

# apply minimal theme
p_reproduction <-
  p_reproduction +
  theme_bw() +
  theme(
    strip.background = element_rect(fill='white', color='black', linewidth = 0.8)
  )

# Combine plots -------------------------------------
p_dec_prop_pres + p_reproduction

ggsave('plots/fig_exp1Main.png', width = 7, height = 4.5, scale=1, dpi=1200, device = png)
