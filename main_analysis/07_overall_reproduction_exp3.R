# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, stringr, colorspace, tibble, ggplot2, ggh4x, patchwork)

# Functions ---------------------------------------------------------------

source('main_analysis/main_functions.R')

# Data --------------------------------------------------------------------

dat <- read_csv('data/processed/all_data_filtered_exp3.csv', col_types = cols())

# Formatting --------------------------------------------------------------

# Create condition column, convert other columns to factor to set 
# plot order of condition
dat <- format_columns(dat, sep='\n')


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
p_reproduction <- 
  p_reproduction + 
  facet_wrap(. ~ bias_source, labeller = labeller(bias_source = labels_bias_source))

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
            x=rep(c(1.5, 3.5, 5.5), 2), 
            y=rep(c(.046, .046, .046), 2), 
            # Gap between BF and Cohen's d 
            gap=-.008)

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
      plot.title = element_blank(),
      strip.background = element_rect(fill='white', color='black', linewidth = 0.8)
    )

# Combine plots -------------------------------------
p_reproduction

ggsave('plots/fig_exp3Main.png', width = 11.5, height = 5, scale=.5, dpi=1200, device = png)
