# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, stringr, ggplot2, tibble, patchwork, BayesFactor, effectsize, scales)

# Functions ---------------------------------------------------------------

source('main_analysis/main_functions.R')

make_plot <- function(df) {
  
  n_bins <- 480
  bin_width <- .003; 
  
  df %>%
    ggplot(aes(x=answer, 
               fill = interaction(bias_source, bias_direction, sep='_'),
               color = interaction(bias_source, bias_direction, sep='_'))) +
    facet_wrap(. ~ bias_source + contrast_level, scales='free_y', 
               labeller = labeller(bias_source = labels_bias_source, 
                                   contrast_level = c('absent' = 'Absent', 'low'='Low Contrast', 'high'='High Contrast'),
                                   .multi_line = FALSE)) +
    geom_histogram(data=df %>% filter(bias_direction=='present'), bins=n_bins, alpha=.7, position = 'identity', binwidth = bin_width) +
    geom_histogram(data=df %>% filter(bias_direction=='control'), bins=n_bins, alpha=.4, position = 'identity', binwidth = bin_width) +
    scale_x_continuous(breaks=breaks_pretty(n=3)) + 
    scale_y_continuous(breaks=breaks_pretty(n=3)) +
    ylab('Count') + xlab('Reproduced contrast') + 
    ggtitle('Overall distribution of reproduction responses') + 
    scale_fill_manual(values=get_condition_colors(), breaks=c('cue_control', 'cue_present'), labels=c('cue_present'='Bias', 'cue_control'='No bias')) +
    scale_color_manual(values=get_condition_colors(), breaks=c('cue_control', 'cue_present'), labels=c('cue_present'='Bias', 'cue_control'='No bias'))  +
    # apply minimal theme
    theme_bw() +
    guides(shape='none',
           linetype='none',
           color=guide_legend('Bias direction', override.aes = list(shape=c(NA,NA), fill=c('gray', 'gray10'), color=c('gray', 'gray10'))),
           fill=guide_legend('Bias direction'), override.aes = list(shape=c(NA,NA), fill=c('gray', 'gray10'), color=c('gray', 'gray10'))) +
    theme(legend.position = 'bottom')
  
}


# Experiment 1 ------------------------------------------------------------

# data
dat <- read_csv('data/processed/all_data_filtered_exp1.csv')

# formatting
# Create condition column, convert other columns to factor to set 
# plot order of condition
dat <- format_columns(dat, sep='\n')

# reproduction data
rep_data <-
  dat %>% 
  filter(trial_type == 'reproduction') %>% 
  mutate(answer = abs(as.double(answer))) 

# non-zero reproductions 
# Convert to perceived space and normalize by dividing each
# value by the SD of the whole population
rep_data <-
  rep_data %>% 
  mutate(value = answer) %>%
  group_by(participant, bias_source)

# plot
make_plot(rep_data) +
  scale_x_continuous(breaks=breaks_pretty(n=3), limits = c(-.01, .11)) 

ggsave('plots/exp1_rep_dist_all.png', width = 8, height = 5, scale = 1, dpi=600)

# Experiment 2 ------------------------------------------------------------

# data
dat <- read_csv('data/processed/all_data_filtered_exp2.csv')

# formatting
# Create condition column, convert other columns to factor to set 
# plot order of condition
dat <- format_columns(dat, sep='\n')

# reproduction data
rep_data <-
  dat %>% 
  filter(trial_type == 'reproduction') %>% 
  mutate(answer = abs(as.double(answer))) 

# non-zero reproductions 
# Convert to perceived space and normalize by dividing each
# value by the SD of the whole population
rep_data <-
  rep_data %>% 
  mutate(value = answer) %>%
  group_by(participant, bias_source)

# plot
make_plot(rep_data) +
  scale_x_continuous(breaks=breaks_pretty(n=3), limits = c(-.01, .08)) 

ggsave('plots/exp2_rep_dist_all.png', width = 8, height = 5, scale = 1, dpi=600)
