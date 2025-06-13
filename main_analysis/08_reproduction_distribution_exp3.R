# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, stringr, ggplot2, tibble, patchwork, BayesFactor, effectsize, scales)

# Functions ---------------------------------------------------------------

source('main_analysis/main_functions.R')

# Data --------------------------------------------------------------------

dat <- read_csv('data/processed/all_data_filtered_exp3.csv', col_types = cols())

# Formatting --------------------------------------------------------------

# Create condition column, convert other columns to factor to set 
# plot order of condition
dat <- format_columns(dat, sep='\n')

# Reproduction data -------------------------------------------------------

rep_data <-
  dat %>% 
  filter(trial_type == 'reproduction') %>% 
  mutate(answer = abs(as.double(answer))) 

## Absent reproductions --------------------------

# Note that there are only 40 absent trials in the base rate biased
# condition. Because of this there are only 40 possible prop present
# possible values. That's why those points look more discrete than 
# the rest

rep_data_zero <- 
  rep_data %>% 
  mutate(answer = ceiling(answer)) %>% 
  group_by(bias_source, bias_direction, participant, contrast_level) %>% 
  # mean response minus one to make the proprotion of absent responses
  reframe(prop_zero = 1-mean(answer))

# Plot
p_proportion_zero <-
  plot_proportion_zero_reproduction(rep_data_zero) +
  ylab('Proportion of zero\nconctrast reproductions')

# Test difference
rep_proportion_zero_bf <- 
  rep_data_zero %>% 
  rename(value = prop_zero) %>% 
  get_bf_d(., c('present', 'control'))

# horizontal justification for plotting
rep_proportion_zero_bf <- 
  rep_proportion_zero_bf %>% 
  mutate(hjust=case_when(contrast_level == 'absent' ~ 'center',
                         contrast_level == 'low' ~ 'center',
                         contrast_level == 'high' ~ 'center'))

# Coordinates for plotting BF and Cohen's d
x_prop_zero <- c(c(.9, 2, 3.1),
                 c(.9, 2, 3.1))
y_prop_zero <- c(c(-0.05, -0.05, -0.05),
                 c(-0.05, -0.05, -0.05))

# Gap between BF an Cohen's d
gap_prop_zero <- -.05

# add bf and cohen's d to plot
p_proportion_zero <-
  p_proportion_zero + 
  geom_text(data=rep_proportion_zero_bf, 
            aes(x=x_prop_zero, y=y_prop_zero, label=bf_lab, hjust=hjust),
            size=2.4, color='black', parse = TRUE, inherit.aes = FALSE) +
  geom_text(data=rep_proportion_zero_bf, 
            aes(x=x_prop_zero, y=y_prop_zero+gap_prop_zero, label=d_lab, hjust=hjust),
            size=2.4, color='black', parse = TRUE, inherit.aes = FALSE) 

# apply minimal theme
p_proportion_zero <-
  p_proportion_zero +
  scale_color_manual(breaks=c('cue_control', 'cue_present'), 
                     labels=c('cue_control'='No bias', 'cue_present'='Bias'),
                     values=get_condition_colors()) +
  guides(
    linetype='none',
    fill='none',
    color=guide_legend('Bias direction', override.aes = list(alpha=c(1,1), fill=c('gray', 'gray10'), color=c('gray', 'gray10')))
  ) +
  theme_bw() +
  theme(
    legend.direction = 'horizontal',
    legend.position = 'top',
    legend.justification = 'right',
    legend.margin = margin(0,0,-5,0, unit="points"),
    legend.key.size = unit(10, 'points'),
    plot.title = element_blank(),
    strip.background = element_rect(fill='white', color='black', linewidth = 0.8),
    axis.title.y = element_text(size=15)
  ) 

# Non-zero reproductions --------------------------------------------------

# Remove zero contrast reproductions
rep_data <-
  rep_data %>% 
  filter(answer!=0) %>%
  mutate(value = answer)

# Get average per subject
rep_data_sub_summary <- 
  rep_data %>% 
  filter(value!=0) %>% 
  select(bias_source, bias_direction, participant, contrast_level, value) %>% 
  group_by(bias_source, bias_direction, participant, contrast_level) %>% 
  reframe(value = median(value))

# Some participants have no trials after filtering reproductions that are zero
remove_subjects <- 
  rep_data_sub_summary %>% 
  pivot_wider(names_from = bias_direction, values_from = value) %>% 
  filter(if_any(c(present, control), is.na)) %>% 
  select(bias_source, participant, contrast_level) %>% 
  distinct()

# Count them
remove_subjects %>% 
  group_by(bias_source, contrast_level) %>% 
  reframe(count = n())

# Run t-tests and effect size
bf_all_data <-
  rep_data_sub_summary %>% 
  anti_join(remove_subjects, by = join_by(bias_source, participant, contrast_level)) %>%  
  get_bf_d(., c('control', 'present'))

# Get summary for plotting. When using stat_summary values are removed
# when adjusting the limits of the plot
rep_all_data_summary <-
  rep_data_sub_summary %>% 
  group_by(bias_source, bias_direction, contrast_level) %>% 
  reframe(value = mean_se(value)) %>% 
  unnest(value) %>% 
  mutate(nudge_y = case_match(bias_direction, 'control'~0, 'present'~.002))

# -------------------------------------------------------------------------

# Plot distributions
p_rep_non_zero <-
  plot_reproduction_distribution(rep_data) +
  # add error bars
  geom_errorbarh(data=rep_all_data_summary %>% filter(bias_direction=='present'), 
               aes(y=-50, x=y, xmin=ymin, xmax=ymax),
               height=50, show.legend = FALSE, inherit.aes = TRUE) +
  geom_errorbarh(data=rep_all_data_summary %>% filter(bias_direction=='control'), 
                 aes(y=-35, x=y, xmin=ymin, xmax=ymax),
                 height=50, show.legend = FALSE, inherit.aes = TRUE) +
  # add mean
  geom_point(data=rep_all_data_summary %>% filter(bias_direction=='present'),
             aes(x=y, y=-50), shape=16) +
  geom_point(data=rep_all_data_summary %>% filter(bias_direction=='control'),
             aes(x=y, y=-35), shape=16) +
  scale_x_continuous(breaks = c(0.0, 0.03, 0.06, 0.09), limits = c(0, .085)) +
  ylab('Non-zero reproductions count') 
  

# coordinates to plot BF and Cohen's d value
x_dist=rep(c(.05,.05,.055), 2)
y_dist=rep(c(480, 480, 480), 2)
# gap between BF and Cohen's d
gap_dist <- -80

# plot bf and cohen's d
p_rep_non_zero <-
  p_rep_non_zero +
  geom_text(data=bf_all_data, 
            aes(x=x_dist, y=y_dist, label=bf_lab),
            size=3, color='black', parse = TRUE, inherit.aes = FALSE) +
  geom_text(data=bf_all_data, 
            aes(x=x_dist, y=y_dist+gap_dist, label=d_lab),
            size=3, color='black', parse = TRUE, inherit.aes = FALSE) +
  theme(legend.position = 'none',
        strip.text = element_text(size=10)) 


# apply minimal theme
p_rep_non_zero <-
  p_rep_non_zero +
  theme_bw() +
  theme(
    legend.direction = 'horizontal',
    legend.position = 'top',
    legend.justification = 'right',
    legend.margin = margin(0,0,-5,0, unit="points"),
    legend.key.size = unit(10, 'points'),
    # plot.title = element_text(margin = margin(0,0,-5,0, unit="points")),
    plot.title = element_blank(),
    strip.background = element_rect(fill='white', color='black', linewidth = 0.8),
    axis.title.y = element_text(size=15)
  ) +
  guides(shape='none',
         linetype='none',
         color=guide_legend('Bias direction', override.aes = list(shape=c(NA,NA), fill=c('gray', 'gray10'), color=c('gray', 'gray10'))),
         fill=guide_legend('Bias direction'), override.aes = list(shape=c(NA,NA), fill=c('gray', 'gray10'), color=c('gray', 'gray10')))

# Combine plots -----------------------------------------------------------

(p_proportion_zero | p_rep_non_zero)  
  # plot_layout(guides = 'collect') & 
  # theme(legend.position = 'bottom',
  #       legend.box.spacing = unit(-3, "mm")) &
  # guides(color='none',
  #        shape='none',
  #        fill=guide_legend('Bias direction', override.aes = list(shape=c(NA,NA), fill=c('gray', 'gray10'), color=c('gray', 'gray10'))))

ggsave('plots/fig_exp3Rep.png', width = 11.5, height = 5, scale=.8, dpi=600, device = png, bg='white')

