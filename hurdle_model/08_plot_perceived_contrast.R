# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, tidybayes, ggplot2, patchwork)  

# model_colors <- c('empirical'='gray70')

get_samples <- function(fit, condition) {
  
  # settings
  # minimum samples per cell
  min_samples <- 100
  # number of samples to draw
  n_samples <- 500
  
  cat(sprintf('%s--- sampling data', condition))
  
  # predict reproduction responses
  sampled_data <- unique_data %>% 
    filter(bias_source == condition) %>% 
    add_predicted_draws(fit, value = 'pred', ndraws = n_samples) %>%
    filter(pred != 0)
  
  while(TRUE) {
    print('sampling data')
    
    # check which level needs more samples 
    sample_more <- 
      sampled_data %>% 
      group_by(participant, bias_source, cond, contrast_level, stim) %>% 
      count %>% 
      filter(n < min_samples) %>% 
      select(-n)
    
    if (!nrow(sample_more)) {
      break
    }
    
    # sample more data
    new_sample <- sample_more %>% 
      add_predicted_draws(fit, value='pred', ndraws = n_samples) %>%
      filter(pred != 0)
    
    # combine old and new sample
    sampled_data <- bind_rows(sampled_data, new_sample)
    
  }
  
  # Get a 1000 draws from every combination 
  sampled_data <- sampled_data %>% 
    group_by(participant, bias_source, cond, contrast_level, stim) %>% 
    sample_n(min_samples)
  
  return(sampled_data)
  
}

# functions
source('hurdle_model/hurdle_functions.R')
source('main_analysis/main_functions.R')

# experiment, condition and model info to iterate through
experiments <- c('exp1', 'exp2')
conditions <- c('baserate', 'cue', 'payoff')

# list to save the plots
plots <- list()

for (e in experiments) {
  
  # read model fit
  fit_br <- read_model(e, 'baserate', 'full')
  fit_cu <- read_model(e, 'cue', 'full')
  fit_pa <- read_model(e, 'payoff', 'full')

  # read data
  data <- read_csv(paste0(path_to_data, sprintf('all_data_filtered_%s.csv', e)), col_types = cols())
  # Filter reproduction data and pick important columns
  data <- data %>% filter(trial_type == 'reproduction')
  data <- data %>% mutate(across(c(target_contrast, answer), ~abs(as.double(.x))))
  data <- data %>% select(participant, bias_source, bias_direction, contrast_level, target_contrast, answer) %>% rename(stim=target_contrast, response=answer, cond=bias_direction)
  data <- data %>% mutate(contrast_level = factor(contrast_level, levels=c('absent','low','high'))) 
  # unique trials observed data for predicting new data
  unique_data <- data %>% select(participant, bias_source, cond, contrast_level, stim) %>% distinct()
  
  # get samples for each condition
  draws_br <- get_samples(fit_br, 'baserate')
  draws_cu <- get_samples(fit_cu, 'cue')
  draws_pa <- get_samples(fit_pa, 'payoff')
  
  # combine draws
  draws <-
    bind_rows(
      draws_br, draws_pa, draws_cu
    ) %>%
    group_by(participant, bias_source, cond, contrast_level) %>% 
    reframe(value = mean(pred)) 
  
  # get condition summary
  draws_summ <-
    draws %>% 
    group_by(participant, bias_source, cond, contrast_level) %>% 
    reframe(response=mean(value)) %>% 
    group_by(bias_source, cond, contrast_level) %>% 
    reframe(response=mean_se(response)) %>% 
    unnest(response)
  
  # Get observed data summaries
  data <- 
    # Get non zero reproduction
    data %>% 
    mutate(contrast_level = factor(contrast_level, levels=c('absent','low','high'))) %>% 
    filter(response != 0) %>% 
    group_by(participant, bias_source, cond, contrast_level) %>% 
    reframe(response = mean(response, na.rm = TRUE))

  # Plot delta --------------------------------------------------------------
  
  draws_summary <-
    draws %>% 
    pivot_wider(names_from = cond, values_from = value) %>% 
    group_by(bias_source, contrast_level) %>% 
    reframe(delta=mean_cl_normal(present-control)) %>% 
    unnest(delta)
  
  draws_summary <- 
    draws_summary %>% 
    mutate(bias_source = factor(bias_source, levels = c('cue', 'baserate', 'payoff', 'spacer')))
  
  data_summary <- 
    data %>% 
    pivot_wider(names_from = cond, values_from = response) %>% 
    group_by(bias_source, contrast_level) %>% 
    reframe(delta=mean_cl_normal(present-control)) %>% 
    unnest(delta)
  
  data_summary <- 
    data_summary %>% 
    mutate(bias_source = factor(bias_source, levels = c('cue', 'baserate', 'payoff'))) %>% 
    mutate(data_type='empirical') 
  
  shapes <- c(19, 19, 19, 19, 8)
  
  p <-
    data_summary %>% 
    ggplot(aes(x=contrast_level, y=y, color=data_type)) +
    facet_wrap(. ~ bias_source, labeller = labeller(bias_source = labels_bias_source)) + 
    geom_hline(yintercept = 0, linetype=2) +
    geom_errorbar(data=data_summary, aes(y=y, ymin = ymin, ymax = ymax), width=0, size=6, alpha=.5) +
    geom_point(data=data_summary, aes(y=y), shape=8, size=2.5) +
    geom_errorbar(data=draws_summary, aes(y=y, ymin = ymin, ymax = ymax, color=bias_source), width=.3, size=.8, position = position_nudge(x = 0), alpha=1) +
    geom_point(data=draws_summary, aes(y=y, color=bias_source), size=1.8, position = position_nudge(x = 0), alpha=1) +
    ylab('Effect on\nreproduced contrast') +
    scale_x_discrete(labels = str_to_title) +
    scale_y_continuous(labels = scales::percent, breaks = scales::breaks_pretty(n=8)) +
            scale_color_manual(
      breaks = c('cue', 'baserate', 'payoff', 'spacer', 'empirical'),
      values = c(get_condition_colors(), get_model_colors(), 'spacer'='black'), 
      labels = c('cue'='Predicted', 'baserate'='', 'payoff'='', 'spacer'='', 'empirical'='Observed'), 
      drop = FALSE) +
    scale_fill_manual(values = c(get_condition_colors(), get_model_colors())) +
    guides(color=guide_legend('Data', override.aes = list(shape=shapes), label.position='left')
           # fill=guide_legend('Data')
           ) +
    xlab('Contrast level') +
    theme_bw() +
    theme(
      legend.position = 'bottom',
      # legend.margin = margin(-10,0,0,0, unit="points"),
      # legend.position.inside =  c(.9, .65),
      legend.direction = 'horizontal',
      legend.key.spacing.x = unit(-5, 'points'),
      # legend.key.spacing.y = unit(0, 'mm'),
      legend.title = element_text(size=12),
      # legend.box.background = element_rect(fill='gray98', color='gray75'),
      strip.background = element_rect(fill='white', color='black'),
      # axis.text.x = element_text(size=11, angle=45, hjust=1), 
      strip.text = element_text(size=12),
      axis.text.x = element_text(size=10), 
      axis.text.y = element_text(size=12), 
      axis.title.y = element_text(size=17),
      axis.title.x = element_text(size=15)) +
    ggtitle(list('exp1'='Experiment 1', 'exp2'='Experiment 2')[e])
  
  # save plot
  plots[e] <- list(p)
  
}

# combine plots adjust axis and limits
(plots[['exp1']] | (plots[['exp2']]+theme(axis.title.y = element_blank()))) &
  # (plots[['exp1']] | plots[['exp2']]) &
  theme(legend.position = 'top', legend.justification = 'right',
        legend.margin = margin(0,0,-5,0, unit="points"),
        plot.title = element_text(margin = margin(0,0,-15,0, unit="points"))) &
  # theme(legend.position.inside = c(.5, .5)) 
  # plot_layout(axis_titles = 'collect') &
  scale_y_continuous(labels = scales::percent, 
                     limits = c(-.0043, 0.013), 
                     breaks=scales::breaks_pretty(n=3))
# save
ggsave('plots/pred_nonzero_rep.png', scale = .8, width = 16.8, height = 4.5)



