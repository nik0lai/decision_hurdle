# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, readr, dplyr, tidyr, ggplot2, tibble, stringr, purrr, patchwork, brms, car, marginaleffects, ggh4x, scales, gridExtra)

# Functions
source('main_analysis/main_functions.R')
source('hurdle_model/hurdle_functions.R')

# Loop through experiments ------------------------------------------------

experiments <- c('exp1', 'exp2')

for (e in experiments) {
  
  # predicted data ----------------------------------------------------------
  
  dat_ppc <- 
    dir('hurdle_model/ppc/', pattern = e, full.names = TRUE) %>% 
    map(read_csv, col_types = cols(participant = col_character())) %>% 
    bind_rows()
  
  # get full model data and create some factors for ordering the plot
  dat_ppc <- 
    dat_ppc %>% 
    filter(model_name == 'full') %>% 
    mutate(contrast_level = factor(contrast_level, levels = c('absent', 'low', 'high'))) %>% 
    mutate(bias_source = factor(bias_source, levels = c('cue', 'baserate', 'payoff')))
  
  # get summary values of hurdle, invert hurdle to make it similar to detection responses
  dat_ppc <- 
    dat_ppc %>% 
    select(participant, bias_source, cond, contrast_level, hurdle_prob) %>% 
    group_by(participant, bias_source, cond, contrast_level) %>% 
    reframe(hurdle_prob = mean(hurdle_prob))
  
  # observed data -----------------------------------------------------------
  
  dat_obs <- read_csv(sprintf('data/processed/all_data_filtered_%s.csv', e), col_types = cols())
  
  # get detection data 
  dat_obs <- 
    dat_obs %>% 
    filter(trial_type == 'detection') %>% 
    select(bias_source, bias_direction, participant, contrast_level, answer) %>% 
    rename(cond=bias_direction) %>% 
    mutate(answer = as.integer(answer == 'present')) %>% 
    mutate(bias_source = factor(bias_source),
           contrast_level = factor(contrast_level)) 
  
  # invert proportion so it indicates rate of absent responses
  dat_obs <- dat_obs %>% mutate(answer = 1-answer)
  
  # summarize data
  dat_obs <- 
    dat_obs %>% 
    group_by(participant, bias_source, cond, contrast_level) %>% 
    reframe(answer = mean(answer)) 
  
  # Join data ---------------------------------------------------------------
  
  # join hurdle and detection data
  all_dat <- dat_ppc %>% full_join(dat_obs, by = join_by(participant, bias_source, cond, contrast_level))
  
  # get summary data for plotting
  
  all_dat_summ <-
    all_dat %>% 
    mutate(hurdle_prob = hurdle_prob) %>% 
    select(participant, bias_source, cond, contrast_level, hurdle_prob, answer) %>% 
    pivot_longer(names_to = 'task', values_to = 'value', cols=c(hurdle_prob , answer)) %>% 
    group_by(bias_source, cond, contrast_level, task) %>%
    reframe(mean = mean(value)) %>% 
    pivot_wider(names_from = task, values_from = mean)
  
  # plot detection and hurdle -----------------------------------------------
  
  # position dodge to off set the points of each condition (control, bias)
  pd <- position_dodge2(width = 1)
  
  # make plot
  p_det_rep <-
    all_dat_summ %>% 
    ggplot(aes(x=contrast_level, group=cond, color=interaction(bias_source, cond, sep='_'))) +
    facet_wrap(. ~ bias_source, ncol = 1, scales = 'free', 
               labeller = labeller(bias_source=labels_bias_source),
               strip.position = 'right') +
    geom_line(aes(y=hurdle_prob, group=contrast_level, linetype='Reproduction', color=bias_source), 
              position = pd, linewidth=.3) +
    geom_line(aes(y=answer, group=contrast_level, linetype='Detection', color=bias_source), position=pd, linewidth=.3) +
    geom_point(aes(y=answer, shape='Detection'), position = pd, size=2) +
    geom_point(aes(y=hurdle_prob, shape='Reproduction'), position = pd, size=2) +
    geom_point(aes(y=.5, x='low', shape='det'), alpha=0, position = pd, size=2) +
      geom_point(aes(y=.5, x='low', shape='rep'), alpha=0, position = pd, size=2) +
    scale_x_discrete(labels = function(x) str_to_title(x)) +
    # manually create a legend for bias/control condition
    scale_color_manual(breaks=c('baserate_control', 'cue_control', 'baserate_present', 'cue_present'), 
                       labels=c('baserate_control'='', 'cue_control'='', 'baserate_present'='No bias', 'cue_present'='Bias'), 
                       values = get_condition_colors()) +
    scale_shape_manual(breaks = c('Detection', 'Reproduction', 'det', 'rep'), 
                       labels = c('Detection'='', 'Reproduction'='', 'det'='Detection', 'rep'='Reproduction'), 
                       values = c(19,17,3, 4), drop = FALSE) +
    xlab('Contrast level') +
    ylab('Proportion of absent answers (detection task)\nProbability of zero reproductions (reproduction task)') +
    guides(
      color=guide_legend('Condition', ncol=2, 
                         override.aes = list(fill=c('gray', 'gray10', 'gray', 'gray10'), 
                                             color=c('gray', 'gray10', 'gray', 'gray10'),
                                             shape=c(19, 19, 17, 17))),
      # linetype=guide_legend('Task', ncol=2),
      linetype='none',
      shape=guide_legend('Task', ncol=2,
                         override.aes = list(fill=c('gray', 'gray', 'gray10', 'gray10'), 
                                             color=c('gray', 'gray', 'gray10', 'gray10'), 
                                             shape=c(19, 17, 19, 17),
                                             alpha=1),
                         alpha=1)
    ) +
    ylim(0,1) +
    theme_bw() +
    theme(strip.background = element_rect(fill='white'),
          legend.key.spacing.x = unit(-2, 'points'))
  
  # regression --------------------------------------------------------------
  
  # make df with logit transform data to run regression
  all_dat_diff_logit <-
    all_dat %>% 
    pivot_longer(names_to = 'measure', values_to = 'value', cols = c(hurdle_prob, answer)) %>%
    # invert values, this is only done so the plot goes left to right, down to up
    mutate(value = 1-value) %>% 
    # logit transform to run regression
    mutate(value = logit(value)) %>% 
    pivot_wider(names_from = cond, values_from = value) %>% 
    mutate(diff = present - control) %>% 
    select(-c(control, present)) %>% 
    pivot_wider(names_from = measure, values_from = diff)
  
  # run regression between prop. absent and prob not passing the hurdle
  reg_fits <-
    all_dat_diff_logit %>% 
    group_by(bias_source) %>% 
    nest() %>% 
    mutate(data=map(data, ~.x %>% mutate(bias_source=bias_source))) %>% 
    mutate(reg = map(data, get_regression))
  
  
  # Check regression fit (r-hat values) -------------------------------------
  
  get_rhat_vals <- function(fit, threshold=1.01) {
    rhat_values <- fit %>% rhat() 
    rhat_values <- rhat_values %>% unname() < threshold
    passed <- all(rhat_values)
    cat(sprintf('all r-hat values are equal or lower than %s: %s', threshold, passed))
  }
  
  # Print r-hat values
  # get_rhat_vals(fit=reg_fits$reg[[1]])
  # get_rhat_vals(reg_fits$reg[[2]])
  # get_rhat_vals(reg_fits$reg[[3]])
  
  # Get bf
  reg_fits <- 
    reg_fits %>% 
    mutate(bf = unlist(map(reg, ~hypothesis(.x, 'answer > 0')$hypothesis$Evid.Ratio))) %>% 
    mutate(bias_source = factor(bias_source, levels=c('cue', 'baserate', 'payoff')))
  
  # -------------------------------------------------------------------------
  
  # Get observed data
  emp_data <- 
    reg_fits %>% 
    ungroup() %>% 
    select(-bias_source) %>% 
    unnest(data)
  
  # Predict regression line for plotting
  pred_data <-
    reg_fits  %>% 
    mutate(pred=map(reg, ~predictions(.x, by='answer', re_formula=NA))) %>% 
    select(-c(data, reg, bf)) %>% 
    unnest(pred)
  
  # get r2 values 
  r2_values <-
    reg_fits %>% 
    mutate(r2 = round(map(reg, ~bayes_R2(.x))[[1]][[1]], 2)) %>% 
    select(-c(data, reg, bf)) 
  
  # Plot regression ---------------------------------------------------------
  
  # coordinates to plot r2 values. different for each experiment
  if (e == 'exp2') {
    r2_values <- r2_values %>% 
      mutate(x = case_match(bias_source, 
                            'baserate' ~ -.5,
                            'cue' ~ -.5,
                            'payoff' ~ -.1),
             y = case_match(bias_source, 
                            'baserate' ~ 3.8,
                            'cue' ~ 1.6,
                            'payoff' ~ 2.2)
      )
  } else if (e == 'exp1') {
    r2_values <- r2_values %>% 
      mutate(x = case_match(bias_source, 
                            'baserate' ~ 1.15,
                            'cue' ~ -.2,
                            'payoff' ~ .8),
             y = case_match(bias_source, 
                            'baserate' ~ 4.3,
                            'cue' ~ 2.1,
                            'payoff' ~ 2)
      )
  }
  
  # plot
  p_reg <-
    emp_data  %>%
    ggplot(aes(x=answer, y=hurdle_prob, color=bias_source)) +
    facet_wrap(. ~ bias_source, ncol = 1, scales = 'free', 
               labeller = labeller(bias_source=labels_bias_source),
               strip.position = 'right') +
    geom_point() +
    scale_color_manual(values = get_condition_colors()) +
    theme_bw() +
    theme(strip.background = element_rect(fill='white'),
          strip.text = element_text(size=12),
          legend.position = 'none', 
          axis.title = element_text(size=14),
          axis.text = element_text(size=11))
  
  # add regression line
  p_reg <-
    p_reg +
    geom_ribbon(data = pred_data, color=NA,
                aes(ymin=conf.low, ymax=conf.high, y=estimate, x=answer), alpha=.3) +
    geom_line(data = pred_data,
              aes(y=estimate, x=answer)) +
    scale_y_continuous(breaks = breaks_pretty(n=3)) +
    xlab('Effect on detection task') +
    ylab('Predicted effect on zero-contrast reproductions') +
    geom_text(data=r2_values, 
              aes(x=x, y=y, label=paste0('r^2 == ', r2)), 
              parse = TRUE)
  
  # Combine plots -----------------------------------------------------------
  
  # adjust the legend of the raw data
  p_det_rep <- 
    p_det_rep +
    theme(
      strip.text = element_text(size=12),
      axis.text = element_text(size=10),
      
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=12),
      legend.position = 'top',
      legend.direction = 'vertical',
      legend.spacing.y = unit(-1, 'mm'),
      legend.title = element_text(size=10),
      legend.text = element_text(size=9),
      legend.key.size = unit(1, 'mm'),
      legend.key.spacing.y = unit(1, 'mm'),
      legend.spacing.x = unit(1,'mm'),
      legend.box.margin = margin(l = -10, unit = 'mm'),
    )
  
  # arrange plots in a grid
  p <- grid.arrange(p_det_rep, p_reg, 
                    widths = c(3,3.5),
                    nrow = 1)
  # save plot
  ggsave(plot = p, filename = sprintf('plots/hurdle_det_regression_%s.png', e),
         height = 8.7, width = 9, dpi=300, scale=.6, device=png)
  
}

