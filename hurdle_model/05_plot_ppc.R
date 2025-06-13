# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, readr, dplyr, tidyr, ggplot2, tibble, stringr, purrr, patchwork, colorspace)

source('main_analysis/main_functions.R')
source('hurdle_model/hurdle_functions.R')

# Models to plot
model_to_plot <- c('full', 'contrast', 'hurdle')

# Experiments -------------------------------------------------------------

experiments <- c('exp1', 'exp2')

for (e in experiments) {
  
  # data --------------------------------------------------------------------
  
  dat <- dir('hurdle_model/ppc/', pattern = e, full.names = TRUE) %>%
    map(read_csv, col_types=cols(participant = col_character())) %>% 
    bind_rows()
  
  dat <- 
    dat %>% 
    mutate(contrast_level = factor(contrast_level, levels = c('absent', 'low', 'high'))) %>% 
    mutate(bias_source = factor(bias_source, levels = c('cue', 'baserate', 'payoff'))) 
  
  # All reproduction responses ----------------------------------------------
  
  data_obs <- dat %>% 
    select(participant, bias_source, cond, contrast_level, stim, response) 
  
  data_obs <- data_obs %>% 
    distinct() %>% 
    pivot_wider(names_from = cond, values_from = response) %>% 
    mutate(diff = present - control)
  
  p_load(ggdist)
  alpha <- .7
  p_obs <-
    data_obs %>%
    mutate(data_type='empirical') %>%
    ggplot(aes(y=contrast_level, x=diff, color=data_type, fill=data_type)) +
    facet_wrap(. ~ bias_source, ncol = 1, axes = 'margins', labeller = labeller(bias_source=labels_bias_source)) +
    stat_summary(aes(linetype= data_type), fun.data = mean_cl_normal, geom='errorbar', width=0, size=4, alpha=alpha) +
    stat_summary(fun=mean, geom='point', shape=8, color=darken(get_model_colors()['empirical'], amount = .2)) +
    scale_y_discrete(labels = str_to_title) + 
    ylab('Contrast level') + xlab('Effect on contrast reproduction\n(bias - control)') 
  
  # pred ---------
  
  data_pred <- 
    dat %>% 
    filter(model_name %in% model_to_plot) %>%
    select(model_name, participant, bias_source, cond, contrast_level, stim, response_pred)
  
  # get difference between conditions
  data_pred <- 
    data_pred %>% 
    pivot_wider(names_from = cond, values_from = response_pred) %>% 
    mutate(diff = present - control) 
  
  # remove other columns
  data_pred <- data_pred %>% 
    select(-c(stim, control, present))
  
  dropLeadingZero <- function(l){
    str_replace(l, '0(?=.)', '')
  }

  dodge_width <- 0
  p_obs <-
    p_obs + 
    geom_vline(xintercept = 0, linetype=2, alpha=.3) +
    stat_summary(data=data_pred, aes(y=contrast_level, x=diff, shape=model_name, color=model_name),
                 size=2, fun=mean, geom='point', position = position_dodge(width = .3),
                 inherit.aes = FALSE) +
    stat_summary(data=data_pred, aes(y=contrast_level, x=diff, color=model_name),
                 linetype=1, linewidth=.23, inherit.aes = FALSE,
                 fun.data = mean_cl_normal, geom='errorbar', width=.48, position = position_dodge(width = .3))  +
    scale_x_continuous(breaks = scales::pretty_breaks(n=5), labels = dropLeadingZero) +
    scale_color_manual(values=get_model_colors(), labels = c('full' ='Full-model prediction', 'hurdle'='Hurdle-model prediction', 'contrast'='Contrast-model prediction'), breaks = c('full', 'hurdle', 'contrast')) +
    scale_shape_manual(values=get_model_shapes(), labels = c('full' ='Full-model prediction', 'hurdle'='Hurdle-model prediction', 'contrast'='Contrast-model prediction'), 
                       breaks = c('full', 'hurdle', 'contrast')) + 
    scale_fill_manual(values=get_model_colors()['empirical'], labels = c('empirical'='Observed data')) +
    scale_linetype_manual(values=c(1), labels = c('empirical'='Observed data')) +
    guides(color = guide_legend(title = '', position='inside', nrow = 3),
           shape = guide_legend(title = '', position='inside', nrow = 3),
           fill = guide_legend(title = '', position='inside', override.aes = list(color=get_model_colors()['empirical'], linetype=1)),
           linetype = guide_legend(title = '', position='inside', override.aes = list(color=get_model_colors()['empirical'], linetype=1)))
  
  # apply custom minimal the# apply custom minimal the# apply custom minimal theme
  p_obs <- 
    p_obs +
    theme_bw() +
    theme(
      legend.direction = 'vertical',
      legend.box.background = element_rect(color='gray90'),
      legend.position.inside = c(.68,.15),
      legend.margin =margin(r=4, l=1, t=2,b=1.5, unit = 'mm'),
      legend.title = element_blank(), 
      legend.text  = element_text(size = 6),
      legend.key.size = unit(.8, "lines"),
      axis.title.y = element_text(size=15),
      legend.spacing.y = unit(-2, "mm"),
      legend.key.spacing.y = unit(-.8, "mm"),
      legend.box.margin = margin(b=0, unit='mm'))
  
  # apply color to facet strips
  fix_strip_condition_text_color(p_obs)
  
  if  (e == 'exp1') {
    ggsave(sprintf('plots/hurdle_ppc_diff_rep_%s.png', e), 
           width = 4, height = 5.5, scale=.8, dpi=600, bg='white')
  } else if (e == 'exp2') {
    ggsave(sprintf('plots/hurdle_ppc_diff_rep_%s.png', e), 
           width = 4, height = 5.75, scale=.8, dpi=600, bg='white')
  }

}