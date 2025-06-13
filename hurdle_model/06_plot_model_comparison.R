# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, readr, dplyr, tidyr, ggplot2, tibble, stringr, purrr, scales, patchwork, scales, colorspace)

source('main_analysis/main_functions.R')


# fix_strip_condition_text_color <- function(p) {
#   library('ggpubr')
#   
#   # Make a grob object
#   Pg <- ggplotGrob(p)
#   
#   # Get strip element indexes
#   strips <- which(grepl('strip', Pg$layout$name))
#   
#   # Set strip text color
#   for (i in seq(length(Pg$grobs[strips]))) {
#     # Get condition name to later get condition color
#     strip_label <- Pg$grobs[strips][[i]]$grobs[[1]]$children[[2]]$children[[1]]$label
#     # get condition color
#     
#     strip_text_color <- c('Payoff'='#619CFF', 'Base rate'='#F8766D','Att. cue'='#00ad34')[strip_label][[1]]
#     # strip_text_color <- condition_colors[[deframe(select(enframe(label_bias_source), 2:1))[[str_remove(strip_label, ' .*')]]]]
#     
#     # Set the color of the text
#     # Pg$grobs[strips][[i]]$grobs[[1]]$children[[2]]$children[[1]]$gp$col <- strip_text_color
#     # Set the color of the background
#     Pg$grobs[strips][[i]]$grobs[[1]]$children[[1]]$gp$fill <- lighten(col = strip_text_color, amount = .4)
#   }
#   
#   # Bring back to ggplot
#   p <- as_ggplot(Pg) 
#   
#   return(p)
# }

# Labels ------------------------------------------------------------------

# labels_bias_source <- c('baserate'='Base rate', 'cue'='Att. cue', 'payoff'='Payoff')
# model_colors <- c('hurdle'='#e6ac91ff', 'contrast'='#916cadff', 'full'='white')


experiments <- c('exp1', 'exp2')

for (e in experiments) {
  
  # data --------------------------------------------------------------------
  
  dat <- dir('hurdle_model/bfs_model_comparison/', pattern = e, full.names = TRUE) %>%
    map(read_csv, col_types=cols()) %>% 
    bind_rows()
  
  # Plot all model comparison -----------------------------------------------
  
  all_models <- dat %>% 
    filter(denominator == 'simple')
  
  p_all <-
    all_models %>% 
    mutate(condition = factor(condition, levels=c('cue', 'baserate', 'payoff')),
           model = factor(model, levels=c('contrast', 'hurdle', 'full'))) %>% 
    filter(model != 'simple') %>% 
    ggplot(aes(y=model, x=bf_log, fill=model)) +
    facet_wrap(. ~ condition, ncol=1, labeller=labeller(condition=labels_bias_source), strip.position="right") +
    geom_bar(stat='identity', show.legend = FALSE, linewidth=.3, color='black', width=.8) +
    xlab('(log) BF over baseline model') + ylab('Model') +
    scale_y_discrete(labels=labels_bias_source) +
    scale_x_continuous(transform = pseudo_log_trans(), breaks = breaks_pretty(n=5), guide = guide_axis(check.overlap = TRUE)) +
    # scale_x_log10(labels = label_log(digits = 2)) +
    scale_fill_manual(values=model_colors) +
    # theme_classic() +
    theme_bw() +
    theme(strip.text = element_text(size=10),
          axis.text.y = element_text(size=12,  angle = 35),
          axis.text.x = element_text(size=10, angle = 45, hjust = 1),
          axis.title.y = element_text(size=16),
          axis.title.x = element_text(size=14))
  
  
  # Set color of facets
  p_all <- fix_strip_condition_text_color(p_all)
  
  # Save plot
  ggsave(plot = p_all, filename = sprintf('plots/bf_all_models_%s.png', e),
         width = 3, height = 3, dpi = 300, bg='white')
  
  
  # Plot hypothesis models --------------------------------------------------
  
  relabel_x <- function(x) {
    convert_to_log <- function(x) {
      
      if (is.na(x)) {
        return(NA)
      } else if (x == 0) {
        return(0)
      } else {
        return(paste0('10^', abs(x)))
      }
    }
    return(unlist(map(x, convert_to_log)))
  }
  
  # contrast hurdle annotation coordinates
  if (e == 'exp2') {
    text_x <- -0.4
    text_y <- 0.48
    arrow_start <- -30
    arrow_end <- 30
    breaks <- c(-40, -10, 10, 40, 180)
  } else if (e == 'exp1') {
    text_x <- -.62
    text_y <- 0.48
    arrow_start <- -100
    arrow_end <- 100
    breaks <- c(-600, -80, -10, 10, 80, 600)
  }
  
  hyp_models <- 
    dat %>% 
    filter(denominator == 'contrast')
  
  p_hyp <-
    hyp_models %>%
    filter(model != denominator) %>%
    mutate(evidence = case_when(bf_log < 0 ~ 'contrast',
                                bf_log > 0 ~ 'hurdle')) %>%  
    # mutate(bf_exp = exp(bf_log)) %>% 
    # mutate(bf_exp = case_when(evidence == 'contrast' ~ -bf_exp,
    #                         TRUE ~ bf_exp)) %>%
    mutate(condition = factor(condition, levels=c('payoff', 'baserate', 'cue')),
           model = factor(model, levels=c('contrast', 'hurdle', 'full'))) %>% 
    filter(model == 'hurdle') %>% 
    ggplot(aes(y=condition, x=bf_log, fill=evidence)) +
    geom_bar(stat='identity', show.legend = TRUE, width=.7, linewidth=.3, color='black') +
    xlab('(log) BF') + ylab('Condition') +
    scale_y_discrete(labels=labels_bias_source, position = 'left') +
    scale_fill_manual(values=model_colors, labels=c('contrast'='Contrast Model', 'hurdle'='Hurdle Model')) +
    guides(fill = guide_legend('Evidence favouring', position = 'inside')) +
    geom_segment(aes(x = arrow_start, xend = arrow_end, y = 0.25, yend = 0.25),
                 arrow = arrow(length = unit(2, "mm"), ends = 'both', type='closed')) +
    geom_vline(xintercept = 0, alpha = .8, linetype=2, linewidth=.3) +
    annotate(geom='text', label='favours contrast   |   favours hurdle', x=text_x, y=text_y, hjust=.5, size=3) +
    coord_cartesian(ylim = c(.7, 3)) +
    scale_x_continuous(transform = pseudo_log_trans(), breaks=breaks)
  
  # legend position
  legend_pos <- c(.78, .85)
  
  # apply custom minimal theme
  p_hyp <-
    p_hyp +
    theme_bw() +
    theme(
      legend.box.background = element_rect(fill='gray98', color='gray75'),
      legend.position.inside = legend_pos,
      strip.text = element_text(size=12),
      axis.text.y = element_text(size=13, angle = 45),
      axis.text.x = element_text(size=12),
      axis.title.y = element_text(size=16),
      axis.title.x = element_text(size=16, hjust = 0.5, ),
      legend.margin =margin(r=6, l=1, t=2,b=2, unit = 'mm'),
      legend.title = element_text(size = 11), 
      legend.text  = element_text(size = 11),
      legend.key.size = unit(.9, "lines"),
      legend.key.spacing.y = unit(.5, 'mm'),
      ) 
  # scale_x_continuous(labels = function(x) ggplot2:::parse_safe(relabel_x(x)))
  
  ggsave(plot = p_hyp, 
         filename = sprintf('plots/bf_hyp_models_%s.png', e),
         width = 4.2, height = 3, dpi = 300, bg='white')
  
  # combine -----------------------------------------------------------------
  
  p <-
    # when using ggplotgrob and asggplot to convert back the size of the plot
    # is different to an out of the box ggplot. converting both to grob and 
    # using patchwork::wrap_elements to equalize sizes
    patchwork::wrap_elements(ggplotGrob(p_all)) / patchwork::wrap_elements(ggplotGrob(p_hyp))
  
  if (e == 'exp1') {
    ggsave(plot = p, filename = sprintf('plots/bf_models_%s.png', e),
           height = 20, width = 12, dpi = 600, scale=.35)
    
  } else if (e == 'exp2') {
    ggsave(plot = p, filename = sprintf('plots/bf_models_%s.png', e),
           height = 20, width = 12, dpi = 600, scale=.35)
    
  }
}

