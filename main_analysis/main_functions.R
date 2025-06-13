
# Labels ------------------------------------------------------------------

# Bias manipulation labels
labels_bias_source <- c('cue' = 'Att. cue', 
                        'payoff' = 'Payoff', 
                        'baserate' = 'Base rate')

# Contrast level labels
labels_contrat_levels <- c('absent'='Absent', 'low'='Low contrast', 'high'='High contrast')

model_colors <- c('hurdle'='#e6ac91ff', 'contrast'='#916cadff', 'full'='white')

# Functions ---------------------------------------------------------------

# Filter outliers
outlier_filtering <- function(data, sd_threshold=4) {
  
  # Calculate thresholds and mark outliers
  outliers <- 
    data %>%
    mutate(mean = mean(value),
           upper_boundary = mean + (sd(value) * sd_threshold),
           lower_boundary = mean - (sd(value) * sd_threshold)) %>% 
    mutate(outlier = value >= upper_boundary | value <= lower_boundary) %>% 
    filter(outlier)
  
  return(outliers)
  
}

# Calculate signal detection theory criterion and d'
sdt_calc <- 
  function (hits, misses, crrej, far) 
  {
    if (0 %in% c(hits, misses, crrej, far)) {
      hits = hits + 0.5
      misses = misses + 0.5
      crrej = crrej + 0.5
      far = far + 0.5
    }
    
    # Get hit and false alarm rate 
    hr <- hits/(hits + misses)
    fr <- far/(far + crrej)
    
    # Get SDT measures
    lambda <- qnorm(fr)
    d <- qnorm(hr) - lambda
    ccrit <- (qnorm(hr) + qnorm(fr))/2
    beta <- d * (-lambda - d * 1/2)
    return(tibble(hr, fr, lambda, d, ccrit, beta))
  }

# Create columns with condition and stim codes, also make columns
# into factors to enforce order when plotting
format_columns <- function(df, sep='\n') {
  
  # condition levels
  cond_levels = c(sprintf('baserate%scontrol', sep), 
                  sprintf('baserate%spresent', sep), 
                  sprintf('cue%scontrol', sep), 
                  sprintf('cue%spresent', sep), 
                  sprintf('payoff%scontrol', sep), 
                  sprintf('payoff%spresent', sep))
  
  # stim levels
  stim_levels = c(sprintf('control%sabsent', sep), 
                  sprintf('present%sabsent', sep), 
                  sprintf('control%slow', sep), 
                  sprintf('present%slow', sep), 
                  sprintf('control%shigh', sep), 
                  sprintf('present%shigh', sep))
  
  df %>% 
    # create condition code and stim code formatted for plotting
    mutate(condition_code = paste(bias_source, bias_direction, sep = sep),
           stim_code = paste(bias_direction, contrast_level, sep = sep)) %>% 
    # convert columns into factor to enforce order when plotting
    mutate(condition_code = factor(condition_code, levels = cond_levels),
           stim_code = factor(stim_code, levels = stim_levels),
           bias_source = factor(bias_source, levels = c('cue', 'baserate', 'payoff')),
           contrast_level = factor(contrast_level, levels = c('absent', 'low', 'high')))
  
}

# Plot proportion of present responses in detection task
plot_proportion_present <- function(df) {
  
  # Plot
  df %>% 
    ggplot(., aes(x=stim_code, y=proportion_present, color=bias_source)) +
    facet_wrap(. ~ bias_source, labeller = labeller(bias_source = get_labels_bias_source_sample(df), .multi_line = FALSE), ncol = 1, nrow = 3) +
    geom_point(alpha=.3, show.legend = FALSE) +
    geom_line(aes(group=interaction(participant, contrast_level), color = bias_source), alpha=.2, show.legend = FALSE) +
    stat_summary(fun=mean, geom='point', color='black',size=1) + 
    stat_summary(aes(group=contrast_level), color='black',
                 fun=mean, geom='errorbar', width=.15,
                 fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
                 fun.max = function(x) mean(x) + sd(x)/sqrt(length(x))) +
    ggtitle('Decision task') +
    ylab('Proportion of "present" answers') +
    # xlab('Bias condition and contrast level') +
    xlab('Condition') +
    # scale_x_discrete(labels = function(x) str_to_title(x), guide = guide_axis_nested(delim = '\n')) +
    scale_x_discrete(labels = get_labels_stim_code('\n'), guide = guide_axis_nested(delim = '\n')) +
    scale_color_manual(values=get_condition_colors())
  
}

# get bias source labels with condition sample size
get_labels_bias_source_sample <- function(df) {
  
  df %>% 
    select(participant, bias_source) %>% 
    distinct() %>% 
    group_by(bias_source) %>% 
    reframe(count = n()) %>% 
    full_join(enframe(labels_bias_source), by = c('bias_source' = 'name')) %>% 
    mutate(value = sprintf('%s (N=%s)', value, count)) %>% 
    select(-count) %>% 
    deframe()
  
  
}

# Make labels with bias direction and contrast level
get_labels_stim_code <- function(sep) {
  
  stim_levels = c(sprintf('control%sabsent', sep), 
                  sprintf('present%sabsent', sep), 
                  sprintf('control%slow', sep), 
                  sprintf('present%slow', sep), 
                  sprintf('control%shigh', sep), 
                  sprintf('present%shigh', sep))
  
  lsc <- c(sprintf('Control%sAbsent', sep), 
           sprintf('Present%sAbsent', sep), 
           sprintf('Control%sLow contrast', sep), 
           sprintf('Present%sLow contrast', sep), 
           sprintf('Control%sHigh contrast', sep), 
           sprintf('Present%sHigh contrast', sep))
  names(lsc) <- stim_levels
  
  return(lsc)
}

# Get colors for each condition
get_condition_colors = function(sep='_') {
  
  # ggplot'esque Colors
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  # Load function to lighten and darken colors
  p_load(colorspace)
  
  # Get base color
  base_colors <- gg_color_hue(3)
  
  # Assign colors and brightness factor
  color_at <- 2
  color_br <- 1
  color_po <- 3
  dark_factor <- .4
  light_factor <- .2
  
  # Dictionary with colors
  condition_colors <- c(
    'cue' = base_colors[color_at],
    'baserate' = base_colors[color_br],
    'payoff' = base_colors[color_po],
    
    'cue.present' = darken(base_colors[color_at], dark_factor),
    'cue.control' = lighten(base_colors[color_at], light_factor),
    'baserate.present' = darken(base_colors[color_br], dark_factor),
    'baserate.control' = lighten(base_colors[color_br], light_factor),
    'payoff.present' = darken(base_colors[color_po], dark_factor),
    'payoff.control' = lighten(base_colors[color_po], light_factor),
    'simulated.present' = darken('violet', dark_factor),
    'simulated.control' = lighten('violet', light_factor)
    
  )
  
  # Apply separator
  return(set_names(condition_colors, str_replace(names(condition_colors), '\\.', sep)))
  
}


# get bf (one-tail positive effect t-test) and Cohen's d values 
get_bf_d <- function(data, conditions, rscale=.707) {
  
  # load libraries
  library('BayesFactor')
  library('effectsize')
  library('bayestestR')
  
  # Format data
  data %>% 
    pivot_wider(names_from = bias_direction, values_from = value) %>%
    rename(cond_a = conditions[1],
           cond_b = conditions[2]) %>% 
    group_by(bias_source, contrast_level) %>%
    nest() %>% 
    # Run t-test and effect size
    mutate(
      bf_ttest = map(data, ~ ttestBF(x = .x$cond_a, y = .x$cond_b, paired = TRUE, rscale = rscale, nullInterval = c(-Inf, 0))),
      effSize = map(data, ~ tibble(cohens_d(x = .x$cond_b, y = .x$cond_a, data = .x, paired = TRUE, verbose = FALSE)))
    ) %>% 
    # Extract values
    mutate(
      bf = unlist(map(bf_ttest, ~extractBF(.x, onlybf = TRUE)[[1]])),
      d = unlist(map(effSize, "Cohens_d"))
    ) %>% 
    # Shorten BF values
    mutate(bf = shorter_bf_value(bf)) %>% 
    # Create labels for plotting
    mutate(
      bf_lab = paste0('bold(BF[10]) == "', bf, '"'),
      d_lab = paste0('italic(d) == ', round(d,2)),
      label = paste0('atop(', bf_lab, ', ', 
                     d_lab,')')
    )
}

get_wilcoxon_ttest <- function(cond_a, cond_b) {
  source('main_analysis/wilcoxon_functions.R')
  
  x <- cond_a 
  y <- cond_b 
  
  # Default Cauchy prior width is set to 1/sqrt(2) for the sampler 
  signedRankSamples <- signRankGibbsSampler(xVals = x,
                                            yVals = y, 
                                            nSamples = 5e2)
  
  # Posterior distribution
  # hist(signedRankSamples$deltaSamples, freq = FALSE)
  
  # Give the posterior samples for delta to the function below to compute BF01
  bf <- computeBayesFactorOneZero(signedRankSamples$deltaSamples, 
                                  oneSided = 'left',
                                  whichTest = "Wilcoxon",
                                  priorParameter = 1 / sqrt(2))
  
  return(bf)
  
}

# get bf (one-tail positive effect t-test) and Cohen's d values 
get_bf_d_wilcoxon <- function(data, conditions, rscale=.707) {
  
  # load libraries
  library('effectsize')
  
  # Format data
  data %>% 
    pivot_wider(names_from = bias_direction, values_from = value) %>%
    rename(cond_a = conditions[1],
           cond_b = conditions[2]) %>% 
    group_by(bias_source, contrast_level) %>%
    nest() %>% 
    # Run t-test and effect size
    mutate(
      bf_ttest = map(data, ~ get_wilcoxon_ttest(cond_a = .x$cond_a, cond_b = .x$cond_b)),
      effSize = map(data, ~ tibble(cohens_d(x = .x$cond_b, y = .x$cond_a, data = .x, paired = TRUE, verbose = FALSE)))
    ) %>% 
    # Extract values
    mutate(
      bf = unlist(bf_ttest),
      d = unlist(map(effSize, "Cohens_d"))
    ) %>% 
    # Shorten BF values
    mutate(bf = shorter_bf_value(bf)) %>% 
    # Create labels for plotting
    mutate(
      bf_lab = paste0('bold(BF[10]) == "', bf, '"'),
      d_lab = paste0('italic(d) == ', round(d,2)),
      label = paste0('atop(', bf_lab, ', ', 
                     d_lab,')')
    )
}

# Format BF values
shorter_bf_value <- function(bf) {
  if (bf < 1) {
    bf <- round(bf, 2)
  } else if (bf > 1) {
    if (nchar(round(bf)) > 3) {
      bf <- formatC(bf, format = "e", digits = 1)
    } else if (round(bf) > 1) {
      bf <- round(bf)
    } else {
      bf <- round(bf, 2)
    }
  }
  return(as.character(bf))
}

# Add BF and effect sizes to plot
plot_bf_d <- function(plot, data, x, y, gap) {
  
  plot +
    geom_text(data=data, 
              aes(x=x, y=y, label=bf_lab), 
              size=2.5, color='black', parse = TRUE) +
    geom_text(data=data, 
              aes(x=x, y=y+gap, label=d_lab),
              size=2.5, color='black', parse = TRUE)
  
}

plot_contrast_reproduction <- function(df) {
  
  # plot
  df %>% 
    ggplot(., aes(x=stim_code, y=reproduction, color=bias_source)) +
    facet_wrap(. ~ bias_source, 
               labeller = labeller(bias_source = get_labels_bias_source_sample(df), .multi_line = FALSE), 
               ncol = 1, nrow = 3) +
    geom_point(alpha=.3, show.legend = FALSE) +
    geom_line(aes(group=interaction(participant, contrast_level), color = bias_source), alpha=.2, show.legend = FALSE) +
    stat_summary(fun=mean, geom='point', color='black', size=1) + 
    stat_summary(aes(group=contrast_level), color='black',
                 fun=mean, geom='errorbar', width=.15,
                 fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
                 fun.max = function(x) mean(x) + sd(x)/sqrt(length(x))) +
    ggtitle('Reproduction task') +
    ylab('Reproduced contrast') +
    # xlab('Bias condition and contrast level') +
    xlab('Condition') +
    # scale_x_discrete(labels = function(x) str_to_title(x), guide = guide_axis_nested(delim = '\n')) +
    scale_x_discrete(labels = get_labels_stim_code('\n'), guide = guide_axis_nested(delim = '\n')) +
    scale_color_manual(values=get_condition_colors())
  
}

plot_proportion_zero_reproduction <- function(df) {
  
  df %>% 
    ggplot(aes(x=contrast_level, y=prop_zero, group=bias_direction)) +
    facet_wrap(. ~ bias_source, labeller=labeller(bias_source=get_labels_bias_source_sample(df))) + 
    geom_point(aes(color=interaction(bias_source, bias_direction, sep='_')), 
               position = position_dodge(width = .5), alpha=.2) +
    scale_color_manual(values = get_condition_colors()) +
    stat_summary(fun=mean, geom='point', position = position_dodge(width = .5)) +
    stat_summary(fun=mean, 
                 fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)),
                 fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
                 geom = 'errorbar', color='black', width=.4,
                 position = position_dodge(width = .5)) +
    ylab('Proportion of zero contrast reproductions') +
    # ylab('p(zero contrast reproduction)') +
    xlab('Contrast level') +
    scale_x_discrete(labels = function(x) str_to_title(x)) +
    scale_y_continuous(breaks = c(0, .25, .5, .75, 1)) +
    ggtitle("Proportion of zero reproductions")
  # labs(title = expression(paste("Proportion of ", italic(zero), " reproductions")))
  
}

plot_reproduction_distribution <- function(df) {
  # bin width
  bin_width <- .003; 
  
  # make plot
  df %>%
    ggplot(aes(x=answer, 
               fill = interaction(bias_source, bias_direction, sep='_'),
               color = interaction(bias_source, bias_direction, sep='_'))) +
    facet_grid(rows = vars(bias_source), cols = vars(contrast_level),
               labeller = labeller(bias_source = labels_bias_source, 
                                   contrast_level = c('absent' = 'Absent', 'low'='Low Contrast', 'high'='High Contrast'))) +
    geom_histogram(data=df %>% filter(bias_direction=='present'), alpha=.7, position = 'identity', binwidth = bin_width) +
    geom_histogram(data=df %>% filter(bias_direction=='control'), alpha=.4, position = 'identity', binwidth = bin_width) +
    scale_x_continuous(breaks=breaks_pretty(n=3), limits = c(0, .14)) +
    scale_y_continuous(breaks=breaks_pretty(n=3)) +
    ylab('Count') + xlab('Reproduced contrast') + 
    ggtitle('Distribution of non-zero reproductions') +
    scale_fill_manual(values=get_condition_colors(), breaks=c('cue_control', 'cue_present'), labels=c('cue_present'='Bias', 'cue_control'='No bias')) +
    scale_color_manual(values=get_condition_colors(), breaks=c('cue_control', 'cue_present'), labels=c('cue_present'='Bias', 'cue_control'='No bias')) +
    theme(legend.position = 'none') 
  
}

# color the facet strips using the manipulation colors
fix_strip_condition_text_color <- function(p) {
  library('ggpubr')
  
  # Make a grob object
  Pg <- ggplotGrob(p)
  
  # Get strip element indexes
  strips <- which(grepl('strip', Pg$layout$name))
  
  # Set strip text color
  for (i in seq(length(Pg$grobs[strips]))) {
    # Get condition name to later get condition color
    strip_label <- Pg$grobs[strips][[i]]$grobs[[1]]$children[[2]]$children[[1]]$label
    # get condition color
    
    strip_text_color <- c('Payoff'='#619CFF', 'Base rate'='#F8766D','Att. cue'='#00ad34')[strip_label][[1]]
    # strip_text_color <- condition_colors[[deframe(select(enframe(label_bias_source), 2:1))[[str_remove(strip_label, ' .*')]]]]
    
    # Set the color of the text
    # Pg$grobs[strips][[i]]$grobs[[1]]$children[[2]]$children[[1]]$gp$col <- strip_text_color
    # Set the color of the background
    Pg$grobs[strips][[i]]$grobs[[1]]$children[[1]]$gp$fill <- lighten(col = strip_text_color, amount = .4)
  }
  
  # Bring back to ggplot
  p <- as_ggplot(Pg) 
  
  return(p)
}