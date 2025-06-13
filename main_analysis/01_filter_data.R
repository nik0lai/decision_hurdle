# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, stringr)

# Functions ---------------------------------------------------------------

source('main_analysis/main_functions.R')

# Data --------------------------------------------------------------------

dat <- 
  bind_rows(read_csv('data/all_data_raw_exp1.csv', col_types = cols()) %>% mutate(experiment = 'exp1'),
            read_csv('data/all_data_raw_exp2.csv', col_types = cols()) %>% mutate(experiment = 'exp2'),
            read_csv('data/all_data_raw_exp3.csv', col_types = cols(answer = col_character())) %>% mutate(experiment = 'exp3'))

# Count subjects ---------------------------------------------------------

# exp_name    bias_source       n
# 1 exp1       baserate       64
# 2 exp1       cue           106
# 3 exp1       payoff         68
# 4 exp2       baserate       64
# 5 exp2       cue            51
# 6 exp2       payoff         53
# 7 exp3       baserate       58
# 8 exp3       cue            51

dat %>%
  select(experiment, participant, bias_source) %>% 
  distinct() %>% 
  filter(!is.na(participant)) %>% 
  group_by(experiment, bias_source) %>% 
  summarise(n = n()) 

# Count trials ------------------------------------------------------------

dat %>% 
  group_by(experiment, participant, bias_source) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  select(experiment, bias_source, count) %>% 
  distinct()

# Filtering ---------------------------------------------------------------

# Staircase threshold -----------------------------------------------------

# Get staircase data
staircase_data <- 
  dat %>%
  filter(target_contrast != 0) %>% 
  select(experiment, participant, bias_source, target_contrast) %>% 
  distinct() %>% 
  rename(value = target_contrast) 

# Get outliers
staircase_outliers <-
  staircase_data %>% 
  group_by(experiment) %>% 
  nest() %>%
  transmute(out = map(data, ~outlier_filtering(.x))) %>% 
  unnest(cols = out)

# Remove staircase outliers from data
dat <-
  dat %>% 
  anti_join(staircase_outliers %>% ungroup() %>% select(experiment, participant, bias_source),
            by = join_by(bias_source, participant, experiment))

# Get SDT data ------------------------------------------------------------

sdt_data <-
  dat %>% 
  filter(trial_type == 'detection') %>% 
  mutate(accuracy = case_when(contrast_level=='absent' & answer == 'absent' ~ TRUE,
                              contrast_level %in% c('low', 'high') & answer == 'present' ~ TRUE,
                              TRUE ~ FALSE),
         conf_mat = paste0(tolower(accuracy), '_', answer)) %>%
  group_by(experiment, participant, bias_source, bias_direction, conf_mat) %>%
  summarise(count = n(), 
            .groups = "keep") %>% 
  pivot_wider(data = ., names_from = conf_mat, 
              values_from = count, values_fill = list(count = 0)) %>% 
  do(sdt_calc(hits = .$true_present, misses = .$false_absent, 
              crrej = .$true_absent, far = .$false_present)) %>%
  pivot_longer(data = ., 
               cols = c(hr, fr, lambda, d, ccrit, beta), names_to = "key", 
               values_to = "value") %>% 
  filter(key == 'd') %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  ungroup()

## low d' --------------

# Get low d' subjects
low_d_subs <- 
  sdt_data %>% 
  filter(d <= 0)

# Remove low d' subs
dat <- 
  dat %>% 
  anti_join(low_d_subs %>% ungroup() %>% select(experiment, participant, bias_source),
            by = join_by(bias_source, participant, experiment))

sdt_data <- 
  sdt_data %>% 
  anti_join(low_d_subs %>% ungroup() %>% select(experiment, participant, bias_source),
            by = join_by(bias_source, participant, experiment))

## d' outliers --------------

d_data <-
  sdt_data %>% 
  select(experiment, participant, bias_source, bias_direction, d) %>% 
  distinct() %>% 
  rename(value = d)

# Get d' outliers
d_outliers <- 
  d_data %>% 
  group_by(experiment) %>% 
  nest() %>%
  transmute(out = map(data, ~outlier_filtering(.x))) %>% 
  unnest(cols = out)

# Remove d' outliers from data
dat <-
  dat %>% 
  anti_join(d_outliers %>% ungroup() %>% select(experiment, participant, bias_source),
            by = join_by(bias_source, participant, experiment))
sdt_data <-
  sdt_data %>% 
  anti_join(d_outliers %>% ungroup() %>% select(experiment, participant, bias_source),
            by = join_by(bias_source, participant, experiment))

# Reproduction filtering --------------

rep_data <-
  dat %>%
  filter(trial_type == 'reproduction') %>% 
  mutate(value = abs(target_contrast) - abs(as.double(answer))) %>% 
  group_by(experiment, participant, bias_source, bias_direction) %>% 
  reframe(value = mean(value)) %>% 
  ungroup()

# Get reproduction outliers
rep_outliers <- 
  rep_data %>% 
  group_by(experiment) %>% 
  nest() %>%
  transmute(out = map(data, ~outlier_filtering(.x))) %>% 
  unnest(cols = out)

# Remove outliers
dat <-
  dat %>% 
  anti_join(rep_outliers %>% ungroup() %>% select(experiment, participant, bias_source),
            by = join_by(bias_source, participant, experiment))
sdt_data <-
  sdt_data %>% 
  anti_join(rep_outliers %>% ungroup() %>% select(experiment, participant, bias_source),
            by = join_by(bias_source, participant, experiment))
rep_data <-
  rep_data %>% 
  anti_join(rep_outliers %>% ungroup() %>% select(experiment, participant, bias_source),
            by = join_by(bias_source, participant, experiment))

# Filtering summary -------------------------------------------------------

staircase_outliers
low_d_subs
d_outliers
rep_outliers

bind_rows(staircase_outliers,
          low_d_subs,
          d_outliers,
          rep_outliers) %>% 
  select(experiment, participant, bias_source) %>% 
  distinct() %>% 
  group_by(experiment, bias_source) %>% 
  summarise(count = n())

# Count filtered subjects -------------------------------------------------

# exp_name    bias_source     n
# 1 exp1       baserate       57
# 2 exp1       cue           105
# 3 exp1       payoff         64
# 4 exp2       baserate       58
# 5 exp2       cue            50
# 6 exp2       payoff         50
# 7 exp3       baserate       58
# 8 exp3       cue            50
 
dat %>% 
  select(experiment, participant, bias_source) %>% 
  distinct() %>% 
  filter(!is.na(participant)) %>% 
  group_by(experiment, bias_source) %>% 
  summarise(n = n())

dat %>% 
  select(experiment, participant, bias_source, bias_direction) %>% 
  distinct() %>% 
  group_by(experiment, participant, bias_source) %>% 
  filter(row_number() == 1) %>% 
  group_by(experiment, bias_source, bias_direction) %>% 
  reframe(count = n()) %>% 
  pivot_wider(names_from = bias_direction, values_from = count)

# Save data ---------------------------------------------------------------

write_csv(dat %>% filter(experiment == 'exp1') %>% select(-experiment), 'data/processed/all_data_filtered_exp1.csv')
write_csv(dat %>% filter(experiment == 'exp2') %>% select(-experiment), 'data/processed/all_data_filtered_exp2.csv')
write_csv(dat %>% filter(experiment == 'exp3') %>% select(-experiment), 'data/processed/all_data_filtered_exp3.csv')
