# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(stringr)  

sh_template <- "#!/bin/bash

#Set job requirements
#SBATCH -J SCRIPT_NAME
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --partition=rome
#SBATCH --time=24:00:00

# load R
module load 2023
module load R/4.3.2-gfbf-2023a

# run models  
Rscript --slave -e 'source(\"hurdle_model/hurdle_functions.R\");run_model(experiment = \"EXPERIMENT\", condition = \"CONDITION\", model_name = \"simple\", chains=CHAINS, iter=ITER, future=FUTURE)' &
Rscript --slave -e 'source(\"hurdle_model/hurdle_functions.R\");run_model(experiment = \"EXPERIMENT\", condition = \"CONDITION\", model_name = \"contrast\", chains=CHAINS, iter=ITER, future=FUTURE)' &
Rscript --slave -e 'source(\"hurdle_model/hurdle_functions.R\");run_model(experiment = \"EXPERIMENT\", condition = \"CONDITION\", model_name = \"hurdle\", chains=CHAINS, iter=ITER, future=FUTURE)' &
Rscript --slave -e 'source(\"hurdle_model/hurdle_functions.R\");run_model(experiment = \"EXPERIMENT\", condition = \"CONDITION\", model_name = \"full\", chains=CHAINS, iter=ITER, future=FUTURE)' &
wait"

# Sampling parameters
iter <- '20000'
chains <- '4'
future <- 'FALSE'

# create scripts
for (e in c('exp1', 'exp2')) {
  for (c in c('baserate', 'payoff', 'cue')) {
    
    # create script file name
    sh_file_name <- sprintf('%s_%s.sh', e, str_sub(c, 1, 3))
    # fill in experiment and condition info
    new_sh <- sh_template
    new_sh <- str_replace_all(new_sh, pattern = 'EXPERIMENT', replacement = e)
    new_sh <- str_replace_all(new_sh, pattern = 'CONDITION', replacement = c)
    new_sh <- str_replace_all(new_sh, pattern = 'SCRIPT_NAME', replacement = paste0(e, '_', c))
    new_sh <- str_replace_all(new_sh, pattern = 'CHAINS', replacement = chains)
    new_sh <- str_replace_all(new_sh, pattern = 'ITER', replacement = iter)
    
    # save file
    cat(new_sh, file = paste0('hurdle_model/sh_scripts/', sh_file_name))
  }
}
