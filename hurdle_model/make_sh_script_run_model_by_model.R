# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(stringr)  

# Settings ----------------------------------------------------------------
# When running on server (snellius) the script will also load the R module
# otherwise it will make an sh script that can be run in linux
run_on_server <- TRUE

# Template ----------------------------------------------------------------

# Module added text
server_extra <- "#Set job requirements
#SBATCH -J SCRIPT_NAME
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --partition=rome
#SBATCH --time=24:00:00

# load R
module load 2023
module load R/4.3.2-gfbf-2023a
"

# This is a template to fill in with the experiment and model name
sh_template <- "#!/bin/bash
%s
# run models  
Rscript --slave -e 'source(\"hurdle_model/hurdle_functions.R\");run_model(experiment = \"EXPERIMENT\", condition = \"baserate\", model_name = \"MODEL\", chains=CHAINS, iter=ITER, future=FUTURE)' &
Rscript --slave -e 'source(\"hurdle_model/hurdle_functions.R\");run_model(experiment = \"EXPERIMENT\", condition = \"cue\", model_name = \"MODEL\", chains=CHAINS, iter=ITER, future=FUTURE)' &
Rscript --slave -e 'source(\"hurdle_model/hurdle_functions.R\");run_model(experiment = \"EXPERIMENT\", condition = \"payoff\", model_name = \"MODEL\", chains=CHAINS, iter=ITER, future=FUTURE)' &
wait"

if (run_on_server) {
  sh_template <- sprintf(sh_template, server_extra)
} else {
  sh_template <- sprintf(sh_template, '')
}

cat(sh_template)

# Sampling parameters
iter <- '20000'
chains <- '4'
future <- 'FALSE'

# create scripts
for (e in c('exp1', 'exp2')) {
  for (m in c('simple', 'contrast', 'hurdle', 'full')) {
    
    new_sh <- sh_template
    
    # create script file name
    sh_file_name <- sprintf('%s_%s.sh', e, m)
    # fill in experiment and condition info
    new_sh <- str_replace_all(new_sh, pattern = 'EXPERIMENT', replacement = e)
    new_sh <- str_replace_all(new_sh, pattern = 'MODEL', replacement = m)
    new_sh <- str_replace_all(new_sh, pattern = 'SCRIPT_NAME', replacement = paste0(e, '_', m))
    new_sh <- str_replace_all(new_sh, pattern = 'CHAINS', replacement = chains)
    new_sh <- str_replace_all(new_sh, pattern = 'ITER', replacement = iter)
    new_sh <- str_replace_all(new_sh, pattern = 'FUTURE', replacement = future)
    
    # save file
    cat(new_sh, file = paste0('hurdle_model/sh_scripts/', sh_file_name))
  }
}
