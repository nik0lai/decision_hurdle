
# Because the data was rather large and the model took quite some time to run, the models were 
# run using dutch supercomputer Snellius. The scripts are therefor sh scritps that need to be 
# run in linux and SLURM. To simply run in a unix machine (that has R) the sh scripts can be 
# generated without the server related info (set run_on_server to FALSE when generating the 
# scripts with make_sh_script_run_mocel.R). 

# The models can be also run directly from Rstudio however it will take quite some time 
# (~8-12 hours in my personal laptop). Below there is an example of a script that runs
# all the models in all conditions. The model fit objects will be saved in the fits folder
# once the model is done.

# Get function to run model
source("hurdle_model/hurdle_functions.R")

# Sampling settings
CHAINS <- 4
ITER <- 20000
FUTURE <- FALSE

# Run models - Experiment 1 -----------------------------------------------

exp <- 'exp1'

# base rate
run_model(experiment = exp, condition = "baserate", model_name = "simple", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "baserate", model_name = "contrast", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "baserate", model_name = "hurdle", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "baserate", model_name = "full", chains=CHAINS, iter=ITER, future=FUTURE)

# payoff
run_model(experiment = exp, condition = "payoff", model_name = "simple", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "payoff", model_name = "contrast", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "payoff", model_name = "hurdle", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "payoff", model_name = "full", chains=CHAINS, iter=ITER, future=FUTURE)

# payoff
run_model(experiment = exp, condition = "cue", model_name = "simple", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "cue", model_name = "contrast", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "cue", model_name = "hurdle", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "cue", model_name = "full", chains=CHAINS, iter=ITER, future=FUTURE)

# Run models - Experiment 2 -----------------------------------------------

exp <- 'exp2'

# base rate
run_model(experiment = exp, condition = "baserate", model_name = "simple", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "baserate", model_name = "contrast", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "baserate", model_name = "hurdle", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "baserate", model_name = "full", chains=CHAINS, iter=ITER, future=FUTURE)

# payoff
run_model(experiment = exp, condition = "payoff", model_name = "simple", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "payoff", model_name = "contrast", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "payoff", model_name = "hurdle", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "payoff", model_name = "full", chains=CHAINS, iter=ITER, future=FUTURE)

# payoff
run_model(experiment = exp, condition = "cue", model_name = "simple", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "cue", model_name = "contrast", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "cue", model_name = "hurdle", chains=CHAINS, iter=ITER, future=FUTURE)
run_model(experiment = exp, condition = "cue", model_name = "full", chains=CHAINS, iter=ITER, future=FUTURE)
