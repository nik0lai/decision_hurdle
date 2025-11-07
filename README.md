This repository contains the analysis scripts and data of the article "The act of detecting a stimulus contaminates measures of conscious experience with unrelated cognitive factors". There are two set of scripts, the folder _main_analysis_ contains the behavioral analysis scripts, whereas _hurdle_model_ has the modelling scripts.

The scripts are numbered in the order they should be run. 


# Disentangling perceptual and response biases: The act of detecting a stimulus contaminates measures of conscious experience

This repository contains the data and analysis script of the manuscript "Disentangling perceptual and response biases: The act of detecting a stimulus contaminates measures of conscious experience"

## R and package versions

This project uses [**renv**](https://rstudio.github.io/renv/) to ensure a fully reproducible R environment.

The exact R version (4.3.3 (2024-02-29)) version and package versions used for all analyses are recorded in the `renv.lock` file.

To reproduce the same environment:

```r
install.packages("renv")
renv::restore()
```

Note that `renv` will install the appropriate version of the packages but it won't change your R version. You may need to install R (4.3.3 (2024-02-29)) yourself.

## Analysis

Analysis scripts are numbered in the order they should be run. There are also two scripts that run all the scripts of the basic analysis `run_main_analysis.R` and the modelling `run_hurdle_modelling_analysis.R`.

Compiled models are provided to prevent the need to rerun the models. Alternatively, the compiled models (.rds files) can be deleted to re-run the models.