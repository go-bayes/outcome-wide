#libs.R

#libraries ised

library("dplyr")
library("here")
library("skimr")
library("tidyr")
library("Amelia")
library("ggplot2")
library("purrr")
library("patchwork")
library("kableExtra")
library("parameters")
library("mice")
library("ggokabeito")   # color palette
library("brms") # bayesian estimation
library("rstan")
library("ggpubr")
library("cmdstanr")
rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course
theme_set(theme_pubclean()) # nice theme
# theme_set(theme_classic())
