#libs.R

#libraries ised
library("dplyr")
library("tidyr")
library("here")
library("skimr")
#library("Amelia")
library("ggplot2")
library("purrr")
library("patchwork")
library("kableExtra")
library("parameters")
library("mice")
library("table1")
library("skimr")
library("ggokabeito")   # color palette
#library("brms") # bayesian estimation
#library("rstan")
library("ggpubr")
#library("MatchThem")
library("marginaleffects")
library("mice")
#library("cobalt")
#library("WeightIt")
#library("optmatch")
#library("cmdstanr")
library("formula.tools")
library("splines")
library("parameters")
library("naniar") # missing data
library("conflicted")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("cbind", "base")
#library("speedglm")
library("stdReg") # g-computation
#rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course
theme_set(theme_pubclean()) # nice theme
# theme_set(theme_classic())
