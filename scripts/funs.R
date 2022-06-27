# functions.R
library("here")
library("tidyverse")
library("fs")

# set paths for pushing files (off of github)
push_mods <- fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/mods")
push_figs <- fs::path_expand("~/Users/joseph/The\ Virtues\ Project\ Dropbox/outcomewide/figs")


## function for saving
saveh <- function(df, name) {
  x = df
  saveRDS( x,  here::here(push_mods,  paste0(name, '')))
}


# function for reading
readh <- function(name) {
  df = readRDS(here::here(push_mods,  paste0(name, '')))
  df
}

