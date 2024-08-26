# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) 
library(crew)

# Location of targets functions
source("R/functions.R")

# Set targets options
tar_option_set(
  packages = c(
    "tidyverse",
    "janitor",
    "here",
    "rstan",
    "metafor",
    "brms",
    "bayesplot",
    "tidybayes"
  ),
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  controller = crew_controller_local(workers = 2, launch_max = 10)
)

# Targets list

list(
  # Load in and prepare data
  tar_target(file, here("data","ROM_regional_hypertrophy_data.csv"), format = "file"),
  tar_target(data, read_prepare_data(file)),

  # Setup rstan to run chains in parallel
  tar_target(rstan, rstan_setup()),
  
  # Fit, check, and plot main model
  tar_target(main_model, fit_main_model(data)),
  tar_target(rhat_main_model, make_rhat_plot(main_model)),
  tar_target(trace_plot_main_model, make_trace_plot(main_model)),
  tar_target(pp_check_main_model, make_pp_check(main_model)),
  
  # Fit, check, and plot main model with random slopes included
  tar_target(main_model_r_slopes, fit_main_model_r_slopes(data)),
  tar_target(rhat_main_model_r_slopes, make_rhat_plot(main_model_r_slopes)),
  tar_target(trace_plot_main_model_r_slopes, make_trace_plot(main_model_r_slopes)),
  tar_target(pp_check_main_model_r_slopes, make_pp_check(main_model_r_slopes))
)
