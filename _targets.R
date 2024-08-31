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
    "marginaleffects",
    "tidybayes",
    "broom.mixed",
    "patchwork"
  ),
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker"
  # controller = crew_controller_local(workers = 2, launch_max = 10)
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
  tar_target(main_model_plot_preds, plot_main_model_preds(data, main_model)),
  tar_target(main_model_plot_slopes, plot_main_model_slopes(main_model)),
  tar_target(combined_main_model_plot, combine_main_model_plots(main_model_plot_preds, main_model_plot_slopes)),
  tar_target(tidy_main_model, get_tidy_model(main_model)),
  
  # Fit, check, and plot main model with random slopes included
  tar_target(main_model_r_slopes, fit_main_model_r_slopes(data)),
  tar_target(rhat_main_model_r_slopes, make_rhat_plot(main_model_r_slopes)),
  tar_target(trace_plot_main_model_r_slopes, make_trace_plot(main_model_r_slopes)),
  tar_target(pp_check_main_model_r_slopes, make_pp_check(main_model_r_slopes)),
  tar_target(main_model_r_slopes_plot_preds, plot_main_model_preds(data, main_model_r_slopes)),
  tar_target(main_model_r_slopes_plot_slopes, plot_main_model_slopes(main_model_r_slopes)),
  tar_target(combined_main_model_r_slopes_plot, combine_main_model_plots(main_model_r_slopes_plot_preds, main_model_r_slopes_plot_slopes)),
  tar_target(tidy_main_model_r_slopes, get_tidy_model(main_model_r_slopes)),
  
  # Fit, check, and plot main model with random slopes included and Steele priors
  tar_target(steele_priors, set_steele_priors()),
  tar_target(steele_priors_only_model, fit_steele_priors_only_model(data, steele_priors)),
  tar_target(steele_priors_plot, sample_and_plot_priors(steele_priors_only_model)),
  tar_target(steele_priors_model, fit_steele_priors_model(data, steele_priors)),
  tar_target(rhat_steele_priors_model, make_rhat_plot(steele_priors_model)),
  tar_target(trace_plot_steele_priors_model, make_trace_plot(steele_priors_model)),
  tar_target(pp_check_steele_priors_model, make_pp_check(steele_priors_model)),
  tar_target(steele_priors_model_plot_preds, plot_main_model_preds(data, steele_priors_model)),
  tar_target(steele_priors_model_plot_slopes, plot_main_model_slopes(steele_priors_model)),
  tar_target(combined_steele_priors_model_plot, combine_main_model_plots(steele_priors_model_plot_preds, steele_priors_model_plot_slopes)),
  tar_target(tidy_steele_priors_model, get_tidy_model(steele_priors_model)),
  
  # Make plot tiffs
  tar_target(main_model_plot_tiff, make_plot_tiff(combined_main_model_plot, 10, 5, "plots/main_model.tiff")),
  tar_target(main_model_r_slopes_plot_tiff, make_plot_tiff(combined_main_model_r_slopes_plot, 10, 5, "plots/main_model_r_slopes.tiff")),
  tar_target(steele_priors_plot_tiff, make_plot_tiff(steele_priors_plot, 7.5, 5, "plots/steele_priors.tiff")),
  tar_target(steele_priors_model_plot_tiff, make_plot_tiff(combined_steele_priors_model_plot, 10, 5, "plots/steele_priors_model.tiff"))
  
)
