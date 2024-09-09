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
    "insight",
    "patchwork",
    "bayestestR",
    "quarto",
    "grateful"
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
  tar_target(data_SMD, read_prepare_data_SMD(file)),
  tar_target(data_lnRR, read_prepare_data_lnRR(file)),
  
  # Load in and prepare Wolf et al. (2023) data
  tar_target(file_wolf, here("data","wolf_et_al_meta_data.csv"), format = "file"),
  tar_target(data_wolf_SMD, read_prepare_wolf_data_SMD(file_wolf)),
  tar_target(data_wolf_lnRR, read_prepare_wolf_data_lnRR(file_wolf)),
  

  # Setup rstan to run chains in parallel
  tar_target(rstan, rstan_setup()),
  
  # Standardised Mean Change Models
  
  # Fit, check, and plot pre-registered model with random slopes included with Wolf et al (2023) informed priors for Standarised Mean Changes
  tar_target(wolf_priors_SMD, set_wolf_priors_SMD(data_wolf_SMD)),
  tar_target(wolf_priors_only_model_SMD, fit_wolf_priors_only_model_SMD(data_SMD, wolf_priors_SMD)),
  tar_target(wolf_priors_plot_SMD, sample_and_plot_priors_SMD(wolf_priors_only_model_SMD)),
  tar_target(wolf_priors_model_SMD, fit_wolf_priors_model_SMD(data_SMD, wolf_priors_SMD)),
  tar_target(rhat_wolf_priors_model_SMD, make_rhat_plot(wolf_priors_model_SMD)),
  tar_target(trace_plot_wolf_priors_model_SMD, make_trace_plot(wolf_priors_model_SMD)),
  tar_target(pp_check_wolf_priors_model_SMD, make_pp_check(wolf_priors_model_SMD)),
  tar_target(wolf_priors_model_plot_preds_SMD, plot_main_model_preds_SMD(data_SMD, wolf_priors_model_SMD)),
  tar_target(wolf_priors_model_plot_slopes_SMD, plot_main_model_slopes_SMD(wolf_priors_model_SMD)),
  tar_target(combined_wolf_priors_model_plot_SMD, combine_main_model_plots(wolf_priors_model_plot_preds_SMD, wolf_priors_model_plot_slopes_SMD)),
  tar_target(tidy_wolf_priors_model_SMD, get_tidy_model(wolf_priors_model_SMD)),

  # Fit, check, and plot upper/lower model with uninformed priors
  tar_target(upper_lower_model_SMD, fit_upper_lower_model(data_SMD,wolf_priors_SMD)),
  tar_target(rhat_upper_lower_model_SMD, make_rhat_plot(upper_lower_model_SMD)),
  tar_target(trace_plot_upper_lower_model_SMD, make_trace_plot(upper_lower_model_SMD)),
  tar_target(pp_check_upper_lower_model_SMD, make_pp_check(upper_lower_model_SMD)),
  tar_target(upper_lower_model_plot_preds_SMD, plot_upper_lower_model_preds_SMD(data_SMD, upper_lower_model_SMD)),
  tar_target(tidy_upper_lower_model_SMD, get_tidy_model(upper_lower_model_SMD)),
  
  # Fit, check, and plot muscle model with uninformed priors
  tar_target(muscle_model_SMD, fit_muscle_model(data_SMD,wolf_priors_SMD)),
  tar_target(rhat_muscle_model_SMD, make_rhat_plot(muscle_model_SMD)),
  tar_target(trace_plot_muscle_model_SMD, make_trace_plot(muscle_model_SMD)),
  tar_target(pp_check_muscle_model_SMD, make_pp_check(muscle_model_SMD)),
  tar_target(muscle_model_plot_preds_SMD, plot_muscle_model_preds_SMD(data_SMD, muscle_model_SMD)),
  tar_target(tidy_muscle_model_SMD, get_tidy_model(muscle_model_SMD)),
  
  # Fit, check, and plot muscle action model with uninformed priors
  tar_target(muscle_action_model_SMD, fit_muscle_action_model(data_SMD,wolf_priors_SMD)),
  tar_target(rhat_muscle_action_model_SMD, make_rhat_plot(muscle_action_model_SMD)),
  tar_target(trace_plot_muscle_action_model_SMD, make_trace_plot(muscle_action_model_SMD)),
  tar_target(pp_check_muscle_action_model_SMD, make_pp_check(muscle_action_model_SMD)),
  tar_target(muscle_action_model_plot_preds_SMD, plot_muscle_action_model_preds_SMD(data_SMD, muscle_action_model_SMD)),
  tar_target(tidy_muscle_action_model_SMD, get_tidy_model(muscle_action_model_SMD)),
  
  # Fit, check, and plot uninformed model with uninformed priors
  tar_target(uninformed_model_SMD, fit_uninformed_model(data_SMD)),
  tar_target(rhat_uninformed_model_SMD, make_rhat_plot(uninformed_model_SMD)),
  tar_target(trace_plot_uninformed_model_SMD, make_trace_plot(uninformed_model_SMD)),
  tar_target(pp_check_uninformed_model_SMD, make_pp_check(uninformed_model_SMD)),
  tar_target(uninformed_model_plot_preds_SMD, plot_main_model_preds_SMD(data_SMD, uninformed_model_SMD)),
  tar_target(uninformed_model_plot_slopes_SMD, plot_main_model_slopes_SMD(uninformed_model_SMD)),
  tar_target(combined_uninformed_model_plot_SMD, combine_main_model_plots(uninformed_model_plot_preds_SMD, uninformed_model_plot_slopes_SMD)),
  tar_target(tidy_uninformed_model_SMD, get_tidy_model(uninformed_model_SMD)),
  
  # Fit, check, and plot pre-registered model with random slopes included with Wolf et al (2023) informed priors for Standarised Mean Changes
  tar_target(wolf_priors_model_slopes_SMD, fit_wolf_priors_model_slopes_SMD(data_SMD, wolf_priors_SMD)),
  tar_target(rhat_wolf_priors_model_slopes_SMD, make_rhat_plot(wolf_priors_model_slopes_SMD)),
  tar_target(trace_plot_wolf_priors_model_slopes_SMD, make_trace_plot(wolf_priors_model_slopes_SMD)),
  tar_target(pp_check_wolf_priors_model_slopes_SMD, make_pp_check(wolf_priors_model_slopes_SMD)),
  tar_target(wolf_priors_model_slopes_plot_preds_SMD, plot_main_model_preds_SMD(data_SMD, wolf_priors_model_slopes_SMD)),
  tar_target(wolf_priors_model_slopes_plot_slopes_SMD, plot_main_model_slopes_SMD(wolf_priors_model_slopes_SMD)),
  tar_target(combined_wolf_priors_model_slopes_plot_SMD, combine_main_model_plots(wolf_priors_model_slopes_plot_preds_SMD, wolf_priors_model_slopes_plot_slopes_SMD)),
  tar_target(tidy_wolf_priors_model_slopes_SMD, get_tidy_model(wolf_priors_model_slopes_SMD)),
  
  # Fit, check, and plot uninformed model with random slopes included with James Steele's informed priors for Standarised Mean Changes
  tar_target(wolf_steele_priors_only_model_SMD, fit_wolf_steele_priors_only_model_SMD(data_wolf_SMD)),
  tar_target(wolf_steele_priors_model_SMD, fit_wolf_steele_priors_model_SMD(data_wolf_SMD)),
  tar_target(comparison_wolf_steele_priors_SMD, compare_wolf_steele_priors_SMD(wolf_steele_priors_only_model_SMD,wolf_steele_priors_model_SMD)),
  tar_target(steele_priors_SMD, set_steele_priors_SMD()),
  tar_target(steele_priors_only_model_SMD, fit_steele_priors_only_model_SMD(data_SMD, steele_priors_SMD)),
  tar_target(steele_priors_plot_SMD, sample_and_plot_priors_SMD(steele_priors_only_model_SMD)),
  tar_target(steele_priors_model_SMD, fit_steele_priors_model_SMD(data_SMD, steele_priors_SMD)),
  tar_target(rhat_steele_priors_model_SMD, make_rhat_plot(steele_priors_model_SMD)),
  tar_target(trace_plot_steele_priors_model_SMD, make_trace_plot(steele_priors_model_SMD)),
  tar_target(pp_check_steele_priors_model_SMD, make_pp_check(steele_priors_model_SMD)),
  tar_target(steele_priors_model_plot_preds_SMD, plot_main_model_preds_SMD(data_SMD, steele_priors_model_SMD)),
  tar_target(steele_priors_model_plot_slopes_SMD, plot_main_model_slopes_SMD(steele_priors_model_SMD)),
  tar_target(combined_steele_priors_model_plot_SMD, combine_main_model_plots(steele_priors_model_plot_preds_SMD, steele_priors_model_plot_slopes_SMD)),
  tar_target(tidy_steele_priors_model_SMD, get_tidy_model(steele_priors_model_SMD)),
  
  # Fit, check, and plot uninformed model with random slopes included with other authors informed priors for Standarised Mean Changes
  tar_target(authors_priors_SMD, set_authors_priors_SMD()),
  tar_target(authors_priors_only_model_SMD, fit_authors_priors_only_model_SMD(data_SMD, authors_priors_SMD)),
  tar_target(authors_priors_plot_SMD, sample_and_plot_priors_SMD(authors_priors_only_model_SMD)),
  tar_target(authors_priors_model_SMD, fit_authors_priors_model_SMD(data_SMD, authors_priors_SMD)),
  tar_target(rhat_authors_priors_model_SMD, make_rhat_plot(authors_priors_model_SMD)),
  tar_target(trace_plot_authors_priors_model_SMD, make_trace_plot(authors_priors_model_SMD)),
  tar_target(pp_check_authors_priors_model_SMD, make_pp_check(authors_priors_model_SMD)),
  tar_target(authors_priors_model_plot_preds_SMD, plot_main_model_preds_SMD(data_SMD, authors_priors_model_SMD)),
  tar_target(authors_priors_model_plot_slopes_SMD, plot_main_model_slopes_SMD(authors_priors_model_SMD)),
  tar_target(combined_authors_priors_model_plot_SMD, combine_main_model_plots(authors_priors_model_plot_preds_SMD, authors_priors_model_plot_slopes_SMD)),
  tar_target(tidy_authors_priors_model_SMD, get_tidy_model(authors_priors_model_SMD)),
  
  # Compare Standardised Mean Difference Models
  tar_target(BF_model_comparisons_plot_SMD, plot_BF_model_comparisons(wolf_priors_model_SMD,
                                                                      uninformed_model_SMD,
                                                                      wolf_priors_model_slopes_SMD,
                                                                      steele_priors_model_SMD,
                                                                      authors_priors_model_SMD)),
  
  # Log Response Ratios Models
  
  # Fit, check, and plot pre-registered model with random slopes included with Wolf et al (2023) informed priors for Standarised Mean Changes
  tar_target(wolf_priors_lnRR, set_wolf_priors_lnRR(data_wolf_lnRR)),
  tar_target(wolf_priors_only_model_lnRR, fit_wolf_priors_only_model_lnRR(data_lnRR, wolf_priors_lnRR)),
  tar_target(wolf_priors_plot_lnRR, sample_and_plot_priors_lnRR(wolf_priors_only_model_lnRR)),
  tar_target(wolf_priors_model_lnRR, fit_wolf_priors_model_lnRR(data_lnRR, wolf_priors_lnRR)),
  tar_target(rhat_wolf_priors_model_lnRR, make_rhat_plot(wolf_priors_model_lnRR)),
  tar_target(trace_plot_wolf_priors_model_lnRR, make_trace_plot(wolf_priors_model_lnRR)),
  tar_target(pp_check_wolf_priors_model_lnRR, make_pp_check(wolf_priors_model_lnRR)),
  tar_target(wolf_priors_model_plot_preds_lnRR, plot_main_model_preds_lnRR(data_lnRR, wolf_priors_model_lnRR)),
  tar_target(wolf_priors_model_plot_slopes_lnRR, plot_main_model_slopes_lnRR(wolf_priors_model_lnRR)),
  tar_target(combined_wolf_priors_model_plot_lnRR, combine_main_model_plots(wolf_priors_model_plot_preds_lnRR, wolf_priors_model_plot_slopes_lnRR)),
  tar_target(tidy_wolf_priors_model_lnRR, get_tidy_model(wolf_priors_model_lnRR)),
  
  # Fit, check, and plot upper/lower model with uninformed priors
  tar_target(upper_lower_model_lnRR, fit_upper_lower_model(data_lnRR,wolf_priors_lnRR)),
  tar_target(rhat_upper_lower_model_lnRR, make_rhat_plot(upper_lower_model_lnRR)),
  tar_target(trace_plot_upper_lower_model_lnRR, make_trace_plot(upper_lower_model_lnRR)),
  tar_target(pp_check_upper_lower_model_lnRR, make_pp_check(upper_lower_model_lnRR)),
  tar_target(upper_lower_model_plot_preds_lnRR, plot_upper_lower_model_preds_lnRR(data_lnRR, upper_lower_model_lnRR)),
  tar_target(tidy_upper_lower_model_lnRR, get_tidy_model(upper_lower_model_lnRR)),
  
  # Fit, check, and plot muscle model with uninformed priors
  tar_target(muscle_model_lnRR, fit_muscle_model(data_lnRR,wolf_priors_lnRR)),
  tar_target(rhat_muscle_model_lnRR, make_rhat_plot(muscle_model_lnRR)),
  tar_target(trace_plot_muscle_model_lnRR, make_trace_plot(muscle_model_lnRR)),
  tar_target(pp_check_muscle_model_lnRR, make_pp_check(muscle_model_lnRR)),
  tar_target(muscle_model_plot_preds_lnRR, plot_muscle_model_preds_lnRR(data_lnRR, muscle_model_lnRR)),
  tar_target(tidy_muscle_model_lnRR, get_tidy_model(muscle_model_lnRR)),
  
  # Fit, check, and plot muscle action model with uninformed priors
  tar_target(muscle_action_model_lnRR, fit_muscle_action_model(data_lnRR,wolf_priors_lnRR)),
  tar_target(rhat_muscle_action_model_lnRR, make_rhat_plot(muscle_action_model_lnRR)),
  tar_target(trace_plot_muscle_action_model_lnRR, make_trace_plot(muscle_action_model_lnRR)),
  tar_target(pp_check_muscle_action_model_lnRR, make_pp_check(muscle_action_model_lnRR)),
  tar_target(muscle_action_model_plot_preds_lnRR, plot_muscle_action_model_preds_lnRR(data_lnRR, muscle_action_model_lnRR)),
  tar_target(tidy_muscle_action_model_lnRR, get_tidy_model(muscle_action_model_lnRR)),
  
  # Fit, check, and plot uninformed model with uninformed priors
  tar_target(uninformed_model_lnRR, fit_uninformed_model(data_lnRR)),
  tar_target(rhat_uninformed_model_lnRR, make_rhat_plot(uninformed_model_lnRR)),
  tar_target(trace_plot_uninformed_model_lnRR, make_trace_plot(uninformed_model_lnRR)),
  tar_target(pp_check_uninformed_model_lnRR, make_pp_check(uninformed_model_lnRR)),
  tar_target(uninformed_model_plot_preds_lnRR, plot_main_model_preds_lnRR(data_lnRR, uninformed_model_lnRR)),
  tar_target(uninformed_model_plot_slopes_lnRR, plot_main_model_slopes_lnRR(uninformed_model_lnRR)),
  tar_target(combined_uninformed_model_plot_lnRR, combine_main_model_plots(uninformed_model_plot_preds_lnRR, uninformed_model_plot_slopes_lnRR)),
  tar_target(tidy_uninformed_model_lnRR, get_tidy_model(uninformed_model_lnRR)),
  
  # Fit, check, and plot pre-registered model with random slopes included with Wolf et al (2023) informed priors for Standarised Mean Changes
  tar_target(wolf_priors_model_slopes_lnRR, fit_wolf_priors_model_slopes_lnRR(data_lnRR, wolf_priors_lnRR)),
  tar_target(rhat_wolf_priors_model_slopes_lnRR, make_rhat_plot(wolf_priors_model_slopes_lnRR)),
  tar_target(trace_plot_wolf_priors_model_slopes_lnRR, make_trace_plot(wolf_priors_model_slopes_lnRR)),
  tar_target(pp_check_wolf_priors_model_slopes_lnRR, make_pp_check(wolf_priors_model_slopes_lnRR)),
  tar_target(wolf_priors_model_slopes_plot_preds_lnRR, plot_main_model_preds_lnRR(data_lnRR, wolf_priors_model_slopes_lnRR)),
  tar_target(wolf_priors_model_slopes_plot_slopes_lnRR, plot_main_model_slopes_lnRR(wolf_priors_model_slopes_lnRR)),
  tar_target(combined_wolf_priors_model_slopes_plot_lnRR, combine_main_model_plots(wolf_priors_model_slopes_plot_preds_lnRR, wolf_priors_model_slopes_plot_slopes_lnRR)),
  tar_target(tidy_wolf_priors_model_slopes_lnRR, get_tidy_model(wolf_priors_model_slopes_lnRR)),
  
  # Fit, check, and plot uninformed model with random slopes included with James Steele's informed priors for Standarised Mean Changes
  tar_target(wolf_steele_priors_only_model_lnRR, fit_wolf_steele_priors_only_model_lnRR(data_wolf_lnRR)),
  tar_target(wolf_steele_priors_model_lnRR, fit_wolf_steele_priors_model_lnRR(data_wolf_lnRR)),
  tar_target(comparison_wolf_steele_priors_lnRR, compare_wolf_steele_priors_lnRR(wolf_steele_priors_only_model_lnRR,wolf_steele_priors_model_lnRR)),
  tar_target(steele_priors_lnRR, set_steele_priors_lnRR()),
  tar_target(steele_priors_only_model_lnRR, fit_steele_priors_only_model_lnRR(data_lnRR, steele_priors_lnRR)),
  tar_target(steele_priors_plot_lnRR, sample_and_plot_priors_lnRR(steele_priors_only_model_lnRR)),
  tar_target(steele_priors_model_lnRR, fit_steele_priors_model_lnRR(data_lnRR, steele_priors_lnRR)),
  tar_target(rhat_steele_priors_model_lnRR, make_rhat_plot(steele_priors_model_lnRR)),
  tar_target(trace_plot_steele_priors_model_lnRR, make_trace_plot(steele_priors_model_lnRR)),
  tar_target(pp_check_steele_priors_model_lnRR, make_pp_check(steele_priors_model_lnRR)),
  tar_target(steele_priors_model_plot_preds_lnRR, plot_main_model_preds_lnRR(data_lnRR, steele_priors_model_lnRR)),
  tar_target(steele_priors_model_plot_slopes_lnRR, plot_main_model_slopes_lnRR(steele_priors_model_lnRR)),
  tar_target(combined_steele_priors_model_plot_lnRR, combine_main_model_plots(steele_priors_model_plot_preds_lnRR, steele_priors_model_plot_slopes_lnRR)),
  tar_target(tidy_steele_priors_model_lnRR, get_tidy_model(steele_priors_model_lnRR)),
  
  # Fit, check, and plot uninformed model with random slopes included with other authors informed priors for Standarised Mean Changes
  tar_target(authors_priors_lnRR, set_authors_priors_lnRR()),
  tar_target(authors_priors_only_model_lnRR, fit_authors_priors_only_model_lnRR(data_lnRR, authors_priors_lnRR)),
  tar_target(authors_priors_plot_lnRR, sample_and_plot_priors_lnRR(authors_priors_only_model_lnRR)),
  tar_target(authors_priors_model_lnRR, fit_authors_priors_model_lnRR(data_lnRR, authors_priors_lnRR)),
  tar_target(rhat_authors_priors_model_lnRR, make_rhat_plot(authors_priors_model_lnRR)),
  tar_target(trace_plot_authors_priors_model_lnRR, make_trace_plot(authors_priors_model_lnRR)),
  tar_target(pp_check_authors_priors_model_lnRR, make_pp_check(authors_priors_model_lnRR)),
  tar_target(authors_priors_model_plot_preds_lnRR, plot_main_model_preds_lnRR(data_lnRR, authors_priors_model_lnRR)),
  tar_target(authors_priors_model_plot_slopes_lnRR, plot_main_model_slopes_lnRR(authors_priors_model_lnRR)),
  tar_target(combined_authors_priors_model_plot_lnRR, combine_main_model_plots(authors_priors_model_plot_preds_lnRR, authors_priors_model_plot_slopes_lnRR)),
  tar_target(tidy_authors_priors_model_lnRR, get_tidy_model(authors_priors_model_lnRR)),
  
  # Compare Log Response Ratio Models
  tar_target(BF_model_comparisons_plot_lnRR, plot_BF_model_comparisons(wolf_priors_model_lnRR,
                                                                      uninformed_model_lnRR,
                                                                      wolf_priors_model_slopes_lnRR,
                                                                      steele_priors_model_lnRR,
                                                                      authors_priors_model_lnRR)),
  
  # Make plot tiffs
  tar_target(wolf_priors_plot_SMD_tiff, make_plot_tiff(wolf_priors_plot_SMD, 7.5, 5, "plots/wolf_priors_SMD.tiff")),
  tar_target(wolf_priors_model_plot_SMD_tiff, make_plot_tiff(combined_wolf_priors_model_plot_SMD, 10, 5.5, "plots/wolf_priors_model_SMD.tiff")),
  tar_target(upper_lower_model_plot_preds_SMD_tiff, make_plot_tiff(upper_lower_model_plot_preds_SMD, 10, 5.5, "plots/upper_lower_model_plot_preds_SMD.tiff")),
  tar_target(muscle_model_plot_preds_SMD_tiff, make_plot_tiff(muscle_model_plot_preds_SMD, 10, 5.5, "plots/muscle_model_plot_preds_SMD.tiff")),
  tar_target(muscle_action_model_plot_preds_SMD_tiff, make_plot_tiff(muscle_action_model_plot_preds_SMD, 10, 7.5, "plots/muscle_action_model_plot_preds_SMD.tiff")),
  tar_target(uninformed_model_plot_SMD_tiff, make_plot_tiff(combined_uninformed_model_plot_SMD, 10, 5.5, "plots/uninformed_model_SMD.tiff")),
  tar_target(wolf_priors_model_slopes_plot_SMD_tiff, make_plot_tiff(combined_wolf_priors_model_slopes_plot_SMD, 10, 5.5, "plots/wolf_priors_model_slopes_SMD.tiff")),
  tar_target(authors_priors_plot_SMD_tiff, make_plot_tiff(authors_priors_plot_SMD, 7.5, 5, "plots/authors_priors_SMD.tiff")),
  tar_target(authors_priors_model_plot_SMD_tiff, make_plot_tiff(combined_authors_priors_model_plot_SMD, 10, 5.5, "plots/authors_priors_model_SMD.tiff")),
  tar_target(comparison_wolf_steele_priors_SMD_tiff, make_plot_tiff(comparison_wolf_steele_priors_SMD, 9, 5, "plots/comparison_wolf_steele_priors_SMD.tiff")),
  tar_target(steele_priors_plot_SMD_tiff, make_plot_tiff(steele_priors_plot_SMD, 7.5, 5, "plots/steele_priors_SMD.tiff")),
  tar_target(steele_priors_model_plot_SMD_tiff, make_plot_tiff(combined_steele_priors_model_plot_SMD, 10, 5.5, "plots/steele_priors_model_SMD.tiff")),
  tar_target(BF_model_comparisons_plot_SMD_tiff, make_plot_tiff(BF_model_comparisons_plot_SMD, 10, 5.5, "plots/BF_model_comparisons_plot_SMD.tiff")),
  
  tar_target(wolf_priors_plot_lnRR_tiff, make_plot_tiff(wolf_priors_plot_lnRR, 7.5, 5, "plots/wolf_priors_lnRR.tiff")),
  tar_target(wolf_priors_model_plot_lnRR_tiff, make_plot_tiff(combined_wolf_priors_model_plot_lnRR, 10, 5.5, "plots/wolf_priors_model_lnRR.tiff")),
  tar_target(upper_lower_model_plot_preds_lnRR_tiff, make_plot_tiff(upper_lower_model_plot_preds_lnRR, 10, 5.5, "plots/upper_lower_model_plot_preds_lnRR.tiff")),
  tar_target(muscle_model_plot_preds_lnRR_tiff, make_plot_tiff(muscle_model_plot_preds_lnRR, 10, 5.5, "plots/muscle_model_plot_preds_lnRR.tiff")),
  tar_target(muscle_action_model_plot_preds_lnRR_tiff, make_plot_tiff(muscle_action_model_plot_preds_lnRR, 10, 7.5, "plots/muscle_action_model_plot_preds_lnRR.tiff")),
  tar_target(uninformed_model_plot_lnRR_tiff, make_plot_tiff(combined_uninformed_model_plot_lnRR, 10, 5.5, "plots/uninformed_model_lnRR.tiff")),
  tar_target(wolf_priors_model_slopes_plot_lnRR_tiff, make_plot_tiff(combined_wolf_priors_model_slopes_plot_lnRR, 10, 5.5, "plots/wolf_priors_model_slopes_lnRR.tiff")),
  tar_target(authors_priors_plot_lnRR_tiff, make_plot_tiff(authors_priors_plot_lnRR, 7.5, 5, "plots/authors_priors_lnRR.tiff")),
  tar_target(authors_priors_model_plot_lnRR_tiff, make_plot_tiff(combined_authors_priors_model_plot_lnRR, 10, 5.5, "plots/authors_priors_model_lnRR.tiff")),
  tar_target(comparison_wolf_steele_priors_lnRR_tiff, make_plot_tiff(comparison_wolf_steele_priors_lnRR, 9, 5, "plots/comparison_wolf_steele_priors_lnRR.tiff")),
  tar_target(steele_priors_plot_lnRR_tiff, make_plot_tiff(steele_priors_plot_lnRR, 7.5, 5, "plots/steele_priors_lnRR.tiff")),
  tar_target(steele_priors_model_plot_lnRR_tiff, make_plot_tiff(combined_steele_priors_model_plot_lnRR, 10, 5.5, "plots/steele_priors_model_lnRR.tiff")),
  tar_target(BF_model_comparisons_plot_lnRR_tiff, make_plot_tiff(BF_model_comparisons_plot_lnRR, 10, 5.5, "plots/BF_model_comparisons_plot_lnRR.tiff")),
  
  
  # Reporting
  tar_target(grateful_report, cite_packages(out.dir = ".", cite.tidyverse = TRUE, out.format = "pdf")),
  tar_quarto(wolf_priors_model_diagnostic_plots, path = "plots/wolf_priors_model_diagnostic_plots.qmd"),
  tar_quarto(uninformed_model_diagnostic_plots, path = "plots/uninformed_model_diagnostic_plots.qmd"),
  tar_quarto(wolf_priors_model_slopes_diagnostic_plots, path = "plots/wolf_priors_model_slopes_diagnostic_plots.qmd"),
  tar_quarto(steele_priors_model_diagnostic_plots, path = "plots/steele_priors_model_diagnostic_plots.qmd"),
  tar_quarto(authors_priors_model_diagnostic_plots, path = "plots/authors_priors_model_diagnostic_plots.qmd")
  # tar_quarto(analysis_results, path = "analysis_results.qmd")
  
)

