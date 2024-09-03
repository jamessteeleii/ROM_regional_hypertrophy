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
  

  # Setup rstan to run chains in parallel
  tar_target(rstan, rstan_setup()),
  
  # Standardised Mean Change Models
  
  # Fit, check, and plot main model with uninformed priors
  tar_target(main_model_SMD, fit_main_model(data_SMD)),
  tar_target(rhat_main_model_SMD, make_rhat_plot(main_model_SMD)),
  tar_target(trace_plot_main_model_SMD, make_trace_plot(main_model_SMD)),
  tar_target(pp_check_main_model_SMD, make_pp_check(main_model_SMD)),
  tar_target(main_model_plot_preds_SMD, plot_main_model_preds_SMD(data_SMD, main_model_SMD)),
  tar_target(main_model_plot_slopes_SMD, plot_main_model_slopes_SMD(main_model_SMD)),
  tar_target(combined_main_model_plot_SMD, combine_main_model_plots(main_model_plot_preds_SMD, main_model_plot_slopes_SMD)),
  tar_target(tidy_main_model_SMD, get_tidy_model(main_model_SMD)),
  
  # Fit, check, and plot upper/lower model with uninformed priors
  tar_target(upper_lower_model_SMD, fit_upper_lower_model(data_SMD)),
  tar_target(rhat_upper_lower_model_SMD, make_rhat_plot(upper_lower_model_SMD)),
  tar_target(trace_plot_upper_lower_model_SMD, make_trace_plot(upper_lower_model_SMD)),
  tar_target(pp_check_upper_lower_model_SMD, make_pp_check(upper_lower_model_SMD)),
  tar_target(upper_lower_model_plot_preds_SMD, plot_upper_lower_model_preds_SMD(data_SMD, upper_lower_model_SMD)),
  tar_target(tidy_upper_lower_model_SMD, get_tidy_model(upper_lower_model_SMD)),
  
  # Fit, check, and plot muscle model with uninformed priors
  tar_target(muscle_model_SMD, fit_muscle_model(data_SMD)),
  tar_target(rhat_muscle_model_SMD, make_rhat_plot(muscle_model_SMD)),
  tar_target(trace_plot_muscle_model_SMD, make_trace_plot(muscle_model_SMD)),
  tar_target(pp_check_muscle_model_SMD, make_pp_check(muscle_model_SMD)),
  tar_target(muscle_model_plot_preds_SMD, plot_muscle_model_preds_SMD(data_SMD, muscle_model_SMD)),
  tar_target(tidy_muscle_model_SMD, get_tidy_model(muscle_model_SMD)),
  
  # Fit, check, and plot muscle action model with uninformed priors
  tar_target(muscle_action_model_SMD, fit_muscle_action_model(data_SMD)),
  tar_target(rhat_muscle_action_model_SMD, make_rhat_plot(muscle_action_model_SMD)),
  tar_target(trace_plot_muscle_action_model_SMD, make_trace_plot(muscle_action_model_SMD)),
  tar_target(pp_check_muscle_action_model_SMD, make_pp_check(muscle_action_model_SMD)),
  tar_target(muscle_action_model_plot_preds_SMD, plot_muscle_action_model_preds_SMD(data_SMD, muscle_action_model_SMD)),
  tar_target(tidy_muscle_action_model_SMD, get_tidy_model(muscle_action_model_SMD)),
  
  # Fit, check, and plot main model with random slopes included with uninformed priors
  tar_target(main_model_r_slopes_SMD, fit_main_model_r_slopes(data_SMD)),
  tar_target(rhat_main_model_r_slopes_SMD, make_rhat_plot(main_model_r_slopes_SMD)),
  tar_target(trace_plot_main_model_r_slopes_SMD, make_trace_plot(main_model_r_slopes_SMD)),
  tar_target(pp_check_main_model_r_slopes_SMD, make_pp_check(main_model_r_slopes_SMD)),
  tar_target(main_model_r_slopes_plot_preds_SMD, plot_main_model_preds_SMD(data_SMD, main_model_r_slopes_SMD)),
  tar_target(main_model_r_slopes_plot_slopes_SMD, plot_main_model_slopes_SMD(main_model_r_slopes_SMD)),
  tar_target(combined_main_model_r_slopes_plot_SMD, combine_main_model_plots(main_model_r_slopes_plot_preds_SMD, main_model_r_slopes_plot_slopes_SMD)),
  tar_target(tidy_main_model_r_slopes_SMD, get_tidy_model(main_model_r_slopes_SMD)),
  
  # Fit, check, and plot main model with random slopes included with Dorian Varovic and Brad Schoenfeld priors for Standarised Mean Changes
  tar_target(DV_BS_priors_SMD, set_DV_BS_priors_SMD()),
  tar_target(DV_BS_priors_only_model_SMD, fit_DV_BS_priors_only_model_SMD(data_SMD, DV_BS_priors_SMD)),
  tar_target(DV_BS_priors_plot_SMD, sample_and_plot_priors_SMD(DV_BS_priors_only_model_SMD)),
  tar_target(DV_BS_priors_model_SMD, fit_DV_BS_priors_model_SMD(data_SMD, DV_BS_priors_SMD)),
  tar_target(rhat_DV_BS_priors_model_SMD, make_rhat_plot(DV_BS_priors_model_SMD)),
  tar_target(trace_plot_DV_BS_priors_model_SMD, make_trace_plot(DV_BS_priors_model_SMD)),
  tar_target(pp_check_DV_BS_priors_model_SMD, make_pp_check(DV_BS_priors_model_SMD)),
  tar_target(DV_BS_priors_model_plot_preds_SMD, plot_main_model_preds_SMD(data_SMD, DV_BS_priors_model_SMD)),
  tar_target(DV_BS_priors_model_plot_slopes_SMD, plot_main_model_slopes_SMD(DV_BS_priors_model_SMD)),
  tar_target(combined_DV_BS_priors_model_plot_SMD, combine_main_model_plots(DV_BS_priors_model_plot_preds_SMD, DV_BS_priors_model_plot_slopes_SMD)),
  tar_target(tidy_DV_BS_priors_model_SMD, get_tidy_model(DV_BS_priors_model_SMD)),
  
  # Fit, check, and plot main model with random slopes included with Steele priors for Standarised Mean Changes
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
  
  # Compare Standardised Mean Difference Models
  tar_target(BF_model_comparisons_plot_SMD, plot_BF_model_comparisons(main_model_SMD,
                                                                      main_model_r_slopes_SMD,
                                                                      DV_BS_priors_model_SMD,
                                                                      steele_priors_model_SMD)),
  
  # Log Response Ratios Models
  
  # Fit, check, and plot main model with uninformed priors
  tar_target(main_model_lnRR, fit_main_model(data_lnRR)),
  tar_target(rhat_main_model_lnRR, make_rhat_plot(main_model_lnRR)),
  tar_target(trace_plot_main_model_lnRR, make_trace_plot(main_model_lnRR)),
  tar_target(pp_check_main_model_lnRR, make_pp_check(main_model_lnRR)),
  tar_target(main_model_plot_preds_lnRR, plot_main_model_preds_lnRR(data_lnRR, main_model_lnRR)),
  tar_target(main_model_plot_slopes_lnRR, plot_main_model_slopes_lnRR(main_model_lnRR)),
  tar_target(combined_main_model_plot_lnRR, combine_main_model_plots(main_model_plot_preds_lnRR, main_model_plot_slopes_lnRR)),
  tar_target(tidy_main_model_lnRR, get_tidy_model(main_model_lnRR)),
  
  # Fit, check, and plot upper/lower model with uninformed priors
  tar_target(upper_lower_model_lnRR, fit_upper_lower_model(data_lnRR)),
  tar_target(rhat_upper_lower_model_lnRR, make_rhat_plot(upper_lower_model_lnRR)),
  tar_target(trace_plot_upper_lower_model_lnRR, make_trace_plot(upper_lower_model_lnRR)),
  tar_target(pp_check_upper_lower_model_lnRR, make_pp_check(upper_lower_model_lnRR)),
  tar_target(upper_lower_model_plot_preds_lnRR, plot_upper_lower_model_preds_lnRR(data_lnRR, upper_lower_model_lnRR)),
  tar_target(tidy_upper_lower_model_lnRR, get_tidy_model(upper_lower_model_lnRR)),
  
  # Fit, check, and plot muscle model with uninformed priors
  tar_target(muscle_model_lnRR, fit_muscle_model(data_lnRR)),
  tar_target(rhat_muscle_model_lnRR, make_rhat_plot(muscle_model_lnRR)),
  tar_target(trace_plot_muscle_model_lnRR, make_trace_plot(muscle_model_lnRR)),
  tar_target(pp_check_muscle_model_lnRR, make_pp_check(muscle_model_lnRR)),
  tar_target(muscle_model_plot_preds_lnRR, plot_muscle_model_preds_lnRR(data_lnRR, muscle_model_lnRR)),
  tar_target(tidy_muscle_model_lnRR, get_tidy_model(muscle_model_lnRR)),
  
  # Fit, check, and plot muscle action model with uninformed priors
  tar_target(muscle_action_model_lnRR, fit_muscle_action_model(data_lnRR)),
  tar_target(rhat_muscle_action_model_lnRR, make_rhat_plot(muscle_action_model_lnRR)),
  tar_target(trace_plot_muscle_action_model_lnRR, make_trace_plot(muscle_action_model_lnRR)),
  tar_target(pp_check_muscle_action_model_lnRR, make_pp_check(muscle_action_model_lnRR)),
  tar_target(muscle_action_model_plot_preds_lnRR, plot_muscle_action_model_preds_lnRR(data_lnRR, muscle_action_model_lnRR)),
  tar_target(tidy_muscle_action_model_lnRR, get_tidy_model(muscle_action_model_lnRR)),
  
  # Fit, check, and plot main model with random slopes included with uninformed priors
  tar_target(main_model_r_slopes_lnRR, fit_main_model_r_slopes(data_lnRR)),
  tar_target(rhat_main_model_r_slopes_lnRR, make_rhat_plot(main_model_r_slopes_lnRR)),
  tar_target(trace_plot_main_model_r_slopes_lnRR, make_trace_plot(main_model_r_slopes_lnRR)),
  tar_target(pp_check_main_model_r_slopes_lnRR, make_pp_check(main_model_r_slopes_lnRR)),
  tar_target(main_model_r_slopes_plot_preds_lnRR, plot_main_model_preds_lnRR(data_lnRR, main_model_r_slopes_lnRR)),
  tar_target(main_model_r_slopes_plot_slopes_lnRR, plot_main_model_slopes_lnRR(main_model_r_slopes_lnRR)),
  tar_target(combined_main_model_r_slopes_plot_lnRR, combine_main_model_plots(main_model_r_slopes_plot_preds_lnRR, main_model_r_slopes_plot_slopes_lnRR)),
  tar_target(tidy_main_model_r_slopes_lnRR, get_tidy_model(main_model_r_slopes_lnRR)),
  
  # Fit, check, and plot main model with random slopes included with Dorian Varovic and Brad Schoenfeld priors for Log Response Ratio
  tar_target(DV_BS_priors_lnRR, set_DV_BS_priors_lnRR()),
  tar_target(DV_BS_priors_only_model_lnRR, fit_DV_BS_priors_only_model_lnRR(data_lnRR, DV_BS_priors_lnRR)),
  tar_target(DV_BS_priors_plot_lnRR, sample_and_plot_priors_lnRR(DV_BS_priors_only_model_lnRR)),
  tar_target(DV_BS_priors_model_lnRR, fit_DV_BS_priors_model_lnRR(data_lnRR, DV_BS_priors_lnRR)),
  tar_target(rhat_DV_BS_priors_model_lnRR, make_rhat_plot(DV_BS_priors_model_lnRR)),
  tar_target(trace_plot_DV_BS_priors_model_lnRR, make_trace_plot(DV_BS_priors_model_lnRR)),
  tar_target(pp_check_DV_BS_priors_model_lnRR, make_pp_check(DV_BS_priors_model_lnRR)),
  tar_target(DV_BS_priors_model_plot_preds_lnRR, plot_main_model_preds_lnRR(data_lnRR, DV_BS_priors_model_lnRR)),
  tar_target(DV_BS_priors_model_plot_slopes_lnRR, plot_main_model_slopes_lnRR(DV_BS_priors_model_lnRR)),
  tar_target(combined_DV_BS_priors_model_plot_lnRR, combine_main_model_plots(DV_BS_priors_model_plot_preds_lnRR, DV_BS_priors_model_plot_slopes_lnRR)),
  tar_target(tidy_DV_BS_priors_model_lnRR, get_tidy_model(DV_BS_priors_model_lnRR)),
  
  # Fit, check, and plot main model with random slopes included with Steele priors for Log Response Ratio
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
  
  # Compare Log Response Ratio Models
  tar_target(BF_model_comparisons_plot_lnRR, plot_BF_model_comparisons(main_model_lnRR,
                                                                      main_model_r_slopes_lnRR,
                                                                      DV_BS_priors_model_lnRR,
                                                                      steele_priors_model_lnRR)),
  
  # Make plot tiffs
  tar_target(main_model_plot_SMD_tiff, make_plot_tiff(combined_main_model_plot_SMD, 10, 5.5, "plots/main_model_SMD.tiff")),
  tar_target(main_model_r_slopes_plot_SMD_tiff, make_plot_tiff(combined_main_model_r_slopes_plot_SMD, 10, 5.5, "plots/main_model_r_slopes_SMD.tiff")),
  tar_target(upper_lower_model_plot_preds_SMD_tiff, make_plot_tiff(upper_lower_model_plot_preds_SMD, 10, 5.5, "plots/upper_lower_model_plot_preds_SMD.tiff")),
  tar_target(muscle_model_plot_preds_SMD_tiff, make_plot_tiff(muscle_model_plot_preds_SMD, 10, 5.5, "plots/muscle_model_plot_preds_SMD.tiff")),
  tar_target(muscle_action_model_plot_preds_SMD_tiff, make_plot_tiff(muscle_action_model_plot_preds_SMD, 10, 7.5, "plots/muscle_action_model_plot_preds_SMD.tiff")),
  tar_target(upper_lower_model_plot_preds_lnRR_tiff, make_plot_tiff(upper_lower_model_plot_preds_lnRR, 10, 5.5, "plots/upper_lower_model_plot_preds_lnRR.tiff")),
  tar_target(muscle_model_plot_preds_lnRR_tiff, make_plot_tiff(muscle_model_plot_preds_lnRR, 10, 5.5, "plots/muscle_model_plot_preds_lnRR.tiff")),
  tar_target(muscle_action_model_plot_preds_lnRR_tiff, make_plot_tiff(muscle_action_model_plot_preds_lnRR, 10, 7.5, "plots/muscle_action_model_plot_preds_lnRR.tiff")),
  tar_target(DV_BS_priors_plot_SMD_tiff, make_plot_tiff(DV_BS_priors_plot_SMD, 7.5, 5, "plots/DV_BS_priors_SMD.tiff")),
  tar_target(DV_BS_priors_model_plot_SMD_tiff, make_plot_tiff(combined_DV_BS_priors_model_plot_SMD, 10, 5.5, "plots/DV_BS_priors_model_SMD.tiff")),
  tar_target(steele_priors_plot_SMD_tiff, make_plot_tiff(steele_priors_plot_SMD, 7.5, 5, "plots/steele_priors_SMD.tiff")),
  tar_target(steele_priors_model_plot_SMD_tiff, make_plot_tiff(combined_steele_priors_model_plot_SMD, 10, 5.5, "plots/steele_priors_model_SMD.tiff")),
  tar_target(main_model_plot_lnRR_tiff, make_plot_tiff(combined_main_model_plot_lnRR, 10, 5.5, "plots/main_model_lnRR.tiff")),
  tar_target(main_model_r_slopes_plot_lnRR_tiff, make_plot_tiff(combined_main_model_r_slopes_plot_lnRR, 10, 5.5, "plots/main_model_r_slopes_lnRR.tiff")),
  tar_target(steele_priors_plot_lnRR_tiff, make_plot_tiff(steele_priors_plot_lnRR, 7.5, 5, "plots/steele_priors_lnRR.tiff")),
  tar_target(steele_priors_model_plot_lnRR_tiff, make_plot_tiff(combined_steele_priors_model_plot_lnRR, 10, 5.5, "plots/steele_priors_model_lnRR.tiff")),
  tar_target(DV_BS_priors_plot_lnRR_tiff, make_plot_tiff(DV_BS_priors_plot_lnRR, 7.5, 5, "plots/DV_BS_priors_lnRR.tiff")),
  tar_target(DV_BS_priors_model_plot_lnRR_tiff, make_plot_tiff(combined_DV_BS_priors_model_plot_lnRR, 10, 5.5, "plots/DV_BS_priors_model_lnRR.tiff")),
  tar_target(BF_model_comparisons_plot_SMD_tiff, make_plot_tiff(BF_model_comparisons_plot_SMD, 10, 5.5, "plots/BF_model_comparisons_plot_SMD.tiff")),
  tar_target(BF_model_comparisons_plot_lnRR_tiff, make_plot_tiff(BF_model_comparisons_plot_lnRR, 10, 5.5, "plots/BF_model_comparisons_plot_lnRR.tiff")),
  
  # Reporting
  tar_target(grateful_report, cite_packages(out.dir = ".", cite.tidyverse = TRUE, out.format = "pdf")),
  tar_quarto(diagnostic_plots, path = "plots/diagnostic_plots.qmd")
)

