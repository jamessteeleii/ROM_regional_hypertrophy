# Read in data and prepare
read_prepare_data <- function(file) {
  
  data <- read.csv(here("data", "ROM_regional_hypertrophy_data.csv"), dec = ",") |>
  
  # clean up names
  clean_names() |>
  
  # add code for each arm; note "within" designs have mutliple conditions but a single group
  mutate(arm_number = if_else(design == "Within", paste(study_number, design), paste(study_number,group)),
         arm_number = as.factor(unclass(factor(unlist(arm_number))))) |>
  
  # add code for effects
  rowid_to_column("effect_number") |>
  
  # convert means/sds to numeric
  mutate(m_pre = as.numeric(m_pre),
         m_post = as.numeric(m_post),
         sd_pre = as.numeric(sd_pre),
         sd_post = as.numeric(sd_post),
         mean_muscle_length = as.numeric(mean_muscle_length)*100
  ) |>
  
  # rescale muscle length and centre at midpoint along with site
  mutate(mean_muscle_length_centred = (mean_muscle_length)-50,
         site_centred = site-50) |>
  
  # add assumed pre-post correlation
  mutate(ri = 0.7)


# Calculate standardised effects
data <- escalc(
  measure = "SMCRH",
  m1i = m_post,
  m2i = m_pre,
  sd1i = sd_post,
  sd2i = sd_pre,
  ri = ri,
  ni = n,
  data = data
)

}

# Setup rstan to run quicker
rstan_setup <- function() {
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores() - 1)
}

# Misc functions
make_plot_tiff <- function(plot, width, height, path) {
  ggsave(
    path,
    plot,
    width = width,
    height = height,
    device = "tiff",
    dpi = 300
  )
  
}

get_tidy_model <- function(model) {
  tidy(model)
}

# Fit main model
fit_main_model <- function(data) {
  main_model_w_prior <-
    brm(
      yi | se(sqrt(vi)) ~ mean_muscle_length_centred * site_centred +
        (1 | study_number) +
        (1 | arm_number) +
        (1 | effect_number),
      data = data,
      chains = 4,
      cores = 4,
      seed = 1988,
      warmup = 2000,
      iter = 8000,
      control = list(adapt_delta = 0.99)
    )
}

# Secondary models
fit_main_model_r_slopes <- function(data) {
  main_model_w_prior <-
    brm(
      yi | se(sqrt(vi)) ~ mean_muscle_length_centred * site_centred +
        (mean_muscle_length_centred + site_centred| study_number) +
        (1 | arm_number) +
        (1 | effect_number),
      data = data,
      chains = 4,
      cores = 4,
      seed = 1988,
      warmup = 2000,
      iter = 8000,
      control = list(adapt_delta = 0.99)
    )
}

# Model checks
make_rhat_plot <- function(model) {
  mod_rhat <- enframe(brms::rhat(model)) |>
    filter(!str_detect(name, "^r_id"))
  
  rhat_main_params <- mod_rhat$value
  
  mcmc_rhat(rhat_main_params) +
    scale_x_continuous(breaks = c(1, 1.01, 1.02, 1.03, 1.04, 1.05)) +
    geom_vline(xintercept = 1.01,
               linetype = "dashed",
               alpha = 0.25)
}

make_trace_plot <- function(model) {
  plot(model)
}

make_pp_check <- function(model) {
  pp_check(model)
}