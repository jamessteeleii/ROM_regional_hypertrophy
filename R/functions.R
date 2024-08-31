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
  
  # rescale muscle length and centre along with site
  mutate(mean_muscle_length_centred = (mean_muscle_length-50)/100,
         site_centred = (site-50)/100) |>

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

data <- data |>
  
  # add study weights/sizes
  mutate(
    wi = 1/sqrt(vi),
    size = 0.5 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE)))

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
  model <-
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
  model <-
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

set_steele_priors <- function() {
  prior <-
    c(
      # Priors set for Intercept (overall fixed mean) and tau at study level from Steele et al., (2023) DOI: 10.1080/02640414.2023.2286748
      set_prior("student_t(3, 0.34, 0.025)", class = "b", coef = "Intercept"),
      set_prior("student_t(3, 0.21, 0.025)", class = "sd", coef = "Intercept", group = "study_number"),
      
      # Wolf et al (2023) DOI: https://doi.org/10.47206/ijsc.v3i1.182 found similar (point estimate withing 0.1 SESOI) effects for short length partial vs full ROM
      # Given constraints on interaction effects, the estimate for long vs short from this is implausbly large given general 0.34 effects for RT on hypertrophy
      # It would seem more reasonable to have more mass on a null effect, but slightly more weight towards small (<0.1) effects for muscle length
      # This prior is set such that a difference of 50% in length reflecting a typical difference in short vs long training (at a site of 50%) produces effects of the magnitudes ~0 to ~0.05
      # set_prior("skew_normal(0.01, 0.0005, 3)", class = "b", coef = "mean_muscle_length_centred",),
      set_prior("skew_normal(0.04, 0.05, 5)", class = "b", coef = "mean_muscle_length_centred",),
      
      # It is unclear (DOI: 10.1519/SSC.0000000000000574) whether hypertrophy differs by site of measurement overall 
      # Thus we set a prior centred on null effects but allowing for effects of similar magnitude (~0.05) in either direction
      # set_prior("student_t(3, 0, 0.0002)", class = "b", coef = "site_centred"),
      set_prior("student_t(3, 0, 0.02)", class = "b", coef = "site_centred"),
      
      # Lastly given interaction constraints, the interaction of length and site must necessarily be small given other priors
      # Thus we set this to have half the width of the prior for site
      # set_prior("student_t(3, 0, 0.0001)", class = "b", coef = "mean_muscle_length_centred:site_centred")
      set_prior("student_t(3, 0, 0.01)", class = "b", coef = "mean_muscle_length_centred:site_centred")
      
      # All other priors for variance parameters are kept as default weakly regularising
    )
}

fit_steele_priors_only_model <- function(data, prior) {
  model <-
    brm(yi | se(sqrt(vi)) ~ 0 + Intercept + mean_muscle_length_centred * site_centred +
          (mean_muscle_length_centred + site_centred | study_number) +
          (1 | arm_number) +
          (1 | effect_number),
        data = data,
        prior = prior,
        chains = 4,
        cores = 4,
        seed = 1988,
        warmup = 2000,
        iter = 8000,
        control = list(adapt_delta = 0.99),
        sample_prior = "only"
    )
}

sample_and_plot_priors <- function(model) {
  
  model |>
    gather_draws(b_Intercept, b_mean_muscle_length_centred, b_site_centred, `b_mean_muscle_length_centred:site_centred`) |>
    mutate(.value = case_when(
      .variable == "b_mean_muscle_length_centred" ~ .value/2,
      .variable == "b_site_centred" ~ .value/2,
      .variable == "b_mean_muscle_length_centred:site_centred" ~ .value/2,
      .variable == "b_Intercept" ~ .value
      
    )) |>
    ggplot(aes(x=.value)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.75) +
    stat_slab(alpha = 0.75) + 
    facet_wrap(".variable") +
    scale_x_continuous(limits = c(-0.1,0.4), breaks = seq(-0.5,0.5, length = 11)) +
    labs(
      x = "Model Coefficients",
      y = "Density",
      title = "Prior distribution for fixed (i.e., population level) effects",
      subtitle = "Note, with the exception of the Intercept, coefficients have been halved to reflect a typical short vs long length/site difference of 50%"
    ) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          plot.subtitle = element_text(size = 8))
}

fit_steele_priors_model <- function(data, prior) {
  
  # Set initial values to improve chain convergence
  set_inits <- function(seed = 1) {
    
    set.seed(seed)
    list(
      beta = rnorm(n = 1, mean = 0.3, sd = 0.1),
      sd = runif(n = 1, min = 0, max = 1)
    )
    
  }
  
  list_of_inits <- list(
    # different seed values will return different results
    set_inits(seed = 1),
    set_inits(seed = 2),
    set_inits(seed = 3),
    set_inits(seed = 4)
  )
  
  model <-
    brm(yi | se(sqrt(vi)) ~ 0 + Intercept + mean_muscle_length_centred * site_centred +
          (mean_muscle_length_centred + site_centred | study_number) +
          (1 | arm_number) +
          (1 | effect_number),
        data = data,
        prior = prior,
        chains = 4,
        cores = 4,
        seed = 1988,
        warmup = 2000,
        iter = 8000,
        init = list_of_inits,
        control = list(adapt_delta = 0.99)
    )
}

# Plot models
plot_main_model_preds <- function(data, model) {
  
  # Interaction plot
  preds <- crossing(
    mean_muscle_length_centred = seq(-0.35, 0.35, length = 101),
    site_centred = c(-0.25,0,0.25),
    vi = 0
  ) |>
    add_epred_draws(model, re_formula = NA)
  
  summary <- preds |>
    group_by(mean_muscle_length_centred, site_centred) |>
    mean_qi() 
  
  summary |>
    ggplot(aes(x = (mean_muscle_length_centred*100)+50, y = .epred)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.75) +
    geom_ribbon(aes(ymin = .epred.lower, ymax = .epred.upper,
                    group = as.factor((site_centred*100)+50), fill = (site_centred*100)+50),
                alpha = 0.5, color = "black", size = 0.25) +
    geom_line(aes(y = .epred,
                  group = as.factor((site_centred*100)+50)), size = 1, color = "black") +
    geom_line(aes(y = .epred,
                  group = as.factor((site_centred*100)+50), color = (site_centred*100)+50)) +
    geom_point(data = data,
               aes(y = yi, size = size + 0.25), color = "black", fill = NA, shape = 21, alpha = 0.5) +
    geom_point(data = data,
               aes(y = yi, size = size, color = (site_centred*100)+50), alpha = 0.5) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    scale_x_continuous(breaks = c(0,25,50,75,100)) +
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5)) +
    guides(
      size = "none",
      color = "none"
    ) +
    labs(
      x = "Mean Muscle Length (%)",
      y = "Standardised Mean Change",
      fill = "Site of Measurement (%)"
      ) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          legend.position = "bottom",
          axis.title = element_text(size=10))
  
}

plot_main_model_slopes <- function(model) {
  
  # Interaction plot - slopes
  slopes <- avg_slopes(
    model,
    variables = "mean_muscle_length_centred",
    newdata = datagrid(
      mean_muscle_length_centred = seq(-0.35, 0.35, length = 101),
      site_centred = c(-0.25,0,0.25),
      vi = 0
    ),
    by = "site_centred"
  ) |>
    posterior_draws()
  
  
  rope_percents <- tibble(
    site_centred = c(-0.25,0,0.25),
    rope_percent = c(
      bayestestR::rope(filter(slopes, site_centred == -0.25)$draw/2, range = c(-0.1, 0.1))$ROPE_Percentage,
      bayestestR::rope(filter(slopes, site_centred == 0)$draw/2, range = c(-0.1, 0.1))$ROPE_Percentage,
      bayestestR::rope(filter(slopes, site_centred == 0.25)$draw/2, range = c(-0.1, 0.1))$ROPE_Percentage
    )
  )
  
  summary <- avg_slopes(
    model,
    variables = "mean_muscle_length_centred",
    newdata = datagrid(
      mean_muscle_length_centred = seq(-0.35, 0.35, length = 101),
      site_centred = c(-0.25,0,0.25),
      vi = 0
    ),
    by = "site_centred"
  )
  
  slopes |>
    ggplot(aes(x = (site_centred*100)+50, y = draw/2)) +
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
    geom_hline(yintercept = c(-0.1,0.1), linetype = "dashed") +
    stat_slabinterval(aes(fill = (site_centred*100)+50), alpha = 0.75) +
    annotate("text", (summary$site_centred*100)+45, summary$estimate/2, label = round(summary$estimate/2,2), size = 3) +
    annotate("text", (summary$site_centred*100)+45, summary$conf.low/2, label = round(summary$conf.low/2,2), size = 3) +
    annotate("text", (summary$site_centred*100)+45, summary$conf.high/2, label = round(summary$conf.high/2,2), size = 3) +
    annotate("rect", xmin = 10, xmax = 90, ymin = 0.825, ymax = 1.125, color = "black", fill = "white") +
    annotate("text", (rope_percents$site_centred*100)+50, 0.9, label = glue::glue("{round(rope_percents$rope_percent*100,2)}%"), size = 3) +
    annotate("text", x=50, y=1.05, label="Percentage of Posterior Distibution within ROPE [-0.1,0.1]", size = 3) +
    scale_fill_viridis_c() +
    scale_x_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) +
    scale_y_continuous(breaks = c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1), limits = c(-1.15,1.15)) +
    guides(
      color = "none"
    ) +
    labs(
      x = "Site of Measurement (%)",
      y = "Slope for Muscle Length",
      fill = "Site of Measurement (%)"
    ) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          axis.title = element_text(size=10))
  
  
}

combine_main_model_plots <- function(preds_plot, slopes_plot) {
  (preds_plot + slopes_plot) +
    plot_annotation(
      title = "Interaction between mean muscle length and site of measurement",
      subtitle = "Global grand mean and 95% quantile intervals presented for predictions and slopes at 25%, 50%, and 75% of centred site of measurement",
      caption = "Note, the slopes have been transformed to the effect when increasing muscle length by 50% to reflect typical difference between short vs long lengths"
    ) + 
    plot_layout(guides = "collect") & theme(legend.position = "bottom")
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