# Read in data and prepare
read_prepare_data_SMD <- function(file) {
  
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

read_prepare_data_lnRR <- function(file) {
  
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
    measure = "ROMC",
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
      control = list(adapt_delta = 0.99),
      save_pars = save_pars(all = TRUE)
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
      control = list(adapt_delta = 0.99),
      save_pars = save_pars(all = TRUE)
    )
}

fit_upper_lower_model <- function(data) {
  
  # assigning the deviation contrasts
  
  data$body_region <- as.factor(data$body_region)
  
  contrasts(data$body_region) = contr.sum(length(unique(data$body_region)))
  
  model <-
    brm(
      yi | se(sqrt(vi)) ~ mean_muscle_length_centred * site_centred * body_region +
        (1 | study_number) +
        (1 | arm_number) +
        (1 | effect_number),
      data = data,
      chains = 4,
      cores = 4,
      seed = 1988,
      warmup = 2000,
      iter = 8000,
      control = list(adapt_delta = 0.99),
      save_pars = save_pars(all = TRUE)
    )
}

fit_muscle_model <- function(data) {
  
  data <- data |>
    
    # relabel secondary predictors
    mutate(muscle = case_when(
      muscle == "VL" ~ "Vastus Lateralis",
      muscle == "BFL" ~ "Biceps femoris Long Head",
      muscle == "ST" ~ "Semitendinosus",
      muscle == "RF" ~ "Rectus Femoris",
      muscle == "VM" ~ "Vastus Medialis",
      muscle == "VI" ~ "Vastus intermedius",
      .default = muscle
    )) 
  
    # assigning the deviation contrasts
    
    data$muscle <- as.factor(data$muscle)
    
    contrasts(data$muscle) = contr.sum(length(unique(data$muscle)))
    
    model <-
      brm(
        yi | se(sqrt(vi)) ~ mean_muscle_length_centred * site_centred * muscle +
          (1 | study_number) +
          (1 | arm_number) +
          (1 | effect_number),
        data = data,
        chains = 4,
        cores = 4,
        seed = 1988,
        warmup = 2000,
        iter = 8000,
        control = list(adapt_delta = 0.99),
        save_pars = save_pars(all = TRUE)
      )
}

fit_muscle_action_model <- function(data) {
  
  # assigning the deviation contrasts
  
  data$muscle_action <- as.factor(data$muscle_action)
  
  contrasts(data$muscle_action) = contr.sum(length(unique(data$muscle_action)))
  
  model <-
    brm(
      yi | se(sqrt(vi)) ~ mean_muscle_length_centred * site_centred * muscle_action +
        (1 | study_number) +
        (1 | arm_number) +
        (1 | effect_number),
      data = data,
      chains = 4,
      cores = 4,
      seed = 1988,
      warmup = 2000,
      iter = 8000,
      control = list(adapt_delta = 0.99),
      save_pars = save_pars(all = TRUE)
    )
}

# Models with informed priors

sample_and_plot_priors_SMD <- function(model) {
  
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
    stat_slab(alpha = 0.75, normalize = "panels") + 
    facet_wrap(".variable") +
    scale_x_continuous(limits = c(-0.1,0.5), breaks = seq(-0.5,0.5, length = 11)) +
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

sample_and_plot_priors_lnRR <- function(model) {
  
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
    stat_slab(alpha = 0.75, normalize = "panels") + 
    facet_wrap(".variable") +
    scale_x_continuous(limits = c(-0.05,0.075), breaks = c(-0.05,-0.025,0,0.025,0.05,0.075,0.1)) +
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

# James Steele priors
set_steele_priors_SMD <- function() {
  prior <-
    c(
      # Priors set for Intercept (overall fixed mean) and tau at study level from Steele et al., (2023) DOI: 10.1080/02640414.2023.2286748
      set_prior("student_t(103, 0.3372, 0.0253)", class = "b", coef = "Intercept"),
      set_prior("student_t(3, 0.2111, 0.048)", class = "sd", coef = "Intercept", group = "study_number"),
      set_prior("student_t(3, 0.0224, 0.0517)", class = "sd", coef = "Intercept", group = "arm_number"),
      set_prior("student_t(3, 0.0477, 0.0401)", class = "sd", coef = "Intercept", group = "effect_number"),

      # Wolf et al (2023) DOI: https://doi.org/10.47206/ijsc.v3i1.182 found similar (point estimate withing 0.1 SESOI) effects for short length partial vs full ROM
      # Given constraints on interaction effects, the estimate for long vs short from this is implausibly large given general 0.34 effects for RT on hypertrophy
      # It would seem more reasonable to have more mass on a null effect, but slightly more weight towards small (<0.1) effects for muscle length
      # This prior is set such that a difference of 50% in length reflecting a typical difference in short vs long training (at a site of 50%) produces effects of the magnitudes ~0 to ~0.05
      set_prior("skew_normal(0, 0.1, 5)", class = "b", coef = "mean_muscle_length_centred",),
      
      # It is unclear (DOI: 10.1519/SSC.0000000000000574) whether hypertrophy differs by site of measurement overall 
      # Thus we set a prior centred on null effects but allowing for effects of similar magnitude (~0.05) in either direction
      set_prior("student_t(3, 0, 0.04)", class = "b", coef = "site_centred"),
      
      # Lastly given interaction constraints, the interaction of length and site must necessarily be small given other priors
      # Thus we set this to have half the width of the prior for site
      set_prior("student_t(3, 0, 0.02)", class = "b", coef = "mean_muscle_length_centred:site_centred")
      
      # All other priors for variance parameters are kept as default weakly regularising
    )
}

fit_steele_priors_only_model_SMD <- function(data, prior) {
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

fit_steele_priors_model_SMD <- function(data, prior) {
  
  # Set initial values to improve chain convergence
  set_inits <- function(seed = 1) {
    
    set.seed(seed)
    list(
      beta = rnorm(n = 1, mean = 0.3, sd = 0.1),
      sd = runif(n = 1, min = 0, max = 0.5)
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
        control = list(adapt_delta = 0.99),
        save_pars = save_pars(all = TRUE)
    )
}

set_steele_priors_lnRR <- function() {
  prior <-
    c(
      # Priors set for Intercept (overall fixed mean) and tau at study level from Steele et al., (2023) DOI: 10.1080/02640414.2023.2286748
      set_prior("student_t(103, 0.0525, 0.0046)", class = "b", coef = "Intercept"),
      set_prior("student_t(3, 0.0235, 0.0047)", class = "sd", coef = "Intercept", group = "study_number"),
      set_prior("student_t(3, 0, 0.0064)", class = "sd", coef = "Intercept", group = "arm_number"),
      set_prior("student_t(3, 0, 0.0058)", class = "sd", coef = "Intercept", group = "effect_number"),
      
      # Wolf et al (2023) DOI: https://doi.org/10.47206/ijsc.v3i1.182 found similar effects for short length partial vs full ROM, but the long vs short effect was 0.10 lnRR
      # Given constraints on interaction effects, the estimate for long vs short from this is implausibly large given general 0.0525 lnRR effects for RT on hypertrophy
      # It would seem more reasonable to have more mass on a null effect, but slightly more weight towards small (<0.015) effects for muscle length
      # This prior is set such that a difference of 50% in length reflecting a typical difference in short vs long training (at a site of 50%) produces effects of the magnitudes ~0 to ~0.0075
      set_prior("skew_normal(0, 0.016, 5)", class = "b", coef = "mean_muscle_length_centred",),
      
      # It is unclear (DOI: 10.1519/SSC.0000000000000574) whether hypertrophy differs by site of measurement overall 
      # Thus we set a prior centred on null effects but allowing for effects of similar magnitude (~0.0075) in either direction
      set_prior("student_t(3, 0, 0.008)", class = "b", coef = "site_centred"),
      
      # Lastly given interaction constraints, the interaction of length and site must necessarily be small given other priors
      # Thus we set this to have half the width of the prior for site
      set_prior("student_t(3, 0, 0.004)", class = "b", coef = "mean_muscle_length_centred:site_centred")
      
      # All other priors for variance parameters are kept as default weakly regularising
    )
}

fit_steele_priors_only_model_lnRR <- function(data, prior) {
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

fit_steele_priors_model_lnRR <- function(data, prior) {
  
  # Set initial values to improve chain convergence
  set_inits <- function(seed = 1) {
    
    set.seed(seed)
    list(
      beta = rnorm(n = 1, mean = 0.05, sd = 0.004),
      sd = runif(n = 1, min = 0, max = 0.05)
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
        control = list(adapt_delta = 0.99),
        save_pars = save_pars(all = TRUE)
    )
}

# Dorian Varovic and Brad Schoenfeld priors
set_DV_BS_PM_priors_SMD <- function() {
  prior <-
    c(
      # Priors set for Intercept (overall fixed mean) and tau at study level from Steele et al., (2023) DOI: 10.1080/02640414.2023.2286748
      set_prior("student_t(103, 0.3372, 0.0253)", class = "b", coef = "Intercept"),
      set_prior("student_t(3, 0.2111, 0.048)", class = "sd", coef = "Intercept", group = "study_number"),
      set_prior("student_t(3, 0.0224, 0.0517)", class = "sd", coef = "Intercept", group = "arm_number"),
      set_prior("student_t(3, 0.0477, 0.0401)", class = "sd", coef = "Intercept", group = "effect_number"),
      
      # DV & BS had more optimistic priors than JS for both length and site
      set_prior("student_t(3, 0.4, 0.2)", class = "b", coef = "mean_muscle_length_centred",),
      set_prior("student_t(3, 0.4, 0.2)", class = "b", coef = "site_centred"),
      
      # But again given interaction constraints, the interaction of length and site must necessarily be small given other priors
      # Thus we set this to have half the width of the prior for site
      set_prior("student_t(3, 0.2, 0.1)", class = "b", coef = "mean_muscle_length_centred:site_centred")
      
      # All other priors for variance parameters are kept as default weakly regularising
    )
}

fit_DV_BS_PM_priors_only_model_SMD <- function(data, prior) {
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

fit_DV_BS_PM_priors_model_SMD <- function(data, prior) {
  
  # Set initial values to improve chain convergence
  set_inits <- function(seed = 1) {
    
    set.seed(seed)
    list(
      beta = rnorm(n = 1, mean = 0.3, sd = 0.1),
      sd = runif(n = 1, min = 0, max = 0.5)
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
        control = list(adapt_delta = 0.99),
        save_pars = save_pars(all = TRUE)
    )
}

set_DV_BS_PM_priors_lnRR <- function() {
  prior <-
    c(
      # Priors set for Intercept (overall fixed mean) and tau at study level from Steele et al., (2023) DOI: 10.1080/02640414.2023.2286748
      set_prior("student_t(103, 0.0525, 0.0046)", class = "b", coef = "Intercept"),
      set_prior("student_t(3, 0.0235, 0.0047)", class = "sd", coef = "Intercept", group = "study_number"),
      set_prior("student_t(3, 0, 0.0064)", class = "sd", coef = "Intercept", group = "arm_number"),
      set_prior("student_t(3, 0, 0.0058)", class = "sd", coef = "Intercept", group = "effect_number"),
      
      # DV & BS had more optimistic priors than JS for both length and site set to about a 5% effect for a change in 50% of length/site form short/proximal to long/distal
      set_prior("student_t(3, 0.09531018, 0.04)", class = "b", coef = "mean_muscle_length_centred",),
      set_prior("student_t(3, 0.09531018, 0.04)", class = "b", coef = "site_centred"),
      
      # But again given interaction constraints, the interaction of length and site must necessarily be small given other priors
      # Thus we set this to have half the width of the prior for site
      set_prior("student_t(3, 0.04765509, 0.02)", class = "b", coef = "mean_muscle_length_centred:site_centred")
      
      # All other priors for variance parameters are kept as default weakly regularising
    )
}

fit_DV_BS_PM_priors_only_model_lnRR <- function(data, prior) {
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

fit_DV_BS_PM_priors_model_lnRR <- function(data, prior) {
  
  # Set initial values to improve chain convergence
  set_inits <- function(seed = 1) {
    
    set.seed(seed)
    list(
      beta = rnorm(n = 1, mean = 0.005, sd = 0.008),
      sd = runif(n = 1, min = 0, max = 0.05)
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
        control = list(adapt_delta = 0.99),
        save_pars = save_pars(all = TRUE)
    )
}

# Plot models
plot_main_model_preds_SMD <- function(data, model) {
  
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

plot_main_model_slopes_SMD <- function(model) {
  
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
      bayestestR::rope(filter(slopes, site_centred == -0.25)$draw/2, range = c(-0.1, 0.1), ci = 1)$ROPE_Percentage,
      bayestestR::rope(filter(slopes, site_centred == 0)$draw/2, range = c(-0.1, 0.1), ci = 1)$ROPE_Percentage,
      bayestestR::rope(filter(slopes, site_centred == 0.25)$draw/2, range = c(-0.1, 0.1), ci = 1)$ROPE_Percentage
    ),
    pd = c(
      bayestestR::rope(filter(slopes, site_centred == -0.25)$draw/2, range = c(0.1, Inf), ci = 1)$ROPE_Percentage,
      bayestestR::rope(filter(slopes, site_centred == 0)$draw/2, range = c(0.1, Inf), ci = 1)$ROPE_Percentage,
      bayestestR::rope(filter(slopes, site_centred == 0.25)$draw/2, range = c(0.1, Inf), ci = 1)$ROPE_Percentage
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
    # add mean and qi
    annotate("text", (summary$site_centred*100)+45, summary$estimate/2, label = round(summary$estimate/2,2), size = 3) +
    annotate("text", (summary$site_centred*100)+45, summary$conf.low/2, label = round(summary$conf.low/2,2), size = 3) +
    annotate("text", (summary$site_centred*100)+45, summary$conf.high/2, label = round(summary$conf.high/2,2), size = 3) +
    # add percent in rope
    annotate("rect", xmin = 10, xmax = 90, ymin = -1.125, ymax = -0.825, color = "black", fill = "white") +
    annotate("text", (rope_percents$site_centred*100)+50, -1.05, label = glue::glue("{round(rope_percents$rope_percent*100,2)}%"), size = 3) +
    annotate("text", x=50, y=-0.9, label="Percentage of Posterior Distibution Within ROPE [-0.1,0.1]", size = 3) +
    # add probability of positive effect
    annotate("rect", xmin = 10, xmax = 90, ymin = 0.825, ymax = 1.125, color = "black", fill = "white") +
    annotate("text", (rope_percents$site_centred*100)+50, 0.9, label = glue::glue("{round(rope_percents$pd*100,2)}%"), size = 3) +
    annotate("text", x=50, y=1.05, label="Probability of Meaningful Positive Effect (i.e., >0.1)", size = 3) +
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

plot_main_model_preds_lnRR <- function(data, model) {
  
  total_var <- get_variance_random(model)
  
  # Interaction plot
  preds <- crossing(
    mean_muscle_length_centred = seq(-0.35, 0.35, length = 101),
    site_centred = c(-0.25,0,0.25),
    vi = 0
  ) |>
    add_epred_draws(model, re_formula = NA) |>
    mutate(.epred = 100*(exp(.epred+0.5*total_var)-1))
  
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
               aes(y = 100*(exp(yi)-1), size = size + 0.25), color = "black", fill = NA, shape = 21, alpha = 0.5) +
    geom_point(data = data,
               aes(y = 100*(exp(yi)-1), size = size, color = (site_centred*100)+50), alpha = 0.5) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    scale_x_continuous(breaks = c(0,25,50,75,100)) +
    scale_y_continuous(breaks = c(-10,0,10,20,30,40,50)) +
    guides(
      size = "none",
      color = "none"
    ) +
    labs(
      x = "Mean Muscle Length (%)",
      y = "Exponentiated Log Response Ratio (%)",
      fill = "Site of Measurement (%)"
    ) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          legend.position = "bottom",
          axis.title = element_text(size=10))
  
}

plot_main_model_slopes_lnRR <- function(model) {
  
  total_var <- get_variance_random(model)
  
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
    posterior_draws() |>
    mutate(draw = 100*(exp(draw+0.5*total_var)-1))
  
  
  rope_percents <- tibble(
    site_centred = c(-0.25,0,0.25),
    rope_percent = c(
      bayestestR::rope(filter(slopes, site_centred == -0.25)$draw/2, range = c(-3,3), ci = 1)$ROPE_Percentage,
      bayestestR::rope(filter(slopes, site_centred == 0)$draw/2, range = c(-3,3), ci = 1)$ROPE_Percentage,
      bayestestR::rope(filter(slopes, site_centred == 0.25)$draw/2, range = c(-3,3), ci = 1)$ROPE_Percentage
    ),
    pd = c(
      bayestestR::rope(filter(slopes, site_centred == -0.25)$draw/2, range = c(3, Inf), ci = 1)$ROPE_Percentage,
      bayestestR::rope(filter(slopes, site_centred == 0)$draw/2, range = c(3, Inf), ci = 1)$ROPE_Percentage,
      bayestestR::rope(filter(slopes, site_centred == 0.25)$draw/2, range = c(3, Inf), ci = 1)$ROPE_Percentage
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
  ) |>
    mutate(
      estimate = 100*(exp(estimate+0.5*total_var)-1),
      conf.low = 100*(exp(conf.low+0.5*total_var)-1),
      conf.high = 100*(exp(conf.high+0.5*total_var)-1)
    )
  
  slopes |>
    ggplot(aes(x = (site_centred*100)+50, y = draw/2)) +
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
    geom_hline(yintercept = c(-3,3), linetype = "dashed") +
    stat_slabinterval(aes(fill = (site_centred*100)+50), alpha = 0.75) +
    # add mean and qi
    annotate("text", (summary$site_centred*100)+45, summary$estimate/2, label = round(summary$estimate/2,2), size = 3) +
    annotate("text", (summary$site_centred*100)+45, summary$conf.low/2, label = round(summary$conf.low/2,2), size = 3) +
    annotate("text", (summary$site_centred*100)+45, summary$conf.high/2, label = round(summary$conf.high/2,2), size = 3) +
    # add percent in rope
    annotate("rect", xmin = 7.5, xmax = 92.5, ymin = -52.5, ymax = -35.83333, color = "black", fill = "white") +
    annotate("text", (rope_percents$site_centred*100)+50, -48.33333, label = glue::glue("{round(rope_percents$rope_percent*100,2)}%"), size = 3) +
    annotate("text", x=50, y=-40, label="Percentage of Posterior Distibution Within ROPE [-3%,3%]", size = 3) +
    # add probability of positive effect
    annotate("rect", xmin = 10, xmax = 90, ymin = 35.83333, ymax = 52.5, color = "black", fill = "white") +
    annotate("text", (rope_percents$site_centred*100)+50, 40, label = glue::glue("{round(rope_percents$pd*100,2)}%"), size = 3) +
    annotate("text", x=50, y=48.33333, label="Probability of Meaningful Positive Effect (i.e., >3%)", size = 3) +
    scale_fill_viridis_c() +
    scale_x_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) +
    scale_y_continuous(breaks = c(-50,-40,-30,-20,-10,0,10,20,30,40,50), limits = c(-55,55)) +
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

plot_upper_lower_model_preds_SMD <- function(data, model) {
  
  # Interaction plot
  preds <- crossing(
    mean_muscle_length_centred = seq(-0.35, 0.35, length = 11),
    site_centred = c(-0.25,0,0.25),
    body_region = unique(data$body_region),
    vi = 0
  ) |>
    add_epred_draws(model, re_formula = NA)
  
  summary <- preds |>
    group_by(mean_muscle_length_centred, site_centred, body_region) |>
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
    # scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5)) +
    facet_wrap("body_region") +
    guides(
      size = "none",
      color = "none"
    ) +
    labs(
      x = "Mean Muscle Length (%)",
      y = "Standardised Mean Change",
      fill = "Site of Measurement (%)",
      title = "Interaction between mean muscle length, site of measurement, and upper vs lower body",
      subtitle = "Global grand mean and 95% quantile intervals presented for predictions at 25%, 50%, and 75% of centred site of measurement"
    ) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          legend.position = "bottom",
          axis.title = element_text(size=10))
  
}

plot_muscle_model_preds_SMD <- function(data, model) {
  
  data <- data |>
    
    # relabel secondary predictors
    mutate(muscle = case_when(
      muscle == "VL" ~ "Vastus Lateralis",
      muscle == "BFL" ~ "Biceps femoris Long Head",
      muscle == "ST" ~ "Semitendinosus",
      muscle == "RF" ~ "Rectus Femoris",
      muscle == "VM" ~ "Vastus Medialis",
      muscle == "VI" ~ "Vastus intermedius",
      .default = muscle
    )) 
  
  # Interaction plot
  preds <- crossing(
    mean_muscle_length_centred = seq(-0.35, 0.35, length = 11),
    site_centred = c(-0.25,0,0.25),
    muscle = unique(data$muscle),
    vi = 0
  ) |>
    add_epred_draws(model, re_formula = NA)
  
  summary <- preds |>
    group_by(mean_muscle_length_centred, site_centred, muscle) |>
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
    # scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5)) +
    facet_wrap("muscle", nrow=2) +
    guides(
      size = "none",
      color = "none"
    ) +
    labs(
      x = "Mean Muscle Length (%)",
      y = "Standardised Mean Change",
      fill = "Site of Measurement (%)",
      title = "Interaction between mean muscle length, site of measurement, and muscle group",
      subtitle = "Global grand mean and 95% quantile intervals presented for predictions at 25%, 50%, and 75% of centred site of measurement"
    ) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          legend.position = "bottom",
          axis.title = element_text(size=10))
  
}

plot_muscle_action_model_preds_SMD <- function(data, model) {
  
  # Interaction plot
  preds <- crossing(
    mean_muscle_length_centred = seq(-0.35, 0.35, length = 11),
    site_centred = c(-0.25,0,0.25),
    muscle_action = unique(data$muscle_action),
    vi = 0
  ) |>
    add_epred_draws(model, re_formula = NA)
  
  summary <- preds |>
    group_by(mean_muscle_length_centred, site_centred, muscle_action) |>
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
    # scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5)) +
    facet_wrap("muscle_action") +
    guides(
      size = "none",
      color = "none"
    ) +
    labs(
      x = "Mean Muscle Length (%)",
      y = "Standardised Mean Change",
      fill = "Site of Measurement (%)",
      title = "Interaction between mean muscle length, site of measurement, and muscle action",
      subtitle = "Global grand mean and 95% quantile intervals presented for predictions at 25%, 50%, and 75% of centred site of measurement"
    ) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          legend.position = "bottom",
          axis.title = element_text(size=10))
  
}

plot_upper_lower_model_preds_lnRR <- function(data, model) {
  
  total_var <- get_variance_random(model)
  
  # Interaction plot
  preds <- crossing(
    mean_muscle_length_centred = seq(-0.35, 0.35, length = 11),
    site_centred = c(-0.25,0,0.25),
    body_region = unique(data$body_region),
    vi = 0
  ) |>
    add_epred_draws(model, re_formula = NA) |>
    mutate(.epred = 100*(exp(.epred+0.5*total_var)-1))
  
  summary <- preds |>
    group_by(mean_muscle_length_centred, site_centred, body_region) |>
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
               aes(y = 100*(exp(yi)-1), size = size + 0.25), color = "black", fill = NA, shape = 21, alpha = 0.5) +
    geom_point(data = data,
               aes(y = 100*(exp(yi)-1), size = size, color = (site_centred*100)+50), alpha = 0.5) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    scale_x_continuous(breaks = c(0,25,50,75,100)) +
    # scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5)) +
    facet_wrap("body_region") +
    guides(
      size = "none",
      color = "none"
    ) +
    labs(
      x = "Mean Muscle Length (%)",
      y = "Exponentiated Log Response Ratio (%)",
      fill = "Site of Measurement (%)",
      title = "Interaction between mean muscle length, site of measurement, and upper vs lower body",
      subtitle = "Global grand mean and 95% quantile intervals presented for predictions at 25%, 50%, and 75% of centred site of measurement"
    ) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          legend.position = "bottom",
          axis.title = element_text(size=10))
  
}

plot_muscle_model_preds_lnRR <- function(data, model) {
  
  data <- data |>
    
    # relabel secondary predictors
    mutate(muscle = case_when(
      muscle == "VL" ~ "Vastus Lateralis",
      muscle == "BFL" ~ "Biceps femoris Long Head",
      muscle == "ST" ~ "Semitendinosus",
      muscle == "RF" ~ "Rectus Femoris",
      muscle == "VM" ~ "Vastus Medialis",
      muscle == "VI" ~ "Vastus intermedius",
      .default = muscle
    )) 
  
  total_var <- get_variance_random(model)
  
  # Interaction plot
  preds <- crossing(
    mean_muscle_length_centred = seq(-0.35, 0.35, length = 11),
    site_centred = c(-0.25,0,0.25),
    muscle = unique(data$muscle),
    vi = 0
  ) |>
    add_epred_draws(model, re_formula = NA) |>
    mutate(.epred = 100*(exp(.epred+0.5*total_var)-1))
  
  summary <- preds |>
    group_by(mean_muscle_length_centred, site_centred, muscle) |>
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
               aes(y = 100*(exp(yi)-1), size = size + 0.25), color = "black", fill = NA, shape = 21, alpha = 0.5) +
    geom_point(data = data,
               aes(y = 100*(exp(yi)-1), size = size, color = (site_centred*100)+50), alpha = 0.5) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    scale_x_continuous(breaks = c(0,25,50,75,100)) +
    # scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5)) +
    facet_wrap("muscle", nrow=2) +
    guides(
      size = "none",
      color = "none"
    ) +
    labs(
      x = "Mean Muscle Length (%)",
      y = "Exponentiated Log Response Ratio (%)",
      fill = "Site of Measurement (%)",
      title = "Interaction between mean muscle length, site of measurement, and muscle group",
      subtitle = "Global grand mean and 95% quantile intervals presented for predictions at 25%, 50%, and 75% of centred site of measurement"
    ) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          legend.position = "bottom",
          axis.title = element_text(size=10))
  
}

plot_muscle_action_model_preds_lnRR <- function(data, model) {
  
  total_var <- get_variance_random(model)
  
  # Interaction plot
  preds <- crossing(
    mean_muscle_length_centred = seq(-0.35, 0.35, length = 11),
    site_centred = c(-0.25,0,0.25),
    muscle_action = unique(data$muscle_action),
    vi = 0
  ) |>
    add_epred_draws(model, re_formula = NA) |>
    mutate(.epred = 100*(exp(.epred+0.5*total_var)-1))
  
  summary <- preds |>
    group_by(mean_muscle_length_centred, site_centred, muscle_action) |>
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
               aes(y = 100*(exp(yi)-1), size = size + 0.25), color = "black", fill = NA, shape = 21, alpha = 0.5) +
    geom_point(data = data,
               aes(y = 100*(exp(yi)-1), size = size, color = (site_centred*100)+50), alpha = 0.5) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    scale_x_continuous(breaks = c(0,25,50,75,100)) +
    # scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5)) +
    facet_wrap("muscle_action") +
    guides(
      size = "none",
      color = "none"
    ) +
    labs(
      x = "Mean Muscle Length (%)",
      y = "Exponentiated Log Response Ratio (%)",
      fill = "Site of Measurement (%)",
      title = "Interaction between mean muscle length, site of measurement, and muscle action",
      subtitle = "Global grand mean and 95% quantile intervals presented for predictions at 25%, 50%, and 75% of centred site of measurement"
    ) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          legend.position = "bottom",
          axis.title = element_text(size=10))
  
}

plot_BF_model_comparisons <- function(model1,
                                      model2,
                                      model3,
                                      model4) {
  
  BF_mean_models <- bayesfactor_models(model1,
                                       model2,
                                       model3,
                                       model4)
  BF_2log <- function(x) (2*x)
  
  BF_mean_models <-as_tibble(as.matrix(BF_mean_models))  |>
    mutate_at(1:4, BF_2log) |>
    rowid_to_column("Denominator") |>
    mutate(Denominator = case_when(
      Denominator == 1 ~ "Main Model - Uninformed Priors",
      Denominator == 2 ~  "Main Model with Random Slopes - Uninformed Priors",
      Denominator == 3 ~  "Main Model with Random Slopes - D. Varovic, B. Schoenfeld, & P. Mikulic Priors",
      Denominator == 4 ~ "Main Model with Random Slopes - J. Steele Priors"
    )) |>
    rename("Main Model - Uninformed Priors" = 2,
           "Main Model with Random Slopes - Uninformed Priors" = 3,
           "Main Model with Random Slopes - D. Varovic, B. Schoenfeld, & P. Mikulic Priors" = 4,
           "Main Model with Random Slopes - J. Steele Priors" = 5) |>
    pivot_longer(2:5, names_to = "Numerator", values_to = "logBF")
  
  BF_mean_models$Denominator <-  recode(BF_mean_models$Denominator,
                                        "main_model_lnRR" = "Main Model - Uninformed Priors",
                                        "main_model_r_slopes_lnRR" = "Main Model with Random Slopes - Uninformed Priors",
                                        "DV_BS_PM_priors_model_lnRR" = "Main Model with Random Slopes - D. Varovic, B. Schoenfeld, & P. Mikulic Priors",
                                        "steele_priors_model_lnRR" = "Main Model with Random Slopes - J. Steele Priors")
  
  BF_mean_models |> 
    mutate(Denominator = factor(Denominator, levels= c( 
      "Main Model - Uninformed Priors",
      "Main Model with Random Slopes - Uninformed Priors",
      "Main Model with Random Slopes - D. Varovic, B. Schoenfeld, & P. Mikulic Priors",
      "Main Model with Random Slopes - J. Steele Priors")),
      Numerator = factor(Numerator, levels= c( 
        "Main Model - Uninformed Priors",
        "Main Model with Random Slopes - Uninformed Priors",
        "Main Model with Random Slopes - D. Varovic, B. Schoenfeld, & P. Mikulic Priors",
        "Main Model with Random Slopes - J. Steele Priors")),
      logBF = as.numeric(logBF)) |>
    ggplot(aes(x=Numerator, y=Denominator, fill=logBF)) +
    geom_tile() +
    geom_raster() +
    geom_text(aes(label = round(logBF,2))) +
    scale_fill_gradient2(low = "#E69F00", mid="white", high = "#56B4E9") +
    scale_y_discrete(limits=rev, labels = function(x) str_wrap(x, width = 25)) +
    scale_x_discrete(position = "top", labels = function(x) str_wrap(x, width = 25)) +
    labs(title = "Comparing models using 2×log(BF)",
         fill = "2×log(BF)",
         caption = "Kass and Raferty (1995) scale:
       -Inf to 0 = Negative; 0 to 2 = Weak; 2 to 6 = Positive; 6 to 10 = Strong; 10 to +Inf = Very Strong") +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=2.5))
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