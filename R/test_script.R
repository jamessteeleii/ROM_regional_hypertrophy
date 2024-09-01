# https://stats.stackexchange.com/questions/65898/why-could-centering-independent-variables-change-the-main-effects-with-moderatio
# https://stats.stackexchange.com/questions/417029/mean-centering-interaction-terms#:~:text=The%20coefficient%20for%20a%20centered,interaction%20with%20another%20centered%20variable.&text=So%20centering%20x%20changes%20the,the%20xz%20interaction%20unchanged.
library(tidyverse)
library(janitor)
library(metafor)
library(brms)
library(tidybayes)
library(marginaleffects)
library(patchwork)
library(bayestestR)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()-1)

# Read in data and prepare
data <- read.csv("data/ROM_regional_hypertrophy_data.csv", dec = ",") |>
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


# Check McMahon variances for US CSA against other same measures
data |>
  # filter(author == "McMahon et al") |>
  filter(assessment == "US" & type_of_measure == "CSA") |>
  select(study_number, author, effect_number,  m_pre, m_post, sd_pre, sd_post) |>
  rename(pre_m = m_pre,
         post_m = m_post,
         pre_sd = sd_pre,
         post_sd = sd_post) |>
  pivot_longer(4:7,
               names_to = c("time", "parameter"),
               names_sep = "_") |>
  mutate(value = if_else(author != "McMahon et al", value * 100, value)) |>
  pivot_wider(id_cols = 1:4,
              names_from = "parameter",
              values_from = "value") |>
  ggplot(aes(x = log(m), y = log(sd))) +
  # ggplot(aes(x = m, y = sd)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(aes(color = author)) +
  scale_x_continuous(limits = c(2.5,9)) +
  scale_y_continuous(limits = c(2.5,9))
# geom_label(aes(label = effect_number))


# Some initial frequentist models

## No random slopes
### Uncentred predictors
model <- rma.mv(yi, vi,
                random = list(~ 1 | study_number, ~ 1 | arm_number, ~1 | effect_number),
                mods = ~ mean_muscle_length * site,
                data = data
)

model

regplot(model, mod = "mean_muscle_length")

regplot(model, mod = "site")

### Centred predictors
model_centred <- rma.mv(yi, vi,
                        random = list(~ 1 | study_number, ~ 1 | arm_number, ~1 | effect_number),
                        mods = ~ mean_muscle_length_centred * site_centred,
                        data = data
)

model_centred

regplot(model_centred, mod = "mean_muscle_length_centred")

regplot(model_centred, mod = "site_centred")

## With random slopes
### Uncentred predictors
model_slopes <- rma.mv(yi, vi,
                       random = list(~ mean_muscle_length + site | study_number, ~ 1 | arm_number, ~1 | effect_number),
                       mods = ~ mean_muscle_length * site,
                       data = data,
                       struct = "GEN"
)

model_slopes

regplot(model_slopes, mod = "mean_muscle_length")

regplot(model_slopes, mod = "site")

### Centred predictors
model_centred_slopes <- rma.mv(yi, vi,
                               random = list(~ mean_muscle_length_centred + site_centred | study_number, ~ 1 | arm_number, ~1 | effect_number),
                               mods = ~ mean_muscle_length_centred * site_centred,
                               data = data,
                               struct = "GEN"
)

model_centred_slopes

regplot(model_centred_slopes, mod = "mean_muscle_length_centred")

regplot(model_centred_slopes, mod = "site_centred")


# Taken from Wolf et al. (2022) overall estimate
# Read csv as data frame into environment - Note: change source address
data_wolf <- read.csv("data/wolf_et_al_meta_data.csv", na.strings=c(""," ","NA"))

# add within group correlations assumptions
# NOTE - change and rerun to check with 0.5 and 0.9
data_wolf$ri70 <- as.numeric(strrep(0.7, 1))

### Standardised effect size calculations
# Pooled baseline SD
data_wolf$SD_pool <- (((data_wolf$FULL_ni - 1)*data_wolf$FULL_sd_pre) + ((data_wolf$PART_ni - 1)*data_wolf$PART_sd_pre)) / (data_wolf$FULL_ni + data_wolf$PART_ni - 2)

# Effects for measures where increase is good
data_wolf_increase <- subset(data_wolf, increase_decrease == "increase")

data_wolf_increase_int <- escalc(measure="SMCR", m1i=FULL_m_post,
                                 m2i=FULL_m_pre, sd1i=SD_pool, ni=FULL_ni, ri=ri70, data = data_wolf_increase)
data_wolf_increase_con <- escalc(measure="SMCR", m1i=PART_m_post,
                                 m2i=PART_m_pre, sd1i=SD_pool, ni=PART_ni, ri=ri70, data = data_wolf_increase)

data_wolf_increase$yi <- (data_wolf_increase_int$yi - data_wolf_increase_con$yi)
data_wolf_increase$vi <- (data_wolf_increase_int$vi + data_wolf_increase_con$vi)

data_wolf_increase$lnRR_yi <- log(data_wolf_increase$FULL_m_post/data_wolf_increase$FULL_m_pre) - log(data_wolf_increase$PART_m_post/data_wolf_increase$PART_m_pre)

data_wolf_increase$lnRR_vi <- (data_wolf_increase$FULL_sd_post^2/(data_wolf_increase$FULL_m_post^2*data_wolf_increase$FULL_ni)) + (data_wolf_increase$FULL_sd_pre^2/(data_wolf_increase$FULL_sd_post^2*data_wolf_increase$FULL_ni)) + (data_wolf_increase$PART_sd_post^2/(data_wolf_increase$PART_m_post^2*data_wolf_increase$PART_ni)) + (data_wolf_increase$PART_sd_pre^2/(data_wolf_increase$PART_m_pre^2*data_wolf_increase$PART_ni))

# Effects for measures where increase is good
data_wolf_decrease <- subset(data_wolf, increase_decrease == "decrease")

data_wolf_decrease_int <- escalc(measure="SMCR", m1i=FULL_m_pre,
                                 m2i=FULL_m_post, sd1i=SD_pool, ni=FULL_ni, ri=ri70, data = data_wolf_decrease)
data_wolf_decrease_con <- escalc(measure="SMCR", m1i=PART_m_pre,
                                 m2i=PART_m_post, sd1i=SD_pool, ni=PART_ni, ri=ri70, data = data_wolf_decrease)

data_wolf_decrease$yi <- (data_wolf_decrease_int$yi - data_wolf_decrease_con$yi)
data_wolf_decrease$vi <- (data_wolf_decrease_int$vi + data_wolf_decrease_con$vi)

data_wolf_decrease$lnRR_yi <- log(data_wolf_decrease$PART_m_post/data_wolf_decrease$PART_m_pre) - log(data_wolf_decrease$FULL_m_post/data_wolf_decrease$FULL_m_pre)

data_wolf_decrease$lnRR_vi <- (data_wolf_decrease$FULL_sd_post^2/(data_wolf_decrease$FULL_m_post^2*data_wolf_decrease$FULL_ni)) + (data_wolf_decrease$FULL_sd_pre^2/(data_wolf_decrease$FULL_sd_post^2*data_wolf_decrease$FULL_ni)) + (data_wolf_decrease$PART_sd_post^2/(data_wolf_decrease$PART_m_post^2*data_wolf_decrease$PART_ni)) + (data_wolf_decrease$PART_sd_pre^2/(data_wolf_decrease$PART_m_pre^2*data_wolf_decrease$PART_ni))


### Recombine

data_wolf_effects <- rbind(data_wolf_increase, data_wolf_decrease) %>%
  mutate(se = sqrt(vi),
         lnRR_se = sqrt(lnRR_vi),
         wi = 1/sqrt(vi),
         size = 0.5 + 3 * (wi - min(wi, na.rm = TRUE))/(max(wi, na.rm = TRUE) - min(wi, na.rm = TRUE)),
         lnRR_wi = 1/sqrt(lnRR_vi),
         lnRR_size = 0.5 + 3 * (lnRR_wi - min(lnRR_wi, na.rm = TRUE))/(max(lnRR_wi, na.rm = TRUE) - min(lnRR_wi, na.rm = TRUE)))

# Muscle Length - Hypertrophy
data_wolf_effects_hypertrophy <- data_wolf_effects %>%
  mutate(muscle_length = as.factor(muscle_length)) %>%
  filter(outcome_subcategory == "muscle_size" & percent_distal == 0.5)

muscle_length_hypertrophy_model <- brm(yi|se(se) ~ 1 + muscle_length + (1 | study) + (1|group),
                                       data=data_wolf_effects_hypertrophy,
                                       chains = 4,
                                       cores = 4,
                                       seed = 1988,
                                       warmup = 2000,
                                       iter = 6000,
                                       control = list(adapt_delta = 0.99)
)

draws <- muscle_length_hypertrophy_model |>
  spread_draws(b_muscle_lengthshort)

posterior <- tibble(
  m = MASS::fitdistr(draws$b_muscle_lengthshort, "t")$estimate[1]/50,
  s = MASS::fitdistr(draws$b_muscle_lengthshort, "t")$estimate[2]/50,
  df = MASS::fitdistr(draws$b_muscle_lengthshort, "t")$estimate[3]
)

draws |>
  ggplot(aes(x=b_muscle_lengthshort)) +
  stat_slabinterval()


### Need to get proper prior for muscle length slope - rerun models exlcuding any studies from here and get marginal effect estimate
### Also need to add a prior for intercept variance at study level from large MA dataset

default_prior(yi | se(sqrt(vi)) ~ 0 + Intercept + mean_muscle_length_centred * site_centred +
                (mean_muscle_length_centred + site_centred | study_number) +
                (1 | arm_number) +
                (1 | effect_number),
              data = data)

x <- seq(-0.01, 0.1, length=1000)

plot(x,
     dskew_normal(
       x,
       mu = 0,
       sigma = 0.05,
       alpha = 5
     ))

mode_hdi(rskew_normal(
  1e6,
  mu = 0.0006,
  sigma = 0.008,
  alpha = 5
))

plot(
  x,
  dstudent_t(x, 3, 0.005, 0.008)
)

plot(
  x,
  dnorm(x, 0.005, 0.004)
)

main_model_prior <-
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


main_model_w_prior_only <-
  brm(yi | se(sqrt(vi)) ~ 0 + Intercept + mean_muscle_length_centred * site_centred +
        (mean_muscle_length_centred + site_centred | study_number) +
        (1 | arm_number) +
        (1 | effect_number),
      data = data,
      prior = main_model_prior,
      chains = 4,
      cores = 4,
      seed = 1988,
      warmup = 2000,
      iter = 8000,
      control = list(adapt_delta = 0.99),
      sample_prior = "only"
  )



pp_check(main_model_w_prior_only)

# MASS::fitdistr(draws$b_mean_muscle_length_centred, "t")$estimate*50

main_model_w_prior_only |>
  spread_draws(b_mean_muscle_length_centred) |>
  mode_hdi() |>
  mutate(across(1:3, ~ .x/2))

main_model_w_prior_only |>
  spread_draws(b_Intercept) |>
  mode_hdi()

main_model_w_prior_only |>
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
  

# Prior plot
coef_prior_plot <- main_model_w_prior_only |>
  spread_draws(b_site_centred) |> 
  ggplot(aes(x=b_site_centred/2)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.75) +
  stat_slab(alpha = 0.75)  +
  # scale_x_continuous(limits = c(-0.1,0.2), breaks = seq(-0.5,0.5, length = 11)) +
  labs(
    x = "Coefficient for Mean Muscle Length Difference of 50%",
    y = "Density",
    title = "Prior distribution"
  ) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA))


pred_prior_plot <- crossing(
  mean_muscle_length_centred = seq(-0.5, 0.5, length = 101),
  site_centred = c(-0.4,0,0.4),
  vi = 0
) |>
  add_epred_draws(main_model_w_prior_only, re_formula = NA, ndraws = 100) |>
  ggplot(aes(x = (mean_muscle_length_centred*100)+50, y = .epred/2)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.75) +
  geom_line(aes(y = .epred, group = interaction(.draw, site_centred)), alpha = .1) +
  scale_x_continuous(breaks = c(0,25,50,75,100)) +
  facet_grid(.~site_centred) +
  labs(
    x = "Mean Muscle Length (%)",
    y = "Standardised Mean Change",
    title = "Draws from prior distribution"
  ) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA))


(coef_prior_plot + pred_prior_plot) + plot_annotation()


make_standata(yi | se(sqrt(vi)) ~ 0 + Intercept + mean_muscle_length_centred * site_centred + 
                (mean_muscle_length_centred + site_centred | study_number) +
                (1 | arm_number) +
                (1 | effect_number),
              data = data,
              prior = main_model_prior)

plot(
  seq(-0.2, 0.6, length=1000),
  dnorm(seq(-0.2, 0.6, length=1000), 0.25, 0.1)
)

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

# Model w prior
main_model_w_prior <-
  brm(
    yi | se(sqrt(vi)) ~ 0 + Intercept + mean_muscle_length_centred * site_centred + 
      (mean_muscle_length_centred + site_centred | study_number) +
      (1 | arm_number) +
      (1 | effect_number),
    data = data,
    prior = main_model_prior,
    chains = 4,
    cores = 4,
    seed = 1988,
    warmup = 2000,
    iter = 8000,
    init = list_of_inits,
    control = list(adapt_delta = 0.99)
  )

plot(main_model_w_prior)

main_model_w_prior

a <- conditional_effects(main_model_r_slopes, "mean_muscle_length_centred", re_formula = NA)

plot(a, points = TRUE)

b <- conditional_effects(main_model_r_slopes, "site_centred", re_formula = NA)

plot(b, points = TRUE)

c <- conditional_effects(main_model, "mean_muscle_length_centred:site_centred", re_formula = NA)

plot(c, points = TRUE)


# Interaction plot
preds <- crossing(
  mean_muscle_length_centred = seq(-0.35, 0.35, length = 101),
  site_centred = c(-0.25,0,0.25),
  vi = 0
) |>
  add_epred_draws(main_model_w_prior, re_formula = NA)

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
    x = "Mean Muscle Length",
    y = "Standardised Mean Change",
    fill = "Site of Measurement",
    title = "Interaction between mean muscle length and site of measurement",
    subtitle = "Global grand mean and 95% quantile intervals presented for predictions at 25%, 50%, and 75% of centred site of measurement"
  ) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA),
        legend.position = "bottom",
        plot.subtitle = element_text(size=8))



# Interaction plot - slopes
slopes <- avg_slopes(
  main_model_w_prior,
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
  main_model_w_prior,
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
  annotate("rect", xmin = 12.5, xmax = 87.5, ymin = 0.825, ymax = 1.125, color = "black", fill = "white") +
  annotate("text", (rope_percents$site_centred*100)+50, 0.9, label = glue::glue("{round(rope_percents$rope_percent*100,2)}%"), size = 3) +
  annotate("text", x=50, y=1.05, label="Percentage of Posterior Distibution within ROPE [-0.1,0.1]", size = 3) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) +
  scale_y_continuous(breaks = c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1)) +
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
