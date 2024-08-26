# https://stats.stackexchange.com/questions/65898/why-could-centering-independent-variables-change-the-main-effects-with-moderatio
# https://stats.stackexchange.com/questions/417029/mean-centering-interaction-terms#:~:text=The%20coefficient%20for%20a%20centered,interaction%20with%20another%20centered%20variable.&text=So%20centering%20x%20changes%20the,the%20xz%20interaction%20unchanged.
library(tidyverse)
library(janitor)
library(metafor)
library(brms)
library(tidybayes)

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


      # # Taken from Wolf et al. (2022) overall estimate
      # # Read csv as data frame into environment - Note: change source address
      # data_wolf <- read.csv("data/wolf_et_al_meta_data.csv", na.strings=c(""," ","NA"))
      # 
      # # add within group correlations assumptions 
      # # NOTE - change and rerun to check with 0.5 and 0.9
      # data_wolf$ri70 <- as.numeric(strrep(0.7, 1))
      # 
      # ### Standardised effect size calculations
      # # Pooled baseline SD
      # data_wolf$SD_pool <- (((data_wolf$FULL_ni - 1)*data_wolf$FULL_sd_pre) + ((data_wolf$PART_ni - 1)*data_wolf$PART_sd_pre)) / (data_wolf$FULL_ni + data_wolf$PART_ni - 2)
      # 
      # # Effects for measures where increase is good
      # data_wolf_increase <- subset(data_wolf, increase_decrease == "increase")
      # 
      # data_wolf_increase_int <- escalc(measure="SMCR", m1i=FULL_m_post, 
      #                             m2i=FULL_m_pre, sd1i=SD_pool, ni=FULL_ni, ri=ri70, data = data_wolf_increase)
      # data_wolf_increase_con <- escalc(measure="SMCR", m1i=PART_m_post, 
      #                             m2i=PART_m_pre, sd1i=SD_pool, ni=PART_ni, ri=ri70, data = data_wolf_increase)
      # 
      # data_wolf_increase$yi <- (data_wolf_increase_int$yi - data_wolf_increase_con$yi)
      # data_wolf_increase$vi <- (data_wolf_increase_int$vi + data_wolf_increase_con$vi)
      # 
      # data_wolf_increase$lnRR_yi <- log(data_wolf_increase$FULL_m_post/data_wolf_increase$FULL_m_pre) - log(data_wolf_increase$PART_m_post/data_wolf_increase$PART_m_pre)
      # 
      # data_wolf_increase$lnRR_vi <- (data_wolf_increase$FULL_sd_post^2/(data_wolf_increase$FULL_m_post^2*data_wolf_increase$FULL_ni)) + (data_wolf_increase$FULL_sd_pre^2/(data_wolf_increase$FULL_sd_post^2*data_wolf_increase$FULL_ni)) + (data_wolf_increase$PART_sd_post^2/(data_wolf_increase$PART_m_post^2*data_wolf_increase$PART_ni)) + (data_wolf_increase$PART_sd_pre^2/(data_wolf_increase$PART_m_pre^2*data_wolf_increase$PART_ni))
      # 
      # # Effects for measures where increase is good
      # data_wolf_decrease <- subset(data_wolf, increase_decrease == "decrease")
      # 
      # data_wolf_decrease_int <- escalc(measure="SMCR", m1i=FULL_m_pre, 
      #                             m2i=FULL_m_post, sd1i=SD_pool, ni=FULL_ni, ri=ri70, data = data_wolf_decrease)
      # data_wolf_decrease_con <- escalc(measure="SMCR", m1i=PART_m_pre, 
      #                             m2i=PART_m_post, sd1i=SD_pool, ni=PART_ni, ri=ri70, data = data_wolf_decrease)
      # 
      # data_wolf_decrease$yi <- (data_wolf_decrease_int$yi - data_wolf_decrease_con$yi)
      # data_wolf_decrease$vi <- (data_wolf_decrease_int$vi + data_wolf_decrease_con$vi)
      # 
      # data_wolf_decrease$lnRR_yi <- log(data_wolf_decrease$PART_m_post/data_wolf_decrease$PART_m_pre) - log(data_wolf_decrease$FULL_m_post/data_wolf_decrease$FULL_m_pre)
      # 
      # data_wolf_decrease$lnRR_vi <- (data_wolf_decrease$FULL_sd_post^2/(data_wolf_decrease$FULL_m_post^2*data_wolf_decrease$FULL_ni)) + (data_wolf_decrease$FULL_sd_pre^2/(data_wolf_decrease$FULL_sd_post^2*data_wolf_decrease$FULL_ni)) + (data_wolf_decrease$PART_sd_post^2/(data_wolf_decrease$PART_m_post^2*data_wolf_decrease$PART_ni)) + (data_wolf_decrease$PART_sd_pre^2/(data_wolf_decrease$PART_m_pre^2*data_wolf_decrease$PART_ni))
      # 
      # 
      # ### Recombine
      # 
      # data_wolf_effects <- rbind(data_wolf_increase, data_wolf_decrease) %>%
      #   mutate(se = sqrt(vi),
      #          lnRR_se = sqrt(lnRR_vi),
      #          wi = 1/sqrt(vi),
      #          size = 0.5 + 3 * (wi - min(wi, na.rm = TRUE))/(max(wi, na.rm = TRUE) - min(wi, na.rm = TRUE)),
      #          lnRR_wi = 1/sqrt(lnRR_vi),
      #          lnRR_size = 0.5 + 3 * (lnRR_wi - min(lnRR_wi, na.rm = TRUE))/(max(lnRR_wi, na.rm = TRUE) - min(lnRR_wi, na.rm = TRUE)))
      # 
      # # Muscle Length - Hypertrophy
      # data_wolf_effects_hypertrophy <- data_wolf_effects %>%
      #   mutate(muscle_length = as.factor(muscle_length)) %>%
      #   filter(outcome_subcategory == "muscle_size" & percent_distal == 0.5)
      # 
      # muscle_length_hypertrophy_model <- brm(yi|se(se) ~ 1 + muscle_length + (1 | study) + (1|group),
      #                                        data=data_wolf_effects_hypertrophy,
      #                                        chains = 4,
      #                                        cores = 4,
      #                                        seed = 1988,
      #                                        warmup = 2000,
      #                                        iter = 6000,
      #                                        control = list(adapt_delta = 0.99)
      # )
      # 
      # draws <- muscle_length_hypertrophy_model |>
      #   spread_draws(b_muscle_lengthshort)
      # 
      # posterior <- MASS::fitdistr(draws$b_muscle_lengthshort, "t")$estimate/100
      # 
      # draws |>
      #   ggplot(aes(x=b_muscle_lengthshort/100)) +
      #   stat_slabinterval()


### Need to get proper prior for muscle length slope - rerun models exlcuding any studies from here and get marginal effect estimate
### Also need to add a prior for intercept variance at study level from large MA dataset

get_prior(yi | se(sqrt(vi)) ~ mean_muscle_length_centred * site_centred +
            (mean_muscle_length_centred + site_centred| study_number) +
            (1 | arm_number) +
            (1 | effect_number),
          data = data)

main_model_prior <-
  c(
    # set_prior(paste("student_t(",posterior[3],",", posterior[1],",", posterior[2],")"), class = "b", coef = "mean_muscle_length_centred"),
    set_prior("student_t(3, 0.34, 0.025)", class = "Intercept"),
    set_prior("student_t(3, 0.21, 0.025)", class = "sd", coef = "Intercept", group = "study_number")
    
  )


main_model_w_prior <-
  brm(
    yi | se(sqrt(vi)) ~ mean_muscle_length_centred * site_centred +
      (mean_muscle_length_centred + site_centred| study_number) +
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
    sample_prior = TRUE
  )

get_variables(main_model_w_prior)

draws <- main_model_w_prior |>
  spread_draws(sd_study_number__Intercept)

draws |>
  ggplot(aes(x=sd_study_number__Intercept)) +
  stat_slabinterval()

main_model_w_prior <-
  brm(
    yi | se(sqrt(vi)) ~ mean_muscle_length_centred * site_centred +
      (mean_muscle_length_centred + site_centred| study_number) +
      (1 | arm_number) +
      (1 | effect_number),
    data = data,
    prior = main_model_prior,
    chains = 4,
    cores = 4,
    seed = 1988,
    warmup = 2000,
    iter = 8000,
    control = list(adapt_delta = 0.99)
  )

main_model_w_prior

a <- conditional_effects(main_model_w_prior, "mean_muscle_length_centred", re_formula = NA)

plot(a, points = TRUE)

conditional_effects(main_model_w_prior, "site_centred", re_formula = NA)

c <- conditional_effects(main_model_w_prior, "mean_muscle_length_centred:site_centred", re_formula = NA)

plot(c, points = TRUE)

