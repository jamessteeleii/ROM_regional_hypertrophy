library(tidyverse)
library(janitor)
library(metafor)

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
         mean_muscle_length = as.numeric(mean_muscle_length)) |>
  
  # rescale muscle length
  mutate(mean_muscle_length = mean_muscle_length*100) |>
  
  # add assumed pre-post correlation
  mutate(ri = 0.7)



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


data |>
  ggplot(aes(y = factor(effect_number), x = yi)) +
  geom_pointrange(aes(xmin = yi - (vi*1.96), xmax = yi + (vi*1.96)))

n_check <- data |>
  filter(author == "Noorkoiv et al") |>
  arrange(yi)

# Check McMahon variances
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


model <- rma.mv(yi, vi,
       random = ~ 1 | study_number/arm_number/effect_number,
       mods = ~ as.numeric(mean_muscle_length) * site,
       data = data,
       REML = TRUE)

regplot(model, mod = "as.numeric(mean_muscle_length)")

regplot(model, mod = "site")



