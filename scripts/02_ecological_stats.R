### GLOBAL REEF | R WORKSHOP ####
# DAY 2: ECOLOGICAL STATISTICS IN R
# Build from a simple model to the JAE abundance model
# Full analyses and additional examples: /references


### 01. SETUP & DATA ####

source("scripts/00_setup.R")

# Import the prepared survey and feeding-guild datasets.

fish_surveys <- read_csv(
  file.path(data_dir, "fish_surveys_clean.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    Date = as.Date(Date),
    type = factor(type, levels = reef_type_levels),
    pair = factor(pair, levels = pair_levels)
  )

fish_guilds <- read_csv(
  file.path(data_dir, "fish_guilds_clean.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    Date = as.Date(Date),
    type = factor(type, levels = reef_type_levels),
    pair = factor(pair, levels = pair_levels),
    feeding_guild = factor(
      feeding_guild,
      levels = feeding_guild_levels
    ),
    survey_id = factor(survey_id)
  )

# Standardise time using the survey dataset.
# Apply the same transformation to the guild dataset.

first_date <- min(fish_surveys$Date)

mean_day <- mean(as.numeric(fish_surveys$Date - first_date))
sd_day <- sd(as.numeric(fish_surveys$Date - first_date))

fish_surveys <- fish_surveys %>%
  mutate(
    date_s = (as.numeric(Date - first_date) - mean_day) / sd_day
  )

fish_guilds <- fish_guilds %>%
  mutate(
    date_s = (as.numeric(Date - first_date) - mean_day) / sd_day
  )


### 02. UNDERSTANDING OUR RESPONSE ####

# Our response is a count: total fish per survey.
# Each survey is one observation in this dataset.

glimpse(fish_surveys)

# Inspect the distribution of fish counts.

ggplot(fish_surveys, aes(x = total_abundance)) +
  geom_histogram(bins = 25) +
  labs(
    x = "Total fish abundance per survey",
    y = "Number of surveys"
  ) +
  theme_clean

# Compare mean and variance.

fish_surveys %>%
  summarise(
    mean_count = mean(total_abundance),
    variance = var(total_abundance)
  )
# HUGE amount of variance.... we'll come back to this 

### 03. OUR FIRST STATISTICAL MODEL ####

# A linear model describes the relationship between
# a response variable and an explanatory variable.
#
# total_abundance ~ type means abundance is modelled
# as a function of reef type.

model_lm <- lm(
  total_abundance ~ type,
  data = fish_surveys
)

summary(model_lm)

# QUESTION:
# Is the normal-error assumption appropriate for counts?


### 04. MODELLING COUNT DATA ####

#### 4.1 Poisson GLM ####

# A Poisson GLM is designed for count responses.
# The log link keeps expected counts positive.

model_pois <- glm(
  total_abundance ~ type,
  family = poisson, ## <- we changed this and specified the distributional family 
  data = fish_surveys
)

summary(model_pois)

# Exponentiate coefficients to interpret
# multiplicative effects on expected abundance.

exp(coef(model_pois))
# Result: Natural reefs average 149.7 fish per survey; artificial reefs have 0.863 times this abundance (13.7% fewer) under the Poisson model.


#### 4.2 Negative-binomial GLM ####

# Ecological counts often show overdispersion (i.e. when the variance exceeds the mean, which it definitely does here!!!) .
# Negative-binomial distributions allow extra variation.

model_nb <- glmmTMB(
  total_abundance ~ type,
  family = nbinom2, # changed here to negative binomial 
  data = fish_surveys
)

summary(model_nb)

# Compare fitted models using AIC as one measure
# of model fit penalised for complexity.

AIC(model_pois, model_nb)
# lower AIC = better fit, so nb model is WAY better 
# RESULTS: 
# Poisson suggests a highly significant reef-type difference (p < 0.001), but its underestimated variance makes this inference unreliable.
# Negative binomial accounts for overdispersion: the estimated difference remains 13.7%, but is no longer significant (p = 0.176).
# AIC decreases from 14039 to 2195, providing strong evidence that the negative-binomial distribution fits these data better.

### 05. ADDING TIME & INTERACTIONS ####

# First, add a common temporal trend.

model_time <- glmmTMB(
  total_abundance ~ type + date_s, # "+ date_s" is an additive effect
  family = nbinom2,
  data = fish_surveys
)

summary(model_time)

# RESULT: 
# The additive model estimates a common temporal slope for both reef types, with no evidence of an overall temporal change (p = 0.582).

# Now allow different temporal slopes for each reef type.
# The * operator includes main effects and their interaction.

model_interaction <- glmmTMB(
  total_abundance ~ type * date_s, # changing + to * means that these two factors are now interacting 
  family = nbinom2,
  data = fish_surveys
)

summary(model_interaction)

# RESULT: 
# The interaction allows artificial and natural reefs to have different temporal slopes, 
# but their estimated difference is not significant (p = 0.522).


# Include the three study pairs.
# This allows reef-type differences in slopes to vary by pair.

model_pair <- glmmTMB(
  total_abundance ~ type * pair * date_s, # three-way interaction!
  family = nbinom2,
  data = fish_surveys
)

summary(model_pair)

# RESULT 
# Temporal differences between reef types vary among study pairs, with the 
# strongest positive artificial-versus-natural slope difference at Aow Mao.
# The negative three-way coefficients indicate that this slope difference is substantially smaller at No Name and Sattakut.

# QUESTION:
# How does the interpretation change when we add
# a time-by-reef-type interaction?


### 06. FIXED & RANDOM EFFECTS ####

# Total abundance combines several ecological guilds.
# We now use one count per feeding guild, per survey.

glimpse(fish_guilds)

# Fixed effects describe systematic ecological relationships.
# Random effects account for shared variation in grouped data.
#
# Multiple guild counts from the same survey are not independent.
#
# (1 | survey_id) adds a random intercept for each survey, because we went back to the same sites multiple times.

model_guild <- glmmTMB(
  Count ~ feeding_guild * type * pair * date_s +
    (1 | survey_id),
  family = nbinom2,
  data = fish_guilds
)

summary(model_guild)


### 07. THE ATTRACTOR VS PRODUCER MODEL ####

# The submitted attractor vs producer model also included a guild-specific
# zero-inflation component.
#
# This is an instructor-led demonstration of the full model.

model_jae <- glmmTMB(
  Count ~ feeding_guild * type * pair * date_s +
    (1 | survey_id),
  ziformula = ~feeding_guild,
  family = nbinom2,
  data = fish_guilds
)

summary(model_jae)


### 08. MODEL DIAGNOSTICS ####

# A model must be checked, not just fitted.
# DHARMa simulates scaled residuals.

set.seed(42)

model_residuals <- simulateResiduals(
  model_jae,
  n = 500
)

plot(model_residuals)

# Test for dispersion and excess zeros.

testDispersion(model_residuals)
testZeroInflation(model_residuals)

# DISCUSS:
# What would systematic residual patterns suggest?


### 09. INTERPRETING ECOLOGICAL EFFECTS ####

# Interactions make individual coefficients difficult
# to interpret without considering factor combinations.
#
# emtrends() estimates temporal slopes within reef types
# and study pairs on the conditional model's log scale.

trends <- emtrends(
  model_jae,
  ~type | pair,
  var = "date_s",
  component = "cond"
)

summary(trends)

# Calculate Artificial minus Natural slope differences.
# Natural is the first factor level.

contrast(
  trends,
  method = list(
    "Artificial - Natural" = c(-1, 1)
  )
)

# ECOLOGICAL INTERPRETATION:
# Do temporal patterns differ between artificial and
# natural reefs?
#
# RESULT: 
# Aow Mao: Artificial reefs show a positive temporal slope (0.489), while natural reefs show a slightly negative slope (-0.095).
# The artificial–natural difference is significant at Aow Mao (β = 0.583, p < 0.0001), but not at No Name (p = 0.648) or Sattakut (p = 0.173).

 
### 10. BACK-TRANSFORM & PLOT GLMM PREDICTIONS ####

# Create prediction dates for each reef type, pair and feeding guild.
# Keep predictions within the observed date range of each group.

prediction_grid <- fish_guilds %>%
  group_by(pair, type, feeding_guild) %>%
  reframe(
    date_s = seq(min(date_s), max(date_s), length.out = 100)
  ) %>%
  mutate(
    survey_id = fish_guilds$survey_id[1]
  )

# Predict on the log scale, with standard errors.
# re.form = NA excludes survey-specific random effects.

pred <- predict(
  model_jae,
  newdata = prediction_grid,
  type = "link",
  se.fit = TRUE,
  re.form = NA
)

# Back-transform estimates and 95% confidence intervals.
# exp() converts log expected counts back into fish abundance.

prediction_grid <- prediction_grid %>%
  mutate(
    predicted = exp(pred$fit),
    lower = exp(pred$fit - 1.96 * pred$se.fit),
    upper = exp(pred$fit + 1.96 * pred$se.fit),
    Date = first_date + (date_s * sd_day + mean_day)
  )

# Plot observed counts, fitted trajectories and confidence ribbons.

fig_glmm <- ggplot(
  fish_guilds,
  aes(x = Date, y = Count, colour = type)
) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_ribbon(
    data = prediction_grid,
    aes(x = Date, ymin = lower, ymax = upper, fill = type),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  geom_line(
    data = prediction_grid,
    aes(y = predicted),
    linewidth = 0.9
  ) +
  facet_grid(pair ~ feeding_guild, scales = "free_y") +
  scale_colour_manual(values = reef_cols) +
  scale_fill_manual(values = reef_cols) +
  labs(
    x = "Survey date",
    y = "Expected feeding-guild abundance",
    colour = "Reef type",
    fill = "Reef type"
  ) +
  theme_clean

fig_glmm

### 11. OPTIONAL: GENERALISED ADDITIVE MODELS ####

# GAMs allow relationships to curve rather than assuming
# a constant rate of change over time.
#
# Each reef type and study pair receives its own temporal smooth.

fish_surveys <- fish_surveys %>%
  mutate(
    reef_pair = interaction(pair, type, drop = TRUE)
  )

model_gam <- mgcv::gam(
  total_abundance ~ reef_pair +
    s(date_s, by = reef_pair, k = 5),
  family = mgcv::nb(),
  method = "REML",
  data = fish_surveys
)

summary(model_gam)

# Check whether the smooth complexity is adequate.

mgcv::gam.check(model_gam)


#### 11.1 Generate GAM predictions ####

gam_grid <- fish_surveys %>%
  group_by(pair, type, reef_pair) %>%
  reframe(
    date_s = seq(min(date_s), max(date_s), length.out = 100),
    .groups = "drop"
  ) %>%
  mutate(
    reef_pair = factor(
      as.character(reef_pair),
      levels = levels(fish_surveys$reef_pair)
    )
  )

# Predict on the log scale and back-transform.

gam_pred <- predict(
  model_gam,
  newdata = gam_grid,
  type = "link",
  se.fit = TRUE
)

gam_grid <- gam_grid %>%
  mutate(
    predicted = exp(gam_pred$fit),
    lower = exp(gam_pred$fit - 1.96 * gam_pred$se.fit),
    upper = exp(gam_pred$fit + 1.96 * gam_pred$se.fit),
    Date = first_date + (date_s * sd_day + mean_day)
  )


#### 11.2 Plot nonlinear GAM trajectories ####

fig_gam <- ggplot(
  fish_surveys,
  aes(x = Date, y = total_abundance, colour = type)
) +
  geom_point(alpha = 0.3, size = 1) +
  geom_ribbon(
    data = gam_grid,
    aes(x = Date, ymin = lower, ymax = upper, fill = type),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  geom_line(
    data = gam_grid,
    aes(y = predicted),
    linewidth = 1
  ) +
  facet_wrap(~pair, ncol = 1) +
  scale_colour_manual(values = reef_cols) +
  scale_fill_manual(values = reef_cols) +
  labs(
    x = "Survey date",
    y = "Expected total fish abundance",
    colour = "Reef type",
    fill = "Reef type"
  ) +
  theme_clean

fig_gam



### COMPARE LINEAR MODELS AND GAMS ####

# Add predictions from the linear-time negative-binomial model.
gam_grid$linear_pred <- predict(
  model_pair,
  newdata = gam_grid,
  type = "response"
)

# Reshape predictions so each model gets its own panel.
compare_grid <- gam_grid %>%
  select(pair, type, Date, predicted, linear_pred) %>%
  pivot_longer(
    cols = c(linear_pred, predicted),
    names_to = "model",
    values_to = "fit"
  ) %>%
  mutate(
    model = factor(
      model,
      levels = c("linear_pred", "predicted"),
      labels = c("Linear-time NB GLM", "Nonlinear GAM")
    )
  )

# Repeat observed data across both model panels.
compare_observed <- fish_surveys %>%
  tidyr::crossing(
    model = levels(compare_grid$model)
  )

# Plot models side by side with matching axes.
fig_compare <- ggplot(
  compare_observed,
  aes(x = Date, y = total_abundance, colour = type)
) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_line(
    data = compare_grid,
    aes(y = fit),
    linewidth = 1
  ) +
  facet_grid(
    pair ~ model,
    scales = "free_y"
  ) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Survey date",
    y = "Total fish abundance per survey",
    colour = "Reef type"
  ) +
  theme_clean

fig_compare

### 12. FINAL DISCUSSION ####

# What have we learned?
#
# 1. Model structure depends on the response distribution
# 2. Interactions describe conditional relationships
# 3. Random effects address dependence in grouped observations
# 4. Diagnostics are essential
# 5. Statistical results require ecological interpretation

