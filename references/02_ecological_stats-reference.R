
### GLOBAL REEF | R WORKSHOP ####
# DAY 2: ECOLOGICAL STATISTICS IN R
#
# Research question:
# How do temporal fish abundance patterns differ between
# artificial and natural reefs, study pairs and feeding guilds?
#
# This workshop introduces:
#   - Sampling units and statistical inference
#   - Probability distributions and overdispersion
#   - Linear models and generalised linear models
#   - Fixed effects, interactions and random effects
#   - Negative-binomial generalised linear mixed models
#   - Model diagnostics, predictions and contrasts
#
# We progressively introduce statistical concepts using
# the actual data and model structure from our attractor vs producer study.
#
# IMPORTANT:
# This is a teaching progression, not an automated procedure
# for selecting the statistically "best" model.


### 01. SETUP ####

# Load packages, project paths, factor levels and plotting settings.
source("scripts/00_setup.R")

# Import the analysis-ready datasets created during Day 1.
#
# fish_surveys: one row per survey
# fish_guilds: one row per survey and feeding guild

fish_surveys <- read_csv(
  file.path(data_dir, "fish_surveys_clean.csv"),
  show_col_types = FALSE
)

fish_guilds <- read_csv(
  file.path(data_dir, "fish_guilds_clean.csv"),
  show_col_types = FALSE
)

# CSV files do not preserve R factor classes.
# Restore the categorical ordering defined in 00_setup.R.

fish_surveys <- fish_surveys %>%
  mutate(
    Date = as.Date(Date),
    type = factor(type, levels = reef_type_levels),
    pair = factor(pair, levels = pair_levels),
    survey_id = factor(survey_id)
  )

fish_guilds <- fish_guilds %>%
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

# Check the datasets before beginning.

glimpse(fish_surveys)
glimpse(fish_guilds)

# Models for count data require non-negative integer responses.

stopifnot(
  !anyNA(fish_surveys$total_abundance),
  !anyNA(fish_guilds$Count),
  all(fish_surveys$total_abundance >= 0),
  all(fish_guilds$Count >= 0),
  all(fish_surveys$total_abundance ==
        floor(fish_surveys$total_abundance)),
  all(fish_guilds$Count == floor(fish_guilds$Count))
)


### 02. SAMPLING & STATISTICAL INFERENCE ####

#### 2.1 What is our sampling unit? ####

# An observational unit is the unit contributing one
# observation to the statistical analysis.
#
# For total abundance:
#   One survey = one site on one date
#
# For feeding-guild abundance:
#   One survey contributes several guild observations.
#
# Measurements from the same survey share environmental
# conditions and are not necessarily independent.

nrow(fish_surveys)
n_distinct(fish_surveys$survey_id)

nrow(fish_guilds)
n_distinct(fish_guilds$survey_id)

# Check the number of feeding-guild observations per survey.

fish_guilds %>%
  count(survey_id, name = "n_guilds") %>%
  count(n_guilds)

# Check the number of independent surveys per reef type.

fish_surveys %>%
  count(pair, type)


#### 2.2 Descriptive statistics ####

# Descriptive statistics summarise observed patterns.
# Inferential statistics estimate relationships and
# quantify uncertainty about those relationships.

fish_surveys %>%
  group_by(type) %>%
  summarise(
    n = n(),
    mean = mean(total_abundance),
    median = median(total_abundance),
    variance = var(total_abundance),
    sd = sd(total_abundance),
    .groups = "drop"
  )


#### 2.3 Explore the response distribution ####

# Plot the frequency distribution of fish counts.
#
# Ecological count data are often right-skewed and
# may contain many zeros or unusually large values.

ggplot(fish_surveys, aes(x = total_abundance)) +
  geom_histogram(
    bins = 25,
    fill = reef_cols["Artificial"],
    colour = "white"
  ) +
  labs(
    x = "Total fish abundance per survey",
    y = "Number of surveys"
  ) +
  theme_clean

# Compare mean and variance within each reef type.
#
# The Poisson distribution assumes that conditional
# variance equals the conditional mean.
# Larger observed variances motivate us to investigate
# overdispersion, although raw variance alone does not
# diagnose overdispersion after fitting a model.

fish_surveys %>%
  group_by(type) %>%
  summarise(
    mean_count = mean(total_abundance),
    variance = var(total_abundance),
    variance_to_mean = variance / mean_count,
    .groups = "drop"
  )


### 03. INTRODUCTION TO LINEAR MODELS ####

#### 3.1 What is a statistical model? ####

# A model describes the relationship between a response
# variable and one or more explanatory variables.
#
# Response: total fish abundance
# Predictor: reef type
#
# We begin with a simple linear model:
#
# total_abundance ~ type
#
# The ~ symbol means "modelled as a function of".

model_lm <- lm(
  total_abundance ~ type,
  data = fish_surveys
)

# summary() reports estimated coefficients,
# standard errors, test statistics and model fit.

summary(model_lm)


#### 3.2 Understanding model coefficients ####

# Natural is the reference category because we specified:
# reef_type_levels <- c("Natural", "Artificial")
#
# The intercept estimates the mean for Natural reefs.
# The typeArtificial coefficient estimates the difference
# between Artificial and Natural reefs.

coef(model_lm)

# Fitted values are predictions from the model.

head(fitted(model_lm))

# Residuals are observed values minus fitted values.

head(residuals(model_lm))


#### 3.3 Examine linear model assumptions ####

# Standard linear models assume an appropriate linear
# mean structure, independent errors, approximately
# normal residuals and constant residual variance.
#
# Count data often violate the latter two assumptions.
#
# Our repeated site surveys also raise questions about
# independence that this simple demonstration ignores.

plot(model_lm)

# DISCUSSION:
# Does a Gaussian linear model seem appropriate for
# non-negative integer fish abundances?


### 04. GENERALISED LINEAR MODELS ####

#### 4.1 Poisson regression ####

# Generalised linear models (GLMs) extend linear models
# to other response distributions.
#
# A Poisson GLM is commonly used for count data.
#
# family = poisson specifies:
#   - Poisson response distribution
#   - Log link function (default)
#
# The log link ensures predicted mean counts are positive.

model_poisson <- glm(
  total_abundance ~ type,
  family = poisson(link = "log"),
  data = fish_surveys
)

summary(model_poisson)


#### 4.2 Understanding the log link ####

# Poisson model coefficients are estimated on the log scale.
#
# Exponentiating a coefficient converts it to a
# multiplicative effect on the expected count.
#
# For typeArtificial:
#   exp(beta) > 1 indicates higher expected counts
#   exp(beta) < 1 indicates lower expected counts
#
# The interpretation is conditional on the fitted model.

coef(model_poisson)

exp(coef(model_poisson))

# Predict expected counts for each reef type.

new_reefs <- data.frame(
  type = factor(
    c("Natural", "Artificial"),
    levels = reef_type_levels
  )
)

predict(
  model_poisson,
  newdata = new_reefs,
  type = "response"
)


#### 4.3 Checking overdispersion ####

# A Poisson model assumes conditional variance = mean.
#
# Overdispersion occurs when the variance exceeds
# what the fitted Poisson model allows.
#
# A simple introductory diagnostic is the Pearson
# chi-square statistic divided by residual degrees of freedom.
#
# This is an approximate indicator, not a definitive test.

poisson_dispersion <- sum(
  residuals(model_poisson, type = "pearson")^2
) / df.residual(model_poisson)

poisson_dispersion

# Values substantially above 1 suggest overdispersion.
# The DHARMa diagnostics later provide a fuller assessment.


#### 4.4 Negative-binomial regression ####

# Negative-binomial models are useful for overdispersed
# count data because they permit greater variance.
#
# We use glmmTMB, which also supports mixed models.
#
# nbinom2 uses a quadratic mean-variance relationship.
# nbinom1 instead uses a linear mean-variance relationship.
#
# Neither is automatically preferable in every dataset.

model_nb <- glmmTMB(
  total_abundance ~ type,
  family = nbinom2,
  data = fish_surveys
)

summary(model_nb)

# Compare the Poisson and negative-binomial models.
# AIC provides one comparison of predictive fit and
# model complexity, but does not replace diagnostics
# or ecological reasoning.

AIC(model_poisson, model_nb)

# QUESTION:
# Why might a negative-binomial model better represent
# fish abundance than a Poisson model?


### 05. ADDING TIME TO OUR MODELS ####

#### 5.1 Create a numerical time variable ####

# R stores Dates internally as numbers, but models become
# easier to interpret when we explicitly define time.
#
# date_num = days since the first survey
#
# date_s = standardised time:
#   subtract the mean and divide by the standard deviation
#
# Standardisation helps with model fitting and creates
# a consistent time scale across the analysis.
#
# We calculate the time scaling from the survey dataset
# and reuse it for the guild dataset.

first_date <- min(fish_surveys$Date)

date_mean <- mean(as.numeric(fish_surveys$Date - first_date))
date_sd <- sd(as.numeric(fish_surveys$Date - first_date))

stopifnot(date_sd > 0)

fish_surveys <- fish_surveys %>%
  mutate(
    date_num = as.numeric(Date - first_date),
    date_s = (date_num - date_mean) / date_sd
  )

fish_guilds <- fish_guilds %>%
  mutate(
    date_num = as.numeric(Date - first_date),
    date_s = (date_num - date_mean) / date_sd
  )

# Inspect the transformed variables.

fish_surveys %>%
  select(Date, date_num, date_s) %>%
  head()


#### 5.2 Add time as a predictor ####

# A model with two additive predictors:
#
# total_abundance ~ type + date_s
#
# This estimates a reef-type difference and a common
# temporal slope, assuming the slope is the same
# for both reef types.

model_nb_time <- glmmTMB(
  total_abundance ~ type + date_s,
  family = nbinom2,
  data = fish_surveys
)

summary(model_nb_time)


#### 5.3 Introduce interactions ####

# What if abundance changes differently over time
# on artificial and natural reefs?
#
# We need an INTERACTION.
#
# type * date_s is shorthand for:
# type + date_s + type:date_s
#
# The interaction allows each reef type to have
# a different temporal slope.

model_nb_interaction <- glmmTMB(
  total_abundance ~ type * date_s,
  family = nbinom2,
  data = fish_surveys
)

summary(model_nb_interaction)

# Compare additive and interaction specifications.
# This is an illustrative comparison, not a rule that
# interactions should be retained only when significant.

AIC(model_nb_time, model_nb_interaction)


#### 5.4 Visualising an interaction ####

# Predictions are often easier to understand than
# individual regression coefficients.
#
# Create new observations spanning the study period,
# for each reef type.
#
# expand.grid() generates the combinations we request.

prediction_grid <- expand.grid(
  date_s = seq(
    min(fish_surveys$date_s),
    max(fish_surveys$date_s),
    length.out = 100
  ),
  type = factor(reef_type_levels, levels = reef_type_levels)
)

# Predict the expected abundance on the response scale.

prediction_grid$predicted <- predict(
  model_nb_interaction,
  newdata = prediction_grid,
  type = "response"
)

# Convert scaled time back into calendar dates.

prediction_grid <- prediction_grid %>%
  mutate(
    Date = first_date + (date_s * date_sd + date_mean)
  )

# Plot observed counts and predicted trends.

ggplot(
  fish_surveys,
  aes(x = Date, y = total_abundance, colour = type)
) +
  geom_point(alpha = 0.35) +
  geom_line(
    data = prediction_grid,
    aes(y = predicted),
    linewidth = 1
  ) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Survey date",
    y = "Total fish abundance",
    colour = "Reef type"
  ) +
  theme_clean


### 06. ACCOUNTING FOR STUDY PAIRS ####

#### 6.1 Add study pair ####

# Our artificial and natural reefs are organised into
# three distinct paired systems.
#
# Pair may explain baseline differences in fish abundance.
#
# First, add pair as an additive predictor.

model_nb_pair <- glmmTMB(
  total_abundance ~ type + pair + date_s,
  family = nbinom2,
  data = fish_surveys
)

summary(model_nb_pair)


#### 6.2 Different slopes among study pairs ####

# The ecological question is not simply whether
# artificial reefs have more fish overall.
#
# We also want to know whether their temporal
# trajectories differ across reef pairs.
#
# type * pair * date_s includes:
#   - Three main effects
#   - All two-way interactions
#   - The three-way interaction
#
# This permits distinct temporal trajectories
# for each reef type within each pair.

model_nb_threeway <- glmmTMB(
  total_abundance ~ type * pair * date_s,
  family = nbinom2,
  data = fish_surveys
)

summary(model_nb_threeway)

# Three-way interactions are difficult to interpret
# from coefficients alone.
#
# We will use estimated slopes and model predictions
# to communicate these relationships.


### 07. FROM TOTAL COUNTS TO FEEDING GUILDS ####

#### 7.1 Why examine ecological guilds? ####

# Total abundance combines ecologically different fishes.
#
# Herbivores, invertivores, mesopredators and higher
# trophic-level predators may respond differently.
#
# Our data are now structured as:
# one row per survey AND feeding guild.

fish_guilds %>%
  select(
    survey_id, pair, type, Date, feeding_guild, Count
  ) %>%
  head(12)

# Summarise the observed guild counts.

fish_guilds %>%
  group_by(type, feeding_guild) %>%
  summarise(
    mean_count = mean(Count),
    variance = var(Count),
    .groups = "drop"
  )


#### 7.2 Introduce feeding guild as a predictor ####

# Feeding guild becomes an additional fixed effect.
#
# This model permits average abundance differences
# between guilds but assumes common reef-type and
# temporal relationships across the guilds.
#
# It is a teaching step, not our final model.

model_guild_additive <- glmmTMB(
  Count ~ feeding_guild + type + pair + date_s,
  family = nbinom2,
  data = fish_guilds
)

summary(model_guild_additive)


#### 7.3 Introduce interactions with guild ####

# The ecological question requires us to test whether
# the artificial-natural reef difference in temporal
# trends varies across feeding guilds and study pairs.
#
# We therefore specify a four-way interaction:
#
# feeding_guild * type * pair * date_s
#
# This includes all lower-order terms and interactions.

model_guild_fixed <- glmmTMB(
  Count ~ feeding_guild * type * pair * date_s,
  family = nbinom2,
  data = fish_guilds
)

summary(model_guild_fixed)

# However, observations from different feeding guilds
# within the same survey are not independent.
#
# The model above is therefore an instructional
# intermediate step, not our preferred final inference.


### 08. GENERALIZED LINEAR MIXED MODELS ####

#### 8.1 Fixed effects vs random effects ####

# FIXED EFFECTS describe systematic relationships that
# we want to estimate, such as:
#   - Reef type
#   - Study pair
#   - Feeding guild
#   - Time and their interactions
#
# RANDOM EFFECTS represent variation associated with
# grouping structures and account for dependence.
#
# Four feeding-guild counts from the same survey share
# conditions and sampling history.
#
# (1 | survey_id) adds a random intercept for each survey.
#
# The 1 indicates an intercept.
# The | identifies the grouping variable.


#### 8.2 Add a survey random intercept ####

model_guild_mixed <- glmmTMB(
  Count ~ feeding_guild * type * pair * date_s +
    (1 | survey_id),
  family = nbinom2,
  data = fish_guilds
)

summary(model_guild_mixed)

# VarCorr() displays the estimated random-effect variance.

VarCorr(model_guild_mixed)

# The random intercept permits surveys to differ
# in their baseline expected abundance.
#
# It accounts for shared survey-level variation,
# although further spatial or temporal correlation
# could remain in the data.


#### 8.3 Introduce zero inflation ####

# Ecological count data can contain many zeros.
#
# A negative-binomial distribution already permits zeros.
# Therefore, zero inflation is not automatically needed.
#
# A zero-inflated model adds a second process describing
# the probability of an additional structural zero.
#
# The submitted attractor vs producer model used a guild-specific
# zero-inflation component:
#
# ziformula = ~feeding_guild

model_guild_zi <- glmmTMB(
  Count ~ feeding_guild * type * pair * date_s +
    (1 | survey_id),
  ziformula = ~feeding_guild,
  family = nbinom2,
  data = fish_guilds
)

summary(model_guild_zi)

# Compare the fitted models as an initial reference.
# We must also examine diagnostics, convergence,
# biological meaning and parameter identifiability.

AIC(model_guild_mixed, model_guild_zi)


#### 8.4 Our attractor vs producer abundance model ####

# We have now reached the structure of the submitted
# attractor vs producer abundance model:
#
# Response:
#   Feeding-guild abundance per survey
#
# Fixed effects:
#   Feeding guild, reef type, pair, standardised date
#   and their interactions
#
# Random effects:
#   Survey-level random intercept
#
# Distribution:
#   Negative binomial (nbinom2)
#
# Zero-inflation component:
#   Feeding guild
#
# Save the final model under a descriptive name.

abundance_final <- model_guild_zi


### 09. MODEL DIAGNOSTICS ####

#### 9.1 Why check model assumptions? ####

# A model producing coefficients and p-values does
# not mean the model is necessarily appropriate.
#
# Diagnostics help evaluate whether the assumed
# distribution and residual structure are reasonable.
#
# DHARMa uses simulations to produce scaled residuals.


#### 9.2 Simulate residuals ####

# Set a seed so simulated diagnostics are reproducible.

set.seed(42)

residuals_final <- simulateResiduals(
  fittedModel = abundance_final,
  n = 1000
)

# Inspect the default diagnostic plots.

plot(residuals_final)


#### 9.3 Test important assumptions ####

# Test for residual dispersion.

testDispersion(residuals_final)

# Test whether the model predicts the observed
# frequency of zeros adequately.

testZeroInflation(residuals_final)

# Test for systematic deviations in the residual
# distribution.

testUniformity(residuals_final)


#### 9.4 Check residual patterns ####

# Inspect potential residual relationships with
# continuous time and categorical grouping variables.

plotResiduals(
  residuals_final,
  fish_guilds$date_s
)

plotResiduals(
  residuals_final,
  fish_guilds$feeding_guild
)

# We should also consider whether observations
# across repeated visits to the same sites display
# residual temporal dependence.
#
# A survey random intercept does not automatically
# account for serial correlation between dates.


#### 9.5 Check the model fit ####

# Check warnings, convergence and potential
# model singularities or identifiability issues.

summary(abundance_final)

performance::check_convergence(abundance_final)

# DISCUSSION:
# If a diagnostic reveals a problem, what assumption
# might be incorrect?
#
# Would changing the response distribution help?
# Could an omitted temporal structure be responsible?
# Would another model specification be more defensible?


### 10. INTERPRETING MODEL RESULTS ####

#### 10.1 Why not interpret every coefficient? ####

# A four-way interaction model has many coefficients.
#
# Individual coefficients depend on:
#   - Reference factor levels
#   - The scale of the response
#   - Which interactions are included
#
# Estimated marginal trends allow us to ask more
# directly interpretable ecological questions.


#### 10.2 Estimated temporal trends by reef type ####

# emtrends() estimates the slope with respect to date_s.
#
# We use the conditional count-model component.
# Slopes are on the log scale by default here.
#
# Equal weighting gives equal emphasis to the factor
# combinations represented in the marginal estimates.

trends_type <- emtrends(
  abundance_final,
  ~type,
  var = "date_s",
  component = "cond",
  weights = "equal"
)

summary(trends_type)


#### 10.3 Compare reef-type slopes ####

# A contrast tests the difference between estimated
# artificial and natural reef temporal slopes.
#
# Our factor order is Natural, Artificial.
# Therefore c(-1, 1) gives Artificial - Natural.

contrast(
  trends_type,
  method = list(
    "Artificial - Natural" = c(-1, 1)
  )
)

# The result represents a difference in LOG-scale slopes,
# not a difference in fish counts or raw linear growth.


#### 10.4 Trends within each study pair ####

# Do artificial and natural reef trajectories differ
# in the same way across all three paired systems?

trends_pair <- emtrends(
  abundance_final,
  ~type | pair,
  var = "date_s",
  component = "cond"
)

summary(trends_pair)

pair_contrasts <- contrast(
  trends_pair,
  method = list(
    "Artificial - Natural" = c(-1, 1)
  )
)

summary(pair_contrasts)


#### 10.5 Trends within pairs and feeding guilds ####

# Now estimate separate artificial and natural
# reef slopes for every pair and feeding guild.

trends_guild <- emtrends(
  abundance_final,
  ~type | pair * feeding_guild,
  var = "date_s",
  component = "cond"
)

summary(trends_guild)

# Calculate the artificial-natural difference
# in temporal slopes within each combination.

guild_contrasts <- contrast(
  trends_guild,
  method = list(
    "Artificial - Natural" = c(-1, 1)
  )
)

# Adjust for multiple comparisons using Holm's method
# across the full collection of pair-by-guild contrasts.

summary(
  guild_contrasts,
  by = NULL,
  adjust = "holm"
)

# NOTE:
# The multiple-comparison family and correction
# should be specified based on the research questions,
# not chosen according to which produces significance.


### 11. VISUALISING MODEL PREDICTIONS ####

#### 11.1 Create a prediction dataset ####

# To visualise the fitted model, we calculate predicted
# counts over time for every reef-type, pair and guild.
#
# Prediction dates are limited to the observed range
# within each pair-by-type-by-guild combination.
#
# This avoids drawing trajectories outside the period
# actually represented by each group.

prediction_guilds <- fish_guilds %>%
  group_by(pair, type, feeding_guild) %>%
  reframe(
    date_s = seq(
      min(date_s),
      max(date_s),
      length.out = 100
    )
  )

# glmmTMB requires a survey_id column because the
# model includes (1 | survey_id).
#
# We use an existing factor level as a placeholder.
# re.form = NA below excludes survey-specific random
# effects from the population-level prediction.

prediction_guilds <- prediction_guilds %>%
  mutate(
    survey_id = factor(
      levels(fish_guilds$survey_id)[1],
      levels = levels(fish_guilds$survey_id)
    )
  )


#### 11.2 Obtain model predictions ####

# type = "link" returns predictions on the log-count scale.
#
# se.fit = TRUE requests standard errors.
#
# re.form = NA excludes random effects.
#
# The default component used here is the conditional
# abundance model, rather than a zero-adjusted mean.

predicted_values <- predict(
  abundance_final,
  newdata = prediction_guilds,
  type = "link",
  se.fit = TRUE,
  re.form = NA
)

# Add predictions and their uncertainty intervals.
#
# Approximate 95% confidence limits:
# fitted estimate +/- 1.96 * standard error
#
# Because the link function is logarithmic,
# exponentiation converts values back to counts.

prediction_guilds <- prediction_guilds %>%
  mutate(
    fit_link = predicted_values$fit,
    se_link = predicted_values$se.fit,
    predicted = exp(fit_link),
    lower = exp(fit_link - 1.96 * se_link),
    upper = exp(fit_link + 1.96 * se_link),
    Date = first_date + (date_s * date_sd + date_mean)
  )


#### 11.3 Plot observed and predicted abundance ####

# The points are observed feeding-guild counts.
# The lines are conditional model predictions.
# The ribbons represent approximate uncertainty
# around the fitted conditional mean.
#
# These intervals are NOT intervals around
# individual future observations.

fig_model <- ggplot(
  fish_guilds,
  aes(x = Date, y = Count, colour = type)
) +
  geom_point(
    alpha = 0.25,
    size = 0.9
  ) +
  geom_ribbon(
    data = prediction_guilds,
    aes(
      x = Date,
      ymin = lower,
      ymax = upper,
      fill = type
    ),
    inherit.aes = FALSE,
    alpha = 0.2,
    colour = NA
  ) +
  geom_line(
    data = prediction_guilds,
    aes(x = Date, y = predicted),
    linewidth = 0.9
  ) +
  facet_grid(
    pair ~ feeding_guild,
    scales = "free_y"
  ) +
  scale_colour_manual(values = reef_cols) +
  scale_fill_manual(values = reef_cols) +
  labs(
    x = "Survey date",
    y = "Feeding-guild abundance",
    colour = "Reef type",
    fill = "Reef type"
  ) +
  theme_clean

fig_model

# INTERPRETATION:
#
# Which feeding guilds show contrasting trajectories?
# Are those differences consistent across the three pairs?
# How does the mature Sattakut wreck compare with the
# newer Aow Mao and No Name deployments?
#
# Differences in abundance or trajectories alone cannot
# establish whether artificial reefs increase production
# rather than attract existing fish.


### 12. EXPORT STATISTICAL RESULTS ####

#### 12.1 Save estimated temporal slopes ####

# Convert emmeans summaries into data frames,
# allowing us to export the results as CSV files.

write_csv(
  as.data.frame(summary(trends_pair)),
  file.path(stats_dir, "02_temporal_trends_by_pair.csv")
)

write_csv(
  as.data.frame(summary(pair_contrasts)),
  file.path(stats_dir, "02_reef_type_slope_contrasts.csv")
)

write_csv(
  as.data.frame(
    summary(guild_contrasts, by = NULL, adjust = "holm")
  ),
  file.path(stats_dir, "02_guild_slope_contrasts.csv")
)


#### 12.2 Save the model figure ####

ggsave(
  filename = file.path(figures_dir, "03_glmm_predictions.png"),
  plot = fig_model,
  width = 14,
  height = 9,
  dpi = 300,
  bg = "white"
)


#### 12.3 Save the fitted model ####

# saveRDS() preserves an R object, including
# the fitted model and its associated information.
#
# readRDS() can reopen the model in a future session.

saveRDS(
  abundance_final,
  file.path(stats_dir, "02_abundance_glmm.rds")
)

# Example for a future session:
#
# abundance_final <- readRDS(
#   file.path(stats_dir, "02_abundance_glmm.rds")
# )


### 13. OPTIONAL: NONLINEAR TEMPORAL MODELS ####

#### 13.1 Why consider nonlinear relationships? ####

# So far, time enters our models as a linear predictor.
#
# However, ecological processes may change nonlinearly:
#   - Rapid initial colonisation
#   - Seasonal changes
#   - Changes in recruitment
#   - Gradual stabilisation
#
# Generalised additive models (GAMs) can represent
# smooth nonlinear relationships.


#### 13.2 Demonstrate a negative-binomial GAM ####

# We return to total abundance for a simpler example.
#
# s(date_num) represents a smooth function of time.
#
# k controls the maximum basis complexity.
# It is NOT the degree of a polynomial.
#
# This example is exploratory and does not replace
# our full attractor vs producer mixed model.

model_gam <- mgcv::gam(
  total_abundance ~ type + pair + s(date_num, k = 5),
  family = mgcv::nb(),
  method = "REML",
  data = fish_surveys
)

summary(model_gam)

# Plot the estimated smooth effect.

plot(
  model_gam,
  pages = 1,
  shade = TRUE
)

# A GAM can identify nonlinear trends, but flexible
# curves require appropriate data support and diagnostics.
#
# Smooth temporal effects alone do not account for
# dependence among repeated surveys at the same site.


### 14. FREQUENTIST & BAYESIAN INFERENCE ####

#### 14.1 Two approaches to statistical inference ####

# Our models today use frequentist estimation.
#
# Frequentist inference:
#   - Estimates model parameters from observed data
#   - Uses sampling distributions for uncertainty
#   - Reports confidence intervals and p-values
#
# Bayesian inference:
#   - Combines a likelihood with prior distributions
#   - Estimates posterior distributions of parameters
#   - Reports credible intervals and posterior probabilities
#
# Neither framework removes the need for:
#   - Appropriate model structure
#   - Defensible sampling assumptions
#   - Diagnostics and ecological interpretation
#
# EXAMPLE:
# Our earlier marine ecological research used Bayesian
# models to estimate habitat-associated patterns.
#
# We will discuss the conceptual differences using
# published examples rather than fitting a full Bayesian
# model during this workshop.


### 15. REPRODUCIBILITY & FINAL DISCUSSION ####

#### 15.1 Record our analytical environment ####

sessionInfo()


#### 15.2 Final discussion questions ####

# 1. Why is a linear model not always appropriate
#    for ecological count data?
#
# 2. What does the log link function achieve?
#
# 3. What does overdispersion tell us?
#
# 4. What is the difference between a main effect
#    and an interaction?
#
# 5. Why did we include a random survey intercept?
#
# 6. Why do we examine model diagnostics?
#
# 7. What can differences in artificial and natural
#    reef abundance tell us?
#
# 8. Why can't abundance trends alone distinguish
#    fish attraction from increased production?


#### 15.3 Workshop takeaways ####

# We have progressed from:
#
#   A simple comparison of average abundance
#
# To:
#
#   A negative-binomial mixed model accounting for
#   feeding guild, reef type, pair, time and survey
#   structure, with guild-specific zero inflation.
#
# The goal is not to memorise every model specification.
#
# The goal is to understand how research questions,
# sampling design, response distributions and model
# assumptions determine analytical decisions.
#
# A model is only useful when its assumptions and
# ecological interpretation are defensible.

message("Day 2 ecological statistics workshop complete.")
