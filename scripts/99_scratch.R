# 99_scratch.r 


# the final model we're trying to build 
fish_model <- glmmTMB(
  Count ~ feeding_guild * type * pair * date_s 
  + (1 | survey_id),
  ziformula = ~feeding_guild, 
  family = nbinom1, 
  data = abundance_data
)


### FOUR COMMON PROBABILITY DISTRIBUTIONS ####

set.seed(42)

dist_data <- bind_rows(
  tibble(value = rnorm(1000, mean = 20, sd = 5),
         distribution = "Gaussian"),
  tibble(value = rbinom(1000, size = 1, prob = 0.6),
         distribution = "Binomial"),
  tibble(value = rpois(1000, lambda = 20),
         distribution = "Poisson"),
  tibble(value = rnbinom(1000, mu = 20, size = 2),
         distribution = "Negative Binomial")
) %>%
  mutate(
    distribution = factor(
      distribution,
      levels = c("Gaussian", "Binomial", "Poisson", "Negative Binomial")
    )
  )

ggplot(dist_data, aes(x = value)) +
  geom_histogram(
    bins = 30,
    fill = "#477F91",
    colour = "white"
  ) +
  facet_wrap(~distribution, scales = "free", ncol = 2) +
  labs(
    x = "Observed value",
    y = "Frequency"
  ) +
  theme_clean +
  theme(
    strip.text = element_text(face = "bold", size = 12)
  )

### POISSON DISTRIBUTIONS ####

poisson_data <- expand.grid(
  Count = 0:30,
  lambda = c(2, 5, 12)
) %>%
  mutate(
    Probability = dpois(Count, lambda),
    lambda = factor(lambda, levels = c(2, 5, 12))
  )

# Overlapping probability curves
ggplot(
  poisson_data,
  aes(x = Count, y = Probability, colour = lambda, fill = lambda)
) +
  geom_area(alpha = 0.15, position = "identity") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_colour_manual(values = c("#B9C7B5", "#477F91", "#1F4E6B")) +
  scale_fill_manual(values = c("#B9C7B5", "#477F91", "#1F4E6B")) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  labs(
    x = "Fish count",
    y = "Probability",
    colour = expression(lambda),
    fill = expression(lambda)
  ) +
  theme_clean

### POISSON VS NEGATIVE BINOMIAL ####

compare_data <- expand.grid(
  Count = 0:50,
  distribution = c("Poisson", "Negative Binomial")
) %>%
  mutate(
    Probability = case_when(
      distribution == "Poisson" ~ dpois(Count, lambda = 12),
      distribution == "Negative Binomial" ~ dnbinom(Count, mu = 12, size = 3)
    ),
    distribution = factor(
      distribution,
      levels = c("Poisson", "Negative Binomial")
    )
  )

ggplot(
  compare_data,
  aes(
    x = Count,
    y = Probability,
    colour = distribution,
    fill = distribution
  )
) +
  geom_area(alpha = 0.18, position = "identity") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.3, alpha = 0.8) +
  scale_colour_manual(
    values = c("Poisson" = "#477F91", "Negative Binomial" = "#B85C38")
  ) +
  scale_fill_manual(
    values = c("Poisson" = "#477F91", "Negative Binomial" = "#B85C38")
  ) +
  labs(
    x = "Count",
    y = "Probability",
    colour = NULL,
    fill = NULL
  ) +
  theme_clean


### GLM FLOW DIAGRAM ####

library(ggplot2)

glm_flow <- data.frame(
  x = c(1, 3, 5, 7),
  y = c(1, 1, 1, 1),
  label = c(
    "Predictors\n(X variables)",
    "Linear predictor\nη = β0 + β1X1 + β2X2 + ...",
    "Inverse link\n(e.g. exp for Poisson)",
    "Expected response\nE(Y)"
  )
)

ggplot(glm_flow, aes(x, y)) +
  geom_label(
    aes(label = label),
    size = 4.2,
    label.padding = unit(0.35, "lines"),
    label.r = unit(0.15, "lines"),
    fill = "white"
  ) +
  annotate(
    "segment",
    x = c(1.7, 3.7, 5.7), xend = c(2.3, 4.3, 6.3),
    y = c(1, 1, 1), yend = c(1, 1, 1),
    arrow = arrow(length = unit(0.18, "inches")),
    linewidth = 0.8
  ) +
  annotate("text", x = 2, y = 1.18, label = "combine", size = 3.8) +
  annotate("text", x = 4, y = 1.18, label = "transform", size = 3.8) +
  annotate("text", x = 6, y = 1.18, label = "predict", size = 3.8) +
  xlim(0.3, 7.7) +
  ylim(0.7, 1.35) +
  theme_void()


### MODEL INTERACTIONS: TEACHING FIGURES ####

# Create illustrative data for two reef types across three pairs.
# Values are simulated for teaching, NOT fitted model results.

plot_data <- expand.grid(
  date_s = seq(-2, 2, length.out = 100),
  type = factor(reef_type_levels, levels = reef_type_levels),
  pair = factor(pair_levels, levels = pair_levels)
) %>%
  mutate(
    reef = as.numeric(type == "Artificial")
  )
# additive - slide 14 
fig_additive <- plot_data %>%
  filter(pair == "Aow Mao") %>%
  mutate(
    abundance = exp(4.5 - 0.4 * reef + 0.25 * date_s)
  ) %>%
  ggplot(aes(x = date_s, y = log(abundance), colour = type)) +
  geom_line(linewidth = 1.4) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Standardised time",
    y = "Log expected abundance",
    colour = "Reef type"
  ) +
  theme_clean

fig_additive
# adding one two-way interaction 
fig_interaction <- plot_data %>%
  filter(pair == "Aow Mao") %>%
  mutate(
    abundance = exp(
      4.5 - 0.4 * reef +
        0.05 * date_s +
        0.45 * reef * date_s
    )
  ) %>%
  ggplot(aes(x = date_s, y = log(abundance), colour = type)) +
  geom_line(linewidth = 1.4) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Standardised time",
    y = "Log expected abundance",
    colour = "Reef type"
  ) +
  theme_clean

fig_interaction
# three way interaction 
fig_threeway <- plot_data %>%
  mutate(
    # Pair-specific temporal effects
    natural_slope = case_when(
      pair == "Aow Mao" ~ -0.10,
      pair == "No Name" ~ -0.12,
      pair == "Sattakut" ~ -0.03
    ),
    ar_slope_difference = case_when(
      pair == "Aow Mao" ~ 0.60,
      pair == "No Name" ~ 0.05,
      pair == "Sattakut" ~ 0.15
    ),
    abundance = exp(
      4.5 - 0.4 * reef +
        natural_slope * date_s +
        ar_slope_difference * reef * date_s
    )
  ) %>%
  ggplot(aes(x = date_s, y = log(abundance), colour = type)) +
  geom_line(linewidth = 1.4) +
  facet_wrap(~pair, nrow = 1) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Standardised time",
    y = "Log expected abundance",
    colour = "Reef type"
  ) +
  theme_clean

fig_threeway


### GLM VS GLMM: RANDOM INTERCEPTS ####

set.seed(42)

# Simulate three surveys with different baseline abundances.
# All surveys share the same temporal slope.

model_data <- expand.grid(
  time = seq(0, 10, length.out = 30),
  survey = c("Survey 1", "Survey 2", "Survey 3")
) %>%
  mutate(
    survey = factor(survey),
    intercept = case_when(
      survey == "Survey 1" ~ -15,
      survey == "Survey 2" ~ 0,
      survey == "Survey 3" ~ 15
    ),
    abundance = 50 + 3 * time + intercept +
      rnorm(n(), sd = 4)
  )

# Create predictions illustrating the two model structures.
model_lines <- model_data %>%
  mutate(
    GLM = 50 + 3 * time,
    GLMM = 50 + 3 * time + intercept
  ) %>%
  pivot_longer(
    cols = c(GLM, GLMM),
    names_to = "model",
    values_to = "predicted"
  ) %>%
  mutate(
    model = factor(model, levels = c("GLM", "GLMM"))
  )

# Show the same observations in both panels.
observed_data <- model_data %>%
  tidyr::crossing(model = factor(c("GLM", "GLMM"),
                                 levels = c("GLM", "GLMM")))

# Plot one common intercept versus survey-specific intercepts.
ggplot() +
  geom_point(
    data = observed_data,
    aes(x = time, y = abundance, colour = survey),
    alpha = 0.45,
    size = 1.6
  ) +
  geom_line(
    data = model_lines %>% filter(model == "GLMM"),
    aes(x = time, y = predicted, colour = survey),
    linewidth = 1.2
  ) +
  geom_line(
    data = model_lines %>% filter(model == "GLM"),
    aes(x = time, y = predicted),
    colour = "black",
    linewidth = 1.2
  ) +
  facet_wrap(~model, nrow = 1) +
  scale_colour_manual(
    values = c("#B9C7B5", "#477F91", "#1F4E6B")
  ) +
  labs(
    x = "Time",
    y = "Abundance",
    colour = "Survey"
  ) +
  theme_clean +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

### LINK SCALE VS RESPONSE SCALE ####

# Illustrative predictions on the log scale
prediction_data <- expand.grid(
  date_s = seq(-2, 2, length.out = 100),
  type = factor(c("Natural", "Artificial"),
                levels = c("Natural", "Artificial"))
) %>%
  mutate(
    reef = as.numeric(type == "Artificial"),
    
    # Predicted log mean and standard error
    fit = 4.5 - 0.3 * reef +
      (0.05 + 0.35 * reef) * date_s,
    
    se = 0.12 + 0.04 * abs(date_s),
    
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

# Convert log-scale estimates into expected counts
plot_predictions <- prediction_data %>%
  mutate(
    expected = exp(fit),
    lower_count = exp(lower),
    upper_count = exp(upper)
  ) %>%
  select(date_s, type, fit, lower, upper,
         expected, lower_count, upper_count) %>%
  pivot_longer(
    cols = c(fit, expected),
    names_to = "scale",
    values_to = "prediction"
  ) %>%
  mutate(
    lower_ci = if_else(scale == "fit", lower, lower_count),
    upper_ci = if_else(scale == "fit", upper, upper_count),
    scale = factor(
      scale,
      levels = c("fit", "expected"),
      labels = c("Log scale", "Response scale: exp()")
    )
  )

# Plot both scales
ggplot(
  plot_predictions,
  aes(x = date_s, y = prediction, colour = type, fill = type)
) +
  geom_ribbon(
    aes(ymin = lower_ci, ymax = upper_ci),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 1.3) +
  facet_wrap(~scale, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = reef_cols) +
  scale_fill_manual(values = reef_cols) +
  labs(
    x = "Standardised time",
    y = NULL,
    colour = "Reef type",
    fill = "Reef type"
  ) +
  theme_clean +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

### INTERPRETING AN EFFECT SIZE ####

effect_data <- tibble(
  estimate = 0.5833,
  SE = 0.124
) %>%
  mutate(
    lower = estimate - 1.96 * SE,
    upper = estimate + 1.96 * SE,
    ratio = exp(estimate),
    lower_ratio = exp(lower),
    upper_ratio = exp(upper)
  )

ggplot(effect_data, aes(y = 1)) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    colour = "grey55"
  ) +
  geom_segment(
    aes(x = lower_ratio, xend = upper_ratio,
        yend = 1),
    linewidth = 1.4,
    colour = "#1F4E6B"
  ) +
  geom_point(
    aes(x = ratio),
    size = 5,
    colour = "#1F4E6B"
  ) +
  annotate(
    "text",
    x = effect_data$ratio,
    y = 1.16,
    label = "1.79×",
    fontface = "bold",
    size = 6,
    colour = "#1F4E6B"
  ) +
  scale_x_continuous(
    limits = c(0.8, 2.5),
    breaks = seq(1, 2.5, 0.25)
  ) +
  coord_cartesian(ylim = c(0.85, 1.28)) +
  labs(
    x = "Ratio of temporal abundance multipliers",
    y = NULL
  ) +
  theme_clean +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )


### BAYESIAN INFERENCE: PRIOR, LIKELIHOOD, POSTERIOR ####


prior_mu <- 0
prior_sd <- 0.4

like_mu <- 0.8
like_sd <- 0.35

post_sd <- sqrt(1 / (1/prior_sd^2 + 1/like_sd^2))
post_mu <- post_sd^2 * (prior_mu/prior_sd^2 + like_mu/like_sd^2)

bayes_df <- tibble(
  theta = seq(-3, 3, length.out = 1000),
  Prior = dnorm(theta, prior_mu, prior_sd),
  Evidence = dnorm(theta, like_mu, like_sd),
  Posterior = dnorm(theta, post_mu, post_sd)
) %>%
  pivot_longer(-theta, names_to = "component", values_to = "density")

bayes_cols <- c(
  Prior = "#E8D8B5",
  Evidence = "#1F4E6B",
  Posterior = "#8AA6A3"
)

ggplot(bayes_df, aes(theta, density, colour = component, fill = component)) +
  geom_area(alpha = 0.18, position = "identity") +
  geom_line(linewidth = 1.4) +
  scale_colour_manual(values = bayes_cols) +
  scale_fill_manual(values = bayes_cols) +
  annotate("text", x = -1.1, y = 0.26, label = "Prior",
           colour = "#B39B70", size = 5, fontface = "bold") +
  annotate("text", x = 1.55, y = 0.30, label = "Evidence",
           colour = "#1F4E6B", size = 5, fontface = "bold") +
  annotate("text", x = 0.55, y = 1.20, label = "Posterior",
           colour = "#648780", size = 5, fontface = "bold") +
  annotate(
    "text",
    x = -1.4, y = 1.42,
    label = "p(theta~'|'~y) %prop% p(y~'|'~theta) %.% p(theta)",
    parse = TRUE,
    size = 6,
    fontface = "bold"
  ) + 
  coord_cartesian(ylim = c(0, 1.55)) +
  labs(x = expression(theta), y = "Density") +
  theme_clean +
  theme(legend.position = "none")

### BAYESIAN MCMC: SIMULATED CHAINS ####

set.seed(42)

chain_data <- expand.grid(
  iteration = 1:1000,
  chain = factor(paste("Chain", 1:4))
) %>%
  arrange(chain, iteration) %>%
  group_by(chain) %>%
  mutate(
    beta = as.numeric(arima.sim(
      model = list(ar = 0.65),
      n = 1000,
      sd = 0.12
    )) + 0.5
  ) %>%
  ungroup()

ggplot(chain_data, aes(iteration, beta, colour = chain)) +
  geom_line(linewidth = 0.4, alpha = 0.75) +
  facet_wrap(~chain, ncol = 1) +
  scale_colour_manual(
    values = c("#E8D8B5", "#8AA6A3", "#477F91", "#1F4E6B")
  ) +
  labs(
    x = "Post-warmup iteration",
    y = expression(beta)
  ) +
  theme_clean +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.spacing = grid::unit(0.7, "lines")
  )