
### GLOBAL REEF | R WORKSHOP ####
# DAY 1: INTRODUCTION TO DATA ANALYSIS IN R
# Global Reef Marine Science Internship | January 2027
#
# Research question:
# How does fish abundance vary between artificial and
# natural reefs around Koh Tao?
#
# Run code section by section using Cmd/Ctrl + Enter.
#
# A more detailed version is available in /references.


### 01. SETUP ####

# Run our shared setup script to load packages, establish
# project paths and define study-specific plotting settings.

source("scripts/00_setup.R")


### 02. IMPORT & EXPLORE DATA ####

#### 2.1 Import a CSV ####

# read_csv() imports a comma-separated data file.
# The file path was defined in our setup script.
#
# We'll initially treat all columns as text so we can
# inspect and clean the raw records before converting types.

fish_raw <- read_csv(
  abundance_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# Open the dataset in RStudio's data viewer.

View(fish_raw)


#### 2.2 Explore the dataset ####

# Inspect the first six rows.

head(fish_raw)

# View the variable names.

names(fish_raw)

# How many rows and columns are there?

dim(fish_raw)

# Examine the dataset's structure.

glimpse(fish_raw) 
# OR my favourite command: 
str(fish_raw)

# The $ operator selects a column from a data frame.

unique(fish_raw$Site)

# Count the number of records from each site.

table(fish_raw$Site)

# YOUR TURN:
# Use unique() to inspect the Researcher column.


### 03. MANIPULATING DATA WITH DPLYR ####

#### 3.1 Pipes ####

# The pipe operator %>% passes the result of one operation
# into the next function.
#
# Think of it as: "Take this dataset, THEN..."

head(fish_raw)

fish_raw %>%
  head()


#### 3.2 Selecting columns ####

# select() chooses VARIABLES (columns).

fish_raw %>%
  select(Site, Date, Parrotfish)

# We can select several species simultaneously.

fish_raw %>%
  select(Site, Date, Parrotfish, Rabbitfish, Barracuda)


#### 3.3 Filtering observations ####

# filter() chooses OBSERVATIONS (rows).
#
# == means "is equal to".

fish_raw %>%
  filter(Site == "Sattakut")

# %in% allows us to select multiple values.

fish_raw %>%
  filter(Site %in% c("Sattakut", "Hin Pee Wee"))


#### 3.4 Combining operations ####

# Pipes become useful when combining several operations.

fish_raw %>%
  filter(Site == "Sattakut") %>%
  select(Site, Date, Parrotfish, Rabbitfish)

# YOUR TURN:
# Filter for Aow Mao Wreck and select the date,
# researcher and parrotfish counts.


#### 3.5 Creating variables ####

# mutate() creates new variables or modifies existing ones.
#
# We can create a simple total for two fish groups.
# First, convert their text counts into numbers.

fish_raw %>%
  mutate(
    Parrotfish = as.numeric(Parrotfish),
    Rabbitfish = as.numeric(Rabbitfish),
    herbivore_count = coalesce(Parrotfish, 0) +
      coalesce(Rabbitfish, 0)
  ) %>%
  select(Site, Date, herbivore_count) %>%
  head()

# Notice that this does NOT modify fish_raw.
# We would need to assign the result to save it.


### 04. CLEAN & PREPARE THE DATA ####

# INSTRUCTOR DEMONSTRATION
#
# Before analysing the data, we need to:
#   - Remove empty records
#   - Standardise site names and dates
#   - Convert fish counts to numeric values
#   - Reshape species counts into long format
#   - Assign species to ecological feeding guilds
#   - Average observations from repeated surveyors
#   - Aggregate fish abundance by survey
#
# We'll run this section together without examining
# every function in detail.
#
# These processing decisions follow our attractor vs producer study.


#### 4.1 Remove empty records ####

# Replace blank text with missing values (NA).
# Remove completely empty rows and columns.

fish_clean <- fish_raw %>%
  mutate(across(everything(), ~na_if(.x, ""))) %>%
  filter(if_any(everything(), ~ !is.na(.x))) %>%
  select(where(~any(!is.na(.x))))


#### 4.2 Standardise dates and sites ####

# Convert dates from month/day/year text.
# Standardise the name of No Name's natural reef.
#
# Restrict the dataset to the original study period.

fish_clean <- fish_clean %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Site = recode(Site, "No Name" = "No Name Pinnacle")
  ) %>%
  filter(
    !is.na(Date),
    Date <= as.Date("2025-11-30")
  ) 


#### 4.3 Convert species counts ####

# abundance_species is defined in 00_setup.R.
# Check that the expected columns exist.

species_cols <- intersect(
  abundance_species,
  names(fish_clean)
)

stopifnot(
  length(species_cols) == length(abundance_species)
)

# Convert species counts to numeric values.

fish_clean <- fish_clean %>%
  mutate(
    across(all_of(species_cols), as.numeric)
  ) 


#### 4.4 Reshape the dataset ####

# pivot_longer() converts separate species columns into:
#
# Species = recorded fish group
# Count   = observed abundance
#
# Blank count cells represent groups not recorded in a
# completed survey, so we replace these with zero.
#
# ceiling() rounds fractional counts up to whole numbers
# following the original analysis procedure.

fish_long <- fish_clean %>%
  pivot_longer(
    cols = all_of(species_cols),
    names_to = "Species",
    values_to = "Count"
  ) %>%
  mutate(
    Count = ceiling(replace_na(Count, 0))
  ) 

#### 4.5 Add ecological classifications ####

# Join the feeding-guild and site-pair lookup tables
# defined in 00_setup.R.
#
# Exclude rays and eels, as in the attractor vs producer abundance analysis.

fish_long <- fish_long %>%
  left_join(
    abundance_guilds,
    by = "Species"
  ) %>%
  left_join(
    site_lookup %>%
      rename(Site = site, type = type),
    by = "Site"
  ) %>%
  filter(
    !is.na(pair),
    !Species %in% c("Eel", "Ray")
  ) %>%
  mutate(
    Site = factor(Site),
    Type = factor(Type, levels = reef_type_levels),
    Classification = factor(Classification),
    Current = factor(Current),
    Researcher = factor(Researcher),
    Species = factor(Species),
    
    type = factor(type, levels = reef_type_levels),
    pair = factor(pair, levels = pair_levels),
    feeding_guild = factor(feeding_guild, levels = feeding_guild_levels),
    
    Duration = as.numeric(Duration),
    Depth = as.numeric(Depth),
    Visibility = as.numeric(Visibility),
    Boats = as.numeric(Boats),
    total_N = as.numeric(total_N)
  )

# check structure 
str(fish_long)

#### 4.6 Average repeated observer records ####

# Different observers may survey the same site and date.
#
# We average their counts for each fish group,
# then round up using ceiling().
#
# One row now represents a single fish group
# recorded during a particular site-date survey.

fish_species <- fish_long %>%
  group_by(
    Site, pair, type, Date, Species, feeding_guild
  ) %>%
  summarise(
    Count = ceiling(mean(Count, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    survey_id = paste(Site, Date, sep = "_")
  ) 


#### 4.7 Aggregate feeding-guild abundance ####

# Sum the constituent fish groups within each
# feeding guild and survey.
#
# Each survey contributes one count per feeding guild.

fish_guilds <- fish_species %>%
  group_by(
    survey_id, Site, pair, type, Date, feeding_guild
  ) %>%
  summarise(
    Count = sum(Count),
    .groups = "drop"
  )


#### 4.8 Aggregate total survey abundance ####

# Combine guild counts into one total per survey.
#
# Each row now represents one site-date survey.

fish_surveys <- fish_guilds %>%
  group_by(survey_id, Site, pair, type, Date) %>%
  summarise(
    total_abundance = sum(Count),
    .groups = "drop"
  )

# Inspect our final datasets.

glimpse(fish_guilds)
glimpse(fish_surveys)

# Check that each survey has only one total-abundance row.

stopifnot(
  nrow(fish_surveys) == n_distinct(fish_surveys$survey_id)
)

# Export the cleaned datasets for Day 2.

write_csv(
  fish_guilds,
  file.path(data_dir, "fish_guilds_clean.csv")
)

write_csv(
  fish_surveys,
  file.path(data_dir, "fish_surveys_clean.csv")
)


### 05. SUMMARISING ECOLOGICAL DATA ####

#### 5.1 Grouping and summarising ####

# group_by() defines categories to analyse together.
# summarise() calculates a statistic for each group.
#
# Calculate mean fish abundance by reef type.

fish_surveys %>%
  group_by(type) %>%
  summarise(
    mean_abundance = mean(total_abundance),
    .groups = "drop"
  )


#### 5.2 Summarise by study pair ####

# Add another grouping variable to compare the
# three paired artificial and natural reef systems.

survey_summary <- fish_surveys %>%
  group_by(pair, type) %>%
  summarise(
    n_surveys = n(),
    mean_abundance = mean(total_abundance),
    sd_abundance = sd(total_abundance),
    .groups = "drop"
  )

survey_summary

# YOUR TURN:
# Which reef pair has the highest mean abundance?
# Does a difference in means establish a real ecological effect?


### 06. INTRODUCTION TO GGPLOT2 ####

# ggplot2 builds figures by adding layers.
#
# ggplot() = dataset
# aes()    = variables mapped to visual properties
# geom_*() = how the observations are drawn
# facet_*() = separate panels
# labs()   = axis and legend labels
# theme()  = visual formatting


#### 6.1 Define the data and axes ####

# Start with a blank plot.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance)
)


#### 6.2 Add observations ####

# geom_jitter() displays observations with slight
# horizontal displacement to reduce overlapping points.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance)
) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5)


#### 6.3 Add reef-type colours ####

# Mapping colour inside aes() links colour to a variable.
# scale_colour_manual() applies our custom palette.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance, colour = type)
) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5) +
  scale_colour_manual(values = reef_cols) + theme_clean


#### 6.4 Separate the reef pairs ####

# facet_wrap() creates a panel for each study pair.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance, colour = type)
) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5) +
  facet_wrap(~pair, nrow = 1) +
  scale_colour_manual(values = reef_cols) + 
  theme_clean


#### 6.5 Create our finished figure ####

# Add labels and our shared publication-style theme.
# Save the finished plot as an R object.

fig_abundance <- ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance, colour = type)
) +
  geom_jitter(
    width = 0.15,
    height = 0,
    alpha = 0.5
  ) +
  facet_wrap(~pair, nrow = 1) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Reef type",
    y = "Total fish abundance per survey"
  ) +
  theme_clean +
  theme(legend.position = "none")

fig_abundance



### 07. VISUALISING TEMPORAL TRENDS ####

# So far, we've compared total fish abundance between reef types.
#
# Now we'll investigate how abundance changes through time,
# starting with mesopredators at the Aow Mao reef pair.
#
# We will progressively add:
#   1. Filtered observations
#   2. Reef-type colours
#   3. Nonlinear trend lines
#   4. Confidence intervals
#   5. Other feeding guilds
#   6. Other study pairs


#### 7.1 Filter our dataset ####

# fish_guilds contains one observation per feeding guild
# for each site-date survey.
#
# filter() allows us to focus on a specific ecological question.

mesopredators_am <- fish_guilds %>%
  filter(
    pair == "Aow Mao",
    feeding_guild == "Mesopredator"
  )

# Examine the filtered dataset.

glimpse(mesopredators_am)


#### 7.2 Plot abundance through time ####

# Start by plotting individual observations.
#
# Date is mapped to the x-axis.
# Count is the number of mesopredatory fish observed.

ggplot(
  mesopredators_am,
  aes(x = Date, y = Count)
) +
  geom_point()


#### 7.3 Distinguish reef types ####

# Map colour to reef type so we can distinguish
# artificial and natural reef observations.

ggplot(
  mesopredators_am,
  aes(x = Date, y = Count, colour = type)
) +
  geom_point(alpha = 0.6) +
  scale_colour_manual(values = reef_cols)


#### 7.4 Add nonlinear trend lines ####

# geom_smooth() fits a trend through the observations.
#
# LOESS is a flexible smoothing method that can describe
# nonlinear relationships without assuming a straight line.
#
# span controls the degree of smoothing.

ggplot(
  mesopredators_am,
  aes(x = Date, y = Count, colour = type)
) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "loess",
    span = 0.75,
    se = FALSE
  ) +
  scale_colour_manual(values = reef_cols)


#### 7.5 Add confidence intervals ####

# se = TRUE displays approximate 95% confidence intervals
# around the fitted LOESS curves.
#
# fill = type matches the shaded ribbons to reef type.
# alpha controls ribbon transparency.

ggplot(
  mesopredators_am,
  aes(
    x = Date,
    y = Count,
    colour = type,
    fill = type
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "loess",
    span = 0.75,
    se = TRUE,
    alpha = 0.2
  ) +
  scale_colour_manual(values = reef_cols) +
  scale_fill_manual(values = reef_cols)  +
  theme_clean


#### 7.6 Add the other feeding guilds ####

# Remove the feeding-guild filter.
#
# facet_wrap() creates a separate panel for each guild,
# allowing us to compare their temporal patterns.

aow_mao_guilds <- fish_guilds %>%
  filter(pair == "Aow Mao")

ggplot(
  aow_mao_guilds,
  aes(
    x = Date, y = Count,
    colour = type,
    fill = type
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "loess",
    span = 0.75,
    se = TRUE,
    alpha = 0.2
  ) +
  facet_wrap(~feeding_guild, scales = "free_y") +
  scale_colour_manual(values = reef_cols) +
  scale_fill_manual(values = reef_cols) +
  theme_clean


#### 7.7 Add the other study pairs ####

# Now use the complete feeding-guild dataset.
#
# facet_grid() arranges study pairs into rows and
# feeding guilds into columns.
#
# This reveals how temporal patterns vary across
# ecological guilds and reef systems.

fig_time <- ggplot(
  fish_guilds,
  aes(
    x = Date,
    y = Count,
    colour = type,
    fill = type
  )
) +
  geom_point(alpha = 0.35, size = 1) +
  geom_smooth(
    method = "loess",
    span = 0.75,
    se = TRUE,
    alpha = 0.2
  ) +
  facet_grid(
    pair ~ feeding_guild,
    scales = "free_y"
  ) +
  scale_colour_manual(values = reef_cols) +
  scale_fill_manual(values = reef_cols) +
  labs(
    x = "Survey date",
    y = "Feeding-guild abundance per survey",
    colour = "Reef type",
    fill = "Reef type"
  ) +
  theme_clean

fig_time

# DISCUSSION:
# Do artificial and natural reefs show similar patterns?
# Do the patterns differ among feeding guilds?
# Are they consistent across all three study pairs?
#
# NOTE: LOESS is an exploratory visualisation.
# The curves and confidence intervals do not constitute
# our final statistical test of temporal differences.
#
# On Day 2, we'll investigate these relationships
# using negative-binomial regression models.



### 08. EXPORT RESULTS ####

#### 8.1 Export figures ####

# ggsave() saves a ggplot as an image.
# dpi = 300 produces a high-resolution PNG.

ggsave(
  filename = file.path(figures_dir, "day1_abundance.png"),
  plot = fig_abundance,
  width = 9,
  height = 4,
  dpi = 300,
  bg = "white"
)

# Export the temporal figure.

ggsave(
  filename = file.path(figures_dir, "day1_temporal_trends.png"),
  plot = fig_time,
  width = 9,
  height = 8,
  dpi = 300,
  bg = "white"
)


#### 8.2 Export summary statistics ####

# write_csv() saves a data frame as a CSV file.

write_csv(
  survey_summary,
  file.path(stats_dir, "day1_abundance_summary.csv")
)


### 09. WRAP-UP ####

# Today we have:
#
# 1. Imported and explored raw ecological data
# 2. Used pipes and dplyr to manipulate observations
# 3. Cleaned and aggregated repeated field observations
# 4. Calculated ecological summary statistics
# 5. Created and exported scientific figures
#
# DAY 2:
# We will use the same dataset to investigate whether
# differences in fish abundance are statistically supported,
# how abundance changes through time, and how these
# relationships differ across reef types and feeding guilds.

message("Day 1 complete.")
