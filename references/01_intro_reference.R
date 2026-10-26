
### GLOBAL REEF | R WORKSHOP ####
# DAY 1: INTRODUCTION TO DATA ANALYSIS IN R
#
# Research question:
# How does fish abundance vary between artificial and natural
# reefs, across study sites and through time?
#
# This script introduces:
#   - Reproducible research and good coding practices
#   - Objects, functions and data frames
#   - Importing and exploring ecological data
#   - Data manipulation with dplyr and pipes
#   - Grouping and summarising observations
#   - Data visualisation with ggplot2
#   - Exporting scientific figures and tables
#
# Run the script section by section using Cmd/Ctrl + Enter.


### 01. SETUP ####

# source() runs the commands stored in another R script.
# Our setup script loads packages, defines colours and
# creates the directories used by both workshops.

source("scripts/00_setup.R")

# Check the current working directory.
# It should be the root of the RStudio Project.

getwd()

# See the files and folders in the current directory.

list.files()


### 02. INTRODUCTION TO R ####

#### 2.1 Basic calculations ####

# R can be used as a calculator.
# Run each command and look at the Console.

2 + 2
10 * 5
100 / 4
2^3


#### 2.2 Objects and assignment ####

# Objects store values that we can use later.
# The assignment operator <- assigns a value to an object.

fish_count <- 25

# Typing the object name prints its value.

fish_count

# Objects can contain text as well as numbers.
# Text is enclosed in quotation marks.

reef_type <- "Artificial"

reef_type

# We can also use objects in calculations.

survey_count <- 8
total_fish <- fish_count * survey_count

total_fish


#### 2.3 Vectors ####

# A vector contains multiple values.
# c() combines individual values into a vector.

fish_counts <- c(10, 20, 35, 15, 40)

fish_counts

# We can perform calculations using vectors.

mean(fish_counts)
sum(fish_counts)
max(fish_counts)
length(fish_counts)


#### 2.4 Functions and arguments ####

# A function performs an operation.
# Arguments are the information supplied to that function.
#
# In mean(fish_counts), mean() is the function
# and fish_counts is the argument.

mean(fish_counts)

# Functions can accept additional arguments.
# For example, na.rm = TRUE tells R to ignore missing values.

example_counts <- c(10, 20, NA, 40)

mean(example_counts)

mean(example_counts, na.rm = TRUE)

# NA represents a missing value.
# Without na.rm = TRUE, the mean is also NA.


#### 2.5 Comments and script organisation ####

# Any text following # is a comment.
# R ignores comments when executing code.
#
# Comments explain WHY we perform an operation.
# Section headings help organise longer analyses.
#
# In RStudio, headings such as:
#
# ### 03. IMPORT DATA ####
#
# appear in the Document Outline and are clickable.


### 03. IMPORT & EXPLORE DATA ####

#### 3.1 Import a CSV file ####

# We will use wreck fish surveys collected around Koh Tao.
#
# The dataset contains observations from several reef sites,
# including artificial reefs and nearby natural reefs.
#
# read_csv() imports a CSV file into R.
# A CSV is a plain-text table with comma-separated values.
#
# We initially import columns as text to avoid R guessing
# the wrong data types before we inspect the raw records.

fish_raw <- read_csv(
  abundance_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# View the imported dataset.

View(fish_raw)


#### 3.2 Explore the data ####

# head() displays the first six rows.

head(fish_raw)

# names() returns the column names.

names(fish_raw)

# dim() returns the number of rows and columns.

dim(fish_raw)

# glimpse() shows the structure of the dataset,
# including column names and data types.

glimpse(fish_raw)

# In a data frame:
#   - Each row represents a recorded observation
#   - Each column represents a variable
#
# IMPORTANT:
# One raw row is not necessarily one independent survey.
# Multiple observers may record the same site and date.


#### 3.3 Selecting individual columns ####

# The $ operator accesses a named column in a data frame.

fish_raw$Site

# unique() returns the distinct values in a vector.

unique(fish_raw$Site)

unique(fish_raw$Type)

# table() counts how often each value occurs.

table(fish_raw$Site)

# TRY IT:
# Use unique() to inspect the Researcher column.


#### 3.4 Understanding data types ####

# R uses different data types for different information.
#
# Character: text, such as site names
# Numeric: numbers used in calculations
# Date: calendar dates
# Factor: categorical variables with defined levels
#
# Our initial import deliberately treated everything as text.
# We will convert columns into appropriate types below.

class(fish_raw$Site)
class(fish_raw$Date)
class(fish_raw$Parrotfish)


### 04. INTRODUCTION TO DPLYR ####

#### 4.1 What is a pipe? ####

# The pipe operator %>% passes the result on its left
# into the function on its right.
#
# Think of it as saying:
# "Take this dataset, THEN do the following operation."
#
# These two commands produce the same result.

head(fish_raw)

fish_raw %>%
  head()

# Pipes become particularly useful when we want to perform
# several data manipulation steps in a logical sequence.


#### 4.2 select(): Choose columns ####

# select() keeps specific VARIABLES (columns).
#
# Without a pipe:

select(fish_raw, Site, Date, Type)

# With a pipe:

fish_raw %>%
  select(Site, Date, Type)

# We can also remove columns by placing - before a name.

fish_raw %>%
  select(-Researcher)

# TRY IT:
# Select Site, Date, Researcher and Barracuda.


#### 4.3 filter(): Choose rows ####

# filter() keeps OBSERVATIONS (rows) that meet a condition.
#
# == means "is equal to".
# != means "is not equal to".

fish_raw %>%
  filter(Site == "Sattakut")

# Filter for more than one possible site.
# %in% checks whether a value belongs to a specified set.

fish_raw %>%
  filter(Site %in% c("Sattakut", "Hin Pee Wee"))

# Multiple conditions can be combined using & (AND).

fish_raw %>%
  filter(Site == "Sattakut" & Type == "Artificial")

# TRY IT:
# Filter the dataset for observations at Aow Mao.


#### 4.4 mutate(): Create or modify columns ####

# mutate() creates new variables or changes existing ones.
#
# Start with a small example dataset.

example_fish <- tibble(
  Site = c("Aow Mao", "No Name", "Sattakut"),
  Count = c(20, 35, 50)
)

example_fish

# Create a new column containing twice the fish count.

example_fish %>%
  mutate(double_count = Count * 2)

# We can save the result as a new object.

example_fish_updated <- example_fish %>%
  mutate(double_count = Count * 2)

example_fish_updated

# NOTE:
# A pipe does not permanently change the original dataset.
# To keep the result, assign it to an object using <-.


### 05. CLEANING ECOLOGICAL DATA ####

#### 5.1 Why clean data? ####

# Field datasets often contain:
#   - Missing observations
#   - Different spellings of site names
#   - Incorrect data types
#   - Multiple observers recording the same survey
#   - Variables that require ecological classification
#
# Cleaning makes these records consistent and suitable
# for analysis.
#
# We preserve the original imported data in fish_raw.


#### 5.2 Remove empty rows and columns ####

# Empty strings ("") represent blank text fields.
# Convert these to NA so missing values are consistent.

fish_clean <- fish_raw %>%
  mutate(across(everything(), ~na_if(.x, "")))

# Remove rows containing no information.
# if_any() checks whether at least one column meets a condition.

fish_clean <- fish_clean %>%
  filter(if_any(everything(), ~ !is.na(.x)))

# Remove columns that contain no recorded information.

fish_clean <- fish_clean %>%
  select(where(~any(!is.na(.x))))

# Check the dimensions after cleaning.

dim(fish_raw)
dim(fish_clean)


#### 5.3 Convert dates and standardise site names ####

# as.Date() converts text into calendar dates.
#
# The format tells R how the original date is written:
# %m = month
# %d = day
# %Y = four-digit year
#
# recode() replaces specified values in a column.

fish_clean <- fish_clean %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Site = recode(Site, "No Name" = "No Name Pinnacle")
  )

# The attractor vs producer analysis uses observations up to November 2025.
# We exclude missing dates and observations after this point.

fish_clean <- fish_clean %>%
  filter(!is.na(Date), Date <= as.Date("2025-11-30"))

# Examine the date range.

range(fish_clean$Date)

# Check the site names.

unique(fish_clean$Site)


#### 5.4 Convert fish counts into numeric values ####

# The raw dataset stores abundances in separate species columns.
# To calculate with these values, they must be numeric.
#
# abundance_species was defined in our setup script.
# It contains the species columns used in the attractor vs producer study.

species_cols <- intersect(abundance_species, names(fish_clean))

# Confirm that every expected species column is present.

stopifnot(length(species_cols) == length(abundance_species))

# across() applies the same operation to multiple columns.
# Here, as.numeric() converts fish abundance values to numbers.

fish_clean <- fish_clean %>%
  mutate(across(all_of(species_cols), as.numeric))

# Check the new data types.

glimpse(fish_clean)


### 06. RESHAPING & CLASSIFYING DATA ####

#### 6.1 Wide and long data ####

# Our raw dataset is in WIDE format:
# one column for each recorded fish group.
#
# We will convert it to LONG format:
# one column identifying the group (Species)
# and another containing its abundance (Count).
#
# This makes it easier to group, summarise and plot data.

fish_long <- fish_clean %>%
  pivot_longer(
    cols = all_of(species_cols),
    names_to = "Species",
    values_to = "Count"
  )

# Compare the structures of the two datasets.

dim(fish_clean)
dim(fish_long)

fish_long %>%
  select(Site, Date, Species, Count) %>%
  head(10)


#### 6.2 Missing counts and whole-number counts ####

# For this dataset, blank fish-count cells represent
# species not recorded during the survey.
# We therefore replace those missing counts with zero.
#
# In other datasets, NA may indicate missing effort or
# unknown observations. Those values should NOT
# automatically be converted to zero.
#
# The original attractor vs producer cleaning procedure also used ceiling()
# to convert recorded abundance values to integers.

fish_long <- fish_long %>%
  mutate(
    Count = ceiling(replace_na(Count, 0))
  )


#### 6.3 Joining ecological information ####

# The raw data records fish by taxonomic group.
# For ecological analysis, we also want feeding guilds.
#
# left_join() adds information from another table
# using a column shared between the two datasets.
#
# abundance_guilds was created in 00_setup.R.

fish_long <- fish_long %>%
  left_join(abundance_guilds, by = "Species")

# Look at the new feeding_guild column.

fish_long %>%
  select(Species, feeding_guild) %>%
  distinct()

# We can also join site-level information.
# The site_lookup table identifies reef type and study pair.

fish_long <- fish_long %>%
  left_join(
    site_lookup %>%
      rename(Site = site, Type_lookup = type),
    by = "Site"
  )

# Retain the six paired study sites.
# Exclude eel and ray records as in the attractor vs producer analysis.

fish_long <- fish_long %>%
  filter(
    !is.na(pair),
    !Species %in% c("Eel", "Ray")
  )


#### 6.4 Defining categorical variables ####

# Factors represent categorical variables.
# Defining levels controls their ordering in plots and models.
#
# We use the site classifications from the study lookup.

fish_long <- fish_long %>%
  mutate(
    type = factor(Type_lookup, levels = reef_type_levels),
    pair = factor(pair, levels = pair_levels),
    feeding_guild = factor(
      feeding_guild,
      levels = feeding_guild_levels
    )
  )

# Check the classifications.

fish_long %>%
  distinct(Site, type, pair) %>%
  arrange(pair, type)


### 07. SUMMARISING ECOLOGICAL DATA ####

#### 7.1 group_by() and summarise() ####

# group_by() tells R which categories should be
# analysed together.
#
# summarise() calculates a result for each group.
#
# First, calculate mean fish abundance by feeding guild
# across the individual survey records.
#
# NOTE:
# These are descriptive summaries of raw observer records,
# not the final attractor vs producer survey-level analysis.

fish_long %>%
  group_by(feeding_guild) %>%
  summarise(
    mean_count = mean(Count),
    .groups = "drop"
  )

# We can also group by more than one variable.

fish_long %>%
  group_by(type, feeding_guild) %>%
  summarise(
    mean_count = mean(Count),
    .groups = "drop"
  )


#### 7.2 Accounting for multiple observers ####

# Multiple researchers may survey the same site and date.
# Their records are repeated observations of the same survey,
# rather than independent ecological sampling units.
#
# Following the attractor vs producer cleaning procedure, we calculate
# the mean count for each fish group within each site/date.
#
# ceiling() converts the averaged values to integers.
#
# This is a study-specific processing decision,
# not a general rule for all ecological datasets.

fish_species <- fish_long %>%
  group_by(
    Site, pair, type, Date, Species, feeding_guild
  ) %>%
  summarise(
    Count = ceiling(mean(Count, na.rm = TRUE)),
    n_observer_rows = n(),
    n_observers = n_distinct(Researcher, na.rm = TRUE),
    .groups = "drop"
  )

# Each row now represents one fish group
# within one site and survey date.

head(fish_species)


#### 7.3 Create survey identifiers ####

# A unique survey identifier tells us which observations
# belong to the same ecological survey.
#
# paste() combines text from different columns.

fish_species <- fish_species %>%
  mutate(
    survey_id = paste(Site, Date, sep = "_")
  )

# Check how many unique surveys are represented.

n_distinct(fish_species$survey_id)


#### 7.4 Summarise abundance by feeding guild ####

# We now combine species/group counts into feeding guilds
# within each independent survey.
#
# This produces the response used for the main attractor vs producer model.

fish_guilds <- fish_species %>%
  group_by(survey_id, Site, pair, type, Date, feeding_guild) %>%
  summarise(
    Count = sum(Count),
    .groups = "drop"
  )

# Examine the data.

glimpse(fish_guilds)

head(fish_guilds)


#### 7.5 Calculate total abundance per survey ####

# For our introductory plots and simple models,
# we can also combine all feeding guilds into a total.
#
# Each row now represents one survey.

fish_surveys <- fish_guilds %>%
  group_by(survey_id, Site, pair, type, Date) %>%
  summarise(
    total_abundance = sum(Count),
    .groups = "drop"
  )

# Compare the number of rows and surveys.

nrow(fish_guilds)
nrow(fish_surveys)

n_distinct(fish_surveys$survey_id)


#### 7.6 Explore survey-level abundance ####

# Calculate descriptive statistics by reef type.

fish_surveys %>%
  group_by(type) %>%
  summarise(
    n_surveys = n(),
    mean_abundance = mean(total_abundance),
    median_abundance = median(total_abundance),
    sd_abundance = sd(total_abundance),
    .groups = "drop"
  )

# Now calculate the same statistics for each study pair.

survey_summary <- fish_surveys %>%
  group_by(pair, type) %>%
  summarise(
    n_surveys = n(),
    mean_abundance = mean(total_abundance),
    median_abundance = median(total_abundance),
    sd_abundance = sd(total_abundance),
    .groups = "drop"
  )

survey_summary

# TRY IT:
# Which site pair has the greatest mean abundance?
# Does a greater mean necessarily imply a meaningful
# difference between artificial and natural reefs?


### 08. INTRODUCTION TO GGPLOT2 ####

#### 8.1 The grammar of graphics ####

# ggplot2 builds figures using layers.
#
# The main components are:
#
# ggplot()  = dataset and overall mapping
# aes()     = aesthetics (x, y, colour, fill, etc.)
# geom_*()  = how observations are represented
# facet_*() = split figures into panels
# labs()    = titles and axis labels
# theme_*() = visual formatting
#
# We will build each figure one step at a time.


#### 8.2 Start with a blank plot ####

# Define which variables belong on each axis.
# Without a geom, no observations are drawn.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance)
)


#### 8.3 Add observations ####

# geom_point() displays individual observations.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance)
) +
  geom_point()


#### 8.4 Improve point visibility ####

# Some observations overlap.
# geom_jitter() spreads points slightly along the x-axis.
#
# width controls horizontal displacement.
# height = 0 prevents vertical movement of count values.
# alpha controls transparency.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance)
) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5)


#### 8.5 Map colour to a variable ####

# Placing colour = type inside aes() tells ggplot
# to assign colours according to reef type.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance, colour = type)
) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5)


#### 8.6 Apply the study colour palette ####

# scale_colour_manual() uses the named colours
# defined in our setup script.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance, colour = type)
) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5) +
  scale_colour_manual(values = reef_cols)


#### 8.7 Add labels and formatting ####

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance, colour = type)
) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Reef type",
    y = "Total fish abundance per survey",
    colour = "Reef type"
  ) +
  theme_clean


#### 8.8 Use boxplots to show distributions ####

# geom_boxplot() summarises the distribution of values:
# median, interquartile range and potential outliers.
#
# Raw observations remain important because a boxplot
# alone does not show every value.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance, fill = type)
) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(
    aes(colour = type),
    width = 0.15,
    height = 0,
    alpha = 0.4
  ) +
  scale_fill_manual(values = reef_cols) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Reef type",
    y = "Total fish abundance per survey"
  ) +
  theme_clean +
  theme(legend.position = "none")


#### 8.9 Faceting: Compare study pairs ####

# facet_wrap() creates separate panels based on a variable.
# This helps us examine patterns within each study pair.

ggplot(
  fish_surveys,
  aes(x = type, y = total_abundance, fill = type)
) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(
    aes(colour = type),
    width = 0.15,
    height = 0,
    alpha = 0.4
  ) +
  facet_wrap(~pair, nrow = 1) +
  scale_fill_manual(values = reef_cols) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Reef type",
    y = "Total fish abundance per survey"
  ) +
  theme_clean +
  theme(legend.position = "none")


### 09. VISUALISING CHANGE THROUGH TIME ####

#### 9.1 Plot abundance against date ####

# Our research question also concerns temporal change.
# First, plot each survey's abundance against its date.
#
# Here, Date is a continuous calendar variable.

ggplot(
  fish_surveys,
  aes(x = Date, y = total_abundance)
) +
  geom_point()


#### 9.2 Colour observations by reef type ####

ggplot(
  fish_surveys,
  aes(x = Date, y = total_abundance, colour = type)
) +
  geom_point(alpha = 0.6) +
  scale_colour_manual(values = reef_cols)


#### 9.3 Separate the three study pairs ####

ggplot(
  fish_surveys,
  aes(x = Date, y = total_abundance, colour = type)
) +
  geom_point(alpha = 0.6) +
  facet_wrap(~pair, ncol = 1) +
  scale_colour_manual(values = reef_cols)


#### 9.4 Add a descriptive trend ####

# geom_smooth() displays a fitted trend through the data.
#
# method = "lm" adds a linear regression line.
# se = FALSE hides the uncertainty ribbon for now.
#
# IMPORTANT:
# This is a visual introduction to a fitted relationship.
# These simple lines are NOT our final statistical model.
# We will investigate model assumptions and uncertainty
# in detail during Day 2.

ggplot(
  fish_surveys,
  aes(x = Date, y = total_abundance, colour = type)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~pair, ncol = 1) +
  scale_colour_manual(values = reef_cols)


# try it without a linear trend - using the LOESS smoother instead
# span controls how smooth the curve is: 
  # span = 0.5 — more flexible, follows local fluctuations
  # span = 0.75 — moderate smoothing (default)
  # span = 1 — smoother, follows broader trends
ggplot(
  fish_surveys,
  aes(x = Date, y = total_abundance, colour = type)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.75, se = FALSE) +
  facet_wrap(~pair, ncol = 1) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Survey date",
    y = "Total fish abundance per survey",
    colour = "Reef type"
  ) +
  theme_clean

# add se's 
ggplot(
  fish_surveys,
  aes(x = Date, y = total_abundance, colour = type, fill = type)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.75, se = TRUE, alpha = 0.4) +
  facet_wrap(~pair, ncol = 1) +
  scale_colour_manual(values = reef_cols) +
  scale_fill_manual(values = reef_cols) + 
  labs(
    x = "Survey date",
    y = "Total fish abundance per survey",
    colour = "Reef type"
  ) +
  theme_clean

#### 9.5 Format the final temporal figure ####

# We can save an entire plot as an object.
# This makes it easy to reuse or export.

fig_abundance <- ggplot(
  fish_surveys,
  aes(x = Date, y = total_abundance, colour = type)
) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(~pair, ncol = 1) +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Survey date",
    y = "Total fish abundance per survey",
    colour = "Reef type"
  ) +
  theme_clean

# Print the completed figure.

fig_abundance


### 10. VISUALISING FEEDING GUILDS ####

#### 10.1 Compare functional groups ####

# We can now return to fish_guilds.
# Each survey contributes one abundance value
# for each feeding guild.
#
# Different panels allow us to compare ecological groups.

ggplot(
  fish_guilds,
  aes(x = Date, y = Count, colour = type)
) +
  geom_point(alpha = 0.4) +
  facet_wrap(~feeding_guild, scales = "free_y") +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Survey date",
    y = "Feeding-guild abundance",
    colour = "Reef type"
  ) +
  theme_clean


#### 10.2 Build a figure resembling the attractor vs producer analysis ####

# facet_grid() arranges panels into rows and columns.
#
# Rows: study pairs
# Columns: feeding guilds
#
# This figure shows observed values and simple linear trends.
# Day 2 will replace these descriptive trends with
# predictions from a negative-binomial mixed model.

fig_guilds <- ggplot(
  fish_guilds,
  aes(x = Date, y = Count, colour = type)
) +
  geom_point(alpha = 0.35, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_grid(pair ~ feeding_guild, scales = "free_y") +
  scale_colour_manual(values = reef_cols) +
  labs(
    x = "Survey date",
    y = "Observed feeding-guild abundance",
    colour = "Reef type"
  ) +
  theme_clean

fig_guilds


### 11. EXPORTING RESULTS ####

#### 11.1 Save a figure ####

# ggsave() exports a ggplot to a file.
#
# width and height specify figure dimensions in inches.
# dpi controls image resolution.
#
# The output path is relative to our project root.

ggsave(
  filename = file.path(figures_dir, "01_total_abundance.png"),
  plot = fig_abundance,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "white"
)

# Export the feeding-guild figure.

ggsave(
  filename = file.path(figures_dir, "02_guild_abundance.png"),
  plot = fig_guilds,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)


#### 11.2 Save a summary table ####

# write_csv() exports a data frame as a CSV.
# The table can be opened in spreadsheet software
# or imported into another R analysis.

write_csv(
  survey_summary,
  file.path(stats_dir, "survey_abundance_summary.csv")
)


#### 11.3 Save analysis-ready data ####

# These files will be used again during Day 2.
#
# fish_surveys: one observation per survey
# fish_guilds: one observation per survey and feeding guild

write_csv(
  fish_surveys,
  file.path(data_dir, "fish_surveys_clean.csv")
)

write_csv(
  fish_guilds,
  file.path(data_dir, "fish_guilds_clean.csv")
)


### 12. REPRODUCIBILITY & WRAP-UP ####

#### 12.1 Check our final datasets ####

glimpse(fish_surveys)
glimpse(fish_guilds)

# Confirm that survey IDs are unique in survey-level data.

stopifnot(
  nrow(fish_surveys) == n_distinct(fish_surveys$survey_id)
)

# Confirm that abundance values are non-negative integers.

stopifnot(
  all(fish_guilds$Count >= 0),
  all(fish_guilds$Count == floor(fish_guilds$Count))
)


#### 12.2 Record the R environment ####

# sessionInfo() reports the R version, operating system
# and packages used during this analysis.

sessionInfo()


#### 12.3 Final discussion ####

# By the end of this workshop, you should be able to:
#
# 1. Navigate an RStudio Project and GitHub repository
# 2. Write and run organised, commented R scripts
# 3. Import and inspect ecological datasets
# 4. Manipulate data using dplyr and pipes
# 5. Summarise observations at the appropriate survey level
# 6. Build and export figures using ggplot2
#
# DAY 2:
# We will use these same fish datasets to introduce
# statistical inference, regression, GLMs and GLMMs.
#
# Our central question:
# Do fish abundance trajectories differ between artificial
# and natural reefs, and does the relationship depend on
# study pair and feeding guild?

message("Day 1 analysis complete.")
