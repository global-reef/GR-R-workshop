
### GLOBAL REEF | R WORKSHOP ####
# Introduction to Data Analysis & Ecological Statistics
#
# Shared setup for both workshops
# Run from the RStudio Project using:
# source("scripts/00_setup.R")


### 01. INSTALL PACKAGES ####

# R includes a collection of built-in functions, but additional
# packages provide tools for data manipulation, plotting and statistics.
#
# Packages only need to be INSTALLED once on a computer.
# However, they must be LOADED each time a new R session begins.

required_packages <- c(
  "readr",
  "tidyr",
  "ggplot2",
  "lubridate",
  "dplyr",
  "glmmTMB",
  "DHARMa",
  "emmeans",
  "mgcv",
  "performance"
)

# The code below checks which packages are already installed
# and installs only those that are missing.
missing_packages <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}


### 02. LOAD LIBRARIES ####

# library() makes the functions from an installed package
# available for use in the current R session.
#
# We load dplyr last to avoid common conflicts with functions
# from other packages.

suppressPackageStartupMessages({
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  # Statistical modelling packages, used on Day 2
  library(glmmTMB)
  library(DHARMa)
  library(emmeans)
  library(mgcv)
  library(performance)
  
  library(dplyr)
})



### 03. PROJECT DIRECTORIES ####

# Relative file paths allow the project to work on any computer,
# regardless of where the GitHub repository has been cloned.
#
# We use the project root as our working directory.

data_dir <- "data"
figures_dir <- "outputs/figures"
stats_dir <- "outputs/stats"
refs_dir <- "references"

# Create output directories if they do not already exist.
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(stats_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(refs_dir, recursive = TRUE, showWarnings = FALSE)

# Original Global Reef fish survey dataset
abundance_path <- file.path(
  data_dir,
  "2026.10.26_ArtificialReefs_MASTER.csv"
)


### 04. STUDY FACTORS ####

# Factors represent categorical variables with defined levels.
# Their ordering determines how categories appear in plots
# and which reference categories are used in statistical models.

reef_type_levels <- c("Natural", "Artificial")

pair_levels <- c("Aow Mao", "No Name", "Sattakut")

feeding_guild_levels <- c(
  "Herbivore",
  "Invertivore",
  "Mesopredator",
  "HTLP"
)


### 05. STUDY SITE LOOKUP ####

# The six study sites are organised into three paired systems.
# Each pair contains an artificial reef and a natural reef.
#
# This lookup table will be joined to the survey data on Day 1.

site_lookup <- tibble::tribble(
  ~site,               ~type,        ~pair,
  "Aow Mao",           "Natural",    "Aow Mao",
  "Aow Mao Wreck",     "Artificial", "Aow Mao",
  "No Name Pinnacle",  "Natural",    "No Name",
  "No Name Wreck",     "Artificial", "No Name",
  "Hin Pee Wee",       "Natural",    "Sattakut",
  "Sattakut",          "Artificial", "Sattakut"
)


### 06. FISH FEEDING GUILDS ####

# Species recorded in the underwater visual census are
# classified into four ecological feeding guilds.

abundance_guilds <- tibble::tribble(
  ~Species,                ~feeding_guild,
  "Parrotfish",            "Herbivore",
  "Rabbitfish",            "Herbivore",
  "Butterflyfish",         "Invertivore",
  "Angelfish",             "Invertivore",
  "Cleaner_Wrasse",        "Invertivore",
  "Batfish",               "Invertivore",
  "Thicklip",              "Invertivore",
  "Red_Breast",            "Invertivore",
  "Slingjaw",              "Invertivore",
  "Sweetlips",             "Invertivore",
  "Squirrel_Soldier",      "Invertivore",
  "Triggerfish",           "Invertivore",
  "Porcupine_Puffer",      "Invertivore",
  "Ray",                   "Mesopredator",
  "Brown_Stripe_Snapper",  "Mesopredator",
  "Russels_Snapper",       "Mesopredator",
  "lrg_Snapper",           "HTLP",
  "Eel",                   "Mesopredator",
  "Trevally",              "HTLP",
  "Emperorfish",           "Mesopredator",
  "sml_Grouper",           "Mesopredator",
  "lrg_Grouper",           "HTLP",
  "Barracuda",             "HTLP"
)

# Identify the raw species-count columns using the lookup table.
abundance_species <- abundance_guilds$Species


### 07. SHIPWRECK DEPLOYMENT DATES ####

# The two newer shipwrecks were deployed in September 2023.
# Sattakut was already established before this study began.

deployment_lookup <- tibble::tribble(
  ~pair,      ~deployment_date,
  "Aow Mao",  as.Date("2023-09-01"),
  "No Name",  as.Date("2023-09-01"),
  "Sattakut", as.Date(NA)
)


### 08. COLOUR PALETTES ####

# Named colour vectors keep categories visually consistent
# across plots, regardless of their order in the dataset.
# these have been checked for colourblind accessibility.

# Artificial and natural reef colours
reef_cols <- c(
  "Artificial" = "#1F4E6B",
  "Natural" = "#66BFA6"
)

# Ecological feeding guild colours
feeding_guild_cols <- c(
  "Herbivore" = "#E8D8B5",
  "Invertivore" = "#B9C7B5",
  "Mesopredator" = "#477F91",
  "HTLP" = "#1F4E6B"
)


### 09. SHARED PLOTTING THEME ####

# This theme can be added to any ggplot using:
# + theme_clean
#
# A sans-serif font is used for compatibility across computers.

theme_clean <- theme_classic(
  base_family = "sans",
  base_size = 12
) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    legend.position = "right",
    strip.background = element_blank(),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )


### 10. REPRODUCIBILITY ####

# Random operations can produce different results each time.
# Setting a seed makes these operations reproducible.

set.seed(42)

message("Global Reef workshop setup complete.")



## run with: 
# source("scripts/00_setup.R")
