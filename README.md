
# Global Reef | R Workshop

**Introduction to Data Analysis & Ecological Statistics in R**  

This repository contains teaching materials for two workshops introducing reproducible ecological data analysis in R.

We use Global Reef (global-reef.com) wreck fish survey data from paired artificial and natural reefs around Koh Tao, progressing from raw observations to exploratory figures and statistical models.

The **slides explain the concepts**, while the **R scripts provide the practical exercises**. More extensive examples are available in `references/` for independent learning.


## Learning Objectives

### Day 1 — Introduction to Data Analysis in R

- Understand reproducible research, GitHub and RStudio Projects
- Import and explore ecological datasets
- Manipulate data using `dplyr` and pipes
- Clean, classify and aggregate field observations
- Create and export scientific figures using `ggplot2`

### Day 2 — Ecological Statistics in R

- Distinguish descriptive statistics from statistical inference
- Understand Gaussian, Poisson and negative-binomial distributions
- Identify and account for overdispersion
- Fit and interpret linear models, GLMs and GLMMs
- Understand interactions, fixed effects and random effects
- Examine model diagnostics, predictions and uncertainty
- Compare linear temporal models with nonlinear GAMs
- Discuss frequentist and Bayesian approaches to inference

## Getting Started

### 1. Install the Required Software

Before the workshop, install:

1. [R](https://cran.r-project.org/)
2. [RStudio Desktop](https://posit.co/download/rstudio-desktop/)
3. [Git](https://git-scm.com/downloads)

### 2. Clone the GitHub Repository

1. Open this repository on GitHub
2. Click **Code → HTTPS** and copy the repository URL
3. Open RStudio
4. Select **File → New Project → Version Control → Git**
5. Paste the URL and choose a destination
6. Create and open the RStudio Project

Alternatively, download the repository as a ZIP file, extract it and open the `.Rproj` file.

**Important:** Keep the project folder structure intact. All workshop scripts use relative file paths so they can run on different computers.

### 3. Run the Setup Script

Both workshop scripts begin with:

```r
source("scripts/00_setup.R")
```

The setup script:

- Installs missing R packages
- Loads the required libraries
- Defines project paths and output folders
- Establishes study factors and site classifications
- Defines colour palettes and a shared plotting theme

An internet connection may be required the first time you run the setup.

### 4. Open the Workshop Scripts

**Day 1:** `scripts/01_intro_to_r.R`

**Day 2:** `scripts/02_ecological_stats.R`

Work through the scripts section by section using:

- **Mac:** Cmd + Enter
- **Windows/Linux:** Ctrl + Enter

Avoid running the entire script at once. The exercises are designed to be completed progressively alongside the slides.

## Repository Structure

```text
r-workshop/
├── README.md
├── scripts/
│   ├── 00_setup.R
│   ├── 01_intro_to_r.R
│   └── 02_ecological_stats.R
├── data/
│   ├── 2026.10.26_ArtificialReefs_MASTER.csv
│   ├── fish_surveys_clean.csv
│   └── fish_guilds_clean.csv
├── slides/
├── references/
├── outputs/
│   ├── figures/
│   └── stats/
└── LICENSE
```

### Folder Descriptions

| Folder | Purpose |
|---|---|
| `scripts/` | Student-facing practical exercises |
| `data/` | Original and cleaned ecological datasets |
| `slides/` | Workshop presentations |
| `references/` | Extended scripts and additional examples |
| `outputs/figures/` | Exported plots |
| `outputs/stats/` | Exported summary tables and statistical results |

Some generated files will only appear after running the relevant script.

## The Ecological Dataset

The workshops use underwater visual census records from three paired reef systems around Koh Tao, Thailand.

| Study Pair | Natural Reef | Artificial Reef |
|---|---|---|
| Aow Mao | Aow Mao | Aow Mao Wreck |
| No Name | No Name Pinnacle | No Name Wreck |
| Sattakut | Hin Pee Wee | Sattakut |

Fish observations are classified into four ecological feeding guilds:

- Herbivores
- Invertivores
- Mesopredators
- Higher trophic-level predators (HTLP)

### Data Preparation

During Day 1, we:

1. Import and inspect the raw field dataset
2. Standardise dates, site names and data types
3. Reshape fish observations into long format
4. Assign ecological feeding guilds
5. Average repeated observer counts within each site-date survey and fish group, rounding up with `ceiling()`
6. Aggregate fish abundance by survey and feeding guild

These are study-specific processing decisions and may not be appropriate for every ecological dataset.

Two analysis-ready files are generated:

- `fish_surveys_clean.csv` — one observation per site-date survey
- `fish_guilds_clean.csv` — one observation per feeding guild and survey

**Day 2 requires these files.** Participants who missed Day 1 should obtain them from the instructor.

## Statistical Analysis

Day 2 introduces increasingly complex statistical models using the same ecological dataset.

The exercises progress through:

- Gaussian linear models
- Poisson generalised linear models
- Negative-binomial regression
- Additive effects and interactions
- Generalised linear mixed models
- Simulated residual diagnostics
- Estimated marginal trends and contrasts
- Model predictions and confidence intervals
- Generalised additive models

The final abundance GLMM is based on Global Reef's artificial reef research.


## R Packages

The workshop uses the following packages:

- `readr`
- `tidyr`
- `dplyr`
- `ggplot2`
- `lubridate`
- `glmmTMB`
- `DHARMa`
- `emmeans`
- `mgcv`
- `performance`

Missing packages are installed automatically by `00_setup.R`.

## Reproducible Research

A reproducible analysis should:

- Preserve the original dataset
- Document data processing and analytical decisions
- Use scripts rather than undocumented manual transformations
- Use relative paths instead of computer-specific file paths
- Save figures and statistical results systematically
- Examine model assumptions and diagnostics
- Track meaningful changes through version control

The goal is for another researcher to understand, rerun and evaluate the analysis.

## Troubleshooting

**File not found:** Confirm that the RStudio Project is open and the dataset is in the correct folder.

**Package not found:** Run `source("scripts/00_setup.R")` and review any installation errors.

**Object not found:** Run the earlier script sections in order.

**Missing cleaned data on Day 2:** Run the Day 1 cleaning and export sections or obtain the prepared CSV files from the instructor.

## Use and Attribution

Developed for the **Global Reef Marine Science Internship**, Koh Tao, Thailand.

See the repository's `LICENSE` file for applicable reuse terms.

Data access and redistribution should follow Global Reef's data-sharing permissions and any applicable privacy requirements. Publicly sharing code does not automatically authorise redistribution of the underlying survey records. Maintained by scarlett@global-reef.com. 
