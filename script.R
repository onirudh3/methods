# Econometric Methods for Empirical Economics Project
# Anirudh Ravishankar
# January, 2024

# Libraries and data ------------------------------------------------------

library(dplyr)
library(readxl)

# NYTS 2022 Data (takes about 70 seconds to load on my system)
#
# QN1: Age at interview
# QN2: Sex
# QN3: Grade
#
# Race -
# QN4A: Not Hispanic
# QN4B, C, D, E: Yes Hispanic
# QN5A: American Indian or Alaska Native
# QN5B: Asian
# QN5C: Black
# QN5D: Hawaii/Pacific
# QN5E: White
#
# Interesting covariates -
# QN133: How often you use social media?
# QN155: Sexuality?
# QN156: Transgender?
# QN157A, B, C, D: No interest in life, feeling down/depressed/hopeless, etc.
# QN161: Wealth. Does family own cars?
# QN162: Wealth. Do you have your own bedroom?
# QN165: School grades currently
#
# E-Cigarettes -
# QN6: Ever used?
# QN7: First age of use.
#
# Cigarettes -
# QN35: Ever smoked?
# QN36: First age of use.
#
# Cigar -
# QN51: Ever smoked?
# QN52: First age of use.
#
# Chewing Tobacco/Snuff/Dip -
# QN62: Ever used?
# QN63: First age of use.
#
# Tobacco in hookah -
# QN67: Ever used?
# QN68: First age of use.

df <- read_excel("nyts2022.xlsx") %>%
  select(c("newid", "QN1", "QN2", "QN3", "QN4A", "QN4B", "QN4C", "QN4D", "QN4E",
           "QN5A", "QN5B", "QN5C", "QND", "QN5E", "QN133", "QN155", "QN156",
           "QN157A", "QN157B", "QN157C", "QN157D", "QN161", "QN162", "QN165",
           "QN6", "QN7", "QN35", "QN36", "QN51", "QN52", "QN62", "QN63", "QN67",
           "Qn68"))


# Rename variables --------------------------------------------------------



