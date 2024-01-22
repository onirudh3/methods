# Econometric Methods for Empirical Economics Project
# Anirudh Ravishankar
# January, 2024


# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

# For survival models
library(survival)
library(bshazard)

# Split population duration model
library(spduration)

# Data Import and Cleaning ------------------------------------------------

# NYTS 2022 Data (takes about 70 seconds to load on my system)
#
# QN1: Age at interview
# QN2: Sex
#
# Ethnicity -
# QN4B, C, D, E: Hispanic
# QN5A: American Indian or Alaska Native
# QN5B: Asian
# QN5C: Black
# QN5D: Hawaii/Pacific
# QN5E: White
#
# Interesting covariates -
# QN133: How often you use social media?
# QN157A, B, C, D: No interest in life, feeling down/depressed/hopeless, etc.
# QN161: Wealth. Does family own cars?
# QN162: Wealth. Do you have your own bedroom?
# QN165: School grades currently.
#
# Cigarettes -
# QN35: Ever smoked?
# QN36: First age of use.

df <- read_excel("nyts2022.xlsx") %>%
  select(c("newid", "QN1", "QN2", "QN4B", "QN4C", "QN4D", "QN4E",
           "QN5A", "QN5B", "QN5C", "QN5D", "QN5E", "QN133", "QN157A", 
           "QN157B", "QN157C", "QN157D", "QN161", "QN162", "QN165", "QN35", 
           "QN36"))

# How many individuals in raw data?
n_distinct(df$newid) # 28291 individuals


## Consolidate some variables ----

# Hispanic dummy
df <- df %>% 
  mutate(hispanic = rowSums(!is.na(select(., QN4B:QN4E))), .after = QN4E)
df <- df %>% 
  mutate(hispanic = case_when(hispanic == 0 ~ 0, T ~ 1))
df <- subset(df, select = -c(QN4B:QN4E))

# Feeling depressed dummy
df <- df %>% 
  mutate(depressed = rowSums(!is.na(select(., QN157A:QN157D))), .after = QN157D)
df <- df %>% 
  mutate(depressed = case_when(depressed == 0 ~ 0, T ~ 1))
df <- subset(df, select = -c(QN157A:QN157D))


## Rename variables for convenience ----
names(df) <- c("id", "age", "male", "hispanic", "native_indian", 
               "asian", "black", "hawaii_pacific", "white", "social_media_use",
               "depressed", "no_of_cars", "own_bedroom", "school_grades", 
               "cigarette_ever", "exit_age")


## Coding actual values of variables from codebook ----

# ID
df$id <- as.factor(df$id)

# Age
df <- subset(df, !is.na(age)) # Removes 100 individuals
df <- df %>% 
  mutate(age = case_when(age == 1 ~ 9,
                         age == 2 ~ 10,
                         age == 3 ~ 11,
                         age == 4 ~ 12,
                         age == 5 ~ 13,
                         age == 6 ~ 14,
                         age == 7 ~ 15,
                         age == 8 ~ 16,
                         age == 9 ~ 17,
                         age == 10 ~ 18,
                         age == 11 ~ 19, T ~ age))

# Sex
df <- subset(df, !is.na(male)) # Removes additional 169 individuals
df <- df %>% 
  mutate(male = case_when(male == 1 ~ 1, T ~ 0))
df$male <- as.factor(df$male)

# Cigarette ever
df <- subset(df, !is.na(cigarette_ever)) # Removes additional 245 individuals
df <- df %>% 
  mutate(cigarette_ever = case_when(cigarette_ever == 1 ~ 1, T ~ 0))

# Age of first cigarette use (or age at interview if never smoked)
df <- df %>% 
  mutate(exit_age = case_when(exit_age == 1 ~ 8,
                                   exit_age == 2 ~ 9,
                                   exit_age == 3 ~ 10,
                                   exit_age == 4 ~ 11,
                                   exit_age == 5 ~ 12,
                                   exit_age == 6 ~ 13,
                                   exit_age == 7 ~ 14,
                                   exit_age == 8 ~ 15,
                                   exit_age == 9 ~ 16,
                                   exit_age == 10 ~ 17,
                                   exit_age == 11 ~ 18,
                                   exit_age == 12 ~ 19, T ~ exit_age))
df <- df %>% 
  mutate(exit_age = case_when(is.na(exit_age) ~ age, T ~ exit_age))

# Social media use
df <- subset(df, !is.na(social_media_use)) # Removes additional 1689 individuals
df$social_media_use <- as.factor(df$social_media_use)
df <- df %>% 
  mutate(social_media_use = case_when(social_media_use %in% c(1) ~ "Never",
                                      social_media_use %in% c(2, 3, 4) ~ "Weekly",
                                      social_media_use %in% c(5, 6, 7, 8) ~ "Daily",
                                      T ~ social_media_use))

# No. of cars
df <- subset(df, !is.na(no_of_cars)) # Removes additional 1111 individuals
df$no_of_cars <- as.factor(df$no_of_cars)
df <- df %>% 
  mutate(no_of_cars = case_when(no_of_cars == 1 ~ "0",
                                no_of_cars == 2 ~ "1",
                                no_of_cars == 3 ~ ">=2", T ~ no_of_cars))

# Own bedroom
df <- subset(df, !is.na(own_bedroom)) # Removes additional 82 individuals
df <- df %>% 
  mutate(own_bedroom = case_when(own_bedroom == 1 ~ 0, T ~ 1))

# Current school grades
df <- subset(df, !is.na(school_grades)) # Removes additional 216 individuals
df$school_grades <- as.factor(df$school_grades)
df <- df %>% 
  mutate(school_grades = case_when(school_grades == 1 ~ "Mostly A's",
                                   school_grades == 2 ~ "Mostly B's",
                                   school_grades == 3 ~ "Mostly C's",
                                   school_grades == 4 ~ "Mostly D's",
                                   school_grades == 5 ~ "Mostly F's",
                                   school_grades == 6 ~ "None of these",
                                   school_grades == 7 ~ "Not sure", 
                                   T ~ school_grades))


## Some more cleaning ----

# Ethnicity variables
df <- df %>% 
  mutate(native_indian = case_when(is.na(native_indian) ~ 0, T ~ native_indian))
df <- df %>% 
  mutate(asian = case_when(is.na(asian) ~ 0, T ~ asian))
df <- df %>% 
  mutate(black = case_when(is.na(black) ~ 0, T ~ black))
df <- df %>% 
  mutate(hawaii_pacific = case_when(is.na(hawaii_pacific) ~ 0, 
                                    T ~ hawaii_pacific))
df <- df %>% 
  mutate(white = case_when(is.na(white) ~ 0, T ~ white))

# Remove additional 197 individuals without any information about ethnicity
df <- df %>% 
  mutate(race = rowSums((select(., hispanic:white))), .after = white)
df <- subset(df, race != 0)

# How many people report more than one ethnicity?
# plyr::count(df$race) # Install `plyr` package if necessary
df <- subset(df, select = -c(race))


# Summary Statistics ------------------------------------------------------




# Survival Curves ---------------------------------------------------------

# Smoothed hazard curve (change smoothing parameter lambda as per convenience)
fit <- bshazard(Surv(exit_age, cigarette_ever) ~ 1, verbose = F, lambda = 1000, df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard, 
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
ggplot(df_surv, aes(time, hazard)) +
  geom_line(color = "purple") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age in Years") +
  ylab("Hazard") +
  scale_x_continuous(breaks = seq(0, 20, 1), expand = c(0, 0), limits = c(5, 20)) +
  scale_y_continuous(breaks = seq(0, 100, 0.01), expand = c(0, 0), limits = c(0, 0.06))

# Hazard by sex
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
df_surv <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(exit_age, cigarette_ever) ~ 1, data = ., verbose = F, lambda = 1000))) %>%
  ungroup()
ggplot(df_surv, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age in Years", y = "Hazard") +
  theme_classic(base_size = 14) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = "none") +
  theme(legend.position = c(0.2, 0.9)) +
  scale_x_continuous(breaks = seq(0, 20, 1), expand = c(0, 0), limits = c(5, 20)) +
  scale_y_continuous(breaks = seq(0, 100, 0.01), expand = c(0, 0), limits = c(0, 0.06))


# Reformat Data for Split Population Model --------------------------------

# For row splits, we need id, age, cigarette_ever, and the exit_age
df1 <- df[c("id", "cigarette_ever", "age", "exit_age")]

# Creating the row splits
df1 <- df1 %>%
  mutate(start = 0, end = age) %>%
  select(-cigarette_ever) %>%
  gather(cigarette_ever, enter, -id) %>%
  group_by(id) %>%
  arrange(id, enter) %>%
  filter(!is.na(enter)) %>%
  mutate(exit = lead(enter)) %>%
  filter(!is.na(exit), !grepl("time_to_event_out_start", cigarette_ever)) %>%
  mutate(event = lead(grepl("time_to_event", cigarette_ever), default = 0)) %>%
  select(id, enter, exit, event) %>%
  ungroup()

# Cleaning up
df1 <- subset(df1, enter != exit)
df1 <- left_join(df1, select(df, id:school_grades), by = "id") # Add all columns

# Indicator for cigarette ever
df1$event <- ifelse(df1$exit == df1$age, 0, 1)

# How many people smoke the first time at age of interview?
nrow(subset(df, age == exit_age & cigarette_ever == 1)) # 314 people
id_vector <- subset(df, age == exit_age & cigarette_ever == 1) %>% 
  distinct(id) %>% 
  pull() # Get the id's of these 314 individuals
df1 <- df1 %>% 
  mutate(event = case_when(id %in% id_vector ~ 1, T ~ event)) 

# Re-formatted with one year intervals
df1 <- survSplit(df1, cut = c(1:80), start = "enter", end = "exit",
                event = "event")

# Relocate some columns
df1 <- df1 %>% relocate(c(enter, exit, event), .after = age)


# Split Population Model using `spduration` -------------------------------

## Variables to capture survival characteristics, needed by `spduration` ----

# At risk
df1 <- df1 %>% 
  group_by(id) %>% 
  mutate(atrisk = case_when(sum(event) > 0 ~ 1, T ~ 0))

# Failure
df1 <- df1 %>% 
  mutate(failure = event) # Same as event

# End of spell
df1 <- df1 %>% 
  mutate(end.spell = if_else(row_number() == n(), 1, 0))

# t.0
df1 <- df1 %>% 
  mutate(t.0 = enter) # Same as enter

## The model ----

# Split data 
id_vector <- df %>% 
  distinct(id) %>% 
  pull() # Recycling this vector

# Sampling a third of the population, following Schmidt and Witte (1989)
a <- sample(id_vector, 8167)

# Training sample
df_train <- subset(df1, id %in% a)

# Test sample
df_test <- subset(df1, !(id %in% a))

# Log-log model
loglog_model <- spdur(
  exit ~ male + hispanic + native_indian + asian + black + hawaii_pacific + 
    white + social_media_use + depressed + no_of_cars + own_bedroom + school_grades,
  atrisk ~ male + hispanic + native_indian + asian + black + hawaii_pacific + 
    white + social_media_use + depressed + no_of_cars + own_bedroom + school_grades,
  data = df1, distr = "loglog", silent = TRUE)

# Hazard plot
plot(loglog_model, type="hazard", main="Loglog")

# Prediction
loglog_test_p <- predict(loglog_model, newdata = coup_test, na.action = na.omit)

# Separation plot
obs_y <- coup_test[complete.cases(coup_test), "coup"]
separationplot(loglog_test_p, obs_y, newplot = FALSE)
