# Rowan K. O'Hara
# 3.7.2023
# WHRD 2023
# Clean variables
################################################################################

# LOAD PACKAGES & READ IN DATA #################################################
library(tidyverse)
library(knitr)

s4s <- read_csv("../../data/great_S4S_aud_ipv_fam.csv")


# CLEAN DEP VARIABLE ###########################################################
clean_s4s <- s4s %>%
  filter(!is.na(y1f_hea_1a))

clean_dep <- clean_s4s %>%
  rowwise() %>%               # https://dplyr.tidyverse.org/articles/rowwise.html
  mutate(dep_noAnswer = sum(c_across(y1f_hea_1c:y1f_hea_1g) == -99), # https://stackoverflow.com/questions/51783095/compute-row-wise-counts-in-subsets-of-columns-in-dplyr
         dep_skip = sum(c_across(y1f_hea_1c:y1f_hea_1g) == -9),
         dep_filter = sum(c_across(dep_noAnswer:dep_skip))) %>%
  filter(dep_filter < 3)
# n = 7559



# CALCULATE SCORES #############################################################
# Calculate depression scores. These scores are pro-rated sum scores; so after filtering out
# participants who did not answer the threshold number of questions (2) for each variable, the skip
# choose not to answer values are converted to zeros. Then, the filter variables, dep_filter and anx_filter,
# are converted into a counter of the number of questions participant answered by subtracting the amount of
# questions they did not answer from the total questions (4). The symptom scores are calculated by summing all
# of the questions for depression or anxiety. If the participant did not answer all questions (filter variable
# does not equal 4), their score is pro-rated by finding the average of answered questions and multiplying by
# the total number of questions (4).
s4s_clean <- clean_dep %>%
  mutate(y1f_hea_1c = if_else(y1f_hea_1c == -9 | y1f_hea_1c == -99, 0, y1f_hea_1c),
         y1f_hea_1d = if_else(y1f_hea_1d == -9 | y1f_hea_1d == -99, 0, y1f_hea_1d),
         y1f_hea_1e = if_else(y1f_hea_1e == -9 | y1f_hea_1e == -99, 0, y1f_hea_1e),
         y1f_hea_1g = if_else(y1f_hea_1g == -9 | y1f_hea_1g == -99, 0, y1f_hea_1g)) %>%  # https://dplyr.tidyverse.org/reference/if_else.html
  mutate(dep_filter = 4 - dep_filter) %>%
  rowwise() %>%
  mutate(depScore = sum(c_across(y1f_hea_1c:y1f_hea_1g))) %>%  # https://dplyr.tidyverse.org/reference/c_across.html
  mutate(depScore = if_else(dep_filter != 4, (depScore / dep_filter) * 4, depScore))


# CHECK COUNTS #################################################################

s4s_clean %>%
  count(biosex)
# Female = 4872 (64%)
# Male = 2687 (36%)

s4s_clean %>%
  count(ever_ipv)
# Never = 4626 (61%)
# Ever = 2933 (39%)

s4s_clean %>%
  count(ever_aud)
# Never = 5200 (69%)
# Ever = 2359 (31%)

s4s_clean %>%
  group_by(biosex) %>%
  count(ever_ipv)
# Female & Ever = 2061 (42% of females) (27% of total sample)
# Male & Ever = 872 (32% of males) (12% of total sample)

s4s_clean %>%
  group_by(biosex) %>%
  count(ever_aud)
# Female & Ever = 1523 (31% of females) (20% of total sample)
# Male & Ever = 836 (31% of females) (11% of total sample)

s4s_clean %>%
  filter(ever_ipv == 1 & ever_aud == 1) %>%
  count(biosex)
# 1125 participants report IPV and AUD (15% of total sample)
# Female = 777 (16% of females)
# Male = 348 (13% of males)
  
