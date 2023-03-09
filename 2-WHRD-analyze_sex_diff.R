# Rowan K. O'Hara
# 3.7.2023
# WHRD
# Analyze data for WHRD
################################################################################

# LOAD PACKAGES & READ IN DATA #################################################
library(tidyverse)
library(report)
library(modelsummary)
library(patchwork)

s4s <- read_csv("WHRD_clean.csv")


# STATISTICS ###################################################################

### t-test ####

##### depression ----
female_dep <- s4s %>%
  filter(biosex == "Female") %>%
  select(depScore) %>%
  as.vector()

male_dep <- s4s %>%
  filter(biosex == "Male") %>%
  select(depScore) %>%
  as.vector()


dep_ttest <- t.test(x = female_dep$depScore,
                    y = male_dep$depScore, 
                    alternative = "greater",
                    paired = FALSE)


sdDepF <- s4s %>%
  filter(biosex == "Female") %>%
  summarise(depScore = sd(depScore))

sdDepF <- as.numeric(sdDepF)

sdDepM <- s4s %>%
  filter(biosex == "Male") %>%
  summarise(depScore = sd(depScore))

sdDepM <- as.numeric(sdDepM)

report(dep_ttest)
glance(dep_ttest)


### prop-test ####

##### ipv ----
s4s_table <- s4s %>%
  mutate(ever_ipv = as.factor(ever_ipv),
         sumscore_SA = as.factor(if_else(sumscore_SA > 0 | sumscore_other > 0, 1, 0)),
         sumscore_PA = as.factor(sumscore_PA),
         ever_aud = as.factor(ever_aud)) %>%
  mutate(ever_ipv = fct_recode(ever_ipv, Yes = "1", No = "0"),
         sumscore_SA = fct_recode(sumscore_SA, Yes = "1", No = "0"),
         sumscore_PA = fct_recode(sumscore_PA, Yes = "1", No = "0"),
         ever_aud = fct_recode(ever_aud, Yes = "1", No = "0"))

# Size of female and male samples
female_n <- s4s_table %>%
  filter(biosex == "Female") %>%
  nrow() %>%
  as.numeric()

male_n <- s4s_table %>%
  filter(biosex == "Male") %>%
  nrow() %>%
  as.numeric()

# Size of those reporting IPV by sex
female_ipv <- s4s_table %>%
  filter(biosex == "Female" & ever_ipv == "Yes") %>%
  nrow() %>%
  as.numeric()

male_ipv <- s4s_table %>%
  filter(biosex == "Male" & ever_ipv == "Yes") %>%
  nrow() %>%
  as.numeric()

s4s_table %>%
  count(ever_aud)


# Two proportion z-test
ipv_proptest <- prop.test(x = c(female_ipv, male_ipv),
                    n = c(female_n, male_n),
                    alternative = "greater")

glance(ipv_proptest)

##### ipv pa ----

# Size of those reporting IPV by sex
female_pa <- s4s_table %>%
  filter(biosex == "Female" & sumscore_PA == "Yes") %>%
  nrow() %>%
  as.numeric()

male_pa <- s4s_table %>%
  filter(biosex == "Male" & sumscore_PA == "Yes") %>%
  nrow() %>%
  as.numeric()


# Two proportion z-test
pa_proptest <- prop.test(x = c(female_pa, male_pa),
                          n = c(female_n, male_n),
                          alternative = "less")

glance(pa_proptest)


##### ipv sa ----

# Size of those reporting IPV by sex
female_sa <- s4s_table %>%
  filter(biosex == "Female" & sumscore_SA == "Yes") %>%
  nrow() %>%
  as.numeric()

male_sa <- s4s_table %>%
  filter(biosex == "Male" & sumscore_SA == "Yes") %>%
  nrow() %>%
  as.numeric()


# Two proportion z-test
sa_proptest <- prop.test(x = c(female_sa, male_sa),
                          n = c(female_n, male_n),
                          alternative = "greater")

glance(sa_proptest)


##### aud ----

# Size of those reporting AUD by sex
female_aud <- s4s_table %>%
  filter(biosex == "Female" & ever_aud == "Yes") %>%
  nrow() %>%
  as.numeric()

male_aud <- s4s_table %>%
  filter(biosex == "Male" & ever_aud == "Yes") %>%
  nrow() %>%
  as.numeric()


# Two proportion z-test
aud_proptest <- prop.test(x = c(female_aud, male_aud),
                         n = c(female_n, male_n),
                         alternative = "less")

glance(aud_proptest)


### plots ####

ggplot(s4s_table, aes(x = depScore, y = ever_ipv, fill = biosex)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Sex Differences in IPV Exposure and Depression Symptoms",
       x = "Depression Symptom Score",
       y = "IPV Exposure") +
  scale_fill_manual(values = c("#7B68EE", "#00BFFF"), name = "Sex")

ggplot(s4s_table, aes(x = depScore, y = sumscore_PA, fill = biosex)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Sex Differences in IPV Exposure and Depression Symptoms",
       x = "Depression Symptom Score",
       y = "PA IPV Exposure") +
  scale_fill_manual(values = c("#7B68EE", "#00BFFF"), name = "Sex")

ggplot(s4s_table, aes(x = depScore, y = sumscore_SA, fill = biosex)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Sex Differences in IPV Exposure and Depression Symptoms",
       x = "Depression Symptom Score",
       y = "SA IPV Exposure") +
  scale_fill_manual(values = c("#7B68EE", "#00BFFF"), name = "Sex")

ggplot(s4s_table, aes(x = depScore, y = ever_aud, fill = biosex)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Sex Differences in AUD and Depression Symptoms",
       x = "Depression Symptom Score",
       y = "Ever AUD") +
  scale_fill_manual(values = c("#7B68EE", "#00BFFF"), name = "Sex")


# LOGISTIC REGRESSION ##########################################################
s4s_model1 <- glm(ever_aud ~ biosex + ever_ipv + depScore, family = binomial, data = s4s_table)
s4s_model2 <- glm(ever_aud ~ biosex + sumscore_PA + depScore, family = binomial, data = s4s_table)
s4s_model3 <- glm(ever_aud ~ biosex + sumscore_SA + depScore, family = binomial, data = s4s_table)


models <- list(
  "Model 1"     = s4s_model1, # Coeff = 8.48, Std Err = 0.06, p = 8.51e-112
  "Model 2"     = s4s_model2, # Coeff = 6.60, Std Err = 0.05, p = 1.84e-110
  "Model 3"     = s4s_model3 # Coeff = 9.07, Std Err = 0.05, p = 3.79e-32
)

modelsummary(models, "markdown", 
             stars = TRUE,
             title = "Model Comparison")

summary(s4s_model1)
summary(s4s_model2)
summary(s4s_model3)


# Run these to get info for table
glance(s4s_model1)
summary(s4s_model1)$coefficient
summary(s4s_model2)$coefficient
summary(s4s_model3)$coefficient

report(s4s_model1)
report(s4s_model2)
report(s4s_model3)


### plots ####
ggplot(s4s_table, aes(x=ever_aud, y=depScore, color=biosex)) + 
  geom_jitter() +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = F) +
  labs(x = "Ever AUD", y = "Depression Symptom Score") +  
  facet_grid(ever_ipv ~ .) +
  theme_minimal()
