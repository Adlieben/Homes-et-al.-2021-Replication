### LIBRARIES 
library(readxl)
library(tidyverse)
library(dplyr)
library(tibble)
library(rstatix)
install.packages("rstatix")

### LOAD DATA

## Training task

training_df <- read_excel("data/Training_PercentCorrect_(n=50).xls")
# create id column 
training_df <- rowid_to_column(training_df, "ID")
# there is a column missing in the original dataframe, assuming that it is also structered as in previous files, we add it directly
training_df$QuietBabble <- c(rep(1, 25), rep(0, 25))
training_df$QuietBabble <- as.factor(training_df$QuietBabble)
# reshape in long format
training_df_long <- gather(training_df, key = "familiarity", value = "perc", 2:4)
# change factor name 
training_df_long <- training_df_long %>%
  mutate(familiarity_recoded = case_when(
    familiarity == "fam1" ~ "Most Familiar",
    familiarity == "fam2" ~ "Moderately Familiar",
    familiarity == "fam3" ~ "Least Familiar"
  ))
# make variable a factor
training_df_long$familiarity <- as.factor(training_df_long$familiarity_recoded)
# distribution plot by group
ggplot(training_df_long, aes(x = factor(familiarity_recoded), y = perc)) +
        geom_violin(fill = "skyblue") +
        theme_minimal() +
        labs(title = "Training", y="Percentage Correct", x="Talker Familiarity")

## Explicit-Recongition Test
excplicit_rec_df <- read_excel("data/ExplicitRecognition_dPrime_(n=50) (1).xls")
# create id column 
excplicit_rec_df <- rowid_to_column(excplicit_rec_df, "ID")
excplicit_rec_df$QuietBabble <- as.factor(excplicit_rec_df$QuietBabble)
# reshape in long format
excplicit_rec_df_long <- gather(excplicit_rec_df, key = "familiarity", value = "perc", 3:5)
# change factor name 
excplicit_rec_df_long <- excplicit_rec_df_long %>%
  mutate(familiarity_recoded = case_when(
    familiarity == "Fam1" ~ "Most Familiar",
    familiarity == "Fam2" ~ "Moderately Familiar",
    familiarity == "Fam3" ~ "Least Familiar"
  ))
# make variable a factor
excplicit_rec_df_long$familiarity <- as.factor(excplicit_rec_df_long$familiarity_recoded)
# distribution plot by group
ggplot(excplicit_rec_df_long, aes(x = factor(familiarity_recoded), y = perc)) +
  geom_violin(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Explicit-Recongition Test", y="d' (log-linear)", x="Talker Familiarity")


### REPLICATION PART

### Training 
## Original results:
# A two-way mixed ANOVA investigating whether performance during training differed across groups (quiet, babble; between subjects) 
# and familiarity conditions (most familiar, moderately familiar, least familiar; within subjects) revealed no effect of group, 
# F(1, 48) = 0.36, p = .55, ωp2 = −.01, 95% CI = [.00, 1.00], and no significant interaction between group and familiarity, 
# F(2, 96) = 0.21, p = .81, ωp2 = −.01, 95% CI = [.00, 1.00]. There was, however, a significant effect of familiarity, 
# F(1.4, 67.3) = 12.64, p < .001, ωp2 = .19, 95% CI = [.01, .28], with better performance in the most-familiar condition than in the 
#moderately familiar condition, t(49) = 4.29, p < .001, dz = 0.61, 95% CI = [0.30, 0.91], and in the least-familiar condition, 
# t(49) = 3.56, p = .001, dz = 0.50, 95% CI = [0.21, 0.80]. Performance did not differ between the moderately familiar and 
# least-familiar conditions, t(49) = 0.16, p = .88, dz = 0.2, 95% CI = [−0.26, 0.30].

training_model <- aov(perc ~ familiarity_recoded*QuietBabble, data=training_df_long)
summary(training_model)
training_model_2 <- aov(perc ~ familiarity_recoded, data=training_df_long)
TukeyHSD(training_model_2) # NB! they did not say which method they used to compare groups


# Replication results: YES
# significant effect of familiriaty p-value < .001
# also no interaction between familiarity and training condition, p-value .73300 vs original p-value = .81
# there is a statistically significant difference between most and moderately familiar and between most and least familiar but
# no difference between moderate and least familiar

### Explicit-Recongition Test
## Original results:
# was no effect of training group, F(1, 48) < 0.01, p = .95, ωp2 = −.02, 95% CI = [.00, 1.00], and no interaction, 
# F(2, 96) = 2.58, p = .08, ωp2 = .02, 95% CI = [.00, .08]. There was also no effect of familiarity, 
# F(2, 96) = 0.12, p = .89, ωp2 = −.01, 95% CI = [.00, .08]. Collapsing across training groups, we compared recognition d′ 
# in each familiarity condition with chance level (d′ = 0.3) using sign tests. Participants were able to identify all three 
# voices with above-chance accuracy (S ≥ 40, p < .001).

explicit_reg_model <- aov(perc ~ familiarity_recoded*QuietBabble, data=excplicit_rec_df_long)
summary(explicit_reg_model)

# Replication results: YES
# no effect of training, familiarity, interaction effect also is not significant

## Original results:
# Collapsing across training groups, we compared recognition d′ in each familiarity condition with chance level (d′ = 0.3) using sign tests. 
# Participants were able to identify all three voices with above-chance accuracy (S ≥ 40, p < .001).
sign_test_result <- binom.exact(sum(excplicit_rec_df_long$perc > 0.3), nrow(excplicit_rec_df_long), p = 0.5, alternative = "greater")
sign_test_result

# Replication results: YES
# p-value < 0.001
