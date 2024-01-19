########## Loading files --------------------------------------------------
# Libraries
# install.packages("readxl")
# install.packages('MOTE') 
# install.packages('ggforce')
library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(gridExtra)
library(ggforce) # Used for geom_sina
library(MOTE) # This package is mentioned in the study as being used for effect 
# sizes and confidence intervals

# Files
training_df <- read_excel("data/Training.xls") # Contains the percentage of successes on the training task
explicit_rec_df <- read_excel("data/Explicit_recognition.xls") # Contains the d-prime values for the explicit recognition task
voice_int <- read_excel("data/Speech_intelligibility.xls") # Contains the percentage of successes at voice intelligibility task
z <- read_excel("data/z_scores.xlsx") # Contains the z scores computed from the results on each tasks

# The z scores have apparently been computed from the d′ from the explicit-recognition task (Fig. 2b) and the speech-intelligibility-benefit scores (Fig. 2c) 
# for each of the three familiar voices. Since there is no formula given to calculate them, they cannot
# be replicated so we will simply use them as provided.



########## Data cleaning ---------------------------------------------------
# Add an ID variable to each dataset
training_df <- rowid_to_column(training_df, "ID")
explicit_rec_df <- rowid_to_column(explicit_rec_df, "ID")
voice_int <- rowid_to_column(voice_int, "ID")
z <- rowid_to_column(z, "ID")

# Cleaning data
# Check data types
str(voice_int) # All numeric
str(z) # All numeric
str(training_df)
str(explicit_rec_df)

# Turn the - to a _ in some datasets in order to be able to reference the variable in future commands
names(voice_int) <- gsub(x = names(voice_int), pattern = "-", replacement = "_") # The - is used for -3 TMR but hinders computation

# A group indicator (Quiet or Babble) is missing for the training data, we can add it by *assuming* the data
# is ordered identically as in the other data sets.
training_df$QuietBabble <- c(rep(1, 25), rep(0, 25))

# Transform to long format to use dependent and independent variables 
# Put every repeated observation on its own line using gather()
# Modify variables to be used later on
# TRAINING SCORES ---------------------------------------------
training_df_long <- training_df %>% gather(key = "familiarity", value = "perc", 2:4) %>%
  mutate(familiarity = case_when( # Assign names to the familiarity levels
    familiarity == "fam1" ~ "Most",
    familiarity == "fam2" ~ "Moderately",
    familiarity == "fam3" ~ "Least"
  )) %>%
  convert_as_factor(ID, QuietBabble)  # QuietBabble codes the two training groups: 
                                      # Quiet (0) or Babble (1) according to the codebook

# Make familiarity a factor
training_df_long$familiarity <- as.factor(training_df_long$familiarity)

# Order by ID again (optional)
training_df_long <- training_df_long[with(training_df_long,order(ID)),]

# EXPLICIT RECOGNITION SCORES ---------------------------------------------
explicit_rec_df_long <- explicit_rec_df %>% gather(key = "familiarity", value = "perc", 3:5) %>%
  mutate(familiarity = case_when( # Assign names to the familiarity levels
    familiarity == "Fam1" ~ "Most",
    familiarity == "Fam2" ~ "Moderately",
    familiarity == "Fam3" ~ "Least"
  )) %>%
  convert_as_factor(ID, QuietBabble)

# Make familiarity a factor
explicit_rec_df_long$familiarity <- as.factor(explicit_rec_df_long$familiarity)

# Order by ID again (optional)
explicit_rec_df_long <- explicit_rec_df_long[with(explicit_rec_df_long,order(ID)),]

# SPEECH INTELLIGIBILITY SCORES -------------------------------------------
voice_int_long <- voice_int %>% select(-c(AvTMR_Fam1, AvTMR_Fam2, AvTMR_Fam3, AvTMR_BothUnfam, Fam1_Benefit,
                                Fam2_Benefit, Fam3_Benefit)) %>% # Remove the average and benefit scores here
  gather(key = "Condition", value = "Score", TMR_6_Fam1, TMR_6_Fam2, TMR_6_Fam3, TMR_6_BothUnfam, 
         TMR3_Fam1, TMR3_Fam2, TMR3_Fam3, TMR3_BothUnfam) %>%
  convert_as_factor(ID, QuietBabble)

# Separate familiarity and TMR, assign names to their levels
# TMR
voice_int_long$TMR <- "-6"
voice_int_long$TMR[grepl('^TMR3', voice_int_long$Condition)] <- "3"
voice_int_long$TMR <- as.factor(voice_int_long$TMR)

# Familiarity
voice_int_long$fam <- "Unfamiliar Baseline"
voice_int_long$fam[grepl('Fam3$', voice_int_long$Condition)] <- "Least Familiar"
voice_int_long$fam[grepl('Fam2$', voice_int_long$Condition)] <- "Moderately Familiar"
voice_int_long$fam[grepl('Fam1$', voice_int_long$Condition)] <- "Most Familiar"
voice_int_long$fam <- as.factor(voice_int_long$fam)
voice_int_long$fam <- factor(voice_int_long$fam, levels=c('Most Familiar', 'Moderately Familiar', 
                                                'Least Familiar', 'Unfamiliar Baseline')) # Re-order for graph later

# Order by ID again (optional)
voice_int_long <- voice_int_long[with(voice_int_long,order(ID)),]

# FAMILIAR VOICE BENEFIT SCORES -------------------------------------------
# Benefit scores are stored in variables that end in 'Benefit'
voice_int_ben <- voice_int %>% select(Fam1_Benefit,
                                Fam2_Benefit, Fam3_Benefit, ID) %>% # Keep only the benefit scores here
  gather(key = "fam", value = "Benefit", Fam1_Benefit,
         Fam2_Benefit, Fam3_Benefit) %>%
  convert_as_factor(ID)

# Rename familiarity
voice_int_ben$fam[grepl('^Fam3', voice_int_ben$fam)] <- "Least"
voice_int_ben$fam[grepl('^Fam2', voice_int_ben$fam)] <- "Moderately"
voice_int_ben$fam[grepl('^Fam1', voice_int_ben$fam)] <- "Most"
voice_int_ben$fam <- as.factor(voice_int_ben$fam)
voice_int_ben$fam <- factor(voice_int_ben$fam, levels=c('Least', 'Moderately', 
                                                  'Most')) # Re-order the factors

# Order by ID again (optional)
voice_int_ben <- voice_int_ben[with(voice_int_ben,order(ID)),]

# Z SCORES ----------------------------------------------------------------
z_long <- z %>%
  gather(key = "Condition", value = "Score", RECOG_z_Fam1, RECOG_z_Fam2, RECOG_z_Fam3, 
         INTELL_z_Fam1, INTELL_z_Fam2, INTELL_z_Fam3) %>%
  convert_as_factor(ID, QuietBabble)

# Separate familiarity and task (no TMR here), assign names to their levels
# Task
z_long$Task <- "Reco"
z_long$Task[grepl('^INTELL', z_long$Condition)] <- "Intell"
z_long$Task <- as.factor(z_long$Task)

# Familiarity (no unfamiliar baseline here)
z_long$fam <- "Least Familiar"
z_long$fam[grepl('Fam2$', z_long$Condition)] <- "Moderately Familiar"
z_long$fam[grepl('Fam1$', z_long$Condition)] <- "Most Familiar"
z_long$fam <- as.factor(z_long$fam)
z_long$fam <- factor(z_long$fam, levels=c('Most Familiar', 'Moderately Familiar', 
                                                  'Least Familiar')) # Re-order the factors

# Order by ID again (optional)
z_long <- z_long[with(z_long,order(ID)),]


########## Replications ---------------------------------------------------
# YES means the finding was replicated, NO means it was not
# 4 planned mixed ANOVAs are under each finding.

# Since, summary statistics for each task are not presented explicitly, figures 2 and 3
# have been replicated instead.

# FIG 2 ------------------------------------------------------------------
# Task performance in each of the three familiarity condition
# (a) Percentage of correct responses in the training across familiarity conditions
plot1 <- ggplot(training_df_long, aes(x = factor(familiarity), y = perc)) +
  geom_violin(fill = "lightgreen", alpha = 0.5) + 
  geom_sina(col = "darkgreen", alpha = 0.2) + # jitters inside the violin plot
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar',  width = 0.05, col = 'black') + # Constructs the SE intervals
  stat_summary(fun = mean,
               geom = 'point') +
  geom_hline(yintercept = 33, linetype = "dashed", col = "black") + # Use geom_hline for a horizontal line
  coord_cartesian(ylim=c(30, 100)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + # Centers the title
  labs(title = "Training", y="Percentage Correct", x="Talker Familiarity")

# (b) d′ values in the explicit-recognition test across familiarity conditions
plot2 <- ggplot(explicit_rec_df_long, aes(x = factor(familiarity), y = perc)) +
  geom_violin(fill = "lightgreen", alpha = 0.5) + 
  geom_sina(col = "darkgreen", alpha = 0.2) + 
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar',  width = 0.05, col = 'black') + 
  stat_summary(fun = mean,
               geom = 'point') +
  geom_hline(yintercept = 0.3, linetype = "dashed", col = "black") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Explicit-Recognition Test", y="d' (log-linear)", x="Talker Familiarity")

# (c) Familiar-voice benefit across familiarity conditions
plot3 <- ggplot(voice_int_ben, aes(x = factor(fam), y = Benefit)) +
  geom_violin(fill = "lightgreen", alpha = 0.5) + 
  geom_sina(col = "darkgreen", alpha = 0.2) +
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar',  width = 0.05, col = 'black') +
  stat_summary(fun = mean,
               geom = 'point') +
  geom_hline(yintercept = 0, linetype = "dashed", col = "black") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Speech intelligibility Test", y="Familiar-Voice Benifit (%)", x="Talker Familiarity")

# Plotting
grid.arrange(plot1, plot2, plot3, nrow = 1, ncol = 3)

# FIG 3 -----------------------------------------------------------------
# Mean differences and standard errors on the speech-intelligibility test for within-factors (TMR and Familiarity)
# Choose colours
group.colors <- c("#1a5231", "#0dd460", "#53a374", "#5d97c8")

# Plot the means
voice_int_long %>% ggplot(aes(TMR, Score, color = fam, group = fam)) +
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar',  width = 0.05, col = 'black') + # Constructs the SE intervals
  stat_summary(fun = mean,
               geom = 'point', size=5, 
               aes(shape = fam)) + # Plots point estimates for the mean
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = 'line', col = 'black') + # Joins the points together
  scale_shape_manual(values=c(16,16,16,17)) + # Sets the shapes to be like in the original paper
  scale_fill_manual(values=group.colors) + # Sets the colours to be like in the original paper
  scale_colour_manual(values=group.colors) +
  coord_cartesian(ylim=c(20, 80)) +
  xlab('Target-to-masker Ratio (dB)') +
  ylab('Percentage Correct') +
  theme_classic() + # Aesthetic tweak to make it identical to the paper
  theme(legend.title=element_blank()) # Remove legend title since it's not present in the paper

# The graph is similar, except for a disparity between the least familiar and moderately familiar voices
# between TMRs --> NO replication.

# FINDING 1: TRAINING -----------------------------------------------------
# Two-way mixed ANOVA with the DVs: training group (quiet, babble) and 
# familiarity (most familiar, moderately familiar, least familiar)
# IV: Training score

## Original results:
# A two-way mixed ANOVA investigating whether performance during training differed across groups (quiet, babble; between subjects) 
# and familiarity conditions (Most, Moderately, Least; within subjects) revealed no effect of group, 
# F(1, 48) = 0.36, p = .55, ωp2 = −.01, 95% CI = [.00, 1.00], and no significant interaction between group and familiarity, 
# F(2, 96) = 0.21, p = .81, ωp2 = −.01, 95% CI = [.00, 1.00]. There was, however, a significant effect of familiarity, 
# F(1.4, 67.3) = 12.64, p < .001, ωp2 = .19, 95% CI = [.01, .28], with better performance in the most-familiar condition than in the 
#Moderately condition, t(49) = 4.29, p < .001, dz = 0.61, 95% CI = [0.30, 0.91], and in the least-familiar condition, 
# t(49) = 3.56, p = .001, dz = 0.50, 95% CI = [0.21, 0.80]. Performance did not differ between the Moderately and 
# least-familiar conditions, t(49) = 0.16, p = .88, dz = 0.2, 95% CI = [−0.26, 0.30].

## Computing the results
res.aov <- anova_test(
  data = training_df_long, dv = perc, wid = ID,
  between = QuietBabble, within = c(familiarity)
)
get_anova_table(res.aov)

# T-tests between levels using the wide data
t.test(training_df$fam1, training_df$fam2, paired = TRUE) # Paired = TRUE allows for repeated measures
t.test(training_df$fam1, training_df$fam3, paired = TRUE)
t.test(training_df$fam2, training_df$fam3, paired = TRUE)

# Replication results: YES
# significant effect of familiarity p-value < .001
# also no interaction between familiarity and training condition, p-value .73300 vs original p-value = .81
# there is a statistically significant difference between Most and Moderately and between Most and Least but
# no difference between Moderate and Least, as reported in the paper. The p and t values are also correct

# FINDING 2: EXPLICIT RECOGNITION -----------------------------------------
# Two-way mixed ANOVA with the DVs: training group (quiet, babble) and 
# familiarity (most familiar, moderately familiar, least familiar)
# IV: d′ values in the explicit recognition test.

## Original results:
# was no effect of training group, F(1, 48) < 0.01, p = .95, ωp2 = −.02, 95% CI = [.00, 1.00], and no interaction, 
# F(2, 96) = 2.58, p = .08, ωp2 = .02, 95% CI = [.00, .08]. There was also no effect of familiarity, 
# F(2, 96) = 0.12, p = .89, ωp2 = −.01, 95% CI = [.00, .08]. Collapsing across training groups, we compared recognition d′ 
# in each familiarity condition with chance level (d′ = 0.3) using sign tests. Participants were able to identify all three 
# voices with above-chance accuracy (S ≥ 40, p < .001).

## Computing the results
res.aov <- anova_test(
  data = explicit_rec_df_long, dv = perc, wid = ID,
  between = QuietBabble, within = c(familiarity)
)
get_anova_table(res.aov)

# Replication results: YES
# no effect of training, familiarity, interaction effect also is not significant, all are identical

# FINDING 3: SPEECH INTELLIGIBILITY ---------------------------------------
# Three-way mixed ANOVA with the DVs: factors training group (quiet, babble), 
# familiarity (most familiar, moderately familiar, least familiar, unfamiliar), and 
# TMR (−6 dB, +3 dB; within subjects) 
# IV: Percentage of correct responses on the speech intelligibility task

## Computing the results
res.aov <- anova_test(
  data = voice_int_long, dv = Score, wid = ID,
  between = QuietBabble, within = c(fam, TMR)
)
get_anova_table(res.aov)

## Comparing with the results that they have: (Left side is in paper, arrow shows what has been found here)
## In order, in the table:
## Group, F(1, 48) = 0.20, p = .66, --> this is also in the table: YES
## Familiarity: F(2.6, 125.4) = 10.49, p< .001 --> Yes, p-value is tiny and in table: YES
## TMR: F(1, 48) = 140.96, p < .001 --> Yes, p-value is tiny and in table: YES
## Group × Familiarity: F(3, 144) = 0.17, p = .91 --> lower p-value of 0.89, due to wrong df: NO
## Group × TMR: F(1, 48) = 1.19, p = .28 --> this is also in the table: YES
## Familiarity x TMR: F(3, 144) = 7.47, p < .001 --> this is also in the table: YES
## Group × Familiarity × TMR: F(3, 144) = 0.15, p = .93 --> this is also in the table: YES

## Specific t-test between familiarity levels using the wide data
t.test(voice_int$TMR_6_Fam3, voice_int$TMR_6_BothUnfam, paired = TRUE)
t.test(voice_int$TMR3_Fam3, voice_int$TMR3_BothUnfam, paired = TRUE)
## CLAIM: 'All familiar voices were more intelligible than the unfamiliar voices, 
## −6 dB TMR: t(49) ≥ 2.11, p≤ .040, dz= 0.30; +3 dB TMR: t(49) ≥ 2.89, p≤ .006, dz= 0.41': YES

t.test(voice_int$TMR_6_Fam3, voice_int$TMR_6_Fam2, paired = TRUE)
t.test(voice_int$TMR3_Fam3, voice_int$TMR3_Fam2, paired = TRUE)
## CLAIM: 'the moderately familiar  voice  did  not  differ  from  the  least-familiar  voice,
## −6 dB TMR: t(49) = 0.26, p= .79, dz= 0.04, 95% CI = [−0.24, 0.31]; +3 dB TMR: t(49) = 1.03, 
## p= .31, dz=0.15, 95% CI = [−0.13, 0.42]': YES

t.test(voice_int$TMR_6_Fam1, voice_int$TMR_6_Fam2, paired = TRUE)
t.test(voice_int$TMR3_Fam1, voice_int$TMR3_Fam2, paired = TRUE)
## CLAIM: 'the most-familiar voice was more intelligible than the moderately familiar voice, 
## although this difference was significant only at the higher TMR, −6 dB TMR: t(49) = 1.61, 
## p=.11, dz= 0.23, 95% CI = [−0.05, 0.51]; +3 dB TMR: t(49) =2.56, p= .014, dz= 0.36, 95% CI 
## = [0.07, 0.65]': YES

# The t-tests are all consistent with the claims in the article.

## As exploration, an attempt was made here to replicate effect sizes
## The MOTE package is used to calculate effect sizes and confidence intervals, according to the paper
## The package documentation suggests omega.F() is the function to use here, although this is not made
## clear in the study
omega.F(1, 48, 0.202, 50, a = 0.05)$estimate # Group effect
omega.F(2.6, 125.4, 10.494, 50, a = 0.05)$estimate # Familiarity effect
omega.F(1, 48, 140.958, 50, a = 0.05)$estimate # TMR effect
omega.F(3, 144, 0.174, 50, a = 0.05)$estimate # Group x Familiarity effect (the values in the text were used, although they have not been replicated)
omega.F(1, 48, 1.189, 50, a = 0.05)$estimate # Group x TMR effect
omega.F(3, 144, 7.473, 50, a = 0.05)$estimate # Familiarity x TMR effect
omega.F(3, 144, 0.154, 50, a = 0.05)$estimate # Group x Familiarity x TMR effect

## ωp2 = −.02, 95% CI = [.00, 1.00] for group effect --> same values: YES
## ωp2 = .12, 95% CI = [.03, .23] for familiarity effect --> different values: NO
## ωp2 = .59, 95% CI = [.38, .74] for TMR effect --> different values once again: NO
## ωp2 = −.01, 95% CI = [.00, 1.00] for group x familiarity effect --> different effect size: NO
## ωp2 < .01, 95% CI = [.00, .09] for group x TMR effect --> similar values due to rounding: YES
## ωp2 = .03, 95% CI = [.00, .10] for TMR x Familiarity effect --> different effect size and confidence interval: NO
## ωp2 = .00, 95% CI = [.00, 1.00] for group x familiarity x TMR effect --> different effect size: NO

# Given the differing results, it is unlikely this function was used for effect sizes
# But the study doesn't make it possible to know how they used th MOTE package

# FINDING 4: COMPARING TASKS ----------------------------------------------
# Two-way within-subjects ANOVA with the DVs: familiarity (most familiar, moderately 
# familiar, least familiar, unfamiliar) and task (speech intelligibility, voice recognition) using the
# IV: Z scores

## Computing the results
res.aov <- anova_test(
  data = z_long, dv = Score, wid = ID,
  within = c(fam, Task)
)
get_anova_table(res.aov)

## Comparing with the results that they have: (Left side is in paper, arrow shows what has been found here)
## Familiarity x task: F(1.8, 86.7) = 4.55, p = .017 --> This is also in the table: YES