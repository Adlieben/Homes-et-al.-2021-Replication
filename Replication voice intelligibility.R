# YES means the finding was replicated, NO means it was not

# Load the file SpeechIntelligibility_PercentCorrect_(n=50) that I renames in the folder
# Libraries
# install.packages("readxl")
# install.packages('MOTE') 
library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(MOTE) # This package is mentioned in the study as being used for effect 
# sizes and confidence intervals

# Files
data <- read_excel("Dataspeechpercent.xls") # Corresponds to the percentage of successes at voice intelligibility task
data2 <- read_excel("z_scores.xlsx") # Corresponds to the z scores computed from the results on each tasks

# The z scores have apparently been computed from the d′ from the explicit-recognition task (Fig. 2b) and the speech-intelligibility-benefit scores (Fig. 2c) 
# for each of the three familiar voices. Since there is no formula given to calculate them, they cannot
# be replicated so we will simply use them as provided.

# Data cleaning ---------------------------------------------------
# Add an ID variable
data <- rowid_to_column(data, "ID")
data2 <- rowid_to_column(data2, "ID")

# Cleaning data
# Check data
str(data) # All numeric, so good start

# Turn the - to a _ in order to be able to reference the variable in future commands
names(data) <- gsub(x = names(data), pattern = "-", replacement = "_")

# Transform to long format to use dependent and independent variables
# Put every repeated observation on its own line using gather()
data_long <- data %>%
  gather(key = "Condition", value = "Score", TMR_6_Fam1, TMR_6_Fam2, TMR_6_Fam3, TMR_6_BothUnfam, TMR3_Fam1, TMR3_Fam2, TMR3_Fam3, TMR3_BothUnfam) %>%
    convert_as_factor(ID, QuietBabble) # QuietBabble codes the two training groups: 
                                        # Quiet (0) or Babble (1) according to the codebook

# Separate familiarity and TMR, assign names to their levels
# TMR
data_long$TMR <- "-6"
data_long$TMR[grepl('^TMR3', data_long$Condition)] <- "3"
data_long$TMR <- as.factor(data_long$TMR)

# Familiarity
data_long$fam <- "Unfamiliar Baseline"
data_long$fam[grepl('Fam3$', data_long$Condition)] <- "Least Familiar"
data_long$fam[grepl('Fam2$', data_long$Condition)] <- "Moderately Familiar"
data_long$fam[grepl('Fam1$', data_long$Condition)] <- "Most Familiar"
data_long$fam <- as.factor(data_long$fam)
data_long$fam <- factor(data_long$fam, levels=c('Most Familiar', 'Moderately Familiar', 'Least Familiar', 'Unfamiliar Baseline')) # Re-order for graph later

# Order by ID again (optional)
data_long <- data_long[with(data_long,order(ID)),]

# Cleaning data2
# Check data
str(data2) # All numeric, so good start

# Transform to long format to use dependent and independent variables
# Put every repeated observation on its own line using gather()
data2_long <- data2 %>%
  gather(key = "Condition", value = "Score", RECOG_z_Fam1, RECOG_z_Fam2, RECOG_z_Fam3, INTELL_z_Fam1, INTELL_z_Fam2, INTELL_z_Fam3) %>%
  convert_as_factor(ID, QuietBabble) # QuietBabble codes the two training groups: 
                                      # Quiet (0) or Babble (1) according to the codebook

# Separate familiarity and task (no TMR here), assign names to their levels
# Task
data2_long$Task <- "Reco"
data2_long$Task[grepl('^INTELL', data2_long$Condition)] <- "Intell"
data2_long$Task <- as.factor(data2_long$Task)

# Familiarity (no unfamiliar baseline here)
data2_long$fam <- "Least Familiar"
data2_long$fam[grepl('Fam2$', data2_long$Condition)] <- "Moderately Familiar"
data2_long$fam[grepl('Fam1$', data2_long$Condition)] <- "Most Familiar"
data2_long$fam <- as.factor(data2_long$fam)
data2_long$fam <- factor(data2_long$fam, levels=c('Most Familiar', 'Moderately Familiar', 'Least Familiar')) # Re-order the factors

# Order by ID again (optional)
data2_long <- data2_long[with(data2_long,order(ID)),]

# Replications ----------------------------------------------------
# Graph 3: mean differences and standard errors for within-factors (TMR and Familiarity)
# Unfortunately, mean and standard error of the mean  for performance on the speech intelligibility task
# do not appear anywhere outside this graph 3. It is thus replicated as a substitute for summary 
# statistics.
# Choose the same colours as in the paper 
group.colors <- c("#c65c28", "#fe7c1f", "#fe9a59", "#5d97c8")

# Plot the means
data_long %>% ggplot(aes(TMR, Score, color = fam, group = fam)) +
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

# Finding 1: Three-way mixed ANOVA with the factors training group (quiet, babble), 
# familiarity (most familiar, moderately familiar, least familiar, unfamiliar), and 
# TMR (−6 dB, +3 dB; within subjects) 
## Check assumptions
## Outliers
data_long %>%
  group_by(fam, TMR, QuietBabble) %>%
  identify_outliers(Score) # One outlier, it should be fine

## Normality assumption
data_long %>%
  group_by(fam, TMR, QuietBabble) %>%
  shapiro_test(Score) # Some very small p values, assumption is clearly violated

## Homogneity of variance assumption
data_long %>%
  group_by(fam, TMR) %>%
  levene_test(Score ~ QuietBabble) # For the unfamiliar baseline, this assumption is clearly violated

## Computing the results
res.aov <- anova_test(
  data = data_long, dv = Score, wid = ID,
  between = QuietBabble, within = c(fam, TMR)
)
get_anova_table(res.aov)

## Comparing with the results that they have: (Left side is in paper, arrow shows what has been found here)
## Effect of group, F(1, 48) = 0.20, p = .66, --> this is also in the table: YES
## Group × TMR: F(1, 48) = 1.19, p = .28 --> this is also in the table: YES
## Group × Familiarity: F(3, 144) = 0.17, p = .91 --> lower p-value of 0.89, due to wrong df: NO
## Group × Familiarity × TMR: F(3, 144) = 0.15, p = .93 --> this is also in the table: YES
## Interaction between TMR and familiarity, F(3, 144) = 7.47, p < .001 --> this is also in the table: YES
## Main effect of familiarity was significant, F(2.6, 125.4) = 10.49, p< .001 --> Yes, p-value 
## is tiny and in table: YES
## Intelligibility was significantly better at +3 dB than at −6 dB TMR, F(1, 48) = 140.96, p < .001 --> 
## Yes, p-value is tiny and in table: YES

## The MOTE package is then used to calculate effect sizes and confidence intervals
## The package documentation sggests omega.F() is the function to use here, although this is not made
## Clear in the study
omega.F(1, 48, 0.202, 50, a = 0.05)$estimate # Group effect
omega.F(1, 48, 1.189, 50, a = 0.05)$estimate # Group x TMR effect
omega.F(3, 144, 0.174, 50, a = 0.05)$estimate # Group x Familiarity effect (the values in the text were used, although they have not been replicated)

## ωp2 = −.02, 95% CI = [.00, 1.00] for group effect --> same values: YES
## ωp2 < .01, 95% CI = [.00, .09] for group x TMR effect --> similar values due to rounding: YES
## ωp2 = −.01, 95% CI = [.00, 1.00] for group x familiarity effect --> different effect size: NO


# Finding 2: Two-way within-subjects ANOVA with the factors familiarity (most familiar, moderately 
# familiar, least familiar, unfamiliar) and task (speech intelligibility, voice recognition) using the
# z scores provided

## Computing the results
res.aov <- anova_test(
  data = data2_long, dv = Score, wid = ID,
  within = c(fam, Task)
)
get_anova_table(res.aov)

## Comparing with the results that they have: (Left side is in paper, arrow shows what has been found here)
## Two-way interaction between task and familiarity was significant, F(1.8, 86.7) = 4.55, p = .017
## --> This is also in the table: YES

# Finding 3: Follow-up one-way ANOVA with factor familiarity (most familiar, moderately familiar, least 
# familiar, unfamiliar) using task as a dependent variable¨
## The method for conducting these one-way ANOVAs using the z-scores is not made very clear so here is
## is just an idea of how they could have been performed
# Subsetting the long data for both tasks
data_intell <- data2_long %>% filter(Task == 'Intell')
data_reco <- data2_long %>% filter(Task == 'Reco')

# Conducting two one-way ANOVAs, one for each task
res.aov <- aov(Score ~ fam, data = data_intell)
summary(res.aov)
res.aov <- aov(Score ~ fam, data = data_reco)
summary(res.aov)

## Comparing with the results that they have: (Left side is in paper, arrow shows what has been found here)
## Significant effect of familiarity on speech-intelligibility-benefit scores, F(2, 98) = 4.27, p = .017
## --> Very different results here with F(2, 148) = 2.07, p = 0.129: NO
## Non-significant effect of familiarity on explicit-recognition scores, F(2, 98) = 0.15, p = .87
## --> Very different results here with F(2, 147) = 0.06, p = 0.94: NO¨
