library(tidyverse)
library(emmeans)
library(AICcmodavg)
library(effectsize)
library(rstatix)

#import GA data from preprocessing in python
nselected_df <- read.csv('processed_data/nselected.csv') %>%
  mutate(across(where(is.character), as.factor),
         nSelected = as.numeric(nSelected),
         nGeneration = as.factor(nGeneration)) %>%
  janitor::clean_names()
str(nselected_df)

#create list of subject names to filter extra data
sub_list <- as.character(unique(nselected_df$id))

extra_df <- read.csv('extra_data/TMS-EEG_additional_data_anita.csv') %>%
  rename(ID = ParticipantID) %>%
  filter(ID %in% sub_list) %>%
  mutate(Age = as.numeric(Age),
         Sex = as.factor(Sex),
         wasiiq = as.numeric(WASIIQ),
         Years.of.education = as.numeric(Years.of.education)) %>%
  janitor::clean_names()

#add extra data
nsel_df <- merge(nselected_df, extra_df, by = 'id')

#two-way anova of clinical group and trial number on number of selected faces
aov.twoway <- aov(n_selected ~ group+n_generation+group*n_generation, nsel_df)

summary(aov.twoway) #check significance
emmeans(aov.twoway, pairwise~group|n_generation, adjust='Tukey') #post-hoc
eta_squared(aov.twoway) #effect size, 0.02 - small effect

