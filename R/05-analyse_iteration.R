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

#ANOVA
ggboxplot(nselected_df,
          x = "n_generation", y = "n_selected", color = "group", palette='jco')

nselected_df %>%
  group_by(group, n_generation) %>%
  identify_outliers(n_selected) %>%
  filter(is.outlier)

#normality assumption
nselected_df %>%
  group_by(group, n_generation) %>%
  shapiro_test(n_selected)

ggqqplot(nselected_df, "n_selected", ggtheme = theme_bw()) +
  facet_grid(group ~ n_generation)

#homogeneity of variance assumption
nselected_df %>%
  group_by(n_generation) %>%
  levene_test(n_selected ~ group)

#homogeneity of covariances assumption
box_m(nselected_df[, "n_selected", drop = F], nselected_df$group)

#sphericity assumption assessed using Mauchly's test in anova_test function
#perform mixed anova
nsel.aov <- anova_test(
  data = nselected_df, dv = n_selected, wid = id,
  between = group, within = target_emotion)

str(nselected_df)

#Greenhouse-Geisser sphericity correction applied to factors violating assumption
get_anova_table(nsel.aov)

summary(aov(n_selected ~ group+n_generation+target_emotion+
              group*n_generation+group*target_emotion+
              group*n_generation*target_emotion, nselected_df))

ggplot(nselected_df, aes(x = group,
                  y = n_selected,
                  group = n_generation, col = n_generation)) +
  geom_point() + stat_summary(fun = mean, geom = "line") + theme_bw()


#two-way anova of clinical group and trial number on number of selected faces
aov.twoway <- aov(n_selected ~ group+n_generation+target_emotion, nselected_df)

summary(aov.twoway) #check significance
emmeans(aov.twoway, pairwise~group|n_generation, adjust='Bonferroni') #post-hoc

