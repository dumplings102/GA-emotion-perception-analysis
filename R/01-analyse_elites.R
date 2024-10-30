#load packages
library(tidyverse)
library(lme4)
library(lmeTest)
library(ggpubr)
library(effectsize)
library(rstatix)
library(emmeans)
library(MuMIn)

#import distance data and separate by distance measure
dist_df <- read.csv('processed_data/all_elite_distances.csv') %>%
  select(ID,
         TargetEmotion,
         Group,
         Neutral_cosine_distance,
         Centroid_cosine_distance) %>%
  pivot_longer(cols = ends_with('distance'),
               names_to = 'distance_measure',
               values_to = 'distance_score') %>%
  mutate(ID = as.factor(ID),
         TargetEmotion = as.factor(TargetEmotion),
         Group = as.factor(Group),
         distance_measure = as.factor(distance_measure)) %>%
  janitor::clean_names()

#create list of subject names to filter extra data
sub_list <- as.character(unique(dist_df$id))

extra_df <- read.csv('extra_data/TMS-EEG_additional_data_anita.csv') %>%
  rename(ID = ParticipantID) %>%
  filter(ID %in% sub_list) %>%
  mutate(Age = as.numeric(Age),
         Sex = as.factor(Sex),
         wasiiq = as.numeric(WASIIQ),
         Years.of.education = as.numeric(Years.of.education)) %>%
  janitor::clean_names()

#add extra data and separate by distance measure
merged_df <- merge(dist_df, extra_df, by = 'id')

cc_df <- dist_df %>%
  filter(distance_measure == 'Centroid_cosine_distance')

nc_df <- dist_df %>%
  filter(distance_measure == 'Neutral_cosine_distance')

###centroid
#ANOVA
ggboxplot(cc_df,
          x = "target_emotion", y = "distance_score", color = "group", palette='jco')

cc_df %>%
  group_by(group, target_emotion) %>%
  identify_outliers(distance_score) %>%
  filter(is.outlier)

#normality assumption
cc_df %>%
  group_by(group, target_emotion) %>%
  shapiro_test(distance_score)

ggqqplot(cc_df, "distance_score", ggtheme = theme_bw()) +
  facet_grid(group ~ target_emotion)

#homogeneity of variance assumption
cc_df %>%
  group_by(target_emotion) %>%
  levene_test(distance_score ~ group)

#homogeneity of covariances assumption
box_m(cc_df[, "distance_score", drop = F], cc_df$group)

#sphericity assumption assessed using Mauchly's test in anova_test function
#perform mixed anova
cc.aov <- anova_test(
  data = cc_df, dv = distance_score, wid = id,
  between = group, within = target_emotion)

#Greenhouse-Geisser sphericity correction applied to factors violating assumption
get_anova_table(cc.aov)

#plot effects
ggplot(cc_df, aes(x = group,
                  y = distance_score,
                  group = target_emotion, col = target_emotion)) +
  geom_point() +
  stat_summary(fun = mean, geom = "line") + theme_bw()

#post-hoc
cc_df %>%
  group_by(target_emotion) %>%
  pairwise_t_test(
    distance_score ~ group, paired = F,
    p.adjust.method = "bonferroni"
  )

#linear mixed-effects
lme.1 <- lmer(distance_score ~ group * target_emotion + (1|id), cc_df)

# Check residuals (how well model managed to fit the data)
plot(lme.1)
qqnorm(resid(lme.1))
qqline(resid(lme.1))
hist(resid(lme.1))

# a summary of the effects
summary(lme.1)
# how much variance it is capturing
r.squaredGLMM(lme.1)

# Then we run an ANOVA on top
anova(lme.1)

# post hoc comparisons
emmeans(lme.1, specs = pairwise ~ group | target_emotion)

#linear mixed-effects
lme.1 <- lmer(distance_score ~ group * target_emotion + (1|id), cc_df)

# Check residuals (how well model managed to fit the data)
plot(lme.1)
qqnorm(resid(lme.1))
qqline(resid(lme.1))
hist(resid(lme.1))

# a summary of the effects
summary(lme.1)
# how much variance it is capturing
r.squaredGLMM(lme.1)

# Then we run an ANOVA on top
anova(lme.1)

# post hoc comparisons
emmeans(lme.1, specs = pairwise ~ group | target_emotion, adjust = 'fdr')

###neutral
#linear mixed-effects
lme.2 <- lmer(distance_score ~ group * target_emotion + (1|id), nc_df)#warning - singular fit

#random effect estimated to be near zero
# how much variance it is capturing
r.squaredGLMM(lme.2)

#use linear model instead - remove random effect
lm <- lm(distance_score ~ group * target_emotion, nc_df)

# Check residuals (how well model managed to fit the data)
plot(lm)
qqnorm(resid(lm))
qqline(resid(lm))
hist(resid(lm))

# a summary of the effects
summary(lm)

# Then we run an ANOVA on top
anova(lm)

# post hoc comparisons
emmeans(lm, specs = pairwise ~ group | target_emotion, adjust = 'fdr')

