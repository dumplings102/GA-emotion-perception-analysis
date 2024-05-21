#load packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)
library(AICcmodavg)
library(ggpubr)
library(effectsize)

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
         #MCCB.TotalComposite.Tscore = as.numeric(MCCB.TotalComposite.Tscore),
         Years.of.education = as.numeric(Years.of.education)) %>%
  janitor::clean_names()

#created add extra data and separate by distance measure
merged_df <- merge(dist_df, extra_df, by = 'id')

c_df <- merged_df %>%
  filter(distance_measure == 'Centroid_cosine_distance')

n_df <- merged_df %>%
  filter(distance_measure == 'Neutral_cosine_distance')

#ANOVA
##the effect of clinical group & target emotion on distance measure
###control variables added as covariates
c.one <- aov(distance_score ~ group, c_df) #one-way
c.two <- aov(distance_score ~ group+target_emotion, c_df) #two-way
c.int <- aov(distance_score ~ group*target_emotion, c_df) #interaction
c.age <- aov(distance_score ~ group+target_emotion+age, c_df) #control age
c.sex <- aov(distance_score ~ group+target_emotion+sex, c_df) #control sex
c.education <- aov(distance_score ~ group+target_emotion+years_of_education,
                   c_df) #control education level
c.age_sex_education <- aov(distance_score ~ group+target_emotion+age+sex+
                             years_of_education, c_df) #control age, sex, education
aictab(list(c.one, c.two, c.int, c.age, c.sex, c.education, c.age_sex_education),
       modnames = c('one-way',
                    'two-way',
                    'interaction',
                    'age',
                    'sex',
                    'education',
                    'age+sex+education')) #best fitting model = two-way

#check assumptions
par(mfrow=c(2,2))
plot(c.two)

#post-hoc
summary(c.two)
tukey.c.two <- TukeyHSD(c.two)
tukey.c.two
eta_squared(c.age_sex_education) #effect size, 0.04 - small effect

n.two <- aov(distance_score ~ group+target_emotion, n_df) #two-way neutral model
plot(n.two) #check assumptions
par(mfrow=c(1,1))
summary(n.two)
tukey.n.two <- TukeyHSD(n.two)
tukey.n.two

#linear mixed effects model
c.lmm.controls <- lmer(distance_score ~ group+target_emotion+
                         years_of_education+wasiiq+
                (1|id), c_df) #centroid model with controls
c.lmm <- lmer(distance_score ~ group+target_emotion+
                     (1|id), c_df) #centroid model without controls
c.lmm.noid <- lmer(distance_score ~ group+
                     (1|target_emotion), c_df) #centroid model with target emo
                                              ##as random effect instead of id

n.lmm <- lmer(distance_score ~ group+target_emotion+
                (1|id), n_df) #neutral model

#check proportion of variance explained by the model
r.squaredGLMM(c.lmm, MuMIn.noUpdateWarning = T)
##R2m (marginal) variance explained by fixed effects
###R2c (conditional) variance explained by entire model

plot(c.lmm)
qqnorm(resid(c.lmm))
qqline(resid(c.lmm))
hist(resid(c.lmm))

summary(c.lmm.controls)
anova(c.lmm, c.two, c.lmm.noid, c.lmm.controls) #AIC for two-way ANOVA is lowest
##is linear mixed model better or two-way ANOVA?
###LMM with education and IQ as controls is better fit than without??

anova(n.lmm)

#########correlation between symptom severity and emotion processing##########
