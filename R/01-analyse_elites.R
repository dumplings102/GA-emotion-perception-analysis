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
         wasiiq = as.numeric(WASIIQ),
         Years.of.education = as.numeric(Years.of.education)) %>%
  janitor::clean_names()

#add extra data and separate by distance measure
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
c.age <- aov(distance_score ~ group+target_emotion+age, c_df) #control age
c.iq <- aov(distance_score ~ group+target_emotion+wasiiq, c_df) #control sex
c.edu <- aov(distance_score ~ group+target_emotion+years_of_education, c_df)

aictab(list(c.one, c.two, c.age, c.iq, c.edu),
       modnames = c('one-way',
                    'two-way',
                    'age',
                    'IQ',
                    'education')) #best fitting model = iq

#check assumptions
par(mfrow=c(2,2))
plot(c.two)
par(mfrow=c(1,1))

#model selection
anova(c.two, c.iq)
summary(c.iq)

#post-hoc
summary(c.iq)
TukeyHSD(c.iq)
eta_squared(c.iq) #effect size, 0.04 - small effect

#ANOVA
##the effect of clinical group & target emotion on distance measure
###control variables added as covariates
n.one <- aov(distance_score ~ group, n_df) #one-way
n.two <- aov(distance_score ~ group+target_emotion, n_df) #two-way
n.age <- aov(distance_score ~ group+target_emotion+age, n_df) #control age
n.iq <- aov(distance_score ~ group+target_emotion+wasiiq, n_df) #control sex
n.edu <- aov(distance_score ~ group+target_emotion+years_of_education, n_df)

aictab(list(n.one, n.two, n.age, n.iq, n.edu),
       modnames = c('one-way',
                    'two-way',
                    'age',
                    'IQ',
                    'education')) #best fitting model = age

#check assumptions
par(mfrow=c(2,2))
plot(c.two)
par(mfrow=c(1,1))

#model selection
anova(n.two, n.age)
summary(n.two)

#linear mixed effects model
c.lmm <- lmer(distance_score ~ target_emotion+(1|id), c_df) #distance from centroid
c.lmm.group <- lmer(distance_score ~ group+target_emotion+(1|id), c_df) #group var
#check proportion of variance explained by the model
r.squaredGLMM(c.lmm, MuMIn.noUpdateWarning = T)
##R2m (marginal) variance explained by fixed effects
###R2c (conditional) variance explained by entire model

#model selection
anova(c.lmm, c.lmm.group) #LMM with group has significantly better fit
summary(c.lmm)

#assumptions
plot(c.lmm.group)
qqnorm(resid(c.lmm.group))
qqline(resid(c.lmm.group))
hist(resid(c.lmm.group))

#extract conditional means from random effects
cdist_r.int <- as.data.frame(ranef(c.lmm.group)) %>%
  mutate(group = case_when(str_detect(grp, "S1PT+") ~ "patient",
                           str_detect(grp, "S1HC+") ~ "control")) %>%
  mutate(group=as.factor(group))

ggplot(cdist_r.int,
       aes(x = group, y = condval, fill=group)) +
  stat_halfeye(adjust = 0.5,
               justification = -0.2,
               .width = 0,
               point_colour = NA,
               alpha = .5,
               trim = F,
               scale = .5) +
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5,
               position = position_dodge(0.2)) +
  stat_dots(side = "left",
            justification = 1.15,
            binwidth = .002,
            col=NA) +
  scale_fill_manual(values=c("#999999", "#E69F00"), guide='none') +
  theme_classic() +
  ggtitle("Intercepts") +
  theme(axis.ticks.x = element_blank())
