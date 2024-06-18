#load packages
library(tidyverse)
library(AICcmodavg)
library(ggpubr)
library(effectsize)
library(rstatix)

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
c.two <- aov(distance_score ~ group+target_emotion+group*target_emotion,
             c_df) #two-way
c.age <- aov(distance_score ~ group+target_emotion+group*target_emotion+age,
             c_df) #control age
c.edu <- aov(distance_score ~ group+target_emotion+group*target_emotion+years_of_education,
             c_df) #control years of education
c.sex <- aov(distance_score ~ group+target_emotion+group*target_emotion+sex,
             c_df) #control sex

aictab(list(c.two, c.age, c.sex, c.edu),
       modnames = c('two-way',
                    'age',
                    'sex',
                    'education')) #best fitting model = two-way ANOVA

#model selection
anova(c.two, c.age)

#check assumptions
par(mfrow=c(2,2))
plot(c.two)
par(mfrow=c(1,1))

#signifiance and effect size
summary(c.two)
eta_squared(c.two)

#post-hoc
emmeans(c.two, pairwise~group|target_emotion, adjust='Tukey')

#ANOVA
##the effect of clinical group & target emotion on distance measure
###control variables added as covariates
n.two <- aov(distance_score ~ group+target_emotion+group*target_emotion, n_df) #two-way
n.age <- aov(distance_score ~ group+target_emotion+group*target_emotion+age, n_df) #control age
n.sex <- aov(distance_score ~ group+target_emotion+group*target_emotion+sex, n_df) #control sex
n.edu <- aov(distance_score ~ group+target_emotion+group*target_emotion+years_of_education, n_df)


aictab(list(n.two, n.age, n.sex, n.edu),
       modnames = c('two-way',
                    'age',
                    'sex',
                    'education')) #best fitting model = age

#model selection
anova(n.two, n.age)

###check assumptions
#Linearity
ggscatter(
  n_df, x = "age", y = "distance_score",
  facet.by  = c("group", "target_emotion"),
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

#Homogeneity of regression slopes
n_df %>%
  anova_test(
    distance_score ~ age + group + target_emotion +
      group*target_emotion + age*group +
      age*target_emotion + age*group*target_emotion
  )

#Normality of residuals
model <- lm(distance_score ~ age + group*target_emotion, data = n_df)
model.metrics <- augment(model)
shapiro.test(model.metrics$.resid)

#Homogeneity of variances
levene_test(.resid ~ group*target_emotion, data = model.metrics)

#Outliers
model.metrics %>%
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

#signifiance and effect size
summary(n.two)
eta_squared(n.two)

#no post-hoc because non-significant
