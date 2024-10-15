#load packages
library(tidyverse)
library(AICcmodavg)
library(lme4)
library(ggpubr)
library(effectsize)
library(rstatix)
library(MASS)

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

#ANOVA
ggboxplot(cc_df,
          x = "target_emotion", y = "distance_score", color = "group", palette='jco')

outliers <- cc_df %>%
  group_by(group, target_emotion) %>%
  identify_outliers(distance_score) %>%
  filter(is.outlier)

cc_df <- cc_df %>%
  anti_join(outliers)

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
  between = group, within = target_emotion, type = 2
)

summary(aov(distance_score~group+target_emotion+group*target_emotion, cc_df))

#Greenhouse-Geisser sphericity correction applied to factors violating assumption
get_anova_table(cc.aov)

summary(lm(distance_score ~ group, cc_df))

ggplot(cc_df, aes(x = group,
                  y = distance_score,
                  group = target_emotion, col = target_emotion)) +
  geom_point() + stat_summary(fun = mean, geom = "line") + theme_bw()

cc_df %>%
  pairwise_t_test(
    distance_score ~ target_emotion, paired = F,
    p.adjust.method = "bonferroni"
  )

cc_df %>%
  pairwise_t_test(
    distance_score ~ group, paired = F,
    p.adjust.method = "bonferroni"
  )


#ANOVA
ggboxplot(nc_df,
          x = "target_emotion", y = "distance_score", color = "group", palette='jco')

outliers <- nc_df %>%
  group_by(group, target_emotion) %>%
  identify_outliers(distance_score) %>%
  filter(is.outlier)

nc_df <- nc_df %>%
  anti_join(outliers)

nc_df %>%
  group_by(group, target_emotion) %>%
  shapiro_test(distance_score)

ggqqplot(nc_df, "distance_score", ggtheme = theme_bw()) +
  facet_grid(group ~ target_emotion)

nc_df %>%
  group_by(target_emotion) %>%
  levene_test(distance_score ~ group)

#transformed_dist <- boxcox(distance_score~group, data=nc_df)

#b=boxcox(distance_score~group, data=nc_df)
#lambda <- b$x[which.max(b$y)]

#nc_df$transformed_dist <- (nc_df$distance_score^lambda-1)/lambda

box_m(nc_df[, "distance_score", drop = F], nc_df$group)

nc.aov <- anova_test(
  data = nc_df, dv = distance_score, wid = id,
  between = group, within = target_emotion
)
get_anova_table(nc.aov)

nc_df %>%
    pairwise_t_test(
      distance_score ~ target_emotion, paired = F,
      p.adjust.method = "bonferroni"
    )

cc_df %>%
  pairwise_t_test(
    distance_score ~ target_emotion, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
