#load packages
library(tidyverse)

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

#define data summary function for violin plot
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#check subject number in each condition
check <- dist_df %>%
  group_by(group, id) %>%
  summarise(n = n()) #count=6 bc 2 distance measures per emotion (2 x 3 = 6)
#patient 15 only has 4 - missing fearful trial

#visualise average cosine distance from centroid
ccos.plot <-
  ggplot(dist_df %>%
           filter(distance_measure=='Centroid_cosine_distance'),
         aes(x = target_emotion, y = distance_score, fill = group)) +
  geom_violin(col = NA, alpha = .5) +
  stat_summary(fun.data = data_summary,
               position = position_dodge(0.9),
               size = .5) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  scale_y_continuous(name = "") +
  scale_x_discrete(name = '',
                   labels = c('Anger', 'Fear', 'Happiness')) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = NA),
        plot.background = element_rect(fill = "white")) +
  ggtitle("Cosine distance from centroid of elite faces (N=47)")


ccos.raincloud <-
  ggplot(dist_df %>%
           filter(distance_measure=='Centroid_cosine_distance'),
         aes(x = target_emotion, y = distance_score, fill = group)) +
  stat_halfeye(adjust = 0.5,
               justification = -0.3,
               .width = 0,
               point_colour = NA,
               alpha = .5,
               trim = F,
               scale = .5) +
  geom_boxplot(width = 0.2,
               outlier.color = NA,
               alpha = 0.5,
               position = position_dodge(0.3)) +
  stat_dots(side = "left",
            justification = 1.2,
            binwidth = 0.015,
            col=NA) +
  scale_fill_manual(name = "Group",
                    values=c("#999999", "#E69F00")) +
  scale_y_continuous(name = "Cosine Distance from Centroid") +
  scale_x_discrete(name = "Target Emotion",
                   labels = c('Anger', 'Fear', 'Happiness')) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white")) +
  ggtitle("Variability of elite faces (N=47)")

ggsave('plots/centroid_violin.png', ccos.plot)
ggsave('plots/centroid_raincloud.png', ccos.raincloud, width=5, height=4, dpi=300)

#visualise average cosine distance from neutral
ncos.plot <-
  ggplot(dist_df %>%
           filter(distance_measure=='Neutral_cosine_distance'),
         aes(x = target_emotion,
             y = distance_score,
             fill = group)) +
  geom_violin(col = NA,
              alpha = .5) +
  stat_summary(fun.data = data_summary,
               position = position_dodge(0.9),
               size = .5) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  scale_y_continuous(name = "cosine distance") +
  scale_x_discrete(name = 'Target Emotion',
                   labels = c('Anger', 'Fear', 'Happiness')) +
  theme_classic() +
  ggtitle("Cosine distance from neutral of elite faces (N=47)") +
  theme(axis.ticks.x = element_blank())

ncos.raincloud <-
  ggplot(dist_df %>%
           filter(distance_measure=='Neutral_cosine_distance'),
         aes(x = target_emotion, y = distance_score, fill = group)) +
  stat_halfeye(adjust = 0.5,
               justification = -0.35,
               .width = 0,
               point_colour = NA,
               alpha = .5,
               trim = F,
               scale = .5) +
  geom_boxplot(width = 0.2,
               outlier.color = NA,
               alpha = 0.5,
               position = position_dodge(0.3)) +
  stat_dots(side = "left",
            justification = 1.2,
            binwidth = 0.015,
            col=NA) +
  scale_fill_manual(name = 'Group',
                    values=c("#999999", "#E69F00")) +
  scale_y_continuous(name = "Cosine Distance From Neutral") +
  scale_x_discrete(name = 'Target Emotion',
                   labels = c('Anger', 'Fear', 'Happiness')) +
  ggtitle("Intensity of elite faces (N=47)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = NA),
        plot.background = element_rect(fill = "white"))

ggsave('plots/neutral_violin.png', ncos.plot)
ggsave('plots/neutral_raincloud.png', ncos.raincloud, width=5, height=4,dpi=300)

