library(ggdist)
library(tidyverse)

tidy_extra <- extra_df %>%
  mutate(group = case_when(str_detect(id, "S1PT+") ~ "patient",
                           str_detect(id, "S1HC+") ~ "control")) %>%
  mutate(group = as.factor(group)) %>%
  pivot_longer(cols = c(age, years_of_education),
               names_to = 'demographic_feature',
               values_to = 'years') %>%
  mutate(demographic_feature = as.factor(demographic_feature))

demo_sum <- extra_df %>%
  mutate(group = case_when(str_detect(id, "S1PT+") ~ "patient",
                           str_detect(id, "S1HC+") ~ "control")) %>%
  mutate(group = as.factor(group)) %>%
  group_by(group) %>%
  summarise(n_female = sum(sex=="F"),
            mean_age = mean(age),
            med_age = median(age),
            sd_age = sd(age),
            min_age = min(age),
            max_age = max(age),
            avg_edu = mean(years_of_education),
            sd_edu = sd(years_of_education))

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

ggplot(extra_df %>%
         mutate(group = case_when(str_detect(id, "S1PT+") ~ "patient",
                                  str_detect(id, "S1HC+") ~ "control")) %>%
         mutate(group = as.factor(group)),
       aes(x = group,
           y = age,
           fill = group)) +
  geom_violin(col = NA,
              alpha = .5,
              trim = F) +
  stat_summary(fun.data = data_summary,
               position = position_dodge(0.9),
               size = .5) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme_classic() +
  ggtitle("Age (N=42)") +
  theme(axis.ticks.x = element_blank())

demo.raincloud <-
  ggplot(tidy_extra,
         aes(x = demographic_feature, y = years, fill = group)) +
  stat_halfeye(adjust = 0.5,
               justification = -0.2,
               .width = 0,
               point_colour = NA,
               alpha = .5,
               trim = F,
               scale = .6) +
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5,
               position = position_dodge(0.2)) +
  stat_dots(side = "left",
            justification = 1.15,
            binwidth = .5,
            col=NA) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme_classic() +
  ggtitle("Age (N=42)") +
  theme(axis.ticks.x = element_blank())

ggsave('plots/age_education.png', demo.raincloud)
