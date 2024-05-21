library(tidyverse)

dist_df <- read.csv('processed_data/selected_nonselected_distances.csv') %>%
  slice(-1) %>%
  select(!ends_with('.1')) %>%
  mutate(across(ends_with('cendist'), as.numeric)) %>%
  na.omit()

df_tidy <- dist_df %>%
  pivot_longer(cols = ends_with('cendist'),
               names_to = 'measure',
               values_to = 'score')

df_tidy <- df_tidy %>%
  mutate(abs_score = abs(1-(score)))

df_sum <- df_tidy %>%
  mutate(group = str_extract(ID, '[^S1]{2}')) %>%
  group_by(nGeneration, measure, group) %>%
  summarise(mean = mean(score),
            sd = sd(score),
            n = n(),
            se = sd/sqrt(n))

#iter_plot <-
  ggplot(df_sum %>%
           filter(grepl("cos", measure)),
         aes(x = nGeneration,
             y = mean,
             col = group,
             shape = measure)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymax = mean + se,
                  ymin = mean - se,
                  fill = group),
              col = NA,
              alpha = .1) +
  scale_color_manual(values=c("#999999", "#E69F00")) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  scale_y_continuous(name = "Cosine Distance") +
  scale_shape(labels = c('nonselected', 'selected')) +
  guides(fill = 'none') +
  theme_classic() +
  labs(title = "Distance from centroid of selected and nonselected faces",
       colour = "",
       shape = "")

ggsave('plots/selected_nonselected.png',
       iter_plot)
