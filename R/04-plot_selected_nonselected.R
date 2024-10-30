library(tidyverse)

dist_df <- read.csv('processed_data/selected_nonselected_distances.csv') %>%
  slice(-1) %>%
  pivot_longer(cols = matches('cosdist'),
               names_to = 'measure',
               values_to = 'score') %>%
  mutate(score = as.numeric(score)) %>%
  mutate(type = case_when(str_detect(measure, '.1')~'count',
                          TRUE~'mean'))

df_sum <- dist_df %>%
  filter(type=='mean') %>%
  dplyr::select(-type) %>%
  mutate(group = str_extract(ID, '[^S1]{2}')) %>%
  mutate_at(c('measure', 'group'), factor) %>%
  group_by(nGeneration, measure, group) %>%
  summarise(mean = mean(score, na.rm=T),
            sd = sd(score, na.rm=T),
            n = n(),
            se = sd/sqrt(n),
            ci = 1.96*se) %>%
  ungroup()

str(df_sum)

elite_bs_plot <-
  ggplot(df_sum %>%
           filter(grepl("elite_cosdist_bs", measure)),
         aes(x = nGeneration,
             y = mean,
             col = group,
             shape = measure,
             fill = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymax = mean + ci,
                    ymin = mean - ci),
                width = .2,
                alpha = .5) +
  geom_line() +
  geom_ribbon(aes(ymax = mean + ci,
                  ymin = mean - ci),
              col = NA,
              alpha = .1) +
  scale_color_manual(values=c("#999999", "#E69F00")) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  scale_shape(labels=c('Non-selected', 'Selected')) +
  ylab("Cosine distance") +
  xlab("Trial number") +
  guides(col='none',
         fill=guide_legend(override.aes = list(alpha = 1)),
         shape=guide_legend(reverse=T)) +
  theme_classic() +
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
  labs(title = "Distance from elite of selected and non-selected faces in blendshape space",
       fill = "",
       shape = "")

elite_pca_plot <-
  ggplot(df_sum %>%
           filter(!grepl("bs", measure)) %>%
           filter(grepl("elite", measure)),
         aes(x = nGeneration,
             y = mean,
             col = group,
             shape = measure,
             fill = group)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymax = mean + ci,
                      ymin = mean - ci),
                  width = .2,
                  alpha = .5) +
    geom_line() +
    geom_ribbon(aes(ymax = mean + ci,
                    ymin = mean - ci),
                col = NA,
                alpha = .1) +
    scale_color_manual(values=c("#999999", "#E69F00")) +
    scale_fill_manual(values=c("#999999", "#E69F00")) +
    scale_shape(labels=c('Non-selected', 'Selected')) +
    ylab("Cosine distance") +
    xlab("Trial number") +
    guides(col='none',
           fill=guide_legend(override.aes = list(alpha = 1)),
           shape=guide_legend(reverse=T)) +
    theme_classic() +
    theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
    labs(title = "Distance from elite of selected and non-selected faces in PCA space",
         fill = "",
         shape = "")

cen_pca_plot <-
  ggplot(df_sum %>%
           filter(!grepl("bs", measure)) %>%
           filter(grepl("cen", measure)),
         aes(x = nGeneration,
             y = mean,
             col = group,
             shape = measure,
             fill = group)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymax = mean + ci,
                      ymin = mean - ci),
                  width = .2,
                  alpha = .5) +
    geom_line() +
    geom_ribbon(aes(ymax = mean + ci,
                    ymin = mean - ci),
                col = NA,
                alpha = .1) +
    scale_color_manual(values=c("#999999", "#E69F00")) +
    scale_fill_manual(values=c("#999999", "#E69F00")) +
    scale_shape(labels=c('Non-selected', 'Selected')) +
    ylab("Cosine distance") +
    xlab("Trial number") +
    guides(col='none',
           fill=guide_legend(override.aes = list(alpha = 1)),
           shape=guide_legend(reverse=T)) +
    theme_classic() +
    theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
    labs(title = "Distance from centroid of selected and non-selected faces in PCA space",
         fill = "",
         shape = "")

cen_bs_plot <-
  ggplot(df_sum %>%
           filter(grepl("cen_cosdist_bs", measure)),
         aes(x = nGeneration,
             y = mean,
             col = group,
             shape = measure,
             fill = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymax = mean + ci,
                    ymin = mean - ci),
                width = .2,
                alpha = .5) +
  geom_line() +
  geom_ribbon(aes(ymax = mean + ci,
                  ymin = mean - ci),
              col = NA,
              alpha = .1) +
  scale_color_manual(values=c("#999999", "#E69F00")) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  scale_shape(labels=c('Non-selected', 'Selected')) +
  ylab("Cosine distance") +
  xlab("Trial number") +
  guides(col='none',
         fill=guide_legend(override.aes = list(alpha = 1)),
         shape=guide_legend(reverse=T)) +
  theme_classic() +
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
  labs(title = "Distance from centroid of selected and non-selected faces in blendshape space",
       fill = "",
       shape = "")

ggsave('plots/elite_selected_nonselected_bs.png',
       elite_bs_plot, width=8, height=5, dpi=300)
ggsave('plots/elite_selected_nonselected_pca.png',
       elite_pca_plot, width=8, height=5, dpi=300)

ggsave('plots/cen_selected_nonselected_bs.png',
       cen_bs_plot, width=8, height=5, dpi=300)
ggsave('plots/cen_selected_nonselected_pca.png',
       cen_pca_plot, width=8, height=5, dpi=300)

