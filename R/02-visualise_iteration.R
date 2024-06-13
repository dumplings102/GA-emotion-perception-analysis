library('tidyverse')
#import GA data from preprocessing in python
nselected_df <- read.csv('processed_data/nselected.csv')

nselected_sum <- nselected_df %>%
  group_by(Group, nGeneration) %>%
  summarise(mean = mean(nSelected),
            sd = sd(nSelected),
            n = n(),
            se = sd/sqrt(n),
            ci = 1.96*se)

nselected_plot <-
  ggplot(nselected_sum,
         aes(x = nGeneration,
             y = mean,
             fill = Group)) +
    geom_bar(stat = 'identity',
             position = position_dodge(.9)) +
    geom_errorbar(aes(y = mean,
                      ymin = mean-ci,
                      ymax = mean+ci,
                      group = Group),
                  position = position_dodge(.9),
                  width = .2,
                  alpha = .5) +
  scale_x_continuous(name = "Trial",
                     n.breaks = 8) +
  scale_y_continuous(name = "Number of selected faces",
                     expand = c(0, 0)) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme_minimal() +
  ggtitle("Average number of faces selected per trial",
          subtitle = "All emotions") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = NA),
        plot.background = element_rect(fill = "white"))

ggsave('plots/avg_nselected.png', nselected_plot, width=5, height=4,dpi=300)

nselected_hist <-
ggplot(nselected_df,
       aes(nSelected)) +
  geom_histogram(aes(fill=Group),
                 position = position_dodge(1),
                 binwidth = 1,
                 col = "white") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10),
                     name='Number of selections') +
  scale_y_continuous(name = 'Number of instances',
                     expand = c(0, 0)) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme_minimal() +
  ggtitle("Frequency density of selected faces",
          subtitle = "All emotions") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = NA),
        plot.background = element_rect(fill = "white"))

ggsave('plots/hist_nselected.png', nselected_hist, width=5, height=4, dpi=300)
