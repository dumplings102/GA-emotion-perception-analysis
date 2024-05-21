library('tidyverse')
#import GA data from preprocessing in python
nselected_df <- read.csv('processed_data/nselected.csv')

std.error <- function(x) sd(x)/sqrt(length(x))

nselected_sum <- nselected_df %>%
  group_by(Group, nGeneration) %>%
  summarise(mean = mean(nSelected),
            sd = sd(nSelected),
            se = std.error(nSelected))

nselected_plot <-
  ggplot(nselected_sum,
         aes(x = nGeneration,
             y = mean,
             fill = Group)) +
    geom_bar(stat = 'identity',
             position = position_dodge(.9)) +
    geom_errorbar(aes(y = mean,
                      ymin = mean-se,
                      ymax = mean+se,
                      group = Group),
                  position = position_dodge(.9),
                  width = .5,
                  alpha = .5) +
  scale_x_continuous(#name = "iteration",
                     n.breaks = 8) +
  scale_y_continuous(name = "nSelected",
                     expand = c(0, 0)) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme_minimal() +
  ggtitle("Average number of faces selected per iteration - all emotions",
          subtitle = "HC significantly higher than PT (p<.001)") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = NA),
        plot.background = element_rect(fill = "white"))

ggsave('plots/avg_nselected.png', nselected_plot)

nselected_hist <-
ggplot(nselected_df,
       aes(nSelected)) +
  geom_histogram(aes(fill=Group),
                 position = position_dodge(1),
                 binwidth = 1,
                 col = "white") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_y_continuous(name = 'number of instances',
                     expand = c(0, 0)) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme_minimal() +
  ggtitle("Frequency density of selected faces across all emotions") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = NA),
        plot.background = element_rect(fill = "white"))

ggsave('plots/hist_nselected.png', nselected_hist)
