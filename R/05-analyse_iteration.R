library('tidyverse')
#import GA data from preprocessing in python

nselected_df <- read.csv('processed_data/nselected.csv') %>%
  mutate(across(where(is.character), as.factor),
         nSelected = as.numeric(nSelected),
         nGeneration = as.factor(nGeneration))
str(nselected_df)


aov.twoway <- aov(nSelected ~ Group+nGeneration, nselected_df) #two-way

summary(aov.twoway)

TukeyHSD(aov.twoway)
