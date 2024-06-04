library('tidyverse')
library('lme4')
library('lmerTest')
library('emmeans')
library('MuMIn')
library('AICcmodavg')
library('ggpubr')
library('effectsize')
library(ggdist)
#import GA data from preprocessing in python

nselected_df <- read.csv('processed_data/nselected.csv') %>%
  mutate(across(where(is.character), as.factor),
         nSelected = as.numeric(nSelected),
         nGeneration = as.factor(nGeneration))
str(nselected_df)

#two-way anova of clinical group and iteration on number of selected faces
aov.twoway <- aov(nSelected ~ Group+nGeneration+Group*nGeneration, nselected_df)

plot(aov.twoway) #check assumptions

summary(aov.twoway) #check significance

TukeyHSD(aov.twoway) #post-hoc

eta_squared(aov.twoway) #effect size, 0.02 - small effect

#linear mixed effects model for effect of clinical group on number of selected faces
nsel.lmm1 <- lmer(nSelected ~ Group+nGeneration+TargetEmotion+(1|ID), nselected_df)
nsel.lmm <- lmer(nSelected ~ nGeneration+TargetEmotion+(1|ID), nselected_df)
nsel.lmm2 <- lmer(nSelected ~ nGeneration+(1|ID), nselected_df)
r.squaredGLMM(nsel.lmm1, MuMIn.noUpdateWarning = T)

plot(nsel.lmm1)
qqnorm(resid(nsel.lmm1))
qqline(resid(nsel.lmm1))
hist(resid(nsel.lmm1))

summary(nsel.lmm1)
anova(nsel.lmm1, nsel.lmm, nsel.lmm2)

nsel_r.int <- as.data.frame(ranef(nsel.lmm)) %>%
  mutate(group = case_when(str_detect(grp, "S1PT+") ~ "patient",
                           str_detect(grp, "S1HC+") ~ "control")) %>%
  mutate(group=as.factor(group))

#plot intervept values between groups
plot(nsel_r.int$condval, col=nsel_r.int$group)
ggplot(nsel_r.int,
       aes(x = group, y = condval, fill=group)) +
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
            binwidth = .1,
            col=NA) +
  scale_fill_manual(values=c("#999999", "#E69F00"), guide='none') +
  theme_classic() +
  ggtitle("Intercepts") +
  theme(axis.ticks.x = element_blank())


