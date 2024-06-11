library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)
library(reshape2)
data <- read.csv(file = 'C:/Users/nicol/Documents/Work/Marie Curie Project/SMTI/Stats/pseLog.csv')

# rename cols
colnames(data) <- c("PFD", "PND", "RFD", "RFT")

# add sub column
nSub<-15
data$sub<-c(1:nSub)

# rearrange neat
data <- melt(data, id.vars = "sub", variable.name = "condition", value.name = "val")

# Per essere sicuro che tutti sia categoriale  (dataframe> sub, cond, pse, jnd)
data$condition <- as.factor(data$condition)  # se continuo > as.numeric


model <- lmer(val ~ condition + (1 | sub), data)   # model <- lmer(pse ~ cond * eeg + (1 | sub), data)


# Check residuals (how well model managed to fit the data)
plot(model)
qqnorm(resid(model))
qqline(resid(model))
hist(resid(model))

# a summary of the effects
summary(model)
# how much variance it is capturing
r.squaredGLMM(model)

# Then we run an ANOVA on top
anova(model)

# post hoc comparisons
emmeans(model, specs = pairwise ~ condition)

