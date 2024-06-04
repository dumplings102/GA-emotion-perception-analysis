##plot symptom severity and random intercepts##
intercepts_df <- nsel_r.int %>%
  filter(group=="patient") %>%
  mutate(id = grp,
         nsel_int = condval) %>%
  select(id, nsel_int) %>%
  merge(extra_df %>%
          filter(grepl('PT', id)),
        by='id',
        all.x=T) %>%
  cbind(cdist_r.int %>%
          filter(group=="patient") %>%
          mutate(cdist_int = condval) %>%
          select(cdist_int))

ggplot(intercepts_df,
       (aes(x=panssn_total, y=cdist_int))) +
  geom_point() +
  geom_smooth(method=lm) +
  ggtitle('r=-0.19') +
  xlab('symptom severity') +
  ylab('nSelected intercepts') +
  theme_classic()

intercepts_df

symptom_severity <- intercepts_df %>%
  select(panssn_total) %>%
  unlist()

emotion_percept <- intercepts_df %>%
  select(cdist_int) %>%
  unlist()

correlation <- cor(symptom_severity, emotion_percept)
correlation

regression_model <- lm(emotion_percept ~ symptom_severity)
summary(regression_model)

