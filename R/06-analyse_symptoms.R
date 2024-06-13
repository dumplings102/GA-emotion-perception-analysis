library("ggpubr")

##correlate symptom severity and random intercepts##
int_df <- nsel_r.int %>%
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
          select(cdist_int)) %>%
  mutate(across(where(is.integer), as.numeric))

cor.test(int_df$panssn_total, int_df$cdist_int, method='pearson')
cor.test(int_df$panssn_total, int_df$nsel_int, method='pearson')

#plot
h <- ggscatter(int_df, x = "panss_total", y = "cdist_int",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "symptom severity", ylab = "distance from centroid")
ggsave('plots/correlate_distance.png',
       h, width=5, height=4, dpi=300)

j <- ggscatter(int_df, x = "panss_total", y = "nsel_int",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "symptom severity", ylab = "number of selected faces")
ggsave('plots/correlate_nselect.png',
       j, width=5, height=4, dpi=300)

