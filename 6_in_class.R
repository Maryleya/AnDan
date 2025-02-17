#Был проведен однофакторный дисперсионный анализ с повторными измерениями 
#(one-way RM-ANOVA), который показал, что статистически значимые различия 
#по переменной/фактору времени (p-value=2.03e-11,F=124.6,df=1), однако 
#величина эффекта умеренная (ges=0.099).

install.packages("rstatix")
library(rstatix)
install.packages("ggpubr")
library(ggpubr)

bilingual <- read_tsv("http://coltekin.net/cagri/R/data/bilingual.txt") %>% 
  mutate_if(is.character, factor) %>%
  mutate(age = factor(age, levels = c("preschool","firstgrade","secondgrade")),
         subj = factor(subj)) %>%
  filter(language == "home.only") %>%
  droplevels()

str(bilingual)

skimr::skim(bilingual)

bilingual %>%
  group_by(age) %>%
  rstatix::get_summary_stats(mlu, type = "mean_sd")

ggpubr::ggboxplot(bilingual, x = "age", y = "mlu", add = "point")

bilingual %>%
  group_by(age) %>%
  rstatix::identify_outliers(mlu)

# normality test
bilingual %>%
  group_by(age) %>%
  shapiro_test(mlu)

ggpubr::ggqqplot(bilingual, "mlu", facet.by = "age")

#wid and within - повторные измерения
res.aov <- anova_test(data = bilingual, dv = mlu, wid = subj, within = age)
get_anova_table(res.aov)

res.aov <- anova_test(data = bilingual, dv = mlu, detailed = TRUE, wid = subj, within = age)
get_anova_table(res.aov)

pwc <- bilingual %>%
  pairwise_t_test(
    mlu ~ age, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

pwc <- pwc %>% add_xy_position(x = "age")
ggboxplot(bilingual, x = "age", y = "mlu", add = "point") + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )