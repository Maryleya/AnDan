icelandic <- read_csv("https://raw.githubusercontent.com/LingData2019/LingData2020/master/data/icelandic.csv")

icelandic |>
  ggplot(aes(x = vowel.dur, y = roundness, fill = roundness)) +
  geom_boxplot(notch = TRUE) +
  stat_summary(fun=mean, geom="point", color="red", fill="red") +
  theme_classic()

round <- icelandic %>%
  filter(roundness == "round")

unrounded <- icelandic %>%
  filter(roundness == "unrounded")

t.test(round$vowel.dur, unrounded$vowel.dur, paired=FALSE)

wilcox.test(round$vowel.dur, unrounded$vowel.dur, paired = FALSE)
