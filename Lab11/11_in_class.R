install.packages('ggfortify')
install.packages('FactoMineR')
install.packages('ca')
install.packages("factoextra")
library(factoextra)
library(tidyverse)
library(ggfortify)
library(FactoMineR)
library(ca)
library(vcd)
theme_set(theme_bw())

poetry <- read.csv("poetry_last_in_lines.csv",sep="\t")
poetry

concat_poetry <- table(poetry$Decade, poetry$UPoS)
concat_poetry

df_table <- as.data.frame(table(poetry$Decade, poetry$UPoS))
colnames(df_table) <- c("Decade", "UPoS", "Freq")

ggplot(df_table, aes(x = as.factor(Decade), y = Freq, color = UPoS)) +
  geom_point(size = 3, position = position_jitter(width = 0.2, height = 0)) +
  scale_x_discrete(breaks = unique(df_table$Decade)) +
  scale_y_continuous(breaks = seq(0, max(df_table$Freq), by = 10)) +
  theme_minimal()

ca_result <- CA(concat_poetry, graph = FALSE)
ca_result$eig

fviz_ca_biplot(ca_result, 
               repel = TRUE,
               col.row = "blue",
               col.col = "red"
)
