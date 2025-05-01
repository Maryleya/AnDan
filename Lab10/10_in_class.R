install.packages('ggfortify')
install.packages('FactoMineR')
install.packages('ca')
library(tidyverse)
library(ggfortify)
library(FactoMineR)
library(ca)
library(vcd)
theme_set(theme_bw())

reg_bnc <- read.csv("https://goo.gl/19QywL")
pca <- prcomp(reg_bnc[,-1], center = TRUE, scale. = TRUE)
summary(pca)

pca$rotation

(pca$sdev)^2

autoplot(pca,
         shape = FALSE,
         loadings = TRUE,
         label = TRUE,
         loadings.label = TRUE)+
  theme_bw()

reg_bnc <- cbind(reg_bnc, pca$x)
reg_bnc %>% 
  ggplot(aes(PC1, PC2, color = Reg))+
  geom_point()+
  stat_ellipse()+
  theme_bw()

