install.packages("cluster")
install.packages("dendextend")
library(cluster)
library(dendextend)
df <- read.csv("https://raw.githubusercontent.com/agricolamz/2018-MAG_R_course/master/data/baltic.csv")
df

dist_matrix <- dist(t(df[, 3:6]))

hc <- hclust(dist_matrix)
plot(hc)

diana_result <- diana(dist_matrix)
plot(diana_result)

hc_ward <- hclust(dist_matrix, method = "ward.D2")
plot(hc_ward)

dend_hc <- as.dendrogram(hc)

dend_colored <- color_branches(dend_hc, k = 2)
dend_colored <- color_labels(dend_colored, k = 2)

plot(dend_colored, main = "hclust")

dend_diana <- as.dendrogram(diana_result)

clusters <- cutree(as.hclust(diana_result), k = 2)

dend_colored <- color_branches(dend_diana, k = 2)
dend_colored <- color_labels(dend_colored, k = 2)

plot(dend_colored, main = "DIANA")