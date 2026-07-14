rm(list=ls())
l<- load("data/fullthetaplusclusters.RData")
library(tidyverse)
library(khroma)
library(guideR)
dim(fullthetaplusclusters)


library("gplots")
library("devtools")

library(pheatmap)
# bla[bla>0.1]
rownames(fullthetaplusclusters) <- 1:nrow(fullthetaplusclusters)

annot <- tibble(cl = colnames(fullthetaplusclusters)) %>% separate(cl,into =c("c1","c2"), sep = ",") %>% as.data.frame()
rownames(annot) <- colnames(fullthetaplusclusters)

annot_cols = list(
  'c1' = setNames((safe_pal()(12)),
                   nm = unique(unique(annot$c1))),
  'c2' = setNames((safe_pal()(12)),
                  nm = unique(unique(annot$c2)))
)


max_01 <- fullthetaplusclusters
max_01[max_01>0.1] <- 0.1
rownames(max_01) <- 1:nrow(max_01)

test = pheatmap(max_01,clustering_distance_rows = "canberra",
                clustering_distance_cols = "maximum",
                clustering_method = "average",
                annotation=annot,
                annotation_colors = annot_cols,cutree_rows=4, cutree_cols = 6,
                annotation_legend = FALSE)

test$gtable$layout$name
library(gtable)

legend <- gtable::gtable_filter(test$gtable, "legend")
plot.new()
grid::grid.draw(legend)
# saves legend

test = pheatmap(max_01,clustering_distance_rows = "canberra",
                clustering_distance_cols = "maximum",
                clustering_method = "average",
                annotation=annot,
                annotation_colors = annot_cols,cutree_rows=4, cutree_cols = 6,
                legend=FALSE)

test$gtable$layout$name
library(gtable)

legend <- gtable::gtable_filter(test$gtable, "legend")
plot.new()
grid::grid.draw(legend)
# saves annotation legend

pheatmap(max_01,clustering_distance_rows = "canberra",
         clustering_distance_cols = "maximum",
         clustering_method = "average",
         annotation=annot,
         annotation_colors = annot_cols,
         cutree_rows=4,
         cutree_cols = 6,
         filename="atelier/heatmap.pdf",
         width=20,
         height=12,
         fontsize=30,
         fontsize_col=11,
         fontsize_row=20,
         legend=FALSE)
