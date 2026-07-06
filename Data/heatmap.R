l<- load("/home/mperrot/Téléchargements/fullthetaplusclusters.RData")
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

pheatmap(fullthetaplusclusters,clustering_distance_rows = "canberra",
         clustering_distance_cols = "maximum",
         clustering_method = "average",
         annotation=annot,
         annotation_colors = annot_cols,cutree_rows=4, cutree_cols = 6)

max_01 <- fullthetaplusclusters
max_01[max_01>0.1] <- 0.1
rownames(max_01) <- 1:nrow(max_01)

pheatmap(max_01,clustering_distance_rows = "canberra",
         clustering_distance_cols = "maximum",
         clustering_method = "average",
         annotation=annot,
         annotation_colors = annot_cols,cutree_rows=4, cutree_cols = 6)
