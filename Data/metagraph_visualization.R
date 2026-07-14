rm(list=ls())
library(tidygraph)
library(ggraph)
library(igraph)

# results from gephi analysis on the percentage of nodes in each cluster
gephi_analysis = list("5"=.5667,"7"=.1254,"6"=.1067,"9"=.0653,"4"=.0385,"1"=.0066,"11"=.0005)
sum(sapply(gephi_analysis,function(s) s))

### BEGIN G12 ###
load(paste0('data/res_gibs_full_lps','.Rda'))
load(file=paste0('data/res_gibbs_',ceiling(which.max(res_gibs_full_lps[,12-1]) / 10000),'.Rda'))
load(file="data/adjmat_for_gibbs.RData")

names(res_gibbs_list)=paste0("r",2:20)

res_gibbs_list_r12 = res_gibbs_list$r12
res_gibbs_list=NULL
map_r12 = res_gibbs_list_r12$clusterList[[which.max(res_gibbs_list_r12$lps)]]
table(map_r12)
length(table(map_r12))
### END G12 ###

### BEGIN VISUALIZATION ###

res_clustering = list(clustering = c(map_r12)+1)
clustering = res_clustering$clustering
table(clustering)
length(clustering)
mu_hat_mle = matrix(nrow=12,ncol=12)
for(g in 1:12){
  print(paste0("g: ",g))
  for(k in 1:12){
    print(paste0("k: ",k))
    adjmat_for_gibbs_dummy = matrix(0,ncol=ncol(adjmat_for_gibbs),
                                    nrow=nrow(adjmat_for_gibbs))
    adjmat_for_gibbs_dummy[g==clustering,k==clustering] = 1
    mu_hat_mle[g,k] = sum(adjmat_for_gibbs_dummy*adjmat_for_gibbs)/sum(adjmat_for_gibbs_dummy)
  }
}

write.csv(as.data.frame(mu_hat_mle),file="data/mu_cop28.csv")
mu_hat_mle = as.matrix(read.csv("data/mu_cop28.csv")[,-1])
round(mu_hat_mle*100,2)
# #truncate everything below 0.01 for better visibility
# mu_hat_mle_trunc = mu_hat_mle * (mu_hat_mle > 0.01)
# g <- graph_from_adjacency_matrix(mu_hat_mle, mode = "directed",weighted=TRUE)
# V(g)$label <- 1:12
# layout <- norm_coords(layout, ymin=-1, ymax=1, xmin=-1, xmax=1) * 2
# E(g)$color <- ifelse(E(g)$weight > .5, "red", "gray70")
# plot(
#   g,
#   layout = layout,   # force-directed layout
#   vertex.color = "white",
#   vertex.label.color = "black",
#   edge.width = E(g)$weight*5,  # scale down weights
#   edge.color = "gray50",
#   edge.arrow.size = 0.3,
#   vertex.size=40,
#   vertex.label.cex = 0.5
# )
edges <- data.frame(
  from = c(rep("1",12),
           rep("2",12),
           rep("3",12),
           rep("4",12),
           rep("5",12),
           rep("6",12),
           rep("7",12),
           rep("8",12),
           rep("9",12),
           rep("10",12),
           rep("11",12),
           rep("12",12)),
  to   = c(c(" ",1:12)[-1],
           c(" ",1:12)[-1],
           c(" ",1:12)[-1],
           c(" ",1:12)[-1],
           c(" ",1:12)[-1],
           c(" ",1:12)[-1],
           c(" ",1:12)[-1],
           c(" ",1:12)[-1],
           c(" ",1:12)[-1],
           c(" ",1:12)[-1],
           c(" ",1:12)[-1],
           c(" ",1:12)[-1]),
  weight = c(mu_hat_mle[1,],
             mu_hat_mle[2,],
             mu_hat_mle[3,],
             mu_hat_mle[4,],
             mu_hat_mle[5,],
             mu_hat_mle[6,],
             mu_hat_mle[7,],
             mu_hat_mle[8,],
             mu_hat_mle[9,],
             mu_hat_mle[10,],
             mu_hat_mle[11,],
             mu_hat_mle[12,])+0.0001
)


### START PIERRE'S CODE ###
# K=12
# df <- reshape2::melt(t(mu_hat_mle))
# colnames(mu_hat_mle)=NULL
# colnames(df) <- c("x", "y", "probability")
# df$x = factor(df$x, levels = 1:K)
# df$y = factor(df$y, levels = K:1)
# p1 = ggplot(df, aes(x = x, y = y, fill = probability)) + geom_tile() + scale_x_discrete(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) + scale_fill_gradient(low = "white", high = "red", limits=c(0,.01)) + geom_vline(xintercept = (0:(K-1))+0.5, color = "black", linewidth = 1
#   )+  geom_hline(yintercept = (0:(K-1))+0.5, color = "black", linewidth = 1
#   )+ theme(
#     axis.text.x = element_text(size = 10),
#     axis.text.y = element_text(size = 10),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     legend.position = "none",
#     plot.title = element_text(size=11),
#     aspect.ratio = 1
#   )+ labs(title = "Connectivity matrix mu")
# p1
### END PIERRE'S CODE ###

load("data/cluster_sizes_cop28.RData")
cluster_sizes= cluster_sizes_cop28
names(cluster_sizes) = NULL
nodes <- data.frame(
  name = c("1", "2", "3", "4", "5", "6","7","8","9","10","11","12"),
  size = c(cluster_sizes)/100
)

load("data/senderreceivertext_cop28_clustered.RData")


graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
testmatrix = matrix(1,nrow=12,ncol=12)
testmatrix[,1] = 2
theta_colors = c("red","blue","brown","green","black","pink")
# E(graph)$edge_color = c(" ", theta_colors)[-1]
# E(graph)$edge_color[E(graph)$edge_color=="1"] = "rest"
# E(graph)$edge_color[E(graph)$edge_color=="9"] = "lossanddammage"
# E(graph)$edge_color[E(graph)$edge_color=="10"] = "sadhguru"
# E(graph)$edge_color[E(graph)$edge_color=="13"] = "regionalmedia"
# E(graph)$edge_color[E(graph)$edge_color=="18"] = "newsaboutafrica"
# E(graph)$edge_color[E(graph)$edge_color=="20"] = "alternativemedia"



sapply(1:12, function(g) senderreceivertext_cop28_clustered$sender.username[
  senderreceivertext_cop28_clustered$clustersender==g][
    which.max(senderreceivertext_cop28_clustered$user.followers[
      senderreceivertext_cop28_clustered$clustersender==g])])

library(stringi)
stri_reverse(stri_enc_toutf8("سكاي نيوز عربية"))
V(graph)$name <- c("Honnashree\n#SaveSoil",
                   "BBC\nNews\n(World)",
                   "United\nNations",
                   "Santhanam",
                   "Hillary\nClinton",
                   "EL\nPAÍS",
                   "The\nNew\nYork\nTimes",
                   "Reuters",
                   " ",
                   "Amitav\nGhosh",
                   "Al\nGore",
                   "Narendra\nModi")

# colors from gephi
colors=c("#ff385d",
         "#00ba14",
         "#6900a7",
         "#f810ff",
         "#00fcf8",
         "#153dff",
         "#ffff05",
         "black",
         "#ffb242",
         "#0bff0e",
         "#cd7600",
         "#13ffd3")

set.seed(2026)
V(graph)$group = rep("x",12)
V(graph)$group[11]="y"
V(graph)$group[1]="z"
my_theme <- theme_bw() +
  theme(strip.background = element_rect(fill = "white"), text = element_text(face="bold", size=12),
  )
theme_set(my_theme)
plot_cop28_thamessolution = ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.7,
                 arrow = arrow(type = "closed", length = unit(5, "mm")),
                 end_cap = circle(3, "mm")
  ) +
  geom_node_point(aes(size = size,color=group),fill="white",
                  shape = 21,
                  stroke = 1.2) +
  scale_edge_width(range = c(0.3, 3),name = "probability") +
  scale_size(range = c(14, 32)) +
  geom_node_text(aes(label = name),size=1.9) +
  theme(legend.position = "none") +
  scale_color_manual(values = c(
    x = "black",
    y = "#153dff",
    z = "#13ffd3"
  ))
plot_cop28_thamessolution

ggsave("atelier/plot_cop28_thamessolution.png",
       plot_cop28_thamessolution,width=6,height=6,device="png",dpi = 600)
