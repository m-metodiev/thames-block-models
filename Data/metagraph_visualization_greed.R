rm(list=ls())
library(tidygraph)
library(ggraph)
library(igraph)

# results from gephi analysis on the percentage of nodes in each cluster
gephi_analysis = list("5"=.5667,"7"=.1254,"6"=.1067,"9"=.0653,"4"=.0385,"1"=.0066,"11"=.0005)
sum(sapply(gephi_analysis,function(s) s))

### BEGIN G37 greed ###
load(file=paste0('data/res_greed','.Rda'))
map_greed_r37 = res@cl-1
table(map_greed_r37)
length(table(map_greed_r37))
### END G37 greed ###

### BEGIN Calculate mu_hat_mle for greed results ###
res_clustering = list(clustering = c(map_greed_r37)+1)
clustering = res_clustering$clustering
table(clustering)
length(clustering)
mu_hat_mle = matrix(nrow=37,ncol=37)
load(file="data/adjmat_for_gibbs.RData")
for(g in 1:37){
  print(paste0("g: ",g))
  for(k in 1:37){
    print(paste0("k: ",k))
    adjmat_for_gibbs_dummy = matrix(0,ncol=ncol(adjmat_for_gibbs),
                                    nrow=nrow(adjmat_for_gibbs))
    adjmat_for_gibbs_dummy[g==clustering,k==clustering] = 1
    mu_hat_mle[g,k] = sum(adjmat_for_gibbs_dummy*adjmat_for_gibbs)/sum(adjmat_for_gibbs_dummy)
  }
}

write.csv(as.data.frame(mu_hat_mle),file="data/mu_cop28_greed.csv")
mu_hat_mle_greed = as.matrix(read.csv("data/mu_cop28_greed.csv")[,-1])
round(mu_hat_mle_greed*100,2)
### END Calculate mu_hat_mle for greed results ###

### BEGIN VISUALIZATION ###

# res_clustering = list(clustering = c(map_r12 )+1)
# clustering = res_clustering$clustering
# table(clustering)
# length(clustering)
# mu_hat_mle = matrix(nrow=12,ncol=12)
# for(g in 1:12){
#   print(paste0("g: ",g))
#   for(k in 1:12){
#     print(paste0("k: ",k))
#     adjmat_for_gibbs_dummy = matrix(0,ncol=ncol(adjmat_for_gibbs),
#                                     nrow=nrow(adjmat_for_gibbs))
#     adjmat_for_gibbs_dummy[g==clustering,k==clustering] = 1
#     mu_hat_mle[g,k] = sum(adjmat_for_gibbs_dummy*adjmat_for_gibbs)/sum(adjmat_for_gibbs_dummy)
#   }
# }
#
# write.csv(as.data.frame(mu_hat_mle),file="mu_cop28.csv")
# mu_hat_mle = as.matrix(read.csv("mu_cop28.csv")[,-1])
# round(mu_hat_mle*100,2)

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
  from = c(rep("1",37),
           rep("2",37),
           rep("3",37),
           rep("4",37),
           rep("5",37),
           rep("6",37),
           rep("7",37),
           rep("8",37),
           rep("9",37),
           rep("10",37),
           rep("11",37),
           rep("12",37),
           rep("13",37),
           rep("14",37),
           rep("15",37),
           rep("16",37),
           rep("17",37),
           rep("18",37),
           rep("19",37),
           rep("20",37),
           rep("21",37),
           rep("22",37),
           rep("23",37),
           rep("24",37),
           rep("25",37),
           rep("26",37),
           rep("27",37),
           rep("28",37),
           rep("29",37),
           rep("30",37),
           rep("31",37),
           rep("32",37),
           rep("33",37),
           rep("34",37),
           rep("35",37),
           rep("36",37),
           rep("37",37)),
  to   = c(c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1],
           c(" ",1:37)[-1]),
  weight = c(mu_hat_mle_greed[1,],
             mu_hat_mle_greed[2,],
             mu_hat_mle_greed[3,],
             mu_hat_mle_greed[4,],
             mu_hat_mle_greed[5,],
             mu_hat_mle_greed[6,],
             mu_hat_mle_greed[7,],
             mu_hat_mle_greed[8,],
             mu_hat_mle_greed[9,],
             mu_hat_mle_greed[10,],
             mu_hat_mle_greed[11,],
             mu_hat_mle_greed[12,],
             mu_hat_mle_greed[13,],
             mu_hat_mle_greed[14,],
             mu_hat_mle_greed[15,],
             mu_hat_mle_greed[16,],
             mu_hat_mle_greed[17,],
             mu_hat_mle_greed[18,],
             mu_hat_mle_greed[19,],
             mu_hat_mle_greed[20,],
             mu_hat_mle_greed[21,],
             mu_hat_mle_greed[22,],
             mu_hat_mle_greed[23,],
             mu_hat_mle_greed[24,],
             mu_hat_mle_greed[25,],
             mu_hat_mle_greed[26,],
             mu_hat_mle_greed[27,],
             mu_hat_mle_greed[28,],
             mu_hat_mle_greed[29,],
             mu_hat_mle_greed[30,],
             mu_hat_mle_greed[31,],
             mu_hat_mle_greed[32,],
             mu_hat_mle_greed[33,],
             mu_hat_mle_greed[34,],
             mu_hat_mle_greed[35,],
             mu_hat_mle_greed[36,],
             mu_hat_mle_greed[37,])+0.0001
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

#estimation of tau
round(100*table(map_greed_r37)/sum(table(map_greed_r37)),2)

cluster_sizes= table(map_greed_r37)
names(cluster_sizes) = NULL
nodes <- data.frame(
  name = c(" ", 1:37)[-1],
  size = c(cluster_sizes)/100
)

load("data/senderreceivertext_cop28_clustered_greed.RData")


graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
# testmatrix = matrix(1,nrow=37,ncol=37)
# testmatrix[,1] = 2
#theta_colors = c("red","blue","brown","green","black","pink")
# E(graph)$edge_color = c(" ", theta_colors)[-1]
# E(graph)$edge_color[E(graph)$edge_color=="1"] = "rest"
# E(graph)$edge_color[E(graph)$edge_color=="9"] = "lossanddammage"
# E(graph)$edge_color[E(graph)$edge_color=="10"] = "sadhguru"
# E(graph)$edge_color[E(graph)$edge_color=="13"] = "regionalmedia"
# E(graph)$edge_color[E(graph)$edge_color=="18"] = "newsaboutafrica"
# E(graph)$edge_color[E(graph)$edge_color=="20"] = "alternativemedia"



sapply(1:37, function(g) senderreceivertext_cop28_clustered_greed$sender.username[
  senderreceivertext_cop28_clustered_greed$clustersender==g][
    which.max(senderreceivertext_cop28_clustered_greed$user.followers[
      senderreceivertext_cop28_clustered_greed$clustersender==g])])

V(graph)$name <-  c("UN\nClimate\nChange",
                    "We\nDon't\nHave\nTime",
                    "Al\nGore",
                    "Greenpeace",
                    "Extinction\nRebellion\nXR\nGlobal",
                    "Greta\nThunberg",
                    "United\nNations",
                    "Bloomberg",
                    "Reuters",
                    "SDG2030",
                    "Amitav\nGhosh",
                    "Laurie\nGarrett",
                    "Elizabeth\nMay",
                    "Mr.\nClimate",
                    "Gargi\n#SaveSoil",
                    "SouravD. for\n#SaveSoil\n#Conscious\nPlanet",
                    "Isha\nFoundation\nUSA",
                    "Santhanam",
                    "Isha\nFoundation",
                    "ANI",
                    "Narendra\nModi",
                    " ",
                    "Bill", # 1
                    "𝐑𝐞𝐚𝐥\n𝐌𝐀𝐒𝐇",
                    "Office of\nthe Governor\nof California",
                    "The\nNew\nTimes\n(Rwanda)",
                    "Department\nof\nState",
                    "EL\nPAÍS",
                    " ",
                    " ",
                    "Dia\nMirza", #2,3
                    "Hillary\nClinton",
                    "Ellie\nGoulding",
                    "Human\nRights\nWatch",
                    "The\nNew\nYork\nTimes",
                    "NDTV",
                    "CGTN")

# includes the arabic names
# V(graph)$name <-  c("UN\nClimate\nChange",
#                     "We\nDon't\nHave\nTime",
#                     "Al\nGore",
#                     "Greenpeace",
#                     "Extinction\nRebellion\nXR\nGlobal",
#                     "Greta\nThunberg",
#                     "United\nNations",
#                     "Bloomberg",
#                     "Reuters",
#                     "SDG2030",
#                     "Amitav\nGhosh",
#                     "Laurie\nGarrett",
#                     "Elizabeth\nMay",
#                     "Mr.\nClimate",
#                     "Gargi\n#SaveSoil",
#                     "SouravD. for\n#SaveSoil\n#Conscious\nPlanet",
#                     "Isha\nFoundation\nUSA",
#                     "Santhanam",
#                     "Isha\nFoundation",
#                     "ANI",
#                     "Narendra\nModi",
#                     "سكاي نيوز عربية",
#                     "Bill", # 1
#                     "𝐑𝐞𝐚𝐥\n𝐌𝐀𝐒𝐇",
#                     "Office of\nthe Governor\nof California",
#                     "The\nNew\nTimes\n(Rwanda)",
#                     "Department\nof\nState",
#                     "EL\nPAÍS",
#                     "إبراهيم بهزاد",
#                     "عبدالله بن زايد",
#                     "Dia\nMirza", #2,3
#                     "Hillary\nClinton",
#                     "Ellie\nGoulding",
#                     "Human\nRights\nWatch",
#                     "The\nNew\nYork\nTimes",
#                     "NDTV",
#                     "CGTN")

# old names
# V(graph)$name <- c("CGTN",
#                    "The\nNew\nYork\nTimes",
#                    "Ellie\nGoulding",
#                    "Hillary\nClinton",
#                    "Human\nRights\nWatch",
#                    "BBC\nNews\n(World)",
#                    "Amnesty\nInternational",
#                    "Narendra\nModi",
#                    " ",#"سكاي نيوز عربية",
#                    "EL\nPAÍS",
#                    "Rebel\nNews",
#                    "Maria\nSarungi\nTsehai",
#                    "ipbes",
#                    "Global\nCitizen",
#                    "Laurie\nGarrett",
#                    "Elizabeth\nMay",
#                   "Parents\nFor Future\n#EndFossil\nFuels",
#                    "Mr.\nClimate",
#                  "UN\nClimate\nChange",
#                    "Al\nGore",
#                    "Mike\nHudema",
#                    "Greenpeace",
#                    "Climate\nReality",
#                    "Greta\nThunberg",
#                    "United\nNations",
#                    "We\nDon't\nHave\nTime",
#                    "Reuters",
#                    "Sadhguru",
#                   "Gargi\n#SaveSoil",
#                   "SouravD. for\n#SaveSoil\n#Conscious\nPlanet",
#                    "Isha\nFoundation\nUSA",
#                    "Dubai\nOne",
#                    "Santhanam",
#                    "Office of\nthe Governor\nof California",
#                     "The\nNew\nTimes\n(Rwanda)",
#                    "Vatican News",
#                    "Gerald\nNgao")

my_theme <- theme_bw() +
  theme(strip.background = element_rect(fill = "white"), text = element_text(face="bold", size=12),
  )
theme_set(my_theme)

set.seed(2026)
V(graph)$group = rep("x",37)
V(graph)$group[c(1,3)]="y"
V(graph)$group[c(15,16,17,19)]="z"
plot_cop28_greedsolution = ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.7,
                 arrow = arrow(type = "closed", length = unit(8, "mm"))
  ) +
  geom_node_point(aes(size = size,color=group), fill = "white",
                  shape = 21,
                  stroke = 1.2) +
  scale_edge_width(range = c(0.3, 3),name = "probability") +
  scale_size(range = c(14, 32)) +
  geom_node_text(aes(label = name),size=1.6) +
  theme(legend.position = "none") +
  scale_color_manual(values = c(
    x = "black",
    y = "#153dff",
    z = "#13ffd3"
  ))
plot_cop28_greedsolution

ggsave("atelier/plot_cop28_greedsolution.png",
       plot_cop28_greedsolution,width=9,height=9,device="png",dpi = 200)

# mu=mu_hat_mle
#
# df <- reshape2::melt(t(mu))
# K=12
# colnames(df) <- c("x", "y", "probability")
# df$x = factor(df$x, levels = 1:K)
# df$y = factor(df$y, levels = K:1)
#
# ggraph::ggraph()
# p1 = ggplot(df, aes(x = x, y = y, fill = probability)) + geom_tile() + scale_x_discrete(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) + scale_fill_gradient(low = "white", high = "red", limits=c(0, 1)
#   ) + geom_vline(xintercept = (0:(K-1))+0.5, color = "black", linewidth = 1
#   )+  geom_hline(yintercept = (0:(K-1))+0.5, color = "black", linewidth = 1
#   )+ theme(
#     axis.text.x = element_text(size = 10),
#     axis.text.y = element_text(size = 10),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     legend.position = "none",
#     plot.title = element_text(size=11),
#     aspect.ratio = 1
#   )+ labs(title = "connectivity matrix")
# p1

to_latex_matrix <- function(mat, env = "bmatrix") {
  rows <- apply(mat, 1, function(x) paste(x, collapse = " & "))
  body <- paste(rows, collapse = " \\ ")

  for(i in rows){
    cat(paste0(i,"\\\\\n"))
  }
}
to_latex_matrix(round(100*mu_hat_mle_greed[,1:13],2))
to_latex_matrix(round(100*mu_hat_mle_greed[,14:26],2))
to_latex_matrix(round(100*mu_hat_mle_greed[,27:37],2))
