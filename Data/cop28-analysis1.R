rm(list=ls())
library(data.table)
library(Matrix)
library(igraph)
library(dplyr)

X2022_COP28_twitter_Jan_Dec_2023_v2_0 <- fread("2022-COP28_twitter_Jan_Dec_2023_v2.0.csv")

# TO READ: 
# uid := idStr => Tweet primary key
# user.idStr => points to the user who made / retweeted the tweet
# retweet.user.idStr => points to the user who made the tweet which is being retweeted
# retweet.idStr => points to the tweet which is being retweeted
# quote.user.idStr => points to the user who made the tweet which is being quoted
# quote.idStr =>  points to the tweet which is being quoted
# mentioned_ids => (to be checked) lists of user ids being mentioned separated by | 

#time : from 2023-01-01 01:24:27 UTC to 2023-12-31 23:23:14 UTC

str(X2022_COP28_twitter_Jan_Dec_2023_v2_0)


dim(X2022_COP28_twitter_Jan_Dec_2023_v2_0)

colnames(X2022_COP28_twitter_Jan_Dec_2023_v2_0)

colnames(X2022_COP28_twitter_Jan_Dec_2023_v2_0)[c(42, 73)]
colnames(X2022_COP28_twitter_Jan_Dec_2023_v2_0)[c(42, 74)]


DATA = X2022_COP28_twitter_Jan_Dec_2023_v2_0


EDGES_RT = DATA[, c(42, 73)]
colnames(DATA)[c(42, 73)]
colnames(EDGES_RT) = c("sender", "receiver")

EDGES_QU = DATA[, c(42, 74)]
colnames(DATA)[c(42, 74)]
colnames(EDGES_QU) = c("sender", "receiver")

EDGES = rbind(EDGES_RT, EDGES_QU)
dim(EDGES)

VERTICES1 = DATA[, c(42, 35)]
colnames(DATA)[c(42, 35)]
colnames(VERTICES1) = c("user_id", "username")
VERTICES1$user_id = as.character(VERTICES1$user_id)
VERTICES1 = distinct(VERTICES1, user_id, keep_all=TRUE)



# check .. 
dim(unique(VERTICES1))
length(unique(VERTICES1$user_id))


 
VERTICES2 = DATA[, c(73, 67)]
colnames(DATA)[c(73, 67)]
colnames(VERTICES2) = c("user_id", "username")
VERTICES2$user_id = as.character(VERTICES2$user_id)
VERTICES2 = distinct(VERTICES2, user_id, keep_all=TRUE)

VERTICES3 = DATA[, c(74, 54)]
colnames(DATA)[c(74, 54)]
colnames(VERTICES3) = c("user_id", "username")
VERTICES3$user_id = as.character(VERTICES3$user_id)
VERTICES3 = distinct(VERTICES3, user_id, keep_all=TRUE)

VERTICES = unique(rbind(VERTICES1, VERTICES2, VERTICES3))
colnames(VERTICES)[2] = "username"

dim(VERTICES)
str(VERTICES)

g = graph_from_data_frame(EDGES, directed=TRUE, vertices=VERTICES)

summary(g)

#vertex_attr(g, "name")
# X = as_adjacency_matrix(g)
# sum(X)
# 
# 
# 
# 
# # check
# USERS = c(DATA$user.idStr, DATA$retweet.user.idStr)
# length(unique(USERS))
# 
# X = as_adjacency_matrix(g)
# min(X)
# max(X)
# 
# head(X)

g2 = simplify(g)

###
# plots .. 
d = igraph::degree(g2, mode="in") 
g2 = delete_vertices(g2, which.max(d)) 
###

summary(g2)
X2 = as_adjacency_matrix(g2)
min(X2)
max(X2)
sum(X2)

# # find the largest connected component
larg = which(igraph::components(g2)$csize==max(igraph::components(g2)$csize))
if(length(larg)>0) {
  larg = larg[1]
}
id = which(igraph::components(g2)$membership!=larg)
g3 = delete_vertices(g2, id) # remove vertices NOT in the largest component

summary(g2)
summary(g3)

# DATA$numTweets = 0
# DATA$numTweets[1:40] = sapply(DATA$user.idStr[1:40],function(x) sum(x==DATA$user.idStr))
# DATA$numTweets[1:40000] = sapply(DATA$user.idStr[1:40000],function(x) sum(x==DATA$user.idStr))
# DATA$numTweets[40001:80000] = sapply(DATA$user.idStr[40001:80000],function(x) sum(x==DATA$user.idStr))
# DATA$numTweets[80001:120000] = sapply(DATA$user.idStr[80001:120000],function(x) sum(x==DATA$user.idStr))
# DATA$numTweets[120001:160000] = sapply(DATA$user.idStr[120001:160000],function(x) sum(x==DATA$user.idStr))
# DATA$numTweets[160001:200000] = sapply(DATA$user.idStr[160001:200000],function(x) sum(x==DATA$user.idStr))
# DATA$numTweets[200001:240000] = sapply(DATA$user.idStr[200001:240000],function(x) sum(x==DATA$user.idStr))
# DATA$numTweets[240001:280000] = sapply(DATA$user.idStr[240001:280000],function(x) sum(x==DATA$user.idStr))
# DATA$numTweets[280001:320000] = sapply(DATA$user.idStr[280001:320000],function(x) sum(x==DATA$user.idStr))
# DATA$numTweets[320001:360000] = sapply(DATA$user.idStr[320001:360000],function(x) sum(x==DATA$user.idStr))
# DATA$numTweets[360001:400000] = sapply(DATA$user.idStr[360001:400000],function(x) sum(x==DATA$user.idStr))
# for(s in 80:108){
#   DATA$numTweets[(s*40000+1):((s+1)*40000)] = sapply(DATA$user.idStr[(s*40000+1):((s+1)*40000)],function(x) sum(x==DATA$user.idStr))
#   print(s)
# }
# DATA$numTweets[(s*40000+1):length(DATA$numTweets)] = sapply(DATA$user.idStr[(s*40000+1):length(DATA$numTweets)],function(x) sum(x==DATA$user.idStr))
# 
# 
# DATA$numTweets[400001:4373354] = sapply(DATA$user.idStr[400001:4373354],function(x) sum(x==DATA$user.idStr))
# numtweetsvec = DATA$numTweets
# 
# save(numtweetsvec,file="numtweetsvec.RData")
#load(file="numtweetsvec.RData")
#DATA$numTweets = numtweetsvec
#hist(DATA$numTweets)

# plots .. 
d = igraph::degree(g3, mode="in") 

mean(d)

hist(d, probability = TRUE)

de = density(d) # estimation non paramétrique de la loi des degrés (en complément de l'histogramme)
lines(de, col="red")


# simple clustering

# res = cluster_louvain(g3)
# 
# cl = membership(res)

# selection based on degrees
threshold = 15
ID = which(d<threshold)
length(ID)

g4 = delete_vertices(g3, ID) # remove vertices 
summary(g4)
###

# # find the largest connected component
larg = which(igraph::components(g4)$csize==max(igraph::components(g4)$csize))
if(length(larg)>0) {
  larg = larg[1]
}
id = which(igraph::components(g4)$membership!=larg)
g5 = delete_vertices(g4, id) # remove vertices NOT in the largest component
###

adjmat_for_gibbs = as_adjacency_matrix(g5, sparse = TRUE)
save(adjmat_for_gibbs,file="data/adjmat_for_gibbs.RData")

adjacency_list_cop28 = as_edgelist(g5)
adjacency_df_cop28 = as.data.frame(adjacency_list_cop28)
names(adjacency_df_cop28) = c("sender","receiver")

adjacency_df_cop28$senderreceiver = paste(adjacency_df_cop28$sender, 
                                          adjacency_df_cop28$receiver, 
                                                sep = "_")
DATA$combine_user.idStr_retweet.user.idStr = paste(DATA$user.idStr, 
                                                   DATA$retweet.user.idStr, 
                                                   sep = "_")
DATA$combine_user.idStr_quote.user.idStr = paste(DATA$user.idStr, 
                                                 DATA$quote.user.idStr, 
                                                 sep = "_")

senderreceivertext_cop28_retweets = DATA[(DATA$combine_user.idStr_retweet.user.idStr %in% adjacency_df_cop28$senderreceiver),]
senderreceivertext_cop28_retweets$sender = senderreceivertext_cop28_retweets$user.idStr
senderreceivertext_cop28_retweets$receiver = senderreceivertext_cop28_retweets$retweet.user.idStr
sender.username_retweets = senderreceivertext_cop28_retweets$user.name
receiver.username_retweets = senderreceivertext_cop28_retweets$retweet.user.name

senderreceivertext_cop28_quotes = DATA[(DATA$combine_user.idStr_quote.user.idStr %in% adjacency_df_cop28$senderreceiver),]
senderreceivertext_cop28_quotes$sender = senderreceivertext_cop28_quotes$user.idStr
senderreceivertext_cop28_quotes$receiver = senderreceivertext_cop28_quotes$quote.user.idStr
sender.username_quotes = senderreceivertext_cop28_quotes$user.name
receiver.username_quotes = senderreceivertext_cop28_quotes$quote.user.name

senderreceivertext_cop28 = rbind(senderreceivertext_cop28_retweets,
                                 senderreceivertext_cop28_quotes)
sender.username = c(sender.username_retweets,
                    sender.username_quotes)
receiver.username = c(receiver.username_retweets,
                      receiver.username_quotes)
senderreceivertext_cop28$sender = as.character(senderreceivertext_cop28$sender)

senderreceivertext_cop28_onlysenderreceivertext = senderreceivertext_cop28[,c("sender","receiver","text")]

# the sender is the one who retweets
# the receiver is the one being retweeted

# remove special signs etc
senderreceivertext_cop28_onlysenderreceivertext$text <- gsub("[[:punct:]]", "", 
                                                             senderreceivertext_cop28_onlysenderreceivertext$text)
senderreceivertext_cop28_onlysenderreceivertext$text <- gsub("\\bhttps\\S*", "", 
                                                             senderreceivertext_cop28_onlysenderreceivertext$text)
senderreceivertext_cop28_onlysenderreceivertext$text <- gsub("\n", " ", 
                                                             senderreceivertext_cop28_onlysenderreceivertext$text)
senderreceivertext_cop28_onlysenderreceivertext$text <- gsub("[0-9]+", "", 
                                                             senderreceivertext_cop28_onlysenderreceivertext$text)
senderreceivertext_cop28_onlysenderreceivertext$text <- gsub("\\b\\w{1,4}\\b", "", 
                                                             senderreceivertext_cop28_onlysenderreceivertext$text)

write.csv(senderreceivertext_cop28_onlysenderreceivertext,"data/senderreceivertext_cop28.csv",row.names = FALSE)

dim(senderreceivertext_cop28)

### BEGIN G12 (can only be run after fitting thames) ###
load(paste0('data/res_gibs_full_lps','.Rda'))
load(file=paste0('data/res_gibbs_',ceiling(which.max(res_gibs_full_lps[,12-1]) / 10000),'.Rda'))
names(res_gibbs_list)=paste0("r",2:20)

res_gibbs_list_r12 = res_gibbs_list$r12
res_gibbs_list=NULL
map_r12 = res_gibbs_list_r12$clusterList[[which.max(res_gibbs_list_r12$lps)]]
table(map_r12)
length(table(map_r12))
### END G12 ###

cluster_id_cop28 = c(map_r12)
names(cluster_id_cop28) = rownames(adjmat_for_gibbs)

senderreceivertext_cop28_clustered = senderreceivertext_cop28
senderreceivertext_cop28_clustered$clustersender = cluster_id_cop28[senderreceivertext_cop28$sender]+1
senderreceivertext_cop28_clustered$clusterreceiver = cluster_id_cop28[senderreceivertext_cop28$receiver]+1
senderreceivertext_cop28_clustered$sender.username = sender.username
senderreceivertext_cop28_clustered$receiver.username = receiver.username
save(senderreceivertext_cop28_clustered,file = "data/senderreceivertext_cop28_clustered.RData")

cluster_sizes_cop28 = table(map_r12)
save(cluster_sizes_cop28,file="data/cluster_sizes_cop28.RData")

# ### BEGIN G2 ###
# load(paste0('res_gibs_full_lps_','.Rda'))
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,12-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
#
# res_gibbs_list_r12 = res_gibbs_list$r12
# res_gibbs_list=NULL
# map_r12 = res_gibbs_list_r12$clusterList[[which.max(res_gibbs_list_r12$lps)]]
# table(map_r12)
# length(table(map_r12))
# ### END G2 ###
#
# cluster_id_cop28_greed = c(map_greed_r37)
# names(cluster_id_cop28_greed) = rownames(adjmat_for_gibbs)
#
# senderreceivertext_cop28_clustered_greed = senderreceivertext_cop28
# senderreceivertext_cop28_clustered_greed$clustersender = cluster_id_cop28_greed[senderreceivertext_cop28$sender]+1
# senderreceivertext_cop28_clustered_greed$clusterreceiver = cluster_id_cop28_greed[senderreceivertext_cop28$receiver]+1
# senderreceivertext_cop28_clustered_greed$sender.username = sender.username
# senderreceivertext_cop28_clustered_greed$receiver.username = receiver.username
# save(senderreceivertext_cop28_clustered_greed,file = "senderreceivertext_cop28_clustered_greed.RData")


### BEGIN G37 greed (only works after obtaining the greed output)###
load(file=paste0('data/res_greed','.Rda'))
map_greed_r37 = res@cl-1
table(map_greed_r37)
length(table(map_greed_r37))
### END G37 greed ###

cluster_id_cop28_greed = c(map_greed_r37)
names(cluster_id_cop28_greed) = rownames(adjmat_for_gibbs)

senderreceivertext_cop28_clustered_greed = senderreceivertext_cop28
senderreceivertext_cop28_clustered_greed$clustersender = cluster_id_cop28_greed[senderreceivertext_cop28$sender]+1
senderreceivertext_cop28_clustered_greed$clusterreceiver = cluster_id_cop28_greed[senderreceivertext_cop28$receiver]+1
senderreceivertext_cop28_clustered_greed$sender.username = sender.username
senderreceivertext_cop28_clustered_greed$receiver.username = receiver.username
save(senderreceivertext_cop28_clustered_greed,file = "data/senderreceivertext_cop28_clustered_greed.RData")

cluster_sizes_cop28 = table(map_r12)
save(cluster_sizes_cop28,file="data/cluster_sizes_cop28.RData")
# 
# cluster_names = list()
# for(g in 1:12){
#   # only look at senders/receivers belonging to cluster g
#   trunc_sender_g_dataset = senderreceivertext_cop28_clustered[(senderreceivertext_cop28_clustered$clustersender == g),]
#   trunc_receiver_g_dataset = senderreceivertext_cop28_clustered[(senderreceivertext_cop28_clustered$clusterreceiver == g),]
#   cluster_names[[g]] = unique(c(paste0(trunc_sender_g_dataset$sender.username,
#                                       "_",
#                                       trunc_sender_g_dataset$sender),
#                                 paste0(trunc_receiver_g_dataset$receiver.username,
#                                       "_",
#                                       trunc_receiver_g_dataset$receiver)))
# }
# 
# save(cluster_names,file="cluster_names.RData")
# 
# # df_text_cop28 = as.data.frame(matrix(nrow=nrow(adjmat_for_gibbs),ncol=3))
# # names(df_text_cop28) = c("sender","receiver","text")
# # df_text_cop28$sender = rownames(adjmat_for_gibbs)
# # df_text_cop28$receiver = colnames(adjmat_for_gibbs)
# # sum(0==sapply(1:1000, function(s) nrow(DATA[which((df_text_cop28[s,1] == DATA$user.idStr)&((df_text_cop28[s,2] == DATA$retweet.user.idStr)|(df_text_cop28[s,2] == DATA$quote.user.idStr))),])))
# # dim(DATA[which(df_text_cop28[1,2] == DATA$retweet.user.idStr ),])
# # dim(DATA[which(df_text_cop28[1,2] == DATA$quote.user.idStr),])
# # ### BEGIN EXTRACT NAMES AND TEXT ###
# # 
# # res_clustering_list = list(clustering = c(map_r12)+1)
# # clustering = res_clustering_list$clustering
# # length(clustering)
# # 
# # # data on a person that retweeted
# # list_allnames_was_retweeted_or_quoted = list()
# # list_alltexts_was_retweeted_or_quoted = list()
# # list_allidStr_was_retweeted_or_quoted = list()
# # list_UseridStr_was_retweeted_or_quoted = list()
# # list_RetweetidStr_was_retweeted_or_quoted = list()
# # list_QuoteidStr_was_retweeted_or_quoted = list()
# # counts = table(clustering)
# # names(counts)=NULL
# # component_names = list()
# # 
# # # write down all positions of retweeded names within the truncated COP28 dataset
# # nodes_pos_was_retweeted_full = sapply(1:length(unique(rownames(adjmat_for_gibbs))),
# #                                  function(s) unique(which(as.character(DATA$retweet.user.idStr) == rownames(adjmat_for_gibbs)[s])))
# # allnames_full = sapply(nodes_pos_was_retweeted_full, function(node) unique(DATA$retweet.user.name[node]))
# # allRetweetUserIdStr_full = sapply(nodes_pos_was_retweeted_full, function(node) unique(as.character(DATA$retweet.user.idStr[node])))
# # length(unlist(allRetweetUserIdStr_full))
# # length(allnames_full)
# # 
# # for(i in 1:12){
# #   print(paste0("component: ",i))
# #   
# #   # WARNING: this could take a while
# #   nodes_pos_was_retweeted = sapply(1:length(unique(rownames(adjmat_for_gibbs)[i==clustering])),
# #                      function(s) unique(which(as.character(DATA$retweet.user.idStr) == rownames(adjmat_for_gibbs)[i==clustering][s])))
# #   nodes_pos_was_retweeted = nodes_pos_was_retweeted[sapply(seq_along(nodes_pos_was_retweeted),function(s) length(nodes_pos_was_retweeted[[s]])>0)]
# #   component_names[[i]] = sapply(nodes_pos_was_retweeted, function(node) unique(DATA$retweet.user.name[node]))
# #   nodes_pos = sapply(1:length(unique(rownames(adjmat_for_gibbs)[i==clustering])),
# #                        function(s) unique(which((as.character(DATA$retweet.user.idStr) == rownames(adjmat_for_gibbs)[i==clustering][s])|
# #                                                   (as.character(DATA$quote.user.idStr) == rownames(adjmat_for_gibbs)[i==clustering][s]))))
# #   nodes_pos = nodes_pos[sapply(seq_along(nodes_pos),function(s) length(nodes_pos[[s]])>0)]
# #   if(length(nodes_pos)==0){
# #     print("this component has no members")
# #   } else{
# #     print(paste0("number of members: ",counts[i]))
# #     # each tweets in one list entry is from the same user
# #     # so I take the first entry
# #     #allnames = sapply(nodes_pos, function(node) DATA$user.name[node[1]])
# #     list_allidStr_was_retweeted_or_quoted[[i]] = sapply(nodes_pos, function(node) DATA$idStr[node])
# #     allnames = sapply(nodes_pos, function(node) DATA$retweet.user.name[node])
# #     list_allnames_was_retweeted_or_quoted[[i]] = allnames
# #     alltexts = sapply(nodes_pos, function(node) DATA$text[node])
# #     list_alltexts_was_retweeted_or_quoted[[i]] = alltexts
# #     UseridStr = sapply(nodes_pos, function(node) as.character(DATA$user.idStr[node]))
# #     list_UseridStr_was_retweeted_or_quoted[[i]] = UseridStr
# #     RetweetidStr = sapply(nodes_pos, function(node) DATA$retweet.user.idStr[node])
# #     list_RetweetidStr_was_retweeted_or_quoted[[i]] = RetweetidStr
# #     QuoteidStr = sapply(nodes_pos, function(node) DATA$quote.user.idStr[node])
# #     list_QuoteidStr_was_retweeted_or_quoted[[i]] = QuoteidStr
# #   }
# # }
# # df_was_retweeted_or_quoted = as.data.frame(matrix(nrow=1,ncol=7))
# # names(df_was_retweeted_or_quoted) = c("idStr","name","text","UseridStr","RetweetUseridStr","QuoteUseridStr","class")
# # for(i in 1:12){
# #   df_i = as.data.frame(matrix(nrow=length(unlist(list_allidStr_was_retweeted_or_quoted[[i]])),ncol=7))
# #   names(df_i) = names(df_was_retweeted_or_quoted)
# #   df_i$idStr = unlist(list_allidStr_was_retweeted_or_quoted[[i]])
# #   df_i$name = unlist(list_allnames_was_retweeted_or_quoted[[i]])
# #   df_i$text = unlist(list_alltexts_was_retweeted_or_quoted[[i]])
# #   df_i$UseridStr = unlist(list_UseridStr_was_retweeted_or_quoted[[i]])
# #   df_i$RetweetUseridStr = unlist(list_RetweetidStr_was_retweeted_or_quoted[[i]])
# #   df_i$QuoteUseridStr = unlist(list_QuoteidStr_was_retweeted_or_quoted[[i]])
# #   df_i$class = paste0(i)
# #   df_was_retweeted_or_quoted = rbind(df_was_retweeted_or_quoted, df_i)
# # }
# # df_was_retweeted_or_quoted = df_was_retweeted_or_quoted[-1,]
# # df_was_retweeted_or_quoted = unique(df_was_retweeted_or_quoted)
# # df_was_retweeted_or_quoted$RetweetUseridStrPlusQuoteUseridStr = df_was_retweeted_or_quoted$RetweetUseridStr
# # df_was_retweeted_or_quoted$RetweetUseridStrPlusQuoteUseridStr[df_was_retweeted_or_quoted$RetweetUseridStr == "nan"] = df_was_retweeted_or_quoted$QuoteUseridStr[df_was_retweeted_or_quoted$RetweetUseridStr == "nan"]
# # save(df_was_retweeted_or_quoted,file="df_was_retweeted_or_quoted2.RData")
# # 
# # senderreceivertext =  df_was_retweeted_or_quoted[,c(4,8,3)]
# # senderreceivertext_sorted =  senderreceivertext
# # senderreceivertext_sorted =  senderreceivertext_sorted[order(senderreceivertext_sorted$UseridStr),]
# # senderreceivertext_sorted$UseridStr = c(" ",senderreceivertext_sorted$RetweetUseridStrPlusQuoteUseridStr )[-1]
# # save(senderreceivertext_sorted,file="senderreceivertext_sorted.RData")
# # write.csv(senderreceivertext_sorted,"senderreceivertext_sorted.csv",row.names = FALSE,col.names=FALSE)
# # read.csv("senderreceivertext_sorted.csv", stringsAsFactors = FALSE)[1:10,]
# # g=2
# # res_gibs_list_full_g = res_gibbs_list[[g-1]]
# # load(file=paste0('res_gibbs_full_g2','.Rda'))
# # g=2
# # for(j in 3:10){
# #   load(file=paste0('res_gibbs_',j,'.Rda'))
# #   res_gibs_list_full_g$clusterList = c(res_gibs_list_full[[g-1]]$clusterList,res_gibbs_list[[g-1]]$clusterList)
# #   print(paste0("j: ",j))
# #   save(res_gibs_list_full,file=paste0('res_gibbs_full_g',g,'.Rda'))
# # }
# # 
# # sapply(2:20, function(g) length(table(res_gibbs_list[[g-1]]$clusterList[[which.max(res_gibbs_list[[g-1]]$lps)]])))
# # max(sapply(2:20, function(g) length(table(res_gibbs_list[[g-1]]$clusterList[[which.max(res_gibbs_list[[g-1]]$lps)]]))))
# # res_gibbs_list=NULL
# # 
# # load(file=paste0('res_gibbs_',2,'.Rda'))
# # sapply(2:20, function(g) length(table(res_gibbs_list[[g-1]]$clusterList[[which.max(res_gibbs_list[[g-1]]$lps)]])))
# # max(sapply(2:20, function(g) length(table(res_gibbs_list[[g-1]]$clusterList[[which.max(res_gibbs_list[[g-1]]$lps)]]))))
# # res_gibbs_list=NULL
# # 
# # load(file=paste0('res_gibbs_',3,'.Rda'))
# # sapply(2:20, function(g) length(table(res_gibbs_list[[g-1]]$clusterList[[which.max(res_gibbs_list[[g-1]]$lps)]])))
# # max(sapply(2:20, function(g) length(table(res_gibbs_list[[g-1]]$clusterList[[which.max(res_gibbs_list[[g-1]]$lps)]]))))
# # res_gibbs_list=NULL
# # 
# # load(file=paste0('res_gibbs_',4,'.Rda'))
# # sapply(2:20, function(g) length(table(res_gibbs_list[[g-1]]$clusterList[[which.max(res_gibbs_list[[g-1]]$lps)]])))
# # max(sapply(2:20, function(g) length(table(res_gibbs_list[[g-1]]$clusterList[[which.max(res_gibbs_list[[g-1]]$lps)]]))))
# # res_gibbs_list=NULL
# 
# # source("thames_funcs.R")
# # source("VBEM.R")
# # res_cop28 = matrix(nrow=20-1,ncol=4)
# # colnames(res_cop28) = c("thames","risvb","chibs","vbem")
# # for(g in 2:20){
# #   load(file=paste0('res_gibbs_',1,'.Rda'))
# #   res_gibbs_list_g = res_gibbs_list[[g-1]]
# #   res_gibbs_list=NULL
# #   map_g = res_gibbs_list_g$clusterList[[which.max(res_gibbs_list_g$lps)]]
# #   #table(map_g)
# #   
# #   # compute the label switching
# #   clustermat = simplify2array(res_gibbs_list_g$clusterList)
# #   clustermat = t(clustermat[1,,])
# #   clustermat_relabelled = relabel_with_ECR(g,clustermat,c(map_g))
# #   
# #   res_cop28[g-1,1] = thames_sbm(adjmat_for_gibbs, g,
# #                                 res_gibbs_list_g$lps,clustermat_relabelled)$log_zhat_inv
# #   res_cop28[g-1,2] = risvb_estim(adjmat_for_gibbs, g,
# #                                  res_gibbs_list_g$lps,
# #                                  clustermat_relabelled)$log_zhat_inv
# #   res_cop28[g-1,3] = compute_ChibPartition(res_gibbs_list_g$lps, 
# #                                            res_gibbs_list_g$clusterList, g)
# #   G_vbem = VBEM_rep(adjmat_for_gibbs, g, n0=rep(1, g), eta0=matrix(1, g, g), 
# #                     zeta0=matrix(1, g, g), max_outer=100, eps_outer=1e-6, 
# #                     nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# #   res_cop28[g-1,4] = max(G_vbem$ELBO)
# # }
# 
# ### BEGIN computing MAP estimates for tau and mu ###
# 
# #clustering_res = c(map_r40)
# clustering_res = 1+c(map_r12)
# mu_hat_mle = matrix(nrow=12,ncol=12)
# for(g in 1:12){
#   print(paste0("g: ",g))
#   for(k in 1:12){
#     print(paste0("k: ",k))
#     adjmat_for_gibbs_dummy = matrix(0,ncol=ncol(adjmat_for_gibbs),nrow=nrow(adjmat_for_gibbs))
#     adjmat_for_gibbs_dummy[g==clustering_res,k==clustering_res] = 1
#     mu_hat_mle[g,k] = sum(adjmat_for_gibbs_dummy*adjmat_for_gibbs)/sum(adjmat_for_gibbs_dummy)
#   }
# }
# mean(apply(mu_hat_mle, 1, is.na))*12
# 
# heatmap(mu_hat_mle[12:1,1:12],Rowv = NA, Colv = NA,labRow=12:1,labCol=1:12)
# 
# #non_na_mu_hat_mle = mu_hat_mle
# #non_na_mu_hat_mle[is.na(mu_hat_mle)] = -1
# #heatmap(non_na_mu_hat_mle)
# 
# # the "true" number of components is 21
# #40-mean(sapply(mu_hat_mle[1,], is.na))*40
# 
# #mu_hat_mle_21 = mu_hat_mle[!is.na(mu_hat_mle[1,]),!is.na(mu_hat_mle[1,])]
# 
# round(mu_hat_mle,2)
# 
# # 7 has only 1 cluster, but is connected with everyone
# # table(map_r40)
# # 
# # 
# # 
# # table(map_r40)
# # tau_hat_mle = table(map_r40)/sum(table(map_r40))
# # round(tau_hat_mle,2)
# # names(tau_hat_mle)=c("",1:21)[-1]
# # 
# # # round down a bit for the mu
# # mu_hat_mle_21[mu_hat_mle_21>=0.9] = 0.9
# # 
# # set.seed(2025)
# # sim_data = fastGenerateSBM(nrow(adjmat_g4), tau_hat_mle,mu_hat_mle_21, directed = TRUE)
# # res = gibbs_sampler(sim_data$X, 21, 
# #                       init = list(sim_data$cl-1), 
# #                       max_iterations = 10000,seed=1)
# # lps = res$lps#[-1]
# # clusterList = res$clusterList
# # cl_gibbs= clusterList[[which.max(lps)]]
# # mean(sapply(clusterList,function(cluster) adjustedRandIndex(cl_gibbs,cluster)==1))
# ### END computing MAP estimates for tau and mu ##
# 
# # # compute the label switching
# # clustermat = simplify2array(res_gibbs_list$r40$clusterList)
# # clustermat = t(clustermat[1,,]) + 1
# # ecr_results = label.switching::ecr(c(map_r40)+1,clustermat,40)
# # 
# # # convert to matrices
# # #C_list = lapply(res_gibbs_list$r40$clusterList,function(cluster) sapply(cluster,function(c(rep(0,cluster),1,rep(0,40-cluster+1)))
# # clusterList_relabelled = list()
# # for(iter in 1:nrow(clustermat)){
# #   clusterList_relabelled[[iter]] = sapply(res_gibbs_list$r40$clusterList[[iter]], 
# #                                        function(cluster_i) ecr_results$permutations[iter,cluster_i+1])
# # }
# # 
# # # the label switching only applies to few entries
# # mean(apply(ecr_results$permutations,1,
# #            function(x) sum(ecr_results$permutations[1,]!=x)>0))
# # 
# # # the uncertainty is quite low, though often larger than 1
# # num_uncertain_nodes_01 = sapply(1:nrow(adjmat_for_gibbs), 
# #                              function(s) which(cumsum(sort(table(sapply(clusterList_relabelled, 
# #                                                                         function(cluster) cluster[s]))/
# #                                                              sum(table(sapply(clusterList_relabelled, 
# #                                                                               function(cluster) cluster[s]))),
# #                                                            decreasing = TRUE))>0.75)[1]==1)
# # table(num_uncertain_nodes_01)
# # table(num_uncertain_nodes_01)/sum(table(num_uncertain_nodes_01))
# # 
# # # select sample from one uncertain node
# # uncertain_node_01 = sapply(1:10000,function(s) clusterList_relabelled[[s]][!num_uncertain_nodes_01][1])
# # 
# # # compute the estimator of the node probability
# # uncertain_node_01_estim = cumsum(uncertain_node_01==14)/(1:length(uncertain_node_01))
# # plot(uncertain_node_01_estim)
# # 
# # c(mean((uncertain_node_01==14)[1:5000]),
# #   mean((uncertain_node_01==14)[5000:10000]),
# #   mean(uncertain_node_01==14))
# # 
# # tau_estim = sapply(1:nrow(adjmat_for_gibbs), function(s) as.numeric(names(which.max(table(sapply(clusterList_relabelled, 
# #                                      function(cluster) cluster[s]))/
# #                           sum(table(sapply(clusterList_relabelled, 
# #                                            function(cluster) cluster[s])))))[1])) # find "map" estimator based on averaging
# # sum(sapply(clusterList_relabelled, function(cluster) mean(tau_estim[num_uncertain_nodes_01] != cluster[num_uncertain_nodes_01]) < 0.01))
# # clusterList_relabelled[[1]]
# # 
# # difficult_nodes = which(num_uncertain_nodes_01==FALSE)
# # #sapply(difficult_nodes, function(s) length(table(sapply(clusterList_relabelled, function(cluster) cluster[which(num_uncertain_nodes_01==FALSE)[1]]))))
# # 
# # # the number of clusters is always the same, though much lower than 40
# # num_clusters = sapply(seq_along(res_gibbs_list$r40$clusterList), function(s) length(table(res_gibbs_list$r40$clusterList[[s]])))
# # barplot(table(num_clusters),xlab="number of clusters",ylab="number of iterations")
# # plot(num_clusters,ylab="number of clusters",xlab="T")
# # table(num_clusters)
# 
# ### END gibbs ###
# 
# ### BEGIN VISUALIZATION ###
# res_clustering_list_greed = list(clustering=clustering(res))
# res_clustering_list_gibbs = list(clustering = c(map_r6)+1)
# analyse = function(res_clustering_list){
#   adjmat_g4 = as_adjacency_matrix(g4)
#   clustering = res_clustering_list$clustering
#   length(clustering)
#   
#   unique_ids = unique(as.character(DATA$user.idStr))
#   
#   list_avg_num_followers_from_id_with_most_avg_followers = list()
#   list_name_from_id_with_most_avg_followers = list()
#   list_tweets_from_id_with_most_avg_followers = list()
#   list_allnames = list()
#   counts = table(clustering)
#   names(counts)=NULL
#   for(i in 1:12){
#     print(paste0("component: ",i))
#     
#     # WARNING: this could take a while
#     rownames(adjmat_g4)[i==clustering]
#     nodes_pos = sapply(1:length(unique(rownames(adjmat_g4)[i==clustering])), 
#                        function(s) unique(which(as.character(DATA$user.idStr) == rownames(adjmat_g4)[i==clustering][s])))
#     
#     nodes_pos = sapply(1:length(unique(rownames(adjmat_g4)[i==clustering])), 
#                        function(s) unique(which(as.character(DATA$retweet.user.idStr) == rownames(adjmat_g4)[i==clustering][s])))
#     
#     nodes_pos = nodes_pos[sapply(seq_along(nodes_pos),function(s) length(nodes_pos[[s]])>0)]
#     if(length(nodes_pos)==0){
#       print("this component has no members")
#       list_avg_num_followers_from_id_with_most_avg_followers[[i]] = list()
#       list_name_from_id_with_most_avg_followers[[i]] = list()
#       list_tweets_from_id_with_most_avg_followers[[i]] = list()
#     } else{
#       print(paste0("number of members: ",counts[i]))
#       # number of times they retweeted, quoted someone
#       #rowSums(adjmat_g4[i==clustering(res),])
#       # number of times they got retweeted, quoted by someone
#       #colSums(adjmat_g4[,i==clustering(res)])
#       
#       avg_num_followers = sapply(nodes_pos,function(node) mean(DATA$user.followers[node]))
#       
#       id_with_most_avg_followers = nodes_pos[[which.max(avg_num_followers)]]
#       
#       name_from_id_with_most_avg_followers = DATA$user.name[id_with_most_avg_followers[1]]
#       print(paste0("username of user with highest number of followers: ",
#                    name_from_id_with_most_avg_followers))
#       list_name_from_id_with_most_avg_followers[[i]] = name_from_id_with_most_avg_followers
#       
#       # each tweets in one list entry is from the same user 
#       # so I take the first entry
#       allnames = sapply(nodes_pos, function(node) DATA$user.name[node[1]])
#       allnames = sapply(nodes_pos, function(node) DATA$retweet.user.name[node[1]])
#       
#       list_allnames[[i]] = allnames
#       
#       tweets_from_id_with_most_avg_followers = length(id_with_most_avg_followers)
#       print(paste0("number of tweets of user with highest number of followers: ",
#                    tweets_from_id_with_most_avg_followers))
#       list_tweets_from_id_with_most_avg_followers[[i]] = tweets_from_id_with_most_avg_followers
#       
#       avg_num_followers_from_id_with_most_avg_followers = max(avg_num_followers)
#       print(paste0("average number of followers of user with highest number of followers: ",
#                    avg_num_followers_from_id_with_most_avg_followers))
#       list_avg_num_followers_from_id_with_most_avg_followers[[i]] = avg_num_followers_from_id_with_most_avg_followers
#       
#     }
#     #sapply(1:10, function(s) unique(DATA$user.name[which(as.character(DATA$user.idStr) == rownames(adjmat_g4)[i==clustering(res)][s])]))
#     #print(DATA$user.name[which(as.character(DATA$user.idStr) == rownames(adjmat_g4)[i==clustering(res)][1])][1])
#   }
#   
#   library(tidygraph)
#   library(ggraph)
#   
#   i=6
#   mat_cop28_i3 = adjmat_g4[i==clustering,i==clustering]
#   colnames(mat_cop28_i3) = allnames
#   rownames(mat_cop28_i3) = allnames
#   graph_cop28_i3 = graph_from_adjacency_matrix(mat_cop28_i3)
#   ggraph(graph_cop28_i3,layout="stress")+
#     geom_node_point() +
#     geom_edge_link(alpha = 0.1) +
#     geom_node_point(size = 2) +
#     geom_node_text(aes(label = name), size = 5, repel = TRUE) +
#     theme_void() +
#     geom_edge_link(
#       arrow = arrow(length = unit(5, "mm")),       # small arrowheads
#       end_cap = circle(5, 'mm'),
#       alpha = 1                                  # semi-transparent for readability
#     )
#   summary(g5)
#   
#   df = data.frame(matrix(0,11,5))
#   names(df) = c("avg_num_followers_from_id_with_most_avg_followers",
#                 "name_from_id_with_most_avg_followers",
#                 "num_tweets_from_id_with_most_avg_followers",
#                 "num_members",
#                 "component_id")
#   df$avg_num_followers_from_id_with_most_avg_followers = simplify2array(list_avg_num_followers_from_id_with_most_avg_followers[-1])
#   df$name_from_id_with_most_avg_followers = simplify2array(list_name_from_id_with_most_avg_followers[-1])
#   df$num_tweets_from_id_with_most_avg_followers = simplify2array(list_tweets_from_id_with_most_avg_followers[-1])
#   df$num_members = c(2195,140,7293,1028,8,400,106,1008,219,840,621)
#   df$component_id = 1:11
#   
#   plot(df$component_id,df$avg_num_followers_from_id_with_most_avg_followers)
#   plot(df$component_id,df$num_tweets_from_id_with_most_avg_followers)
#   plot(df$component_id,df$num_members)
#   
#   write.csv(df,file="summary_of_components.csv")
#   read.csv("summary_of_components.csv")
#   save(list_allnames,file="listallnames.RData")
#   load(file="listallnames.RData")
#   
#   df_allnames = as.data.frame(matrix(nrow=10,ncol=12))
#   names(df_allnames) = paste0("cluster_",1:12)
#   
#   for(i in 2:12){
#     df_allnames[,i] = list_allnames[[i]][1:10]
#   }
#   View(df_allnames)
#   df_allnames[11,] = c(0,df$name_from_id_with_most_avg_followers)
#   df_allnames[12,] = c(0,df$num_members)
#   
#   mu_hat_mle = matrix(nrow=12,ncol=12)
#   for(g in 1:12){
#     for(k in 1:12){
#       adjmat_g4_dummy = matrix(0,ncol=ncol(adjmat_g4),nrow=nrow(adjmat_g4))
#       adjmat_g4_dummy[g==clustering(res),k==clustering(res)] = 1
#       mu_hat_mle[g,k] = sum(adjmat_g4_dummy*adjmat_g4)/sum(adjmat_g4_dummy)
#     }
#   }
#   
#   write.csv(as.data.frame(mu_hat_mle),file="mu_for_twitter_data.csv")
#   read.csv("mu_for_twitter_data.csv")
#   
#   #truncate everything below 0.01 for better visibility
#   mu_hat_mle_trunc = mu_hat_mle * (mu_hat_mle > 0.01)
#   
#   mu_hat_mle_trunc[,1] = 0 # remove connections to the NA node
#   
#   mu_hat_mle_trunc = mu_hat_mle_trunc
#   g <- graph_from_adjacency_matrix(mu_hat_mle_trunc, mode = "directed",weighted=TRUE)
#   
#   # Add labels (optional)
#   V(g)$label <- c("NA",df$name_from_id_with_most_avg_followers)
#   V(g)$label = gsub(" ", "\n",V(g)$label)
#   
#   
#   V(g)$label[3] = "Extinction\nRebellion"
#   #V(g)$label[23] = "Senator\nSherry\nRehman"
#   #V(g)$label[35] = "Bill\nOmondi"
#   #V(g)$label[36] = "Sky\nNews\nArabic"
#   #V(g)$label[37] = "Abdullah\nbin\nZayed"
#   #V(g)$label[34] = "Real\nMASH"
#   
#   layout <- layout_with_fr(g, niter = 50000, repulserad = 1*vcount(g)^2)
#   #layout <- layout_with_kk(g)
#   layout <- norm_coords(layout, ymin=-1, ymax=1, xmin=-1, xmax=1) * 2
#   E(g)$color <- ifelse(E(g)$weight > .5, "red", "gray70")
#   plot(
#     g,
#     layout = layout,   # force-directed layout
#     vertex.color = "white",
#     vertex.label.color = "black",
#     edge.width = E(g)$weight*5,  # scale down weights
#     edge.color = "gray50",
#     edge.arrow.size = 0.3,
#     vertex.size=40,
#     vertex.label.cex = 0.5
#   )
#   ggraph(g)
# }
# ### END VISUALIZATION ###