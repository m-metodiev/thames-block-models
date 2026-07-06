# install.packages("torch")
# library(torch)
# install_torch()
# pkgs <- c("torch", "topicmodels.etm", "word2vec", "doc2vec", "udpipe", "uwot")
# install.packages(pkgs)
# Install if needed
library(tm)
library(torch)
library(topicmodels.etm)
library(word2vec)
library(udpipe)
library(cld3)
library(SnowballC)
library(dplyr)
library(topicmodels)

# number of members in each components


#i=5
#length(list_allnames)
load(file = "data/senderreceivertext_cop28_clustered.RData")

texts = senderreceivertext_cop28_clustered$text
#cleaned_texts = sapply(texts, function(text) gsub(strsplit(text,":")[[1]][1],"",text))
length(texts)

#word_count <- lengths(strsplit(texts, "\\s+"))
#texts_filtered <- texts[word_count > 10]

all_languages = sapply(texts, function(x) detect_language(x))
sum(is.na(all_languages))

is_arabic <- grepl("[\u0600-\u06FF]", texts)
all_languages[is_arabic] = "ar"

# is_japanese <- grepl("[\u3040-\u309f\u30a0-\u30ff]", texts)
# all_languages[is_japanese]
# 
# is_chinese_likely <- function(text) {
#   has_han  <- grepl("[\u4e00-\u9fff]", text)
#   has_kana <- grepl("[\u3040-\u309f\u30a0-\u30ff]", text)
#   
#   has_han & !has_kana
# }
# all_languages[is_chinese_likely(texts)]

tab_vec = sort(table(all_languages),decreasing=TRUE)
names(tab_vec )=NULL
df=as.data.frame(t(rbind(names(sort(table(all_languages),decreasing=TRUE)),
                      "&",
                      tab_vec ,
        "&",
        round(sort(100*tab_vec/sum(tab_vec),decreasing=TRUE),1),
        "\\")))
df[1:10,]
# all_languages_truefalse = all_languages =="ja"
# 
# names(all_languages_truefalse) = NULL
# all_languages_truefalse[is.na(all_languages_truefalse)]=FALSE
# all_languages[all_languages_truefalse][1:10]
# 
# print(round(sort(table(sapply(all_languages, function(x) detect_language(x))),decreasing=TRUE)/
#                 sum(sort(table(sapply(all_languages, function(x) detect_language(x))),decreasing=TRUE)),2))
# 
# all_languages = unique(all_languages)
# length(all_languages)

# df for gephy
df_adjlist = senderreceivertext_cop28_clustered[,c("sender","receiver")]
df_adjlist_unique <- df_adjlist[!duplicated(df_adjlist), ]
write.csv(df_adjlist_unique,file = "data/adjlist.csv",row.names = FALSE)
read.csv(file = "data/adjlist.csv",colClasses = "character")
# df for gephy
df_senderclusters = senderreceivertext_cop28_clustered[,c("sender","clustersender")]
names(df_senderclusters) = c("Id","cluster")

df_receiverclusters = senderreceivertext_cop28_clustered[,c("receiver","clusterreceiver")]
names(df_receiverclusters) = c("Id","cluster")

df_clusters = rbind(df_senderclusters,df_receiverclusters)
df_clusters_unique <- df_clusters[!duplicated(df_clusters), ]
write.csv(df_clusters_unique,file = "data/clusterlist.csv",row.names = FALSE)
read.csv(file = "data/clusterlist.csv",colClasses = "character")

df_clusters_dichotomized = df_clusters_unique
names(df_clusters_dichotomized) = c("Id","iseleven")
df_clusters_dichotomized$iseleven[df_clusters_dichotomized$iseleven!="11"] = "1"
write.csv(df_clusters_dichotomized,file = "data/clusterlistiseleven.csv",row.names = FALSE)
read.csv(file = "data/clusterlistiseleven.csv",colClasses = "character")
## loads of SAVESOIL ##

# one third uses the hashtag savesoil
# load(file="data/cluster_names.RData")
# cluster_names[[1]]
# mean(grepl("SaveSoil", cluster_names[[1]], ignore.case = TRUE))

#cluster_names[[2]]
# Taken together, these names represent a professional and institutional climate policy ecosystem:
#   scientists producing climate knowledge,
# NGOs and activists advocating for stronger climate action,
# journalists covering the issue, and
# policymakers or international bodies engaged in climate governance.

#cluster_names[[3]]
# Together, these accounts represent a core transnational climate policy and advocacy network composed of:
#   leading climate activists and campaign organizations,
# scientists and expert communicators,
# international institutions and negotiators, and
# journalists covering climate politics.

#cluster_names[[4]]
# This network reflects a hybrid coalition around soil and environmental sustainability, consisting of:
#   international agricultural and development institutions,
# soil science and environmental research actors,
# spiritual organizations connected to the campaign, and
# a very large base of grassroots supporters mobilized online through the Save Soil movement.

#length(cluster_names[[5]])
# very large: 6279 usernames

#length(cluster_names[[6]])
# very large: 1222 usernames

#length(cluster_names[[7]])
# very large: 1441 usernames

#cluster_names[[8]]
# Sustainable land use and agriculture – represented by entities like the Food and Land Use Coalition, World Agroforestry, Indigo Ag, CIFOR-ICRAF, and Food Systems Partnership.
# Climate change and biodiversity – highlighted by FAO Climate Change & Biodiversity, UN Biodiversity, UN Land and Drought, and figures like Ibrahim Thiaw.
# Soil health advocacy – numerous mentions of the #SaveSoil campaign (Cavita Sharma, Louisa, GabriellaCadei, GargiC), and individuals connected to Isha Foundation and outreach initiatives.
# Media coverage – news outlets such as Reuters, ANI, CNN Business Arabic, Firstpost, DD News, News18, Emirates News, and The National are included, showing global dissemination of these issues.
# Political and institutional engagement – high-profile figures like Mariam Almheiri, Mariam bint Mohammed Almheiri, and Secretary Antony Blinken, alongside organizations like UNHCR, indicate policy-level interest and influence.
# Faith and advocacy networks – groups like Faith For Earth Coalition and Isha Foundation reflect intersection of spiritual leadership and environmental activism.


#length(cluster_names[[9]])

#length(cluster_names[[10]])

# Climate action & advocacy: Many accounts belong to climate activists, organizations, and journalists (e.g., Extinction Rebellion Global, Climate Justice Victoria, Covering Climate Now, WRI Ross Center, Climate Watcher).
# Environmental organizations & initiatives: Groups promoting SDGs, sustainable cities, and indigenous/environmental justice (e.g., Indigenous Women and Girls Initiative, SDG2030, Operation Noah).
# Public figures / scientists / communicators: Academics, journalists, and advisors in climate science and policy (e.g., Paul Brown, Laurie Garrett, Andrea Learned, Guy Lane, Ashis Basu).
# Social media activists & influencers: Accounts using emojis, Mastodon/Bluesky handles, and personal branding to promote climate and justice messaging.

#cluster_names[[11]]

#cluster_names[[12]]

# The list primarily represents climate, energy, and sustainability stakeholders, including:
#   International organizations (UN, World Bank, EU, IAEA)
# Governments and political leaders
# Media outlets (mainstream, climate-focused, and alternative)
# NGOs, advocacy groups, and think tanks
# Scientists, activists, and corporate/philanthropic influencers
# Special focus on COP28, youth mobilization, and nuclear energy discussions

texts_of_component_g = senderreceivertext_cop28_clustered$text

# remove the username information of retweets
strings_without_RT = sapply(texts_of_component_g[startsWith(texts_of_component_g,"RT @")],
                            function(text) gsub(strsplit(text,":")[[1]][1],"",text))
names(strings_without_RT) = NULL
cleaned_texts_of_component_g = texts_of_component_g
cleaned_texts_of_component_g[startsWith(cleaned_texts_of_component_g,"RT @")] =
  strings_without_RT


# Convert to Corpus
corpus <- VCorpus(VectorSource(cleaned_texts_of_component_g))

# Remove stop words
corpus <- tm_map(corpus, content_transformer(tolower)) # optional: lowercase
# Combine stop words from multiple languages
stopwords_all <- c(
  stopwords("en"),   # English
  stopwords("fr"),   # French
  stopwords("dutch"),
  stopwords("danish"),
  stopwords("finnish"),
  stopwords("german"),
  stopwords("hungarian"),
  stopwords("italian"),
  stopwords("norwegian"),
  stopwords("portuguese"),
  stopwords("russian"),
  stopwords("spanish"),
  stopwords("swedish"),
  c("هذا","مثال","في","من","على","و")
)
corpus <- tm_map(corpus, removeWords, stopwords_all)

# Get back a character vector
cleaned_texts_of_component_g <- sapply(corpus, as.character)

cleaned_texts_of_component_g <- gsub("[[:punct:]]", "", cleaned_texts_of_component_g)
cleaned_texts_of_component_g <- gsub("\\bhttps\\S*", "", cleaned_texts_of_component_g)
cleaned_texts_of_component_g <- gsub("\n", " ", cleaned_texts_of_component_g)
cleaned_texts_of_component_g <- gsub("[0-9]+", "", cleaned_texts_of_component_g)
cleaned_texts_of_component_g <- gsub("\\b\\w{1,4}\\b", "", cleaned_texts_of_component_g)

# takes too much time for this dataset
# library(udpipe)
# model <- udpipe_download_model(language = "english")
# ud_model <- udpipe_load_model(model$file_model)
# 
# cleaned_texts_of_component_g <- sapply(cleaned_texts_of_component_g,
#                                        function(text) paste(as.data.frame(udpipe_annotate(ud_model, x = text))$token[as.data.frame(udpipe_annotate(ud_model, x = text))$upos == "NOUN"], collapse = " "))
#   
# annotation <- udpipe_annotate(ud_model, x = text)
# anno_df <- as.data.frame(annotation)
# 
# nouns <- anno_df$token[anno_df$upos == "NOUN"]
# paste(nouns, collapse = " ")

#cleaned_texts_of_component_g = list_allnames[[1]]
df <- data.frame(
  doc_id = seq_along(cleaned_texts_of_component_g),
  text = cleaned_texts_of_component_g,
  stringsAsFactors = FALSE
)

words <- c("running", "runs", "ran", "studies", "studying")

tidy_df <- df %>%
  unnest_tokens(word, text) %>%                       # split into words
  mutate(word = wordStem(word)) %>%                   # apply stemming
  filter(!word %in% stopwords_all) %>%                # remove stopwords
  count(doc_id, word, name = "frequency") %>%         # count occurrences
  rename(term = word)

# with the trigram stuff
# tidy_df <- df %>%
#   unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%          # split into words
#   filter(!trigram %in% stopwords_all) %>%   # remove stopwords
#   count(doc_id, trigram, name = "frequency") %>%  # count occurrences
#   rename(term = trigram)
# tidy_df = tidy_df[!is.na(tidy_df$term),]
# tidy_df = tidy_df[-(1:20),]
# table(tidy_df$frequency)

# tidy_df <- df %>%
#   unnest_tokens(word, text) %>%          # split into words
#   filter(!word %in% stopwords_all) %>%   # remove stopwords
#   count(doc_id, word, name = "frequency") %>%  # count occurrences
#   rename(term = word)
# table(tidy_df$frequency)

dtm <- document_term_matrix(document_term_frequencies(tidy_df))
save(dtm,file=paste0("data/dtm",".RData"))
load(file=paste0("data/dtm",".RData"))
#dtm <- dtm[dtm_rowsums(dtm) > 0, ]

##
## Toy example without pretrained word embeddings
##
# model       <- ETM(k = 10, embeddings = 500, vocab = colnames(dtm))
# optimizer   <- optim_adam(params = model$parameters, lr = 0.005, weight_decay = 0.0000012)
# epochs      <- 40
# 
# overview    <- model$fit(data = dtm, optimizer = optimizer, epoch = epochs, batch_size = 1000)
# terminology <- predict(model, type = "terms", top_n = 10)
# thetas = predict(model, newdata = dtm)
# save(terminology,file=paste0("terminology_component_",i,".RData"))
# load(file=paste0("terminology_component_",i,".RData"))
# 
# save(thetas,file=paste0("thetas_",i,".RData"))
# sapply(terminology,function(x) x$term)
# terminology

# senderreceivertext_cop28_clustered$doc_id = 1:nrow(senderreceivertext_cop28_clustered)
# theta_colors = matrix(ncol=12,nrow=12)
# theta_colors_prob = matrix(ncol=12,nrow=12)
# for(g1 in 1:12){
#   print(paste0("g1: ",g1))
#   for(g2 in 1:12){
#     print(paste0("g2: ",g2))
#     selected_document_id = 
#       senderreceivertext_cop28_clustered[(senderreceivertext_cop28_clustered$clustersender == g1)&
#                                            (senderreceivertext_cop28_clustered$clusterreceiver == g2),"doc_id"]
#     theta_g1g2 = thetas[as.numeric(rownames(thetas)) %in% selected_document_id$doc_id,]
#     if(sum(as.numeric(rownames(thetas)) %in% selected_document_id$doc_id)!=1){
#       theta_g1g2 = colSums(theta_g1g2)/nrow(theta_g1g2)
#     } # no need to normalize if only 1 row
#     if(sum(is.na(theta_g1g2))>0){
#       next
#     }
#     theta_colors[g1,g2] = which.max(theta_g1g2)
#     theta_colors_prob[g1,g2] = max(theta_g1g2)
#   }
# }

# cl = apply(thetas,1,which.max)
# sort(thetas[,1],decreasing=TRUE)[1:10]
# hist(thetas)
# summary(c(thetas))
# image(thetas)
# install.packages("topicmodels")
library(topicmodels)
dim(dtm)

set.seed(2026)
topictestlda = topicmodels::LDA(dtm, control = list(estimate.alpha = FALSE,  alpha = 1), k = 40)
save(topictestlda,file="data/topictestlda.RData")
#image(topictestlda@gamma)

beta = exp(topictestlda@beta)
rowSums(beta)
colnames(beta) = colnames(dtm)
terms <- function(beta,K,n=200,s=1){
  words = colnames(beta)
  ent = apply(beta,2,function(x){y = x/sum(x); 1+sum(y*log(y))/log(K)})
  spec=matrix(0, dim(beta)[1], dim(beta)[2]) 
  for (k in 1:K){
    spec[k, ] = beta[k,] * ent^s
    Wk = words[order(spec[k, ],decreasing = TRUE)[1:n]]
    cat('* Topic',k,':'); print(paste(Wk,collapse = ', ')); cat('\n')
  }
  return(spec)
}
terms(beta,40)

#terms(topictestlda, 10)
# all very similar, though 
# 9=lossanddammage
# 10=sadhguru
# 13=regional media coverage and official messaging from the UAE, emphasizing themes such as sustainable development, international participation, and delivering climate commitments.
# 18=related to africa
# 20=alternative media
thetas=topictestlda@gamma
theta_colors = matrix(ncol=12,nrow=12)
theta_colors_prob = matrix(ncol=12,nrow=12)
fullthetaplusclusters = matrix(ncol=144,nrow=40)
colnames(fullthetaplusclusters) = c("",1:ncol(fullthetaplusclusters))[-1]
senderreceivertext_cop28_clustered$doc_id = 1:nrow(senderreceivertext_cop28_clustered)
for(g1 in 1:12){
  print(paste0("g1: ",g1))
  for(g2 in 1:12){
    print(paste0("g2: ",g2))
    colnames(fullthetaplusclusters)[g1+(g2-1)*12] = paste0(g1,",",g2)
    selected_document_id = 
      senderreceivertext_cop28_clustered[(senderreceivertext_cop28_clustered$clustersender == g1)&
                                           (senderreceivertext_cop28_clustered$clusterreceiver == g2),"doc_id"]
    theta_g1g2 = thetas[as.numeric(rownames(dtm)) %in% selected_document_id$doc_id,]
    
    # if(sum(as.numeric(rownames(dtm)) %in% selected_document_id$doc_id)!=1){
    #   selected_dtm = dtm[as.numeric(rownames(dtm)) %in% selected_document_id$doc_id,]
    #   theta_g1g2 = colSums(theta_g1g2)/nrow(theta_g1g2)
    #   topictestlda_g1g2 = topicmodels::LDA(selected_dtm, control = list(estimate.alpha = FALSE,  alpha = 1), k = 20)
    #   print(terms(topictestlda_g1g2, 1))
    # } # no need to normalize if only 1 row
    if(sum(is.na(theta_g1g2))>0){
      next
    }
    if(length(theta_g1g2)==0){
      next
    }
    if(is.null(dim(theta_g1g2))){
      theta_colors[g1,g2] = which.max(theta_g1g2)
      theta_colors_prob[g1,g2] = max(theta_g1g2)
      fullthetaplusclusters[,g1+(g2-1)*12] = theta_g1g2
    } else{
      theta_colors[g1,g2] = which.max(colSums(theta_g1g2)/dim(theta_g1g2)[1])
      theta_colors_prob[g1,g2] = max(colSums(theta_g1g2)/dim(theta_g1g2)[1])
      fullthetaplusclusters[,g1+(g2-1)*12] = colSums(theta_g1g2)/dim(theta_g1g2)[1]
    }
  }
}
theta_colors[is.na(theta_colors)] = 0
#theta_colors[(theta_colors!=9)&(theta_colors!=10)&(theta_colors!=13)&(theta_colors!=18)&(theta_colors!=20)] = 1
save(theta_colors,file = "data/theta_colors.RData")
save(theta_colors_prob,file="data/theta_colors_prob.RData")
save(fullthetaplusclusters,file="data/fullthetaplusclusters.RData")
load(file="data/fullthetaplusclusters.RData")
fullthetaplusclusters[is.na(fullthetaplusclusters)]=0

# truncated above 0.1
fullthetaplusclusters[fullthetaplusclusters>0.1]=0.1


# normalize rows
fullthetaplusclusters = t(apply(fullthetaplusclusters,1,function(x) (x-mean(x))/sd(x)))

library(RColorBrewer)
library(pheatmap)
rownames(fullthetaplusclusters) = c(" ",1:40)[-1]

# manual clusters
clusters <- rep(1:12,12)#as.numeric(substr(colnames(fullthetaplusclusters), 1, 2))
#clusters[is.na(clusters)] <- as.numeric(substr(colnames(fullthetaplusclusters), 1, 1))[is.na(clusters)]

#clusters = c(sapply(1:12,function(s) rep(s,12)))

# row annotation (this creates the side bar)
annotation_col <- data.frame(
  Cluster = factor(clusters)
)
rownames(annotation_col) <- colnames(fullthetaplusclusters)

viridis::viridis(10)
#RColorBrewer::brewer.pal(10,"viridis")
# colors for clusters
ann_colors <- list(
  Cluster = c("1" = viridis::viridis(12)[1],
              "2" = viridis::viridis(12)[2],
              "3" = viridis::viridis(12)[3],
              "4" = viridis::viridis(12)[4],
              "5" = viridis::viridis(12)[5],
              "6" = viridis::viridis(12)[6],
              "7" = viridis::viridis(12)[7],
              "8" = viridis::viridis(12)[8],
              "9" = viridis::viridis(12)[9],
              "10" = viridis::viridis(12)[10],
              "11" = viridis::viridis(12)[11],
              "12" = viridis::viridis(12)[12])
)

pheatmap(fullthetaplusclusters,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         gaps_col = (0:11)*12,
         annotation_col=annotation_col,
         annotation_colors = ann_colors)



pheatmap(fullthetaplusclusters,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         annotation_col=annotation_col,
         annotation_colors = ann_colors)
fullthetaplusclusters[fullthetaplusclusters==0]=0.001
pheatmap(log(fullthetaplusclusters),
         cluster_rows = FALSE,
         cluster_cols = FALSE)
which((apply(fullthetaplusclusters,1,max)>0.07)==TRUE)
1/40
1/40

sapply(c(18,19,20,26,28,29),
       function(g) unique(c(senderreceivertext_cop28_clustered_greed$sender.username[senderreceivertext_cop28_clustered_greed$clustersender==g],
                            senderreceivertext_cop28_clustered_greed$receiver.username[senderreceivertext_cop28_clustered_greed$clusterreceiver==g])))

sapply(c(1),
       function(g) unique(c(senderreceivertext_cop28_clustered$sender.username[senderreceivertext_cop28_clustered$clustersender==g],
                            senderreceivertext_cop28_clustered$receiver.username[senderreceivertext_cop28_clustered$clusterreceiver==g])))

library(tm)
library(wordcloud)
library(slam)

# term frequencies (sparse-safe)
freq <- col_sums(dtm)

# word cloud
wordcloud(names(freq), freq, min.freq = 1000)

texts[grepl("savesoil", texts, ignore.case = TRUE)]
