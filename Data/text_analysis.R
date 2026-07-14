rm(list=ls())
library(tm)
library(torch)
library(topicmodels.etm)
library(word2vec)
library(udpipe)
library(cld3)
library(SnowballC)
library(dplyr)
library(tidytext)
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
dim(dtm)

set.seed(2026)
topictestlda = topicmodels::LDA(dtm, control = list(estimate.alpha = FALSE,  alpha = 1, seed=2026), k = 40)
save(topictestlda,file="data/topictestlda.RData")
#image(topictestlda@gamma)

beta = exp(topictestlda@beta)
rowSums(beta)
colnames(beta) = colnames(dtm)

terms <- function(beta,K,n=200,s=1){
  words = colnames(beta)
  ent = apply(beta,2,function(x){y = x/sum(x); 1+sum(y*log(y))/log(K)})
  spec=matrix(0, dim(beta)[1], dim(beta)[2])
  important_words_per_topic = list()
  for (k in 1:K){
    spec[k, ] = beta[k,] * ent^s
    Wk = words[order(spec[k, ],decreasing = TRUE)[1:n]]
    cat('* Topic',k,':'); print(paste(Wk,collapse = ', ')); cat('\n')
    important_words_per_topic[[k]] = paste0("Summarize the following text using 3-5 english words, while not elaborating in any other way. ",paste(Wk,collapse = ', '))
  }
  return(important_words_per_topic)
}
important_words_per_topic = terms(beta,40)


library(ollamar)

summaries_of_topics = list()
for(k in 1:40){
  summary_of_topic <- generate(
    model = "gemma4",
    prompt = important_words_per_topic[[k]],
    temperature = 0,
    seed = 2026,
    output="text"
  )
  summaries_of_topics[[k]] = summary_of_topic
}
save(summaries_of_topics,file="data/summaries_of_topics.RData")
load(file="data/summaries_of_topics.RData")
# install.packages("ollamar")
#
# install.packages("ollamar")
# library(ollamar)
#
# chat(
#   list("Explain principal component analysis in simple terms"),
#   model = "llama3"
# )
#
# messages <- list(
#   list(role = "user", content = "How are you doing?")
# )
# chat("llama3", messages)

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
save(theta_colors,file = "data/theta_colors.RData")
save(theta_colors_prob,file="data/theta_colors_prob.RData")
fullthetaplusclusters[is.na(fullthetaplusclusters)]=0
save(fullthetaplusclusters,file="data/fullthetaplusclusters.RData")
