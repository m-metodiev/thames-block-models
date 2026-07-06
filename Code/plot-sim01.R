rm(list=ls())
library(ggplot2)
library(reshape2)
library(gridExtra)
library(cowplot)
library(viridis)
source("theme.R")


textsize = 17
textsize_axis = 18

### communities ###
df_full_communities = read.csv("data/df_sim01_communities.csv")
df_full_communities = df_full_communities[df_full_communities$lambda<=.7,]

df_full_communities$lambda = c("",df_full_communities$lambda)[-1]
# df_full_communities$thames = abs(df_full_communities$thames)
# df_full_communities$ChibPartitions = abs(df_full_communities$ChibPartitions)
# df_full_communities$VBEM = abs(df_full_communities$VBEM)
# df_full_communities$VBEM_correction = abs(df_full_communities$VBEM_correction)
plot_communities_varyingT_thames = ggplot(df_full_communities,
       aes(x=factor(iters),y=thamessamplesplitting,fill=lambda)) +
  geom_boxplot() + 
  #ylab("absolute error") + 
  ylab("error") +
  xlab("T") + 
  theme(legend.position = "none")  + 
  labs(title="Communities\n") +
  theme(text = element_text(size = textsize)) +
  scale_fill_viridis_d(option = "mako",direction = -1, name = expression(lambda))
  # scale_fill_brewer(palette = "Set1")

plot_communities_varyingT_ChibPartitions = ggplot(df_full_communities,
                                          aes(x=factor(iters),y=ChibPartitions,fill=lambda)) +
  geom_boxplot() + 
  #ylab("absolute error") +
  ylab("error") +
  xlab("T") + 
  theme(legend.position = "none")  + 
  labs(title="Communities\n") +
  theme(text = element_text(size = textsize)) +
  scale_fill_viridis_d(option = "mako",direction = -1, name = expression(lambda))

library(gridExtra)
grid.arrange(plot_communities_varyingT_thames + ylim(c(-.7,.7)),
             plot_communities_varyingT_ChibPartitions + ylim(c(-.7,.7)),ncol=2)

df_thamessamplesplitting_communities = df_full_communities[df_full_communities$iters==10000,][c("thamessamplesplitting","lambda")]
df_thamessamplesplitting_communities$estimator = "0_thamessamplesplitting"
names(df_thamessamplesplitting_communities)[1] = "error"

df_thamesnosamplesplitting_communities = df_full_communities[df_full_communities$iters==10000,][c("thamesnosamplesplitting","lambda")]
df_thamesnosamplesplitting_communities$estimator = "1_thamesnosamplesplitting"
names(df_thamesnosamplesplitting_communities)[1] = "error"

df_risvbsamplesplitting_communities = df_full_communities[df_full_communities$iters==10000,][c("risvbsamplesplitting","lambda")]
df_risvbsamplesplitting_communities$estimator = "2_risvbsamplesplitting"
names(df_risvbsamplesplitting_communities)[1] = "error"

df_risvbnosamplesplitting_communities = df_full_communities[df_full_communities$iters==10000,][c("risvbnosamplesplitting","lambda")]
df_risvbnosamplesplitting_communities$estimator = "3_risvbnosamplesplitting"
names(df_risvbnosamplesplitting_communities)[1] = "error"

df_ChibPartitions_communities = df_full_communities[df_full_communities$iters==10000,][c("ChibPartitions","lambda")]
df_ChibPartitions_communities$estimator = "4_ChibPartitions"
names(df_ChibPartitions_communities)[1] = "error"

df_HarmonicMean_communities = df_full_communities[df_full_communities$iters==10000,][c("HarmonicMean","lambda")]
df_HarmonicMean_communities$estimator = "5_HarmonicMean"
names(df_HarmonicMean_communities)[1] = "error"

df_VBEM_communities = df_full_communities[df_full_communities$iters==10000,][c("VBEM","lambda")]
df_VBEM_communities$estimator = "6_VBEM"
names(df_VBEM_communities)[1] = "error"

df_VBEM_correction_communities = df_full_communities[df_full_communities$iters==10000,][c("VBEM_correction","lambda")]
df_VBEM_correction_communities$estimator = "7_VBEMc"
names(df_VBEM_correction_communities)[1] = "error"

df_estimators_communities = rbind(df_thamessamplesplitting_communities,
                                  df_thamesnosamplesplitting_communities,
                                  df_risvbsamplesplitting_communities,
                                  df_risvbnosamplesplitting_communities,
                                  df_ChibPartitions_communities,
                                  df_HarmonicMean_communities,
                                  df_VBEM_communities,
                                  df_VBEM_correction_communities)
plot_communities_varyinglambda = ggplot(df_estimators_communities[(df_estimators_communities$estimator=="0_thamessamplesplitting")|
                                                                    (df_estimators_communities$estimator=="2_risvbsamplesplitting")|
                                                                    (df_estimators_communities$estimator=="4_ChibPartitions")|
                                                                    (df_estimators_communities$estimator=="5_HarmonicMean")|
                                                                    (df_estimators_communities$estimator=="6_VBEM")|
                                                                    (df_estimators_communities$estimator=="7_VBEMc"),],
                                        aes(x=factor(lambda),y=error,fill=estimator)) +
  geom_boxplot() + 
  #ylab("absolute error") + 
  ylab("error") +
  xlab(expression(lambda)) + 
  theme(legend.position = "none")  + 
  labs(title="Communities\n") +
  theme(text = element_text(size = textsize)) +
  scale_fill_manual(values = c("brown","grey","pink","pink3","darkorange2","beige"),
                    labels =  expression("THAMES","RISVB","ChibPartition","HME","ILvb","ILvbc"),
                    name="") +
  guides(fill = guide_legend(nrow = 1))


plot_communities_varyinglambda_withoutVBEMandHME = ggplot(df_estimators_communities[(df_estimators_communities$estimator=="0_thamessamplesplitting")|
                                                                                (df_estimators_communities$estimator=="2_risvbsamplesplitting")|
                                                                                (df_estimators_communities$estimator=="4_ChibPartitions"),],
       aes(x=factor(lambda),y=error,fill=estimator)) +
  geom_boxplot() + 
  #ylab("absolute error") + 
  ylab("error") +
  xlab(expression(lambda)) + 
  theme(legend.position = "none")  + 
  labs(title="Communities\n") +
  theme(text = element_text(size = textsize)) +
  scale_fill_viridis_d(option = "mako",direction = 1,
                       labels=c("THAMES",
                                "RISVB",
                                "ChibPartitions"))+
  guides(fill = guide_legend(nrow = 1))



### disassortative mixing (symmetric) ###
df_full_disassortative = read.csv("data/df_sim01_disassortative.csv")
df_full_disassortative = df_full_disassortative[df_full_disassortative$lambda<=.7,]
df_full_disassortative$lambda = c("",df_full_disassortative$lambda)[-1]
# df_full_disassortative$thames = abs(df_full_disassortative$thames)
# df_full_disassortative$ChibPartitions = abs(df_full_disassortative$ChibPartitions)
# df_full_disassortative$VBEM = abs(df_full_disassortative$VBEM)
# df_full_disassortative$VBEM_correction = abs(df_full_disassortative$VBEM_correction)
plot_disassortative_varyingT_thames = ggplot(df_full_disassortative,
                                             aes(x=factor(iters),y=thamessamplesplitting,fill=lambda)) +
  geom_boxplot() + 
  # ylab("absolute error") +
  ylab("error") +
  xlab("T") + 
  theme(legend.position = "none")  + 
  labs(title="Disassortative Mixing\n(symmetric)") +
  theme(text = element_text(size = textsize)) +
  scale_fill_viridis_d(option = "mako",direction = -1, name = expression(lambda))

plot_disassortative_varyingT_ChibPartitions = ggplot(df_full_disassortative,
                                                     aes(x=factor(iters),y=ChibPartitions,fill=lambda)) +
  geom_boxplot() + 
  #ylab("absolute error") +
  ylab("error") +
  xlab("T") + 
  theme(legend.position = "none")  + 
  labs(title="Disassortative Mixing\n(symmetric)") +
  theme(text = element_text(size = textsize)) +
  scale_fill_viridis_d(option = "mako",direction = -1, name = expression(lambda))

df_thamessamplesplitting_disassortative = df_full_disassortative[df_full_disassortative$iters==10000,][c("thamessamplesplitting","lambda")]
df_thamessamplesplitting_disassortative$estimator = "0_thamessamplesplitting"
names(df_thamessamplesplitting_disassortative)[1] = "error"

df_thamesnosamplesplitting_disassortative = df_full_disassortative[df_full_disassortative$iters==10000,][c("thamesnosamplesplitting","lambda")]
df_thamesnosamplesplitting_disassortative$estimator = "1_thamesnosamplesplitting"
names(df_thamesnosamplesplitting_disassortative)[1] = "error"

df_risvbsamplesplitting_disassortative = df_full_disassortative[df_full_disassortative$iters==10000,][c("risvbsamplesplitting","lambda")]
df_risvbsamplesplitting_disassortative$estimator = "2_risvbsamplesplitting"
names(df_risvbsamplesplitting_disassortative)[1] = "error"

df_risvbnosamplesplitting_disassortative = df_full_disassortative[df_full_disassortative$iters==10000,][c("risvbnosamplesplitting","lambda")]
df_risvbnosamplesplitting_disassortative$estimator = "3_risvbnosamplesplitting"
names(df_risvbnosamplesplitting_disassortative)[1] = "error"

df_ChibPartitions_disassortative = df_full_disassortative[df_full_disassortative$iters==10000,][c("ChibPartitions","lambda")]
df_ChibPartitions_disassortative$estimator = "4_ChibPartitions"
names(df_ChibPartitions_disassortative)[1] = "error"

df_HarmonicMean_disassortative = df_full_disassortative[df_full_disassortative$iters==10000,][c("HarmonicMean","lambda")]
df_HarmonicMean_disassortative$estimator = "5_HarmonicMean"
names(df_HarmonicMean_disassortative)[1] = "error"

df_VBEM_disassortative = df_full_disassortative[df_full_disassortative$iters==10000,][c("VBEM","lambda")]
df_VBEM_disassortative$estimator = "6_VBEM"
names(df_VBEM_disassortative)[1] = "error"

df_VBEM_correction_disassortative = df_full_disassortative[df_full_disassortative$iters==10000,][c("VBEM_correction","lambda")]
df_VBEM_correction_disassortative$estimator = "7_VBEMc"
names(df_VBEM_correction_disassortative)[1] = "error"

df_estimators_disassortative = rbind(df_thamessamplesplitting_disassortative,
                                     df_thamesnosamplesplitting_disassortative,
                                     df_risvbsamplesplitting_disassortative,
                                     df_risvbnosamplesplitting_disassortative,
                                     df_ChibPartitions_disassortative,
                                     df_HarmonicMean_disassortative,
                                     df_VBEM_disassortative,
                                     df_VBEM_correction_disassortative)
plot_disassortative_varyinglambda = ggplot(df_estimators_disassortative[(df_estimators_disassortative$estimator=="0_thamessamplesplitting")|
                                                                          (df_estimators_disassortative$estimator=="2_risvbsamplesplitting")|
                                                                          (df_estimators_disassortative$estimator=="4_ChibPartitions")|
                                                                          (df_estimators_disassortative$estimator=="5_HarmonicMean")|
                                                                          (df_estimators_disassortative$estimator=="6_VBEM")|
                                                                          (df_estimators_disassortative$estimator=="7_VBEMc"),],
                                           aes(x=factor(lambda),y=error,fill=estimator)) +
  geom_boxplot() + 
  # ylab("absolute error") + 
  ylab("error") +
  xlab(expression(lambda)) + 
  theme(legend.position = "none")  + 
  labs(title="Disassortative Mixing\n(symmetric)") +
  theme(text = element_text(size = textsize)) +
  scale_fill_manual(values = c("brown","grey","pink","pink3","darkorange2","beige"),
                    labels =  expression("THAMES","RISVB","ChibPartition","HME","ILvb","ILvbc"),
                    name="") +
  guides(fill = guide_legend(nrow = 1))
plot_disassortative_varyinglambda_withoutVBEMandHME = ggplot(df_estimators_disassortative[(df_estimators_communities$estimator=="0_thamessamplesplitting")|
                                      (df_estimators_communities$estimator=="2_risvbsamplesplitting")|
                                      (df_estimators_communities$estimator=="4_ChibPartitions"),],
       aes(x=factor(lambda),y=error,fill=estimator)) +
  geom_boxplot() + 
  # ylab("absolute error") + 
  ylab("error") +
  xlab(expression(lambda)) + 
  theme(legend.position = "none")  + 
  labs(title="Disassortative Mixing\n(symmetric)") +
  theme(text = element_text(size = textsize)) +
  scale_fill_viridis_d(option = "mako",direction = 1,
                       labels=c("THAMES",
                                "RISVB",
                                "ChibPartitions")) +
  guides(fill = guide_legend(nrow = 1))
#(df_estimators_communities$estimator=="THAMES")|(df_estimators_communities$estimator=="ChibPartitions")


### disassortative mixing (asymmetric) ###
df_full_disassortative_directed = read.csv("data/df_sim01_disassortative_directed.csv")
df_full_disassortative_directed = df_full_disassortative_directed[df_full_disassortative_directed$lambda<=.7,]

df_full_disassortative_directed$lambda = c("",df_full_disassortative_directed$lambda)[-1]
# df_full_disassortative_directed$thames = abs(df_full_disassortative_directed$thames)
# df_full_disassortative_directed$ChibPartitions = abs(df_full_disassortative_directed$ChibPartitions)
# df_full_disassortative_directed$VBEM = abs(df_full_disassortative_directed$VBEM)
# df_full_disassortative_directed$VBEM_correction = abs(df_full_disassortative_directed$VBEM_correction)
plot_disassortative_directed_varyingT_thames = ggplot(df_full_disassortative_directed,
                                                      aes(x=factor(iters),y=thamessamplesplitting,fill=lambda)) +
  geom_boxplot() + 
  # ylab("absolute error") + 
  ylab("error") +
  xlab("T") + 
  theme(legend.position = "none")  + 
  labs(title="Disassortative Mixing\n(asymmetric)") +
  theme(text = element_text(size = textsize)) +
  scale_fill_viridis_d(option = "mako",direction = -1, name = expression(lambda))

plot_disassortative_directed_varyingT_ChibPartitions = ggplot(df_full_disassortative_directed,
                                                              aes(x=factor(iters),y=ChibPartitions,fill=lambda)) +
  geom_boxplot() + 
  # ylab("absolute error") + 
  ylab("error") +
  xlab("T") + 
  theme(legend.position = "none")  + 
  labs(title="Disassortative Mixing\n(asymmetric)") +
  theme(text = element_text(size = textsize)) +
  scale_fill_viridis_d(option = "mako",direction = -1, name = expression(lambda))

df_thamessamplesplitting_disassortative_directed = df_full_disassortative_directed[df_full_disassortative_directed$iters==10000,][c("thamessamplesplitting","lambda")]
df_thamessamplesplitting_disassortative_directed$estimator = "0_thamessamplesplitting"
names(df_thamessamplesplitting_disassortative_directed)[1] = "error"

df_thamesnosamplesplitting_disassortative_directed = df_full_disassortative_directed[df_full_disassortative_directed$iters==10000,][c("thamesnosamplesplitting","lambda")]
df_thamesnosamplesplitting_disassortative_directed$estimator = "1_thamesnosamplesplitting"
names(df_thamesnosamplesplitting_disassortative_directed)[1] = "error"

df_risvbsamplesplitting_disassortative_directed = df_full_disassortative_directed[df_full_disassortative_directed$iters==10000,][c("risvbsamplesplitting","lambda")]
df_risvbsamplesplitting_disassortative_directed$estimator = "2_risvbsamplesplitting"
names(df_risvbsamplesplitting_disassortative_directed)[1] = "error"

df_risvbnosamplesplitting_disassortative_directed = df_full_disassortative_directed[df_full_disassortative_directed$iters==10000,][c("risvbnosamplesplitting","lambda")]
df_risvbnosamplesplitting_disassortative_directed$estimator = "3_risvbnosamplesplitting"
names(df_risvbnosamplesplitting_disassortative_directed)[1] = "error"

df_ChibPartitions_disassortative_directed = df_full_disassortative_directed[df_full_disassortative_directed$iters==10000,][c("ChibPartitions","lambda")]
df_ChibPartitions_disassortative_directed$estimator = "4_ChibPartitions"
names(df_ChibPartitions_disassortative_directed)[1] = "error"

df_HarmonicMean_disassortative_directed = df_full_disassortative_directed[df_full_disassortative_directed$iters==10000,][c("HarmonicMean","lambda")]
df_HarmonicMean_disassortative_directed$estimator = "5_HarmonicMean"
names(df_HarmonicMean_disassortative_directed)[1] = "error"

df_VBEM_disassortative_directed = df_full_disassortative_directed[df_full_disassortative_directed$iters==10000,][c("VBEM","lambda")]
df_VBEM_disassortative_directed$estimator = "6_VBEM"
names(df_VBEM_disassortative_directed)[1] = "error"

df_VBEM_correction_disassortative_directed = df_full_disassortative_directed[df_full_disassortative_directed$iters==10000,][c("VBEM_correction","lambda")]
df_VBEM_correction_disassortative_directed$estimator = "7_VBEMc"
names(df_VBEM_correction_disassortative_directed)[1] = "error"

df_estimators_disassortative_directed = rbind(df_thamessamplesplitting_disassortative_directed,
                                     df_thamesnosamplesplitting_disassortative_directed,
                                     df_risvbsamplesplitting_disassortative_directed,
                                     df_risvbnosamplesplitting_disassortative_directed,
                                     df_ChibPartitions_disassortative_directed,
                                     df_HarmonicMean_disassortative_directed,
                                     df_VBEM_disassortative_directed,
                                     df_VBEM_correction_disassortative_directed)
plot_disassortative_directed_varyinglambda = ggplot(df_estimators_disassortative_directed[(df_estimators_disassortative_directed$estimator=="0_thamessamplesplitting")|
                                                                                            (df_estimators_disassortative_directed$estimator=="2_risvbsamplesplitting")|
                                                                                            (df_estimators_disassortative_directed$estimator=="4_ChibPartitions")|
                                                                                            (df_estimators_disassortative_directed$estimator=="5_HarmonicMean")|
                                                                                            (df_estimators_disassortative_directed$estimator=="6_VBEM")|
                                                                                            (df_estimators_disassortative_directed$estimator=="7_VBEMc"),],
                                           aes(x=factor(lambda),y=error,fill=estimator)) +
  geom_boxplot() + 
  # ylab("absolute error") + 
  ylab("error") +
  xlab(expression(lambda)) + 
  theme(legend.position = "none")  + 
  labs(title="Disassortative Mixing\n(asymmetric)") +
  theme(text = element_text(size = textsize)) +
  scale_fill_manual(values = c("brown","grey","pink","pink3","darkorange2","beige"),
                    labels =  expression("THAMES","RISVB","ChibPartition","HME","ILvb","ILvbc"),
                    name="") +
  guides(fill = guide_legend(nrow = 1))
plot_disassortative_directed_varyinglambda_withoutVBEMandHME = ggplot(df_estimators_disassortative_directed[(df_estimators_disassortative_directed$estimator=="0_thamessamplesplitting")|
                                                                                                              (df_estimators_disassortative_directed$estimator=="2_risvbsamplesplitting")|
                                                                                                              (df_estimators_disassortative_directed$estimator=="4_ChibPartitions"),],
       aes(x=factor(lambda),y=error,fill=estimator)) +
  geom_boxplot() + 
  # ylab("absolute error") + 
  ylab("error") +
  xlab(expression(lambda)) + 
  theme(legend.position = "none")  + 
  labs(title="Disassortative Mixing\n(asymmetric)") +
  theme(text = element_text(size = textsize)) +
  scale_fill_viridis_d(option = "mako",direction = 1,
                       labels=c("THAMES",
                                "RISVB",
                                "ChibPartitions")) +
  guides(fill = guide_legend(nrow = 1))

# save the plots
plot_thames_varyingT = grid.arrange(plot_communities_varyingT_thames + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),
                                    plot_disassortative_varyingT_thames + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),
                                    plot_disassortative_directed_varyingT_thames + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),ncol=3)
ggsave("atelier/plot_thames_varyingT.pdf",plot_thames_varyingT,width=12,height=4,device="pdf")

plot_ChibPartitions_varyingT = grid.arrange(plot_communities_varyingT_ChibPartitions + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),
                                    plot_disassortative_varyingT_ChibPartitions + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),
                                    plot_disassortative_directed_varyingT_ChibPartitions + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),ncol=3)
ggsave("atelier/plot_ChibPartitions_varyingT.pdf",plot_ChibPartitions_varyingT,width=12,height=4,device="pdf")


# get the legend for lambda
# plot_communities_varyingT_thames_dummy = ggplot(df_full_communities,
#                                                 aes(x=factor(iters),y=thames,fill=lambda)) +
#   geom_boxplot() + 
#   #ylab("absolute error") + 
#   ylab("error") +
#   xlab("T") + 
#   theme(legend.position = "bottom")  + 
#   labs(title="Communities\n") +
#   theme(text = element_text(size = textsize))
plot_communities_varyingT_thames_dummy = plot_communities_varyingT_thames + theme(legend.position = "bottom")
panel_varyingT = cowplot::get_plot_component(plot_communities_varyingT_thames_dummy,"guide-box",return_all = TRUE)
plot(panel_varyingT[[3]])

# save the plots
plot_estimators_varyinglambda = grid.arrange(plot_communities_varyinglambda + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),
                                             plot_disassortative_varyinglambda + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),
                                             plot_disassortative_directed_varyinglambda + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),ncol=3)
ggsave("atelier/plot_estimators_varyinglambda.pdf",
       plot_estimators_varyinglambda,width=12,height=4,device="pdf")

# get the legend for the estimators
plot_communities_varyinglambda_dummy = plot_communities_varyinglambda + theme(legend.position = "bottom")
panel_varyinglambda = cowplot::get_plot_component(plot_communities_varyinglambda_dummy,"guide-box",return_all = TRUE)
plot(panel_varyinglambda[[3]])

### very difficult graph ###
df_full_difficultgraph = read.csv("data/df_sim01_difficultgraph.csv")
df_full_difficultgraph = df_full_difficultgraph[(df_full_difficultgraph$lambda<=.8)&
                                                  (df_full_difficultgraph$iters==10000),]
# the value of lambda doesn't matter in this case, it is just a dummy variable

df_thamessamplesplitting_difficultgraph = df_full_difficultgraph[c("thamessamplesplitting","lambda")]
df_thamessamplesplitting_difficultgraph$estimator = "0_thamessamplesplitting"
names(df_thamessamplesplitting_difficultgraph)[1] = "error"

df_thamesnosamplesplitting_difficultgraph = df_full_difficultgraph[c("thamesnosamplesplitting","lambda")]
df_thamesnosamplesplitting_difficultgraph$estimator = "1_thamesnosamplesplitting"
names(df_thamesnosamplesplitting_difficultgraph)[1] = "error"

df_risvbsamplesplitting_difficultgraph = df_full_difficultgraph[c("risvbsamplesplitting","lambda")]
df_risvbsamplesplitting_difficultgraph$estimator = "2_risvbsamplesplitting"
names(df_risvbsamplesplitting_difficultgraph)[1] = "error"

df_risvbnosamplesplitting_difficultgraph = df_full_difficultgraph[c("risvbnosamplesplitting","lambda")]
df_risvbnosamplesplitting_difficultgraph$estimator = "3_risvbnosamplesplitting"
names(df_risvbnosamplesplitting_difficultgraph)[1] = "error"

df_ChibPartitions_difficultgraph = df_full_difficultgraph[c("ChibPartitions","lambda")]
df_ChibPartitions_difficultgraph$estimator = "4_ChibPartitions"
names(df_ChibPartitions_difficultgraph)[1] = "error"

df_HarmonicMean_difficultgraph = df_full_difficultgraph[c("HarmonicMean","lambda")]
df_HarmonicMean_difficultgraph$estimator = "5_HarmonicMean"
names(df_HarmonicMean_difficultgraph)[1] = "error"

df_VBEM_difficultgraph = df_full_difficultgraph[c("VBEM","lambda")]
df_VBEM_difficultgraph$estimator = "6_VBEM"
names(df_VBEM_difficultgraph)[1] = "error"

df_VBEM_correction_difficultgraph = df_full_difficultgraph[c("VBEM_correction","lambda")]
df_VBEM_correction_difficultgraph$estimator = "7_VBEMc"
names(df_VBEM_correction_difficultgraph)[1] = "error"

df_estimators_difficultgraph = rbind(df_thamessamplesplitting_difficultgraph,
                                     df_thamesnosamplesplitting_difficultgraph,
                                     df_risvbsamplesplitting_difficultgraph,
                                     df_risvbnosamplesplitting_difficultgraph,
                                     df_ChibPartitions_difficultgraph,
                                     df_HarmonicMean_difficultgraph,
                                     df_VBEM_difficultgraph,
                                     df_VBEM_correction_difficultgraph)
plot_difficultgraph_varyinglambda_novbem = ggplot(df_estimators_difficultgraph[(df_estimators_difficultgraph$estimator=="0_thamessamplesplitting")|
                                                                    (df_estimators_difficultgraph$estimator=="2_risvbsamplesplitting")|
                                                                    (df_estimators_difficultgraph$estimator=="4_ChibPartitions")|
                                                                    (df_estimators_difficultgraph$estimator=="5_HarmonicMean"),],
                                        aes(x=estimator,y=error,fill=estimator)) +
  geom_boxplot() + 
  #ylab("absolute error") + 
  ylab("error") +
  xlab(" ") + 
  scale_x_discrete(labels = c(
    "0_thamessamplesplitting" = "THAMES",
    "2_risvbsamplesplitting" = "RISVB",
    "4_ChibPartitions" = "ChibPartition",
    "5_HarmonicMean" = "HME"
    
  )) +
  scale_fill_manual(values = c("brown","grey","pink","pink3"),
                    labels =  expression("THAMES","RISVB","ChibPartition","HME"),
                    name="") +
  theme(legend.position = "none")  + 
  theme(text = element_text(size = textsize))
plot_difficultgraph_varyinglambda_vbem = ggplot(df_estimators_difficultgraph[(df_estimators_difficultgraph$estimator=="6_VBEM")|
                                                                               (df_estimators_difficultgraph$estimator=="7_VBEMc"),],
                                        aes(x=estimator,y=error,fill=estimator)) +
  geom_boxplot() + 
  #ylab("absolute error") + 
  ylab("error") +
  xlab(" ") + 
  scale_x_discrete(labels = c(
    "6_VBEM" = "ILvb",
    "7_VBEMc" = "ILvbc"
  )) +
  scale_fill_manual(values = c("darkorange2","beige"),
                    labels =  expression("ILvb","ILvbc"),
                    name="") +
  theme(legend.position = "none")  + 
  theme(text = element_text(size = textsize))
# save the plots
plot_difficultgraph_varyinglambda = grid.arrange(plot_difficultgraph_varyinglambda_novbem + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),
                                             plot_difficultgraph_varyinglambda_vbem + theme(axis.title = element_text(size = textsize_axis), axis.text  = element_text(size = textsize_axis)),ncol=2)

ggsave("atelier/difficultgraph.pdf",
       plot_difficultgraph_varyinglambda,width=12,height=3,device="pdf")
#plot_difficultgraph_varyinglambda_dummy = plot_difficultgraph_varyinglambda + theme(legend.position = "bottom")
#panel_difficultgraph = cowplot::get_plot_component(plot_difficultgraph_varyinglambda_dummy,"guide-box",return_all = TRUE)
#plot(panel_difficultgraph[[3]])
