rm(list=ls())
source("../../codes/thames_funcs.R")
source("../../codes/compute_Gibbs_lps_full.R")
Rcpp::sourceCpp("../../codes/compute_lps.cpp")
Rcpp::sourceCpp("../../codes/compute_multinom_lps.cpp")
source("../../codes/VBEM.R")
sourceCpp("../../codes/VBEM.cpp")
### BEGIN G2 ###

load(file=paste0("data/adjmat_for_gibbs",".RData"))
load(file=paste0("data/res_gibs_full_lps",".Rda"))

# res_gibbs_list_r2 = res_gibbs_list$r2
# res_gibbs_list=NULL
# map_r2 = res_gibbs_list_r2$clusterList[[which.max(res_gibbs_list_r2$lps)]]
# table(map_r2)
# length(table(map_r2))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r2$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(G,clustermat,c(map_r2))
# G2_vbem = VBEM_rep(adjmat_for_gibbs, 2, n0=rep(1, 2), eta0=matrix(1, 2, 2), zeta0=matrix(1, 2, 2), max_outer=100, eps_outer=1e-6, nb_rep=10, nb_rep_init=10, directed=TRUE,seed=1)
# marglik_G2_vbem = max(G2_vbem$ELBO)
# 
# SBM_VBEM = VBEM_rep(adjmat_for_gibbs, 2, n0=rep(1, 2), eta0=matrix(1, 2, 2), 
#                     zeta0=matrix(1, 2, 2), max_outer=100, eps_outer=1e-6, 
#                     nb_rep=1, nb_rep_init=10, directed=TRUE,seed=1)
# vbem_marglik_estim = max(SBM_VBEM$ELBO)

# 10 for onlyQT
# 11 for onlyRT
# 12 for RT plus QT
# 

log_marglik_thames_values = numeric(20)
for(G in 2:20){
  load(file=paste0('data/res_gibbs_', ceiling(which.max(res_gibs_full_lps[,G-1]) / 10000),'.Rda'))
  names(res_gibbs_list)=paste0("r",2:20)
  
  res_gibbs_list_r2 = res_gibbs_list[[G-1]]#res_gibbs_list$r2
  res_gibbs_list=NULL
  map_r2 = res_gibbs_list_r2$clusterList[[which.max(res_gibbs_list_r2$lps)]]
  table(map_r2)
  length(table(map_r2))
  
  # compute the label switching
  clustermat = simplify2array(res_gibbs_list_r2$clusterList)
  clustermat = t(clustermat[1,,])
  clustermat_relabelled = relabel_with_ECR(G,clustermat,c(map_r2))
  
  maxcut <- log(.Machine$double.xmax) - log(G)
  mincut <- log(.Machine$double.xmin)
  
  p0 = 1 - mean(sapply(1:G, function(g) mean(rowSums((clustermat_relabelled+1)==g)>0)))
  print(paste0("p0 = ",p0))
  if(p0 == 0){
    log_marglik_thames_values[G] = thamesblock(G, logpost=function(cmat) simplify2array(compute_Gibbs_lps_full(adjmat_for_gibbs, 
                                                                                                               t(adjmat_for_gibbs), ncol(adjmat_for_gibbs), 
                                                                                                               G, 
                                                                                                               t(cmat),
                                                                                                               mincut=mincut,
                                                                                                               maxcut=maxcut)[[2]]),
                                               clustermat,
                                               split=TRUE,
                                               seed=2026)$log_zhat_inv
  } else{
    log_marglik_thames_values[G] = 
      -compute_nobile_identity(logZhatGminus1 = -log_marglik_thames_values[G-1],
                              clustermat = clustermat,
                              dirichlet_vec = rep(1,G),
                              n = nrow(clustermat))
  }
  #marglik_G2_risvb = risvb_estim(adjmat_for_gibbs, G,
  #                               res_gibbs_list_r2$lps,
  #                               clustermat_relabelled)
  #marglik_G2_chibs = compute_ChibPartition(res_gibbs_list_r2$lps, res_gibbs_list_r2$clusterList, G)
  
  # compute the variational Bayes estimator
  #G2_vbem = VBEM_rep(adjmat_for_gibbs, G, n0=rep(1, G), eta0=matrix(1, G, G), zeta0=matrix(1, G, G), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=1, directed=TRUE,seed=1)
  #marglik_G2_vbem = max(G2_vbem$ELBO)
  # marglik_G2_thames = list(log_zhat_inv=read.csv(file="data/df_G2_cop28.csv")$thames)
  # marglik_G2_risvb = list(log_zhat_inv=read.csv(file="data/df_G2_cop28.csv")$risvb)
  # marglik_G2_chibs = read.csv(file="data/df_G2_cop28.csv")$chibs
  
  #df_G2_cop28 = as.data.frame(cbind(marglik_G2_thames$log_zhat_inv,G))
  #names(df_G2_cop28) = c("thames","G")#"risvb","chibs","vbem")
  #write.csv(df_G2_cop28,file=paste0("df_G",G,"_cop28",file_ending,".csv"))
  #read.csv(file=paste0("df_G",G,"_cop28",file_ending,".csv"))
}
names(log_marglik_thames_values) = c(" ",2:20)
log_marglik_thames_values=log_marglik_thames_values[-1]
log_marglik_thames_values[which.max(-log_marglik_thames_values)]
### END G2 ###
# 
# ### BEGIN G3 ###
# 
# load(file=paste0('res_gibbs_', ceiling(which.max(res_gibs_full_lps[,3-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r3 = res_gibbs_list$r3
# res_gibbs_list=NULL
# map_r3 = res_gibbs_list_r3$clusterList[[which.max(res_gibbs_list_r3$lps)]]
# table(map_r3)
# length(table(map_r3))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r3$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(3,clustermat,c(map_r3))
# 
# marglik_G3_thames = thames_sbm(adjmat_for_gibbs,3,res_gibbs_list_r3$lps,clustermat_relabelled)
# marglik_G3_risvb = risvb_estim(adjmat_for_gibbs, 3,
#                                res_gibbs_list_r3$lps,
#                                clustermat_relabelled)
# marglik_G3_chibs = compute_ChibPartition(res_gibbs_list_r3$lps, res_gibbs_list_r3$clusterList, 3)
# # compute the variational Bayes estimator
# marglik_G3_vbem = VBEM_rep(adjmat_for_gibbs, 3, n0=rep(1, 3), eta0=matrix(1, 3, 3), zeta0=matrix(1, 3, 3), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=1, directed=TRUE,seed=1)
# marglik_G3_vbem = max(marglik_G3_vbem$max_ELBO)
# 
# df_G3_cop28 = as.data.frame(cbind(marglik_G3_thames$log_zhat_inv,
#                                   marglik_G3_risvb$log_zhat_inv,
#                                   marglik_G3_chibs,
#                                   marglik_G3_vbem))
# names(df_G3_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G3_cop28,file="data/df_G3_cop28.csv")
# read.csv(file="data/df_G3_cop28.csv")
# 
# ### END G3 ###
# 
# ### BEGIN G4 ###
# load(file=paste0('res_gibbs_', ceiling(which.max(res_gibs_full_lps[,4-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r4 = res_gibbs_list$r4
# res_gibbs_list=NULL
# map_r4 = res_gibbs_list_r4$clusterList[[which.max(res_gibbs_list_r4$lps)]]
# table(map_r4)
# length(table(map_r4))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r4$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(4,clustermat,c(map_r4))
# 
# marglik_G4_thames = thames_sbm(adjmat_for_gibbs,4,res_gibbs_list_r4$lps,clustermat_relabelled)
# marglik_G4_risvb = risvb_estim(adjmat_for_gibbs, 4,
#                                res_gibbs_list_r4$lps,
#                                clustermat_relabelled)
# marglik_G4_chibs = compute_ChibPartition(res_gibbs_list_r4$lps, res_gibbs_list_r4$clusterList, 4)
# G4_vbem = VBEM_rep(adjmat_for_gibbs, 4, n0=rep(1, 4), eta0=matrix(1, 4, 4), zeta0=matrix(1, 4, 4), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G4_vbem = max(G4_vbem$max_ELBO)
# 
# df_G4_cop28 = as.data.frame(cbind(marglik_G4_thames$log_zhat_inv,
#                                   marglik_G4_risvb$log_zhat_inv,
#                                   marglik_G4_chibs,
#                                   marglik_G4_vbem))
# names(df_G4_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G4_cop28,file="data/df_G4_cop28.csv")
# read.csv(file="data/df_G4_cop28.csv")
# 
# ### END G4 ###
# 
# ### BEGIN G5 ###
# 
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,5-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r5 = res_gibbs_list$r5
# res_gibbs_list=NULL
# map_r5 = res_gibbs_list_r5$clusterList[[which.max(res_gibbs_list_r5$lps)]]
# table(map_r5)
# length(table(map_r5))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r5$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(5,clustermat,c(map_r5))
# 
# marglik_G5_thames = thames_sbm(adjmat_for_gibbs,5,res_gibbs_list_r5$lps,clustermat_relabelled)
# marglik_G5_risvb = risvb_estim(adjmat_for_gibbs, 5,
#                                res_gibbs_list_r5$lps,
#                                clustermat_relabelled)
# marglik_G5_chibs = compute_ChibPartition(res_gibbs_list_r5$lps, res_gibbs_list_r5$clusterList, 5)
# 
# G5_vbem = VBEM_rep(adjmat_for_gibbs, 5, n0=rep(1, 5), eta0=matrix(1, 5, 5), zeta0=matrix(1, 5, 5), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G5_vbem = max(G5_vbem$max_ELBO)
# 
# df_G5_cop28 = as.data.frame(cbind(marglik_G5_thames$log_zhat_inv,
#                                   marglik_G5_risvb$log_zhat_inv,
#                                   marglik_G5_chibs,
#                                   marglik_G5_vbem))
# names(df_G5_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G5_cop28,file="data/df_G5_cop28.csv")
# read.csv(file="data/df_G5_cop28.csv")
# ### END G5 ###
# 
# ### BEGIN G6 ###
# 
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,6-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r6 = res_gibbs_list$r6
# res_gibbs_list=NULL
# map_r6 = res_gibbs_list_r6$clusterList[[which.max(res_gibbs_list_r6$lps)]]
# table(map_r6)
# length(table(map_r6))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r6$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(6,clustermat,c(map_r6))
# 
# marglik_G6_thames = thames_sbm(adjmat_for_gibbs,6,res_gibbs_list_r6$lps,clustermat_relabelled)
# marglik_G6_risvb = risvb_estim(adjmat_for_gibbs, 6,
#                                res_gibbs_list_r6$lps,
#                                clustermat_relabelled)
# marglik_G6_chibs = compute_ChibPartition(res_gibbs_list_r6$lps, res_gibbs_list_r6$clusterList, 6)
# 
# # compute the variational Bayes estimator
# G6_vbem = VBEM_rep(adjmat_for_gibbs, 6, n0=rep(1, 6), eta0=matrix(1, 6, 6), zeta0=matrix(1, 6, 6), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G6_vbem = max(G6_vbem$max_ELBO)
# 
# df_G6_cop28 = as.data.frame(cbind(marglik_G6_thames$log_zhat_inv,
#                                   marglik_G6_risvb$log_zhat_inv,
#                                   marglik_G6_chibs,
#                                   marglik_G6_vbem))
# names(df_G6_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G6_cop28,file="data/df_G6_cop28.csv")
# read.csv(file="data/df_G6_cop28.csv")
# 
# ### END G6 ###
# 
# ### BEGIN G7 ###
# 
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,7-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r7 = res_gibbs_list$r7
# res_gibbs_list=NULL
# map_r7 = res_gibbs_list_r7$clusterList[[which.max(res_gibbs_list_r7$lps)]]
# table(map_r7)
# length(table(map_r7))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r7$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(7,clustermat,c(map_r7))
# 
# marglik_G7_thames = thames_sbm(adjmat_for_gibbs,7,res_gibbs_list_r7$lps,clustermat_relabelled)
# marglik_G7_risvb = risvb_estim(adjmat_for_gibbs, 7,
#                                res_gibbs_list_r7$lps,
#                                clustermat_relabelled)
# marglik_G7_chibs = compute_ChibPartition(res_gibbs_list_r7$lps, res_gibbs_list_r7$clusterList, 7)
# G7_vbem = VBEM_rep(adjmat_for_gibbs, 7, n0=rep(1, 7), eta0=matrix(1, 7, 7), zeta0=matrix(1, 7, 7), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G7_vbem = max(G7_vbem$max_ELBO)
# 
# df_G7_cop28 = as.data.frame(cbind(marglik_G7_thames$log_zhat_inv,
#                                   marglik_G7_risvb$log_zhat_inv,
#                                   marglik_G7_chibs,
#                                   marglik_G7_vbem))
# names(df_G7_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G7_cop28,file="data/df_G7_cop28.csv")
# read.csv(file="data/df_G7_cop28.csv")
# ### END G7 ###
# 
# ### BEGIN G8 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,8-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r8 = res_gibbs_list$r8
# res_gibbs_list=NULL
# map_r8 = res_gibbs_list_r8$clusterList[[which.max(res_gibbs_list_r8$lps)]]
# table(map_r8)
# length(table(map_r8))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r8$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(8,clustermat,c(map_r8))
# 
# marglik_G8_thames = thames_sbm(adjmat_for_gibbs,8,res_gibbs_list_r8$lps,clustermat_relabelled)
# marglik_G8_risvb = risvb_estim(adjmat_for_gibbs, 8,
#                                res_gibbs_list_r8$lps,
#                                clustermat_relabelled)
# marglik_G8_chibs = compute_ChibPartition(res_gibbs_list_r8$lps, res_gibbs_list_r8$clusterList, 8)
# G8_vbem = VBEM_rep(adjmat_for_gibbs, 8, n0=rep(1, 8), eta0=matrix(1, 8, 8), zeta0=matrix(1, 8, 8), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G8_vbem = max(G8_vbem$max_ELBO)
# df_G8_cop28 = as.data.frame(cbind(marglik_G8_thames$log_zhat_inv,
#                                   marglik_G8_risvb$log_zhat_inv,
#                                   marglik_G8_chibs,
#                                   marglik_G8_vbem))
# names(df_G8_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G8_cop28,file="data/df_G8_cop28.csv")
# read.csv(file="data/df_G8_cop28.csv")
# 
# ### END G8 ###
# 
# ### BEGIN G9 ###
# 
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,9-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r9 = res_gibbs_list$r9
# res_gibbs_list=NULL
# map_r9 = res_gibbs_list_r9$clusterList[[which.max(res_gibbs_list_r9$lps)]]
# table(map_r9)
# length(table(map_r9))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r9$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(9,clustermat,c(map_r9))
# 
# marglik_G9_thames = thames_sbm(adjmat_for_gibbs,9,res_gibbs_list_r9$lps,clustermat_relabelled)
# marglik_G9_risvb = risvb_estim(adjmat_for_gibbs, 9,
#                                res_gibbs_list_r9$lps,
#                                clustermat_relabelled)
# marglik_G9_chibs = compute_ChibPartition(res_gibbs_list_r9$lps, res_gibbs_list_r9$clusterList, 9)
# G9_vbem = VBEM_rep(adjmat_for_gibbs, 9, n0=rep(1, 9), eta0=matrix(1, 9, 9), zeta0=matrix(1, 9, 9), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G9_vbem = max(G9_vbem$max_ELBO)
# 
# df_G9_cop28 = as.data.frame(cbind(marglik_G9_thames$log_zhat_inv,
#                                   marglik_G9_risvb$log_zhat_inv,
#                                   marglik_G9_chibs,
#                                   marglik_G9_vbem))
# names(df_G9_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G9_cop28,file="data/df_G9_cop28.csv")
# read.csv(file="data/df_G9_cop28.csv")
# 
# 
# ### END G9 ###
# 
# ### BEGIN G10 ###
# load(file=paste0('res_gibbs_', ceiling(which.max(res_gibs_full_lps[,10-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r10 = res_gibbs_list$r10
# res_gibbs_list=NULL
# map_r10 = res_gibbs_list_r10$clusterList[[which.max(res_gibbs_list_r10$lps)]]
# table(map_r10)
# length(table(map_r10))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r10$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(10,clustermat,c(map_r10))
# 
# marglik_G10_thames = thames_sbm(adjmat_for_gibbs,10,res_gibbs_list_r10$lps,clustermat_relabelled)
# marglik_G10_risvb = risvb_estim(adjmat_for_gibbs, 10,
#                                 res_gibbs_list_r10$lps,
#                                 clustermat_relabelled)
# marglik_G10_chibs = compute_ChibPartition(res_gibbs_list_r10$lps, res_gibbs_list_r10$clusterList, 10)
# G10_vbem = VBEM_rep(adjmat_for_gibbs, 10, n0=rep(1, 10), eta0=matrix(1, 10, 10), zeta0=matrix(1, 10, 10), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G10_vbem = max(G10_vbem$max_ELBO)
# 
# df_G10_cop28 = as.data.frame(cbind(marglik_G10_thames$log_zhat_inv,
#                                    marglik_G10_risvb$log_zhat_inv,
#                                    marglik_G10_chibs,
#                                    marglik_G10_vbem))
# names(df_G10_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G10_cop28,file="data/df_G10_cop28.csv")
# read.csv(file="data/df_G10_cop28.csv")
# ### END G10 ###
# 
# ### BEGIN G11 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,11-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r11 = res_gibbs_list$r11
# res_gibbs_list=NULL
# map_r11 = res_gibbs_list_r11$clusterList[[which.max(res_gibbs_list_r11$lps)]]
# table(map_r11)
# length(table(map_r11))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r11$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(11,clustermat,c(map_r11))
# 
# marglik_G11_thames = thames_sbm(adjmat_for_gibbs,11,res_gibbs_list_r11$lps,clustermat_relabelled)
# marglik_G11_risvb = risvb_estim(adjmat_for_gibbs, 11,
#                                 res_gibbs_list_r11$lps,
#                                 clustermat_relabelled)
# marglik_G11_chibs = compute_ChibPartition(res_gibbs_list_r11$lps, res_gibbs_list_r11$clusterList, 11)
# G11_vbem = VBEM_rep(adjmat_for_gibbs, 11, n0=rep(1, 11), eta0=matrix(1, 11, 11), zeta0=matrix(1, 11, 11), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G11_vbem = max(G11_vbem$max_ELBO)
# 
# df_G11_cop28 = as.data.frame(cbind(marglik_G11_thames$log_zhat_inv,
#                                    marglik_G11_risvb$log_zhat_inv,
#                                    marglik_G11_chibs,
#                                    marglik_G11_vbem))
# names(df_G11_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G11_cop28,file="data/df_G11_cop28.csv")
# read.csv(file="data/df_G11_cop28.csv")
# ### END G11 ###
# 
# ### BEGIN G12 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,12-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r12 = res_gibbs_list$r12
# res_gibbs_list=NULL
# map_r12 = res_gibbs_list_r12$clusterList[[which.max(res_gibbs_list_r12$lps)]]
# table(map_r12)
# length(table(map_r12))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r12$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(12,clustermat,c(map_r12))
# 
# marglik_G12_thames = thames_sbm(adjmat_for_gibbs,12,res_gibbs_list_r12$lps,clustermat_relabelled)
# marglik_G12_risvb = risvb_estim(adjmat_for_gibbs, 12,
#                                 res_gibbs_list_r12$lps,
#                                 clustermat_relabelled)
# marglik_G12_chibs = compute_ChibPartition(res_gibbs_list_r12$lps, res_gibbs_list_r12$clusterList, 12)
# G12_vbem = VBEM_rep(adjmat_for_gibbs, 12, n0=rep(1, 12), eta0=matrix(1, 12, 12), zeta0=matrix(1, 12, 12), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G12_vbem = max(G12_vbem$max_ELBO)
# 
# df_G12_cop28 = as.data.frame(cbind(marglik_G12_thames$log_zhat_inv,
#                                    marglik_G12_risvb$log_zhat_inv,
#                                    marglik_G12_chibs,
#                                    marglik_G12_vbem))
# names(df_G12_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G12_cop28,file="data/df_G12_cop28.csv")
# read.csv(file="data/df_G12_cop28.csv")
# ### END G12 ###
# 
# ### BEGIN G13 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,13-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r13 = res_gibbs_list$r13
# res_gibbs_list=NULL
# map_r13 = res_gibbs_list_r13$clusterList[[which.max(res_gibbs_list_r13$lps)]]
# table(map_r13)
# length(table(map_r13))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r13$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(13,clustermat,c(map_r13))
# 
# p0_G13 = mean(sapply(1:13, function(g) mean(rowSums((clustermat_relabelled+1)==g)>0)))
# 
# marglik_G13_thames = list(log_zhat_inv=-(log((13-1)/(13-1+nrow(adjmat_for_gibbs)))-marglik_G12_thames$log_zhat_inv-log(p0_G13)))
# marglik_G13_risvb = list(log_zhat_inv=-(log((13-1)/(13-1+nrow(adjmat_for_gibbs)))-marglik_G12_risvb$log_zhat_inv-log(p0_G13)))
# marglik_G13_chibs = -(log((13-1)/(13-1+nrow(adjmat_for_gibbs)))-marglik_G12_chibs-log(p0_G13))
# G13_vbem = VBEM_rep(adjmat_for_gibbs, 13, n0=rep(1, 13), eta0=matrix(1, 13, 13), zeta0=matrix(1, 13, 13), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G13_vbem = max(G13_vbem$max_ELBO)
# 
# df_G13_cop28 = as.data.frame(cbind(marglik_G13_thames$log_zhat_inv,
#                                    marglik_G13_risvb$log_zhat_inv,
#                                    marglik_G13_chibs,
#                                    marglik_G13_vbem))
# names(df_G13_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G13_cop28,file="data/df_G13_cop28.csv")
# read.csv(file="data/df_G13_cop28.csv")
# ### END G13 ###
# 
# ### BEGIN G14 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,14-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r14 = res_gibbs_list$r14
# res_gibbs_list=NULL
# map_r14 = res_gibbs_list_r14$clusterList[[which.max(res_gibbs_list_r14$lps)]]
# table(map_r14)
# length(table(map_r14))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r14$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(14,clustermat,c(map_r14))
# 
# p0_G14 = mean(sapply(1:14, function(g) mean(rowSums((clustermat_relabelled+1)==g)>0)))
# 
# marglik_G14_thames = list(log_zhat_inv=-(log((14-1)/(14-1+nrow(adjmat_for_gibbs)))-marglik_G13_thames$log_zhat_inv-log(p0_G14)))
# marglik_G14_risvb = list(log_zhat_inv=-(log((14-1)/(14-1+nrow(adjmat_for_gibbs)))-marglik_G13_risvb$log_zhat_inv-log(p0_G14)))
# marglik_G14_chibs = log_zhat_inv=-(log((14-1)/(14-1+nrow(adjmat_for_gibbs)))-marglik_G13_chibs-log(p0_G14))
# G14_vbem = VBEM_rep(adjmat_for_gibbs, 14, n0=rep(1, 14), eta0=matrix(1, 14, 14), zeta0=matrix(1, 14, 14), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G14_vbem = max(G14_vbem$max_ELBO)
# 
# df_G14_cop28 = as.data.frame(cbind(marglik_G14_thames$log_zhat_inv,
#                                    marglik_G14_risvb$log_zhat_inv,
#                                    marglik_G14_chibs,
#                                    marglik_G14_vbem))
# names(df_G14_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G14_cop28,file="data/df_G14_cop28.csv")
# read.csv(file="data/df_G14_cop28.csv")
# ### END G14 ###
# 
# ### BEGIN G15 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,15-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r15 = res_gibbs_list$r15
# res_gibbs_list=NULL
# map_r15 = res_gibbs_list_r15$clusterList[[which.max(res_gibbs_list_r15$lps)]]
# table(map_r15)
# length(table(map_r15))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r15$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(15,clustermat,c(map_r15))
# 
# p0_G15 = mean(sapply(1:15, function(g) mean(rowSums((clustermat_relabelled+1)==g)>0)))
# 
# marglik_G15_thames = list(log_zhat_inv=-(log((15-1)/(15-1+nrow(adjmat_for_gibbs)))-marglik_G14_thames$log_zhat_inv-log(p0_G15)))
# marglik_G15_risvb = list(log_zhat_inv=-(log((15-1)/(15-1+nrow(adjmat_for_gibbs)))-marglik_G14_risvb$log_zhat_inv-log(p0_G15)))
# marglik_G15_chibs = -(log((15-1)/(15-1+nrow(adjmat_for_gibbs)))-marglik_G14_chibs-log(p0_G15))
# G15_vbem = VBEM_rep(adjmat_for_gibbs, 15, n0=rep(1, 15), eta0=matrix(1, 15, 15), zeta0=matrix(1, 15, 15), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G15_vbem = max(G15_vbem$max_ELBO)
# 
# df_G15_cop28 = as.data.frame(cbind(marglik_G15_thames$log_zhat_inv,
#                                    marglik_G15_risvb$log_zhat_inv,
#                                    marglik_G15_chibs,
#                                    marglik_G15_vbem))
# names(df_G15_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G15_cop28,file="data/df_G15_cop28.csv")
# read.csv(file="data/df_G15_cop28.csv")
# ### END G15 ###
# 
# ### BEGIN G16 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,16-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r16 = res_gibbs_list$r16
# res_gibbs_list=NULL
# map_r16 = res_gibbs_list_r16$clusterList[[which.max(res_gibbs_list_r16$lps)]]
# table(map_r16)
# length(table(map_r16))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r16$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(16,clustermat,c(map_r16))
# 
# p0_G16 = mean(sapply(1:16, function(g) mean(rowSums((clustermat_relabelled+1)==g)>0)))
# 
# marglik_G16_thames = list(log_zhat_inv=-(log((16-1)/(16-1+nrow(adjmat_for_gibbs)))-marglik_G15_thames$log_zhat_inv-log(p0_G16)))
# marglik_G16_risvb = list(log_zhat_inv=-(log((16-1)/(16-1+nrow(adjmat_for_gibbs)))-marglik_G15_risvb$log_zhat_inv-log(p0_G16)))
# marglik_G16_chibs = -(log((16-1)/(16-1+nrow(adjmat_for_gibbs)))-marglik_G15_chibs-log(p0_G16))
# G16_vbem = VBEM_rep(adjmat_for_gibbs, 16, n0=rep(1, 16), eta0=matrix(1, 16, 16), zeta0=matrix(1, 16, 16), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G16_vbem = max(G16_vbem$max_ELBO)
# 
# df_G16_cop28 = as.data.frame(cbind(marglik_G16_thames$log_zhat_inv,
#                                    marglik_G16_risvb$log_zhat_inv,
#                                    marglik_G16_chibs,
#                                    marglik_G16_vbem))
# names(df_G16_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G16_cop28,file="data/df_G16_cop28.csv")
# read.csv(file="data/df_G16_cop28.csv")
# ### END G16 ###
# 
# ### BEGIN G17 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,17-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r17 = res_gibbs_list$r17
# res_gibbs_list=NULL
# map_r17 = res_gibbs_list_r17$clusterList[[which.max(res_gibbs_list_r17$lps)]]
# table(map_r17)
# length(table(map_r17))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r17$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(17,clustermat,c(map_r17))
# 
# p0_G17 = mean(sapply(1:17, function(g) mean(rowSums((clustermat_relabelled+1)==g)>0)))
# 
# marglik_G17_thames = list(log_zhat_inv=-(log((17-1)/(17-1+nrow(adjmat_for_gibbs)))-marglik_G16_thames$log_zhat_inv-log(p0_G17)))
# marglik_G17_risvb = list(log_zhat_inv=-(log((17-1)/(17-1+nrow(adjmat_for_gibbs)))-marglik_G16_risvb$log_zhat_inv-log(p0_G17)))
# marglik_G17_chibs = -(log((17-1)/(17-1+nrow(adjmat_for_gibbs)))-marglik_G16_chibs-log(p0_G17))
# G17_vbem = VBEM_rep(adjmat_for_gibbs, 17, n0=rep(1, 17), eta0=matrix(1, 17, 17), zeta0=matrix(1, 17, 17), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G17_vbem = max(G17_vbem$max_ELBO)
# 
# df_G17_cop28 = as.data.frame(cbind(marglik_G17_thames$log_zhat_inv,
#                                    marglik_G17_risvb$log_zhat_inv,
#                                    marglik_G17_chibs,
#                                    marglik_G17_vbem))
# names(df_G17_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G17_cop28,file="data/df_G17_cop28.csv")
# read.csv(file="data/df_G17_cop28.csv")
# ### END G17 ###
# 
# ### BEGIN G18 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,18-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r18 = res_gibbs_list$r18
# res_gibbs_list=NULL
# map_r18 = res_gibbs_list_r18$clusterList[[which.max(res_gibbs_list_r18$lps)]]
# table(map_r18)
# length(table(map_r18))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r18$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(18,clustermat,c(map_r18))
# 
# p0_G18 = mean(sapply(1:18, function(g) mean(rowSums((clustermat_relabelled+1)==g)>0)))
# 
# marglik_G18_thames = list(log_zhat_inv=-(log((18-1)/(18-1+nrow(adjmat_for_gibbs)))-marglik_G17_thames$log_zhat_inv-log(p0_G18)))
# marglik_G18_risvb = list(log_zhat_inv=-(log((18-1)/(18-1+nrow(adjmat_for_gibbs)))-marglik_G17_risvb$log_zhat_inv-log(p0_G18)))
# marglik_G18_chibs = -(log((18-1)/(18-1+nrow(adjmat_for_gibbs)))-marglik_G17_chibs-log(p0_G18))
# G18_vbem = VBEM_rep(adjmat_for_gibbs, 18, n0=rep(1, 18), eta0=matrix(1, 18, 18), zeta0=matrix(1, 18, 18), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G18_vbem = max(G18_vbem$max_ELBO)
# 
# df_G18_cop28 = as.data.frame(cbind(marglik_G18_thames$log_zhat_inv,
#                                    marglik_G18_risvb$log_zhat_inv,
#                                    marglik_G18_chibs,
#                                    marglik_G18_vbem))
# names(df_G18_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G18_cop28,file="data/df_G18_cop28.csv")
# read.csv(file="data/df_G18_cop28.csv")
# ### END G18 ###
# 
# ### BEGIN G19 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,19-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r19 = res_gibbs_list$r19
# res_gibbs_list=NULL
# map_r19 = res_gibbs_list_r19$clusterList[[which.max(res_gibbs_list_r19$lps)]]
# table(map_r19)
# length(table(map_r19))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r19$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(19,clustermat,c(map_r19))
# 
# p0_G19 = mean(sapply(1:19, function(g) mean(rowSums((clustermat_relabelled+1)==g)>0)))
# 
# marglik_G19_thames = list(log_zhat_inv=-(log((19-1)/(19-1+nrow(adjmat_for_gibbs)))-marglik_G18_thames$log_zhat_inv-log(p0_G19)))
# marglik_G19_risvb = list(log_zhat_inv=-(log((19-1)/(19-1+nrow(adjmat_for_gibbs)))-marglik_G18_risvb$log_zhat_inv-log(p0_G19)))
# marglik_G19_chibs = -(log((19-1)/(19-1+nrow(adjmat_for_gibbs)))-marglik_G18_chibs-log(p0_G19))
# G19_vbem = VBEM_rep(adjmat_for_gibbs, 19, n0=rep(1, 19), eta0=matrix(1, 19, 19), zeta0=matrix(1, 19, 19), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G19_vbem = max(G19_vbem$max_ELBO)
# 
# df_G19_cop28 = as.data.frame(cbind(marglik_G19_thames$log_zhat_inv,
#                                    marglik_G19_risvb$log_zhat_inv,
#                                    marglik_G19_chibs,
#                                    marglik_G19_vbem))
# names(df_G19_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G19_cop28,file="data/df_G19_cop28.csv")
# read.csv(file="data/df_G19_cop28.csv")
# ### END G19 ###
# 
# ### BEGIN G20 ###
# load(file=paste0('res_gibbs_',ceiling(which.max(res_gibs_full_lps[,20-1]) / 10000),'.Rda'))
# names(res_gibbs_list)=paste0("r",2:20)
# 
# res_gibbs_list_r20 = res_gibbs_list$r20
# res_gibbs_list=NULL
# map_r20 = res_gibbs_list_r20$clusterList[[which.max(res_gibbs_list_r20$lps)]]
# table(map_r20)
# length(table(map_r20))
# 
# # compute the label switching
# clustermat = simplify2array(res_gibbs_list_r20$clusterList)
# clustermat = t(clustermat[1,,])
# clustermat_relabelled = relabel_with_ECR(20,clustermat,c(map_r20))
# 
# p0_G20 = mean(sapply(1:20, function(g) mean(rowSums((clustermat_relabelled+1)==g)>0)))
# 
# marglik_G20_thames = list(log_zhat_inv=-(log((20-1)/(20-1+nrow(adjmat_for_gibbs)))-marglik_G19_thames$log_zhat_inv-log(p0_G20)))
# marglik_G20_risvb = list(log_zhat_inv=-(log((20-1)/(20-1+nrow(adjmat_for_gibbs)))-marglik_G19_risvb$log_zhat_inv-log(p0_G20)))
# marglik_G20_chibs = -(log((20-1)/(20-1+nrow(adjmat_for_gibbs)))-marglik_G19_chibs-log(p0_G20))
# G20_vbem = VBEM_rep(adjmat_for_gibbs, 20, n0=rep(1, 20), eta0=matrix(1, 20, 20), zeta0=matrix(1, 20, 20), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=5, directed=TRUE,seed=1)
# marglik_G20_vbem = max(G20_vbem$max_ELBO)
# 
# df_G20_cop28 = as.data.frame(cbind(marglik_G20_thames$log_zhat_inv,
#                                    marglik_G20_risvb$log_zhat_inv,
#                                    marglik_G20_chibs,
#                                    marglik_G20_vbem))
# names(df_G20_cop28) = c("thames","risvb","chibs","vbem")
# write.csv(df_G20_cop28,file="data/df_G20_cop28.csv")
# read.csv(file="data/df_G20_cop28.csv")
# ### END G20 ###
# 
# df_full_cop28 = rbind(df_G2_cop28,
#                       df_G3_cop28,
#                       df_G4_cop28,
#                       df_G5_cop28,
#                       df_G6_cop28,
#                       df_G7_cop28,
#                       df_G8_cop28,
#                       df_G9_cop28,
#                       df_G10_cop28,
#                       df_G11_cop28,
#                       df_G12_cop28,
#                       df_G13_cop28,
#                       df_G14_cop28,
#                       df_G15_cop28,
#                       df_G16_cop28,
#                       df_G17_cop28,
#                       df_G18_cop28,
#                       df_G19_cop28,
#                       df_G20_cop28)
# # optimal for G=12 when looking at the THAMES or ChibPartitions
# write.csv(df_full_cop28,file="df_full_cop28.csv")
# df_full_cop28=read.csv(file="df_full_cop28.csv")
# plot(2:20,-df_full_cop28$thames,col="red",ylim=c(min(-df_full_cop28$risvb),max(-df_full_cop28$risvb)))
# which.max(-df_full_cop28$thames) + 1
# points(2:20,-df_full_cop28$risvb)
# 
# points((2:20),-df_full_cop28$chibs,col="blue")
# 
# # this makes no sense
# plot((2:20),-df_full_cop28$vbem,col="purple")
# 
# max(-df_full_cop28$risvb)
# min(-df_full_cop28$risvb)
# df_full_cop28$chibs
# 
# #########
