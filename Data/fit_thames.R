rm(list=ls())
source("../Code/thames_funcs.R")
source("../Code/compute_Gibbs_lps_full.R")
Rcpp::sourceCpp("../Code/compute_lps.cpp")
Rcpp::sourceCpp("../Code/compute_multinom_lps.cpp")
source("../Code/VBEM.R")
sourceCpp("../Code/VBEM.cpp")
library(thamesblock)
### BEGIN G2 ###

load(file=paste0("data/adjmat_for_gibbs",".RData"))
load(file=paste0("data/res_gibs_full_lps",".Rda"))

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
  clustermat_relabelled = thamesblock:::relabel_with_ECR(G,clustermat,c(map_r2))

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
                              size_dataset = nrow(clustermat))
  }
}
names(log_marglik_thames_values) = c(" ",2:20)
log_marglik_thames_values=log_marglik_thames_values[-1]
log_marglik_thames_values[which.max(-log_marglik_thames_values)]
