rm(list=ls())
source("../Code/functions.R")

Rcpp::sourceCpp("../Code/fast_generate_sbm.cpp")
Rcpp::sourceCpp("../Code/gibbs.cpp")

library(igraphdata)
data(karate)
karate
adjmat_for_gibbs=as_adjacency_matrix(karate)

for(j in 1:10){
  res_gibbs_list = list()
  for(i in 2:6){
    res_gibbs_list[[i-1]] = gibbs_sampler(adjmat_for_gibbs, i, init = "KMEANS", max_iterations = 12000,seed=i+j*50)

    # burn-in of 2000
    res_gibbs_list[[i-1]]$clusterList = res_gibbs_list[[i-1]]$clusterList[-(1:2000)]
    res_gibbs_list[[i-1]]$lps = res_gibbs_list[[i-1]]$lps[-(1:2000)]
  }
  save(res_gibbs_list,file=paste0('data/res_gibbs_karate_',j,'.Rda'))
}
# 1943.049 sec elapsed
#save(res_gibbs_list,file=paste0('res_gibbs','.Rda'))
#load(file=paste0('res_gibbs','.Rda'))
res_gibs_full_lps = matrix(nrow=100000,ncol=6-1)
for(j in 1:10){
  load(file=paste0('data/res_gibbs_karate_',j,'.Rda'))
  for(g in 2:6){
    k=j-1
    res_gibs_full_lps[(k*10000+1):((k+1)*10000),g-1] = res_gibbs_list[[g-1]]$lps
  }
}
save(res_gibs_full_lps,file=paste0('data/res_gibs_full_lps_karate','.Rda'))
load(paste0('data/res_gibs_full_lps_karate','.Rda'))
