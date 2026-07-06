rm(list=ls())
source("../../codes/functions.R")

Rcpp::sourceCpp("../../codes/fast_generate_sbm.cpp")
Rcpp::sourceCpp("../../codes/gibbs.cpp")

load(file=paste0("data/adjmat_for_gibbs",".RData"))


for(j in 1:10){
  res_gibbs_list = list()
  for(i in 2:20){
    res_gibbs_list[[i-1]] = gibbs_sampler(adjmat_for_gibbs, i, init = "KMEANS", max_iterations = 12000,seed=i+j*50)
    
    # burn-in of 2000
    res_gibbs_list[[i-1]]$clusterList = res_gibbs_list[[i-1]]$clusterList[-(1:2000)]
    res_gibbs_list[[i-1]]$lps = res_gibbs_list[[i-1]]$lps[-(1:2000)]
  }
  save(res_gibbs_list,file=paste0('data/res_gibbs_',j,'.Rda'))
}
# 1943.049 sec elapsed
#save(res_gibbs_list,file=paste0('res_gibbs','.Rda'))
#load(file=paste0('res_gibbs','.Rda'))
res_gibs_full_lps = matrix(nrow=100000,ncol=20-1)
for(j in 1:10){
  load(file=paste0('data/res_gibbs_',j,'.Rda'))
  for(g in 2:20){
    k=j-1
    res_gibs_full_lps[(k*10000+1):((k+1)*10000),g-1] = res_gibbs_list[[g-1]]$lps
  }
}
save(res_gibs_full_lps,file=paste0('data/res_gibs_full_lps','.Rda'))
load(paste0('data/res_gibs_full_lps','.Rda'))

res_gibs_unique_clusters = matrix(nrow=10,ncol=20-1)
for(j in 1:10){
  load(file=paste0('data/res_gibbs_',j,'.Rda'))
  for(g in 2:20){
    res_gibs_unique_clusters[j,g-1] = length(table(simplify2array(res_gibbs_list[[g-1]]$clusterList)))
  }
}
save(res_gibs_unique_clusters,file=paste0('data/res_gibs_unique_clusters','.Rda'))
load(paste0('data/res_gibs_unique_clusters','.Rda'))