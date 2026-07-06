# test the influence of the number of iterations

rm(list=ls())
source("functions.R")
source("VBEM.R")

library(igraph)
library(dplyr)
library(ggplot2)



n_exp = 10
beta = seq(0.3, 0.4, length=n_exp)
#beta = seq(0.1, 0.45, length=n_exp)

n_repeats = 10

n_graphs=20

nb_methods = 4

nsamples = 10000

ari_all_methods = matrix(0, n_exp*n_repeats*nb_methods, 5)

colnames(ari_all_methods) = c("beta", "graph", "Methods", "ari","lpsOfSolution")

index = 0
#set.seed(2025)
for(m in 1:n_repeats){
  for(i in 1:n_exp) {
    cat("beta = ", beta[i], "\n")
    
    ######## set simulation parameters
    lambda = beta[i]
    epsilon = 0.01
    n = 10#400
    K = 2#6
    
    
    # ######## compute density as a reference for the settings below ######## 
    PI = rep(1/K, K)
    mu = matrix(epsilon, K, K)
    diag(mu) = lambda
    rho_ref = as.numeric(PI%*%mu%*%PI) # the quantity used below for normalization
    
    
    # ######## Setting 1: small-scale community structures #########
    #setting = "setting1"
    
    # ######### Setting 2: small-scale community structures with a hub cluster and unbalanced partitions #########
    # setting = "setting2"
    
    # PI = 0.7^(K:1)
    # PI = PI / sum(PI)
    # 
    # mu = matrix(epsilon, K, K)
    # diag(mu) = lambda
    # mu[1, ] = lambda
    # mu[, 1] = lambda
    # 
    # rho_tmp = as.numeric(PI%*%mu%*%PI)
    # mu = rho_ref*mu/rho_tmp
    
    
    # ######## Setting 3: star patterns with balanced partitions #########
    # setting = "setting3"
    # 
    PI = rep(1/K, K)
    mu = matrix(1e-3, K, K)
    diag(mu) = epsilon
    for(k in seq(1, K-1, by=2)) {
      mu[k, k+1] = lambda
      mu[k+1, k] = lambda
    }

    rho_tmp = as.numeric(PI%*%mu%*%PI)
    mu = rho_ref*mu/rho_tmp
    
    ######################## Generate data ####################################
    set.seed(m)
    DATA = fastGenerateSBM(n, PI, mu, directed = TRUE)
    #DATA = fastGenerateSBM(13000, PI, mu, directed = TRUE)
    #DATA$X = DATA$X[1:n,1:n]
    #DATA$cl=DATA$cl[1:n]
    res_random = list(cl=NULL,lps=-Inf)
    res_kmeans = list(cl=NULL,lps=-Inf)
    res_vbem = list(cl=NULL,lps=-Inf)
    res_gibbs_vbem = list(cl=NULL,lps=-Inf)
    
    for(j in 1:n_graphs) {
      cat("graph ", j, "\n")
      
      res = gibbs_sampler(DATA$X, K, init = "random", max_iterations = nsamples,seed=j)
      
      lps = res$lps
      clusterList = res$clusterList
      cl_gibbs= clusterList[[which.max(lps)]]
      
      #index = index + 1
      #ari_all_methods[index, ] = c(beta[i], j, "SBM+Gibbs+random init", adjustedRandIndex(DATA$cl, cl_gibbs), max(lps))
      if(max(lps)>res_random$lps){
        res_random$lps = max(lps)
        res_random$cl = cl_gibbs
      }
      
      res = gibbs_sampler(DATA$X, K, init = "KMEANS", max_iterations = nsamples,seed=j)
      
      lps = res$lps#[-1]
      clusterList = res$clusterList
      cl_gibbs= clusterList[[which.max(lps)]]
      
      #GAMMA = Reduce("+", res$C)/nsamples
      #cl_gibbs = apply(GAMMA, 1, which.max)
      
      #index = index + 1
      #ari_all_methods[index, ] = c(i,beta[i], j, "SBM+Gibbs+kmeans init", adjustedRandIndex(DATA$cl, cl_gibbs), max(lps))
      if(max(lps)>res_kmeans$lps){
        res_kmeans$lps = max(lps)
        res_kmeans$cl = cl_gibbs
      }
      
      
      SBM_VBEM = VBEM_rep(DATA$X, K, n0=rep(1, K), eta0=matrix(1, K, K), zeta0=matrix(1, K, K), max_outer=100, eps_outer=1e-6, nb_rep=1, nb_rep_init=10, directed=TRUE,seed=j)
      #index = index + 1
      
      # using the VBEM as starting point
      res = gibbs_sampler(DATA$X, K, 
                          init = list(SBM_VBEM$cl-1), 
                          max_iterations = nsamples,seed=j)
      
      lps = res$lps#[-1]
      clusterList = res$clusterList
      cl_gibbs= clusterList[[which.max(lps)]]
      
      if(lps[1]>res_vbem$lps){
        res_vbem$lps = lps[1]
        res_vbem$cl = SBM_VBEM$cl
      }
      
      #ari_all_methods[index, ] = c(beta[i], j, "SBM+VBEM+kmeans init", adjustedRandIndex(DATA$cl, SBM_VBEM$cl), lps[1])
      
      #Cmap = Cs[[which.max(lps)]]
      #cl_gibbs = apply(Cmap, 1, which.max)
      
      # GAMMA = Reduce("+", res$C)/nsamples
      # cl_gibbs = apply(GAMMA, 1, which.max)
      
      #index = index + 1
      #ari_all_methods[index, ] = c(beta[i], j, "SBM+Gibbs+VBEM init", adjustedRandIndex(DATA$cl, cl_gibbs), max(lps))
      if(max(lps)>res_gibbs_vbem$lps){
        res_gibbs_vbem$lps = max(lps)
        res_gibbs_vbem$cl = cl_gibbs
      }
    }
    
    ari_all_methods[index, ] = c(beta[i], m, "SBM+Gibbs+random init", adjustedRandIndex(DATA$cl, res_random$cl), res_random$lps)
    index = index + 1
    ari_all_methods[index, ] = c(beta[i], m, "SBM+Gibbs+kmeans init", adjustedRandIndex(DATA$cl, res_kmeans$cl), res_kmeans$lps)
    index = index + 1
    ari_all_methods[index, ] = c(beta[i], m, "SBM+VBEM+kmeans init", adjustedRandIndex(DATA$cl, res_vbem$cl), res_vbem$lps)
    index = index + 1
    ari_all_methods[index, ] = c(beta[i], m, "SBM+Gibbs+VBEM init", adjustedRandIndex(DATA$cl, res_gibbs_vbem$cl), res_gibbs_vbem$lps)
    index = index + 1
  }
}
ari_all_methods=ari_all_methods[-nrow(ari_all_methods),]
#load(file="data/ari_all_methods_sc3.Rdata")
save(ari_all_methods,file="data/ari_all_methods_sc1_n10neww.Rdata")
#save(ari_all_methods,file="data/ari_all_methods_sc1_n400neww.Rdata")
#save(ari_all_methods,file="data/ari_all_methods_sc2_n400neww.Rdata")
#save(ari_all_methods,file="data/ari_all_methods_sc3_n400_neww.Rdata")

ari_all_methods = data.frame(ari_all_methods)
ari_all_methods$beta = as.numeric(ari_all_methods$beta)
ari_all_methods$graph = as.numeric(ari_all_methods$graph)
ari_all_methods$ari = as.numeric(ari_all_methods$ari)
ari_all_methods$lpsOfSolution = as.numeric(ari_all_methods$lpsOfSolution)

write.csv(ari_all_methods,file="data/ari_all_methods_sc1_n10.csv")
#write.csv(ari_all_methods,file="data/ari_all_methods_sc1_n400.csv")
#write.csv(ari_all_methods,file="data/ari_all_methods_sc2_n400.csv")
#write.csv(ari_all_methods,file="data/ari_all_methods_sc3_n400.csv")