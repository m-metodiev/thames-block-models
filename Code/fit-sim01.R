rm(list=ls())
source("true_marglik_funcs.R")
source("compute_Gibbs_lps_full.R")
#source("thames_funcs.R")
source("functions.R")
source("VBEM.R")
Rcpp::sourceCpp("VBEM.cpp")
Rcpp::sourceCpp("compute_lps.cpp")
#Rcpp::sourceCpp("compute_multinom_lps.cpp")
#Rcpp::sourceCpp("compute_lps.cpp")
Rcpp::sourceCpp("compute_loglik.cpp")

library(thamesblock)

lambdas = seq(0.3,0.9,0.1)#sim01
#lambdas = c(0.8,0.9) #sim02
gs_list = list()
ress_list = list()

# the true value of the marginal log likelihood
log_true_margliks_list = list()

# the different estimators
ChibPartition_list = list()
harmonic_mean_estimator_list = list()
thames_sbm_samplesplitting_list = list()
thames_sbm_nosamplesplitting_list = list()
vbem_list = list()
risvbs_samplesplitting_list = list()
risvbs_nosamplesplitting_list = list()

iters = c(10000, 5000, 2000, 1000)#sim01
#iters = c(10000, 5000) #sim02

df_sim01 = NULL
num_sim = 20
for(iter in iters){
  for(i in seq_along(lambdas)){
    gs = list()
    ress = list()
    log_true_margliks = list()
    ChibPartitions = list()
    harmonic_mean_estimators = list()
    thamess_sbm_samplesplitting = list()
    thamess_sbm_nosamplesplitting = list()
    vbem_marglik_estims = list()
    risvbs_samplesplitting = list()
    risvbs_nosamplesplitting = list()
    
    # simulate SBM
    lambda = lambdas[i]
    epsilon = 0.01
    n = 10
    K = 2
    
    # Scenario 1: communities
    # PI = rep(1/K, K)
    # mu = matrix(epsilon, K, K)
    # diag(mu) = lambda
    
    # Scenario 2: disassortative mixing (symmetric)
    # PI = rep(1/K, K)
    # mu = matrix(lambda, K, K)
    # diag(mu) = epsilon
    
    # Scenario 3: disassortative mixing (asymmetric)
    PI = rep(1/K, K)
    mu = matrix(epsilon, K, K)
    mu[2,1] = lambda
  
    for(N in 1:num_sim){
      set.seed(i*num_sim+N)
      g = fastGenerateSBM(n, PI, mu, directed = TRUE)
      
      # # Scenario 4: an easy test
      # g <- make_ring(10)
      # g$X = as_adjacency_matrix(g)
      

      # # Scenario 5: a medium test
      # g <- add_edges(g, c(
      #    1,3,  3,5,  5,7,  7,9,  9,10, 10,1
      # ))

      # asymmetric "hub noise"
      # g <- add_edges(g, c(
      #    2,6, 2,8, 2,10
      # ))
      # #
      # g$X = as_adjacency_matrix(g)
      # g$X[g$X==2] = 1
      # 
      # g$X[1,9] = 1
      # g$X[9,1] = 1
      # 
      # g$X[2,6] = 0
      # g$X[6,2] = 0

      # g$X[2,8] = 0
      # g$X[8,2] = 0
      # 
      # g$X[2,10] = 0
      # g$X[10,2] = 0
      # 
      # g$X[8,6] = 1
      # g$X[6,8] = 1
      # 
      # g$X[6,4] = 1
      # g$X[4,6] = 1
      # 
      # g$X[4,2] = 1
      # g$X[2,4] = 1
      # 
      # g$X[2,10] = 1
      # g$X[10,2] = 1
      # 
      # g$X[10,8] = 1
      # g$X[8,10] = 1

      # Scenario 6: a hard test
      # g$X[2,7] = 1
      # g$X[7,2] = 1
      # 
      # g$X[4,9] = 1
      # g$X[9,4] = 1
      
      #plot(graph_from_adjacency_matrix(g$X), layout = layout_in_circle)
      ### END TODO REMOVE
      
      # scenario 5: complete graph
      # complete_graph_adjmat = matrix(1,n,n)
      # diag(complete_graph_adjmat) = 0
      # g = graph_from_adjacency_matrix(complete_graph_adjmat)
      # plot(g)
      # g$X=as_adjacency_matrix(g)
      
      # does not make sense otherwise
      while(sum(g$X)<=2){
        g = fastGenerateSBM(n, PI, mu, directed = TRUE)
        #g = veryfastGenerateSBM(n, PI, mu, directed=TRUE)
      }
      # genarate sample from the sbm
      nsamples = 2000 + iter
      res = gibbs_sampler(g$X, K, init = "KMEANS", max_iterations = nsamples,seed=N)
      res$lps = res$lps[-(1:2000)] # burn-in
      mean(res$lps==max(res$lps))
      res$clusterList = res$clusterList[-(1:2000)]
      
      lps = res$lps
      mean(lps==max(lps))
      # Cs = lapply(res$clusterList,function(c_g) t(rbind(c_g,1-c_g)))
      
      # compute true value of the log marginal likelihood
      G=K
      a = 1
      b = 1
      n_dirichlet = rep(1,G)
      
      log_true_margliks[[N]] = log_true_marglik(g, a, b, n_dirichlet)
      print(log_true_margliks[[N]])
      
      SBM_VBEM = VBEM_rep(g$X, K, n0=rep(1, K), eta0=matrix(1, K, K), 
                          zeta0=matrix(1, K, K), max_outer=100, eps_outer=1e-6, 
                          nb_rep=10, nb_rep_init=10, directed=TRUE,seed=1)
      vbem_marglik_estim = max(SBM_VBEM$ELBO)
      
      # compute the ChibPartition estimator
      ChibPartition = ChibPartition(lps, res$clusterList, K)
  
      # define the map and relabel
      clustermap = res$clusterList[[which.max(res$lps)]]
      clustermat = simplify2array(res$clusterList)
      clustermat = t(clustermat[1,,])
      clustermat_relabelled = relabel_with_ECR(G,
                                               clustermat,
                                               clustermap)
      
      # compute the thames for the sbm
      # thames_for_the_sbm = thames_sbm(g$X, G, lps, 
      #                                 clustermat_relabelled)
      #browser()
      
      risvb_samplesplitting = risvb(G, lps,
                                           clustermat,
                                           split=TRUE)$log_zhat_inv
      risvb_nosamplesplitting = risvb(G, lps,
                                            clustermat,
                                            split=FALSE)$log_zhat_inv
      
      maxcut <- log(.Machine$double.xmax) - log(G)
      mincut <- log(.Machine$double.xmin)
      thames_for_the_sbm_samplesplitting = thamesblock(G, logpost=function(cmat) simplify2array(compute_Gibbs_lps_full(g$X, t(g$X), n, G, t(cmat),
                                                                                                     mincut=mincut,
                                                                                                     maxcut=maxcut)[[2]]),
                                                      clustermat,
                                                      split=TRUE,
                                                      seed=i*num_sim+N)$log_zhat_inv
      thames_for_the_sbm_nosamplesplitting = thamesblock(G, logpost=function(cmat) simplify2array(compute_Gibbs_lps_full(g$X, t(g$X), n, G, t(cmat),
                                                                                                                      mincut=mincut,
                                                                                                                      maxcut=maxcut)[[2]]),
                                                        clustermat,
                                                        split = FALSE,
                                                        seed=i*num_sim+N)$log_zhat_inv
      
      # compute the harmonic mean estimator
      maxcut <- log(.Machine$double.xmax) - log(G)
      mincut <- log(.Machine$double.xmin)
      logliks = simplify2array(Gibbs_loglik_full(g$X,
                                                 t(g$X),
                                                 n,
                                                 G,
                                                 t(clustermat),
                                                 max_iterations=iter,
                                                 mincut,
                                                 maxcut)[[2]])
      harmonic_mean_estimator = harmonic_mean_estimator(logliks)
      
      gs[[N]] = g
      ress[[N]] = res
      ChibPartitions[[N]] = ChibPartition
      harmonic_mean_estimators[[N]] = harmonic_mean_estimator
      thamess_sbm_samplesplitting[[N]] = thames_for_the_sbm_samplesplitting
      thamess_sbm_nosamplesplitting[[N]] = thames_for_the_sbm_nosamplesplitting
      vbem_marglik_estims[[N]] = vbem_marglik_estim
      risvbs_samplesplitting[[N]] = risvb_samplesplitting
      risvbs_nosamplesplitting[[N]] = risvb_nosamplesplitting
    }
    gs_list[[i]] = gs
    ress_list[[i]] = ress
    log_true_margliks_list[[i]] = log_true_margliks
    
    ChibPartition_list[[i]] = ChibPartitions
    harmonic_mean_estimator_list[[i]] = harmonic_mean_estimators
    thames_sbm_samplesplitting_list[[i]] = thamess_sbm_samplesplitting
    thames_sbm_nosamplesplitting_list[[i]] = thamess_sbm_nosamplesplitting
    vbem_list[[i]] = vbem_marglik_estims
    risvbs_samplesplitting_list[[i]] = risvbs_samplesplitting
    risvbs_nosamplesplitting_list[[i]] = risvbs_nosamplesplitting
  }
  
  # save as data frame
  res_mat = matrix(0,nrow=1,ncol=8)
  for(i in seq_along(lambdas)){
    res_mat = rbind(res_mat,
                    cbind(-simplify2array(ChibPartition_list[[i]])-simplify2array(log_true_margliks_list[[i]]),
                          -simplify2array(harmonic_mean_estimator_list[[i]])-simplify2array(log_true_margliks_list[[i]]),
                          -simplify2array(thames_sbm_samplesplitting_list[[i]])-simplify2array(log_true_margliks_list[[i]]),
                          -simplify2array(thames_sbm_nosamplesplitting_list[[i]])-simplify2array(log_true_margliks_list[[i]]),
                          -simplify2array(risvbs_samplesplitting_list[[i]])-simplify2array(log_true_margliks_list[[i]]),
                          -simplify2array(risvbs_nosamplesplitting_list[[i]])-simplify2array(log_true_margliks_list[[i]]),
                          simplify2array(vbem_list[[i]])-simplify2array(log_true_margliks_list[[i]]),
                          rep(lambdas[i],length(ChibPartition_list[[i]]))))
  }
  res_mat = res_mat[-1,]
  df_fixed_iters = as.data.frame(res_mat)
  names(df_fixed_iters) = c("ChibPartitions",
                            "HarmonicMean",
                            "thamessamplesplitting",
                            "thamesnosamplesplitting",
                            "risvbsamplesplitting",
                            "risvbnosamplesplitting",
                            "VBEM","lambda")
  df_fixed_iters$iters = iter
  if(is.null(df_sim01)){
    df_sim01 = df_fixed_iters
  } else{
    df_sim01 = rbind(df_sim01,df_fixed_iters)
  }
}
df_sim01$VBEM_correction = df_sim01$VBEM + lfactorial(K) # add the K! correction
#-7.571403
#clustermat_relabelled
#nobile_formula_adjustment = -log(2/3)+log(K-1)-log(n+K-1) 
#log(((3-1) / (3 - 1 + 11)) * (3 / 2))
#log_true_margliks[[N]] + nobile_formula_adjustment
#log_true_margliks_list[[1]][[1]] + nobile_formula_adjustment
#write.csv(df_sim01,file="data/df_sim01_communities.csv")
#write.csv(df_sim01,file="data/df_sim01_disassortative.csv")
write.csv(df_sim01,file="data/df_sim01_disassortative_directed.csv")
#write.csv(df_sim01,file="data/df_sim01_difficultgraph.csv")