#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <rgen.h> 
// [[Rcpp::depends(rgen)]]


using namespace Rcpp;
using namespace arma;


double Gibbs_compute_loglik(sp_mat &Y, int n, int G, urowvec& cluster) {
  
  double loglik = 0;
  int i, j;
  sp_mat::iterator edge; // to go through the edges
  
  rowvec cluster_count(G, fill::zeros); // number of nodes in each cluster
  mat connectivity_count(G, G, fill::zeros); // number of edges between pairs of clusters
  
  // prior
  for(i=0; i<n; i++) {
    cluster_count( cluster(i) )++;
  }
  rowvec a = cluster_count + 1;

  //  tau.print("tau: ");
  double asum = 0;

  for(int i = 0; i<a.n_cols;i++){
    asum = asum + a(i);
    //loglik = loglik + lgamma(a(i));
  }
  //loglik = loglik + lgamma(a.n_cols) - lgamma(asum);
  
  // mu
  
  // likelihood
  for(edge=Y.begin(); edge!=Y.end(); ++edge) {
    i = edge.row();
    j = edge.col();
    connectivity_count(cluster(i), cluster(j))++;
  }
  
  
  mat alpha;
  // alpha = C.t()*Y*C;
  alpha = connectivity_count;
  //mat beta = cluster_count.t()*cluster_count - C.t()*C - alpha;
  mat beta = cluster_count.t()*cluster_count - alpha;
  beta.diag() = beta.diag() - cluster_count.t();
  
  alpha += 1;
  beta += 1;
  
  for(int k=0; k<G; k++) {
    for(int l=0; l<G; l++) {
      vec alpha_beta(2);
      alpha_beta(0) = alpha(k, l);
      alpha_beta(1) = beta(k, l);
      loglik = loglik + lgamma(1+1) + lgamma(alpha(l,k)) + lgamma(beta(l,k)) - lgamma(alpha(l,k) + beta(l,k)) - lgamma(1) - lgamma(1);
    }
  }
  //  mu.print("mu: ");
  
  return(loglik);
}

// [[Rcpp::export]]
double Gibbs_call_compute_loglik(sp_mat &Y, sp_mat &tY, int n, int G, urowvec cluster, double mincut, double maxcut) {
  
  double loglikNew = Gibbs_compute_loglik(Y,n, G, cluster);
  
  return(loglikNew);
}

// [[Rcpp::export]]
List Gibbs_loglik_full(sp_mat &Y, sp_mat &tY, int n, int G, umat clusterMat, int max_iterations, double mincut, double maxcut) {
  List loglikList(max_iterations);
  //clusterList(0) = cluster; // save initialization
  urowvec cluster;
  for(int t=0; t<max_iterations; t++) {
    if((t-1)%1000 == 0) {
      std::cout << "iteration " << t-1 << std::endl;
    }
    cluster = clusterMat.col(t).t();
    double loglik = Gibbs_call_compute_loglik(Y, tY, n, G, cluster, mincut, maxcut);
    loglikList(t) = loglik;
  }
  List resultList(2);
  resultList(0) = clusterMat;
  resultList(1) = loglikList;
  return(resultList);
}

