#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <rgen.h> 
// [[Rcpp::depends(rgen)]]


using namespace Rcpp;
using namespace arma;


double Gibbs_Mstep(sp_mat &Y, int n, int G, urowvec& cluster, rowvec &tau, mat &mu) {
  
  double lps = 0;
  int i, j;
  sp_mat::iterator edge; // to go through the edges
  
  // tau
  //rowvec cluster_count;
  
  rowvec cluster_count(G, fill::zeros); // number of nodes in each cluster
  mat connectivity_count(G, G, fill::zeros); // number of edges between pairs of clusters

  for(i=0; i<n; i++) {
    cluster_count( cluster(i) )++; 
  }
  
  
  //cluster_count = sum(C, 0);
  
  rowvec a = cluster_count + 1;
  
  tau = rgen::rdirichlet(a.t()).t();
//  tau.print("tau: ");
  double asum = 0;
  
  for(int i = 0; i<a.n_cols;i++){ // compute exact ICL
    asum = asum + a(i);
    lps = lps + lgamma(a(i));
  }
  lps = lps + lgamma(a.n_cols) - lgamma(asum);

  // mu
  
  
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
      mu(k, l) = rgen::rdirichlet(alpha_beta)(0);
      lps = lps + lgamma(1+1) + lgamma(alpha(l,k)) + lgamma(beta(l,k)) - lgamma(alpha(l,k) + beta(l,k)) - lgamma(1) - lgamma(1);
    }
  }
//  mu.print("mu: ");
  
  return(lps);
}

int Gibbs_Estep(sp_mat &Y, sp_mat &tY, int n, int G, urowvec& cluster, rowvec &tau, mat &mu, double mincut, double maxcut) {
  
  // init : prepare elements: start
  int i, j, k, l;
  double cst;
  
  sp_mat::iterator vertex; // to go through the neighbour of a vertex
  
  rowvec MAXCUT(G);
  MAXCUT.fill(maxcut);
  
  rowvec MINCUT(G);
  MINCUT.fill(mincut);
  
  rowvec MIN(G);
  MIN.fill(DBL_MIN);
  
  rowvec ElogIP(G);
  for(k=0; k<G; k++) {
    ElogIP(k) = log(tau(k));
  }
  
  mat EMUbarMU(G, G);
  mat EMUbarMUt(G, G);
  mat EbarMU(G, G);
  mat EbarMUt(G, G);
  
  for(k=0; k<G; k++) {
    for(l=0; l<G; l++) {
      EMUbarMU(k, l) =  log(mu(k, l)) - log(1-mu(k, l)); 
      EbarMU(k, l) = log(1-mu(k, l)); 
    }
  }
  
  cst = 1;
  
  rowvec nsize(G, fill::zeros); // number of nodes in each cluster
  for(i=0; i<n; i++) {
    nsize( cluster(i) )++; 
  }
  
 //rowvec nsize;
  //nsize = sum(C, 0);
  
  rowvec logGamma_i(G);
  rowvec gamma_i(G);
  for(i=0; i<n; i++) {
    rowvec tmp(G);
    tmp.zeros();
    // for(l=0; l<G; l++) {
    //  // tmp += cst*(nsize(l)-C(i, l))*(EbarMUt.row(l) + EbarMU.row(l));
    //  if(l==cluster(i)) {
    //    tmp += cst(nsize(l) - 1)*(EbarMUt.row(l) + EbarMU.row(l));
    //  }tmp += cst*()
    // }
    for(k=0; k<G; k++) {
      tmp(k) += cst*ElogIP(k);
      for(l=0; l<G; l++) {
        // tmp += cst*(nsize(l)-C(i, l))*(EbarMUt.row(l) + EbarMU.row(l));
        if(l==cluster(i)) {
          tmp(k) += cst*(nsize(l) - 1)*(EbarMU(k, l) + EbarMU(l, k));
        }
        else {
          tmp(k) += cst*nsize(l)*(EbarMU(k, l) + EbarMU(l, k));
        }
      }
      
      for(vertex=Y.begin_col(i); vertex!=Y.end_col(i); ++vertex) { // go through the edges (j, i)
        j = vertex.row();
        l = cluster(j);
        tmp(k) += cst*EMUbarMU(l, k);
      }
      
      for(vertex=tY.begin_col(i); vertex!=tY.end_col(i); ++vertex) { // go through the edges (j, i)
        j = vertex.row();
        l = cluster(j);
        tmp(k) += cst*EMUbarMU(k, l);
      }
      
    }
    
    //logGamma_i = tmp + ElogIP;
    logGamma_i = tmp;
    
   // logGamma_i += cst*(Y.row(i)*C*EMUbarMUt + tY.row(i)*C*EMUbarMU);
   logGamma_i = logGamma_i - max(logGamma_i); // normalize
   logGamma_i = min( logGamma_i, MAXCUT);
   logGamma_i = max( logGamma_i, MINCUT);  
   gamma_i = exp( logGamma_i );  
   gamma_i = gamma_i / sum(gamma_i);
   gamma_i = max( gamma_i, MIN );
   double cluster_id =  rgen::rmultinomial(gamma_i.t());
   
   
   nsize(cluster(i)) = nsize(cluster(i)) - 1;
   cluster(i) = cluster_id;
   nsize(cluster(i)) = nsize(cluster(i)) + 1;
  }
  return(1);
}


// [[Rcpp::export]]
double Gibbs_EMstep(sp_mat &Y, sp_mat &tY, int n, int G, urowvec &cluster, rowvec &tau, mat &mu, double mincut, double maxcut) {
  
  double lpsNew = Gibbs_Mstep(Y,n, G, cluster, tau, mu);
  //double lpsNew = Gibbs_Mstep(Y, n, G, cluster, tau, mu);
  
  Gibbs_Estep(Y, tY, n, G, cluster, tau, mu, mincut, maxcut);
  
  return(lpsNew);
}

// [[Rcpp::export]]
List Gibbs_EMstep_full(sp_mat &Y, sp_mat &tY, int n, int G, urowvec &cluster, rowvec &tau, mat &mu, int max_iterations, double mincut, double maxcut) {
  
  
  List clusterList(max_iterations);
  List lpsList(max_iterations);
  //clusterList(0) = cluster; // save initialization
  for(int t=0; t<max_iterations; t++) {
    if((t-1)%1000 == 0) {
      std::cout << "iteration " << t-1 << std::endl;
    }
    // List res;
    clusterList(t) = cluster;
    double lps = Gibbs_EMstep(Y, tY, n, G, cluster, tau, mu, mincut, maxcut);
    lpsList(t) = lps;
  }
  List resultList(2);
  resultList(0) = clusterList;
  resultList(1) = lpsList;
  return(resultList);
}

