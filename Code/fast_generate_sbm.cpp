#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


using namespace Rcpp;
using namespace arma;



// [[Rcpp::export]]
mat generatesbm(int n, rowvec& cl, mat& mu, int directed){
  
  int i, j, q, r;

  std::random_device rd;
  std::mt19937 gen(rd());
  
  std::bernoulli_distribution p(1);
  
  std::list<vec> edge_list;
  
  std::list<vec>::iterator it; // iterator

  vec v(2);
  if(directed) { // if directed
    for(i=0; i<n; i++) {
      for(j=0; j<n; j++) {
        Rcout << "i = " << i << std::endl;
        Rcout << "j = " << j << std::endl;
        q = cl(i);
        r = cl(j);
        Rcout << "cl(i) = " << q << std::endl;
        Rcout << "cl(j) = " << r << std::endl;
        Rcout << "mu(q,r) = " << mu(q, r) << std::endl;
        
        p = std::bernoulli_distribution( mu(q, r) );
        
        if( p(gen) ) { // if edge created
          vec v(2);
          v(0) = i;
          v(1) = j;
          edge_list.push_back(v); // add edge to the list
        }
      }
    }
  }
  else { // if undirected
    for(i=0; i<n-1; i++) {
      for(j=i+1; j<n; j++) {
        
        q = cl(i);
        r = cl(j);
        
        p = std::bernoulli_distribution( mu(q, r) );
        
        if( p(gen) ) { // if edge created
          vec v(2);
          v(0) = i;
          v(1) = j;
          edge_list.push_back(v); // add edge to the list
        }
      }
    }
    
    
  }
  // transform list as a matrix
  mat edges(edge_list.size(), 2);
  int m=0;

  for (it = edge_list.begin(); it != edge_list.end(); ++it) {
    
    v = *it;
    edges(m, 0) = v(0);
    edges(m, 1) = v(1);
    
    m++;
  }
  
  return(edges);

  
}