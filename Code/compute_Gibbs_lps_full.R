compute_Gibbs_lps_full = function(Y,
                                  tY,
                                  n,
                                  G,
                                  clusterMat,
                                  mincut,
                                  maxcut){
  max_iterations = ncol(clusterMat)
  return(Gibbs_lps_full(Y,
                        tY,
                        n,
                        G,
                        clusterMat,
                        max_iterations,
                        mincut,
                        maxcut))

}
