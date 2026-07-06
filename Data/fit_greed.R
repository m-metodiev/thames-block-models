rm(list=ls())
set.seed(2025)
### BEGIN greed ###
source("../../codes/thames_funcs.R")
Rcpp::sourceCpp("../../codes/compute_lps.cpp")
Rcpp::sourceCpp("../../codes/compute_multinom_lps.cpp")
source("../../codes/VBEM.R")
sourceCpp("../../codes/VBEM.cpp")

library(greed)

load(file=paste0("data/adjmat_for_gibbs",".RData"))
load(file=paste0("data/res_gibs_full_lps",".Rda"))

res = greed(adjmat_for_gibbs, 20, model=new("Sbm",a0=1, b0=1,alpha=1, type="directed"), verbose = FALSE)
#res = greed(as_adjacency_matrix(g4), 20, model=new("Sbm",a0=1, b0=1,alpha=1, type="directed"), verbose = FALSE)
save(res,file=paste0('data/res_greed','.Rda'))
load(file=paste0('data/res_greed','.Rda'))

# l'ICL obtenu est supérieur à celui obtenu avec le package blockmodels donc la partition obtenue ici est meilleure

res@obs_stats$counts
res@obs_stats$x_counts
res

# show groups
res@cl


# dendogram
plot(res)

# number of groups
sum(length(unique(res@cl)))

### END greed ###
