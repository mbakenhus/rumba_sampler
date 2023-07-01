#!/usr/bin/env Rscript

# args: Q nmoves niter wt eps minSample maxSample 
#      ... step.f iter.f step.f0 step.f1 iter.f0 iter.f1
args = commandArgs(trailingOnly=TRUE)


require(Matrix)
require(digest)
require(algstat)

set.seed(2022)

source("./samplers.R")
source("./integerBasis.R")

prefix <- "output/"
system.time({
Q <- 10
n <- 5*Q^3

nmoves <- 5
niter  <- 5
wt <- 1
eps <- 0.05

minSample <- 1e2
maxSample <- 1e3

sparseModel <- NA

if(length(args)==0){
  Q <- 3
  
  nmoves <- 5
  niter  <- 5
  wt <- 1
  eps <- 0.05
  
  minSample <- 1e2
  maxSample <- 1e2
  
  sparseModel <- 65
  
  support <- expand.grid(1:Q, 1:Q, 1:Q)
  
  suppN <- round((sparseModel/100)*nrow(support))
  
  n <- 5*Q^3
  
  
  varlvls <- c(Q,Q,Q)
  facets <- list(c(1,2), c(1,3), c(2,3))
  A <- as(hmat(varlvls, facets), "sparseMatrix")
  
 
  
  step.f <- "output/sparse65_3x3x3_step_output_test.csv"
  iter.f <- "output/sparse65_3x3x3_iteration_output_test.csv"
  
} else {
  Q <- as.integer(args[1])

  nmoves <- as.integer(args[2])
  niter  <- as.integer(args[3])
  
  wt <- as.numeric(args[4])
  eps <- as.numeric(args[5])
  
  minSample <- as.numeric(args[6])
  maxSample <- as.numeric(args[7])
  
  sparseModel <- ifelse(is.na(args[8]), NA, as.numeric(args[8]))
  
  
  n <- 5*Q^3
  
  
  varlvls <- c(Q,Q,Q)
  facets <- list(c(1,2), c(1,3), c(2,3))
  A <- as(hmat(varlvls, facets), "sparseMatrix")
  
  support <- expand.grid(1:Q, 1:Q, 1:Q)
  
  if(!is.na(sparseModel)){
    prefix <- paste0(prefix,"sparse",sparseModel,"_")
    suppN <- round((sparseModel/100)*nrow(support))
  }
  
  step.f <- paste0(prefix,sprintf("%dx%dx%d_step_output_n%d_i%d_s%d.csv",
                           Q,Q,Q, minSample, niter, nmoves))
  iter.f <- paste0(prefix,sprintf("%dx%dx%d_iter_output_n%d_i%d_s%d.csv",
                           Q,Q,Q, minSample, niter, nmoves))
}

if(!is.na(sparseModel)){
  sparseSupp <- support[sort(sample(1:nrow(support), suppN)), ]
  X <- sparseSupp[sample(1:nrow(sparseSupp), n, replace = T), ]
  X <- data.frame(Var1=factor(X[,1], levels=1:Q),
                  Var2=factor(X[,2], levels=1:Q),
                  Var3=factor(X[,3], levels=1:Q))
             
} else {
  X <- support[sample(1:nrow(support), n, replace = T),]
}
u <- table(X)
x0 <- numeric(0)
for(i in 1:Q){
  for(j in 1:Q){
    for(k in 1:Q){
      x0 <- append(x0, u[i,j,k])
    }
  }
}

b <- A %*% x0

lb <- integerBasis(A)

nbasis <- ncol(lb)
lb.init_lambdas_pos <- rep(1/nbasis, nbasis)
lb.init_lambdas_neg <- rep(1/nbasis, nbasis)
lb.init_alpha_pos <-  rep(1, nbasis)
lb.init_beta_pos <- rep(nbasis,nbasis)
lb.init_alpha_neg <- rep(1,nbasis)
lb.init_beta_neg <- rep(nbasis,nbasis)


FA <- movingBayes(t(x0), t(lb),
                  lb.init_alpha_pos,lb.init_alpha_neg,
                  lb.init_beta_pos, lb.init_beta_neg,
                  n_moves=nmoves, n_itr=niter, 
                  min_per_itr=minSample, max_per_itr=maxSample, 
                  weightNew = wt, epsilon=eps, writeToFile = c(step.f, iter.f)) 


cat("\nValidating sample: ")
hashes <- A %*% as(t(FA$fiber),"sparseMatrix")  |> 
  t() |> as("matrix") |>
  unique() |> paste(collapse="") |> 
  digest(algo='sha256')
bhash <- paste(b,collapse="") |> digest(algo='sha256')
cat(sprintf("\n\t(0) b=A*x0 hash: %s", bhash))
cat("\n\t--------------------------------------------------------------------------------------")
for(i in seq_along(hashes)){
  cat(sprintf("\n\t(%d) b=A*x  hash: %s", i,bhash))
}
cat("\ndone")
cat("\n\nFiber Sample variable: FA\n")
}) |> print()

cat(sprintf("\n\nTotal number of unique fiber elements sampled: %d\n", length(FA$fiber_table)))