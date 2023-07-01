#!/usr/bin/env Rscript


# args: k flag.x nmoves niter wt eps minSample maxSample 
#      ... step.f iter.f step.f0 step.f1 iter.f0 iter.f1
args = commandArgs(trailingOnly=TRUE)

require(Matrix)
require(digest)

#set.seed(2022)

source("./samplers.R")
source("./integerBasis.R")

if(length(args)==0){
  k <- 10
  flag.x1 <- FALSE
  nmoves <- 24
  niter  <- 10
  wt <- 1
  eps <- 1
  
  minSample <- 1e3
  maxSample <- 1e3
  checkSupports <- TRUE
  
  step.f <- "output/Ak_step_output_test.csv"
  iter.f <- "output/Ak_iteration_output_test.csv"
  
} else {
  checkSupports <- FALSE
  k <- as.integer(args[1])
  flag.x1 <- as.logical(args[2])
  nmoves <- as.integer(args[3])
  niter  <- as.integer(args[4])
  wt <- as.numeric(args[5])
  eps <- as.numeric(args[6])
  
  minSample <- as.numeric(args[7])
  maxSample <- as.numeric(args[8])
  
  step.f <- as.character(args[9])
  iter.f <- as.character(args[10])
  
  step.f0 <- as.character(args[11])
  step.f1 <- as.character(args[12])
  iter.f0 <- as.character(args[13])
  iter.f1 <- as.character(args[14])
}

# last element of the b vector
L <- 1


system.time({
A <- rbind(
      cbind(
        bdiag(cbind(Diagonal(k),Diagonal(k)), 
              cbind(Diagonal(k),Diagonal(k))),
        as(c(rep(-1,k), rep(0,k)), "sparseMatrix"),
        as(c(rep(0,k), rep(-1,k)), "sparseMatrix")
      ), c(rep(0,4*k),1,1))

lb <- integerBasis(A)

#x0 <- as(sample(min_x0:max_x0, ncol(A), replace=TRUE),"sparseMatrix") 
#b <- A %*% x0

b <- Matrix( c(rep(0,2*k),L), byrow =T)
x0 <- Matrix( c(rep(0,2*k), rep(L,k), rep(0,k), 0, L))
x1 <- Matrix( c(rep(L,k),rep(0,3*k), L, 0))

varc <- 1

nbasis <- ncol(lb)
if(checkSupports){
  lb.supp <- lb!=0
  lb.init_alpha_pos <-  colSums(lb.supp)
  lb.init_alpha_neg <-  lb.init_alpha_pos
  
  lb.init_beta_pos <-    rep(max(lb.init_alpha_pos), nbasis)
  lb.init_beta_neg <-    lb.init_beta_pos
  
  lb.init_lambdas_pos <- lb.init_alpha_pos/lb.init_beta_pos
  lb.init_lambdas_neg <- lb.init_lambdas_pos
} else{
  lb.init_lambdas_pos <- varc*c(rep(1/nbasis, nbasis-1), 1)
  lb.init_lambdas_neg <- varc*c(rep(1/nbasis, nbasis-1), 1)
  
  lb.init_alpha_pos <-   varc*c(rep(1, nbasis-1),nbasis)
  lb.init_beta_pos <-    rep(nbasis, nbasis)
  
  lb.init_alpha_neg <-   varc*c(rep(1, nbasis-1),nbasis)
  lb.init_beta_neg <-    rep(nbasis, nbasis)
}



if(flag.x1){
  FA0 <- movingBayes(t(x0), t(lb),
                     lb.init_alpha_pos,lb.init_alpha_neg,
                     lb.init_beta_pos, lb.init_beta_neg,
                     n_moves=ceiling(nmoves/2), n_itr=niter, 
                     min_per_itr=minSample, max_per_itr=maxSample, 
                     weightNew = wt, epsilon=eps,
                     writeToFile = c(step.f0, iter.f0))  

  FA1 <- movingBayes(t(x1), t(lb),
                    lb.init_alpha_pos,lb.init_alpha_neg,
                    lb.init_beta_pos, lb.init_beta_neg,
                    n_moves=floor(nmoves/2), n_itr=niter, 
                    min_per_itr=minSample, max_per_itr=maxSample, 
                    weightNew = wt, epsilon=eps, 
                    writeToFile = c(step.f1, iter.f1))

  FA1.hashes <- which(!(names(FA1$fiber_table) %in%  names(FA0$fiber_table)))
  FA <- list(
    fiber = rbind(FA0$fiber, FA1$fiber[FA1.hashes,]),
    fiber_table = append(FA0$fiber_table, FA1$fiber_table[FA1.hashes])
  )
} else {
  FA <- movingBayes(t(x0), t(lb),
                     lb.init_alpha_pos,lb.init_alpha_neg,
                     lb.init_beta_pos, lb.init_beta_neg,
                     n_moves=nmoves, n_itr=niter, 
                     min_per_itr=minSample, max_per_itr=maxSample, 
                     weightNew = wt, epsilon=eps, writeToFile = c(step.f, iter.f)) 
}

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