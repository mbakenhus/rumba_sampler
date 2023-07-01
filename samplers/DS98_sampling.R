#!/usr/bin/env Rscript


# args: k flag.x nmoves niter wt eps minSample maxSample 
#      ... step.f iter.f step.f0 step.f1 iter.f0 iter.f1
args = commandArgs(trailingOnly=TRUE)

#args <- c("5", "5", "1", "0.05", "1e2", "1e3", 
#          "output/DS98_step_out_n100_i5_s5.csv",
#          "output/DS98_iter_out_n100_i5_s5.csv")

require(Matrix)
require(digest)

set.seed(2022)

source("./samplers.R")
source("./integerBasis.R")

if(length(args)==0){
  nmoves <- 5
  niter  <- 5
  
  wt <- 1
  eps <- 0.05
  
  minSample <- 1e2
  maxSample <- 1e3
  
  step.f <- "output/DS98_step_output_test.csv"
  iter.f <- "output/DS98_iteration_output_test.csv"
} else {
  nmoves <- as.integer(args[1])
  niter  <- as.integer(args[2])
  
  wt <- as.numeric(args[3])
  eps <- as.numeric(args[4])
  
  minSample <- as.numeric(args[5])
  maxSample <- as.numeric(args[6])
  
  step.f <- as.character(args[7])
  iter.f <- as.character(args[8])
}

system.time({
x0 <- c(68, 119, 26,  7,
        20,  84, 17, 94, 
        15,  54, 14, 10,
         5,  29, 14, 16) |> Matrix()


A <- rbind(
            c(rep(1,4),rep(0,12)),
            c(rep(0,4),rep(1,4),rep(0,8)),
            c(rep(0,8),rep(1,4),rep(0,4)),
            c(rep(0,12), rep(1,4)),
            cbind(diag(4),diag(4),diag(4),diag(4))
) |> Matrix(sparse = T)


b <- A %*% x0



lb <- integerBasis(A) |> t()

#x0 <- as(sample(min_x0:max_x0, ncol(A), replace=TRUE),"sparseMatrix") 
#b <- A %*% x0

b <- A

varc <- 1

nbasis <- nrow(lb)

lb.init_lambdas_pos <- varc*c(rep(1/nbasis, nbasis-1), 1)
lb.init_lambdas_neg <- varc*c(rep(1/nbasis, nbasis-1), 1)

lb.init_alpha_pos <-   varc*c(rep(1, nbasis-1),nbasis)
lb.init_beta_pos <-    rep(nbasis, nbasis)

lb.init_alpha_neg <-   varc*c(rep(1, nbasis-1),nbasis)
lb.init_beta_neg <-    rep(nbasis, nbasis)

FA <- movingBayes(t(x0), lb,
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