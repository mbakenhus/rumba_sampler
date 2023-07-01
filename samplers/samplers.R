require(Matrix)
require(digest)

#' Samples a fiber by updating params using average parameter values 
#' for feasible samples
#' 
#' @param x A numeric vector representing the initial feasible point
#' @param basis A matrix with rows that span the lattice 
#' @param lambdas_pos A numeric vector of means for each row of B 
#' @param lambdas_neg A numeric vector of means for each row of B 
#' @param min_samples A positive integer 
#' @param max_samples A positive integer greater than min_samples
#' @param upper_bds A numeric vector of upper bounds for fiber elements
#' @returns A matrix where rows are samples from the fiber; updated lambdas
simpleSampler <- function(x, basis, 
                          lambdas_pos, lambdas_neg ,
                          min_samples, max_samples, upper_bds=NULL){
  
  # count moves 
  nB <- nrow(basis)
  
  # check nB == length(pos_lambdas) == length(neg_lambdas)
  if(isFALSE(all(c(length(lambdas_pos),length(lambdas_neg)),nB))){
    stop("Error: number of parameters differs from number of basis vectors (rows of B).")
  }
  
  if(isFALSE(min_samples <= max_samples) |
     isFALSE(min_samples > 0 & max_samples > 0)){
    stop("Error: min_samples and max_samples must be positive integers such that min_samples <= max_samples")
  }
  
  if(!is.null(upper_bds) & length(upper_bds) != length(x)){
    stop("Error: upper_bds vector must be same lenght as x")}
  
  # feasible sets initial
  Xfeas <- Matrix(numeric(0), nrow = 0, ncol = length(x), sparse = TRUE)
  Xhash <- numeric(0)
  Ysum <- rep(0, nB)
  Zsum <- rep(0, nB)
  nfeas <- 0 
  
  j <- 0
  
  while(j <= min_samples | (j<=max_samples & nfeas==0)){
  
  j <- j+1

    Yj <- rpois(nB,lambdas_pos)
    Zj <- rpois(nB,lambdas_neg)
    Xj <- x + (Yj - Zj) %*% basis
    
    if( all(Xj>=0) & ifelse(is.null(upper_bds), TRUE, all(Xj <= upper_bds))){
      
      Xj.hash <- paste(Xj, collapse = "") |> digest(algo = 'sha256') 
      
      if(!(Xj.hash %in% names(Xhash))){
        Xfeas <- rbind(Xfeas, Xj)
        Xhash <- append(Xhash, setNames(1,Xj.hash))
        
        Ysum  <- Ysum + Yj
        Zsum  <- Zsum + Zj
        nfeas <- nfeas + 1
        
      } else {
        Xhash[Xj.hash] <- Xhash[Xj.hash] + 1
      }
    }
  }
  if(nfeas==0){
    message("No fiber elements found.")
    return(list(
      fiber = Xfeas, #distinct_feasible_pts,
      lambdas_pos = lambdas_pos,
      lambdas_neg = lambdas_neg,
      sample_table = Xhash
    ))
  } else {

    #distinct_feasible_pts <- unique(Xfeas)
    
    new_lambdas_pos <- Ysum/nfeas
    new_lambdas_neg <- Zsum/nfeas
      
    return(list(
      fiber = Xfeas, #distinct_feasible_pts,
      lambdas_pos = new_lambdas_pos,
      lambdas_neg = new_lambdas_neg,
      sample_table = Xhash
    ))
  }
}


#' Samples a fiber by updating params using conjugate prior 
#' expected parameter values for feasible samples
#' 
#' @param x A numeric vector representing the initial feasible point
#' @param basis A matrix with rows that span the lattice 
#' @param lambdas_pos A numeric vector of means for each row of B 
#' @param lambdas_neg A numeric vector of means for each row of B 
#' @param alpha_pos A numeric vector of shape params for positive prior 
#' @param alpha_neg A numeric vector of shape params for negative prior
#' @param beta_pos A numeric vector of rate params for positive prior
#' @param beta_neg A numeric vector of rate params for negative prior
#' @param min_samples A positive integer 
#' @param max_samples A positive integer greater than min_samples
#' @param upper_bds A numeric vector of upper bounds for fiber elements
#' @returns A matrix where rows are samples from the fiber; updated parameters
bayesSampler <- function(x, basis,
                         lambdas_pos,lambdas_neg, 
                         alpha_pos,  alpha_neg,
                         beta_pos,   beta_neg,
                         min_samples, max_samples,upper_bds=NULL){
  # count moves 
  nB <- nrow(basis)
  # check nB == length(pos_lambdas) == length(neg_lambdas)
  if(isFALSE(all(c(length(lambdas_pos),length(lambdas_neg),
                   length(alpha_pos),length(alpha_neg),
                   length(beta_pos),length(beta_neg)),nB))){
    stop("Error: number of parameters differs from number of basis vectors (rows of B).")
  }
  
  if(isFALSE(min_samples <= max_samples) |
     isFALSE(min_samples > 0 & max_samples > 0)){
    stop("Error: min_samples and max_samples must be positive integers such that min_samples <= max_samples")
  }
  
  if(!is.null(upper_bds) & length(upper_bds) != length(x)){
    stop("Error: upper_bds vector must be same lenght as x")}
  
  # feasible sets initial
  Xfeas <- Matrix(numeric(0), nrow = 0, ncol = length(x), sparse = TRUE)
  Xhash <- numeric(0)
  
  Ysum <- rep(0, nB)
  Zsum <- rep(0, nB)
  nfeas <- 0 
  
  j <- 0
  
  while(j <= min_samples | (j<=max_samples & nfeas==0)){
    
    j <- j+1
    
    Yj <- rpois(nB,lambdas_pos)
    Zj <- rpois(nB,lambdas_neg)
    Xj <- x + (Yj - Zj) %*% basis
    
    if(all(Xj>=0  & ifelse(is.null(upper_bds), TRUE, all(Xj <= upper_bds)))){
      
      Xj.hash <-paste(Xj, collapse = "") |> digest(algo = 'sha256')
      
      if(!(Xj.hash %in% names(Xhash))){
        Xfeas <- rbind(Xfeas, Xj)
        Xhash <- append(Xhash, setNames(1,Xj.hash))
        
        Ysum  <- Ysum + Yj
        Zsum  <- Zsum + Zj
        nfeas <- nfeas + 1
        
      } else {
        Xhash[Xj.hash] <- Xhash[Xj.hash] + 1
      }
    }
  }
  
  if(nfeas==0){
    message("No fiber elements found.")
    return(list(
      fiber = Xfeas, #distinct_feasible_pts,
      lambdas_pos = lambdas_pos,
      lambdas_neg = lambdas_neg,
      alpha_pos = alpha_pos,
      alpha_neg = alpha_neg,
      beta_pos = beta_pos,
      beta_neg = beta_neg,
      sample_table = Xhash
    ))
  } else {
    #distinct_feasible_pts <- unique(Xfeas)
    
    new_alpha_pos <- Ysum + alpha_pos
    new_alpha_neg <- Zsum + alpha_neg
    
    new_beta_pos <- nfeas + beta_pos
    new_beta_neg <- nfeas + beta_neg
    
    new_lambdas_pos <- new_alpha_pos/(new_beta_pos)
    new_lambdas_neg <- new_alpha_neg/(new_beta_neg)
      
    return(list(
      fiber = Xfeas, #distinct_feasible_pts,
      lambdas_pos = new_lambdas_pos,
      lambdas_neg = new_lambdas_neg,
      alpha_pos = new_alpha_pos,
      alpha_neg = new_alpha_neg,
      beta_pos = new_beta_pos,
      beta_neg = new_beta_neg,
      sample_table = Xhash
    ))
  }
}

#' Iterates the simpleSampler() updating the parameters
#' 
#' @param x A numeric vector representing the initial feasible point
#' @param basis A matrix with rows that span the lattice 
#' @param init_lambda_pos A numeric vector of means for positive sample
#' @param init_lambda_neg A numeric vector of means for negative sample
#' @param min_per_itr Minimum number of samples per iteration 
#' @param max_per_itr Maximum number of samples per iteration 
#' @param verbose A logical value where indicating verbose output
#' @param sample_frequency A logical value indicating return of sample frequency
#' @param upper_bds A vector of upper bounds for fiber entries
#' @returns A matrix where rows are samples from the fiber; updated parameters
iterateSimple <- function(x, basis, num_itr,
                          init_lambdas_pos, init_lambdas_neg ,
                          min_per_itr, max_per_itr,
                          verbose=TRUE,sample_frequency=FALSE, upper_bds=NULL){
  
  lambdas_pos <- init_lambdas_pos 
  lambdas_neg <- init_lambdas_neg
  
  if(sample_frequency){message(sprintf("sample frequency not yet implemented"))}
  
  fiber_points <- Matrix(nrow=0,ncol=ncol(basis), sparse=TRUE)
  fiber_hash <- numeric(0)
  lastN <- 0
  
  for(i in 1:num_itr){
    if(verbose){message(sprintf("\niteration: %d",i))}
    XA <- simpleSampler(x, basis,
                       lambdas_pos,lambdas_neg, 
                       min_per_itr, max_per_itr,upper_bds=upper_bds)
    
    lambdas_pos <- XA$lambdas_pos
    lambdas_neg <- XA$lambdas_neg
    newPts <- which(!(names(XA$sample_table) %in% names(fiber_hash)))
    fiber_points <- rbind(fiber_points, XA$fiber[newPts,]) |> as("sparseMatrix")
    fiber_hash <- append(fiber_hash, XA$sample_table[newPts])
    
    if(verbose){
      message(sprintf("\tfiber elements sampled this iteration:  %d", nrow(XA$fiber)))
      message(sprintf("\tunique elements sampled this iteration:  %d", nrow(fiber_points)-lastN))
      message(sprintf("\tcumulative number of unique fiber elements sampled this step:  %d\n", nrow(fiber_points)))
    }
    lastN <- nrow(fiber_points)
  }
  
  return(list(fiber=fiber_points, fiber_table=fiber_hash))
}



#' Iterates the bayesSampler() updating the parameters
#' 
#' @param x A numeric vector representing the initial feasible point
#' @param basis A matrix with rows that span the lattice 
#' @param init_alpha_pos A numeric vector of shape params for positive prior 
#' @param init_alpha_neg A numeric vector of shape params for negative prior
#' @param init_beta_pos A numeric vector of rate params for positive prior
#' @param init_beta_neg A numeric vector of rate params for negative prior
#' @param min_per_itr Minimum number of samples per iteration 
#' @param max_per_itr Maximum number of samples per iteration 
#' @param verbose A logical value where indicating verbose output
#' @param sample_frequency A logical value indicating return of sample frequency
#' @param upper_bds A numeric vector of upper bounds for fiber elements
#' @returns A matrix where rows are samples from the fiber; updated parameters
iterateBayes <- function(x, basis, num_itr, 
                         init_alpha_pos,  init_alpha_neg,
                         init_beta_pos,   init_beta_neg,
                         min_per_itr, max_per_itr,
                         verbose=TRUE, sample_frequency=FALSE,
                         upper_bds=NULL, outputFile=NULL){
  
  lambdas_pos <- init_alpha_pos / init_beta_pos
  lambdas_neg <- init_alpha_neg / init_beta_neg
  alpha_pos <- init_alpha_pos
  alpha_neg <- init_alpha_neg
  beta_pos <- init_beta_pos
  beta_neg <- init_beta_neg
  
  if(sample_frequency){message(sprintf("sample frequency not yet implemented"))}
  
  fiber_points <- Matrix(nrow=0,ncol=ncol(basis), sparse=TRUE)
  fiber_hash <- numeric(0)
  
  lastN <- 0
  
  for(i in 1:num_itr){
  if(verbose){message(sprintf("\niteration: %d",i))}
  XA <- bayesSampler(x, basis,
                       lambdas_pos,lambdas_neg, 
                       alpha_pos,  alpha_neg,
                       beta_pos,   beta_neg,
                       min_per_itr, max_per_itr,
                       upper_bds=upper_bds)
  
  lambdas_pos <- XA$lambdas_pos
  lambdas_neg <- XA$lambdas_neg
  alpha_pos <- XA$alpha_pos
  alpha_neg <- XA$alpha_neg
  beta_pos <- XA$beta_pos
  beta_neg <- XA$beta_neg
  
  newPts <- which(!(names(XA$sample_table) %in% names(fiber_hash)))
  fiber_points <- rbind(fiber_points, XA$fiber[newPts,]) |> as("sparseMatrix")
  fiber_hash <- append(fiber_hash, XA$sample_table[newPts])
  
  nXA <- nrow(XA$fiber)
  nfp <- nrow(fiber_points)
  
  if(verbose){
    message(sprintf("\tfiber elements sampled this iteration:  %d", nXA))
    message(sprintf("\tunique elements sampled this iteration:  %d", nfp-lastN))
    message(sprintf("\tcumulative number of unique fiber elements sampled this step:  %d\n", nfp))
  }
  
  if(!is.null(outputFile)){
    cat(sprintf("%d,%d,%d,%d,%d\n",outputFile[[1]], i, nXA, nfp-lastN, nfp), file=outputFile[[2]], append=TRUE)
  }
  lastN <- nfp
  
  }
  
  return(list(fiber=fiber_points, fiber_table=fiber_hash))
}

#' Moves iterateSimple() updating the initial point
#' 
#' @param init_x A numeric vector representing the initial feasible point
#' @param basis A matrix with rows that span the lattice 
#' @param init_lambda_pos A numeric vector of means for positive sample
#' @param init_lambda_neg A numeric vector of means for negative sample
#' @param n_moves A numeric value indicating the number of starting points to use
#' @param min_per_itr Minimum number of samples per iteration 
#' @param max_per_itr Maximum number of samples per iteration 
#' @param verbose A logical value where indicating verbose output
#' @param sample_frequency A logical value indicating return of sample frequency
#' @param move_method A character denoting the method for deciding the next initial point
#' @param track_inits A logical value indicating to return the initial points
#' @param track_moves A logical value indicating to return a list of moves
#' @param upper_bds A numerical vector of upper bounds for fiber elements
#' @returns A matrix where rows are samples from the fiber
movingSimple <- function(init_x, basis,
                         init_lambdas_pos, init_lambdas_neg ,
                         n_moves, n_itr, 
                         min_per_itr, max_per_itr,
                         verbose=TRUE,
                         sample_frequency=FALSE,
                         move_method='runif',
                         track_inits=FALSE,
                         track_moves=FALSE,
                         upper_bds=NULL){
  
  x <- init_x
  
  
  if(track_inits){message(sprintf("tracking a list of initial points is not yet implemented"))}
  if(track_moves){message(sprintf("tracking a list of moves is not yet implemented"))}
  if(!move_method=='runif'){message(sprintf("move method not yet implemented"))}
  if(sample_frequency){message(sprintf("sample frequency not yet implemented"))}
  
  #fiber_points <- Matrix(nrow=0,ncol=ncol(basis), sparse=TRUE)
  fiber_points <- x
  x.hash <-paste(x, collapse = "") |> digest(algo = 'sha256')
  fiber_hash <- setNames(1,x.hash)
  
  
  lastN <-1
  
  for(i in 1:n_moves){
    if(verbose){message(sprintf("\nStarting point: %d\n",i))}
    XA <- iterateSimple(x, basis, num_itr = n_itr,
                        init_lambdas_pos, init_lambdas_neg ,
                        min_per_itr=min_per_itr, 
                        max_per_itr=max_per_itr,
                        verbose=verbose, sample_frequency=sample_frequency,
                        upper_bds = upper_bds)
    
    newPts <- which(!(names(XA$fiber_table) %in% names(fiber_hash)))
    fiber_points <- rbind(fiber_points, XA$fiber[newPts,]) |> as("sparseMatrix") 
    fiber_hash <- append(fiber_hash, XA$fiber_table[newPts])
    
    nXA <- nrow(XA$fiber)
    nfp <- nrow(fiber_points)
    
    if(verbose){
      message(sprintf("\nSummary for starting point %d:", i))
      message(sprintf("\tnumber of fiber elements sampled this step:  %d", nXA))
      message(sprintf("\tnumber of new fiber elements sampled:  %d", nfp-lastN))
      message(sprintf("\tcumulative number of unique fiber elements sampled:  %d\n", nfp))
    }
    lastN <- nfp
    if(move_method=='runif'){
      x <- fiber_points[sample(1:nfp,1),]  |> as("sparseMatrix") |> t()
    }
  }
  
  return(list(fiber=fiber_points, fiber_table=fiber_hash))
}

#' Moves iterateBayes() updating the initial point
#' 
#' @param init_x A numeric vector representing the initial feasible point
#' @param basis A matrix with rows that span the lattice 
#' @param init_alpha_pos A numeric vector of shape params for positive prior 
#' @param init_alpha_neg A numeric vector of shape params for negative prior
#' @param init_beta_pos A numeric vector of rate params for positive prior
#' @param init_beta_neg A numeric vector of rate params for negative prior
#' @param n_moves A numeric value indicating the number of starting points to use
#' @param min_per_itr Minimum number of samples per iteration 
#' @param max_per_itr Maximum number of samples per iteration 
#' @param verbose A logical value where indicating verbose output
#' @param sample_frequency A logical value indicating return of sample frequency
#' @param move_method A character denoting the method for deciding the next initial point
#' @param track_inits A logical value indicating to return the initial points
#' @param track_moves A logical value indicating to return a list of moves
#' @param upper_bds A numeric vector of upper bounds for fiber elements
#' @param weightNew Weight between 0 and 1 for sampling next move from new points
#' @param epsilon >0 Rate by which to increase the variance: epsilon+init_alpha
#' @returns A matrix where rows are samples from the fiber
movingBayes <- function(init_x, basis,
                        init_alpha_pos,init_alpha_neg,
                        init_beta_pos, init_beta_neg,
                        n_moves, n_itr, 
                        min_per_itr, max_per_itr,
                        verbose=TRUE,
                        sample_frequency=FALSE,
                        move_method='runif',
                        track_inits=FALSE,
                        track_moves=FALSE,
                        upper_bds=NULL, 
                        weightNew=0,
                        epsilon = 0, writeToFile=NULL){
  
  x <- init_x
  vr <-0
  step.f=NULL
  iter.f=NULL
  
  if(track_inits){message(sprintf("tracking a list of initial points is not yet implemented"))}
  if(track_moves){message(sprintf("tracking a list of moves is not yet implemented"))}
  if(!move_method=='runif'){message(sprintf("move method not yet implemented"))}
  if(sample_frequency){message(sprintf("sample frequency not yet implemented"))}
  if(!is.null(writeToFile)){
    if(length(writeToFile) < 2){stop("Two file names required: writeToFile = c(\"step_outfile\", \"iteration_outfile\")")}
    step.f <- writeToFile[1]
    iter.f <- writeToFile[2]
    cat("t,num_sampled,num_new_elements,total_sample_size\n", file=step.f)
    cat("t,i,num_sampled,num_new_elements,total_sample_size\n", file=iter.f)
  } 
  
  #fiber_points <- Matrix(nrow=0,ncol=ncol(basis), sparse=TRUE)
  fiber_points <- x
  x.hash <-paste(x, collapse = "") |> digest(algo = 'sha256')
  fiber_hash <- setNames(1,x.hash)
  
  lastN <-1
  
  for(i in 1:n_moves){
    if(verbose){message(sprintf("\nStarting point: %d\n",i))}
    outF <- NULL 
    if(!is.null(writeToFile)){
      outF <- list(i,iter.f)
    }
    
    XA <- iterateBayes(x, basis, num_itr = n_itr,
                 init_alpha_pos, init_alpha_neg,
                 init_beta_pos,  init_beta_neg,
                 min_per_itr=min_per_itr, 
                 max_per_itr=max_per_itr,
                 verbose=verbose, sample_frequency=sample_frequency,
                 upper_bds=upper_bds, outputFile=outF)
    
    newPts <- which(!(names(XA$fiber_table) %in% names(fiber_hash)))
    fiber_points <- rbind(fiber_points, XA$fiber[newPts,]) |> as("sparseMatrix")
    fiber_hash <- append(fiber_hash, XA$fiber_table[newPts])
    
    nXA <- nrow(XA$fiber)
    nfp <- nrow(fiber_points)
    
    if(verbose){
      message(sprintf("\nSummary for starting point %d:", i))
      message(sprintf("\tnumber of fiber elements sampled this step:  %d", nXA))
      message(sprintf("\tnumber of new fiber elements sampled:  %d", nfp-lastN))
      message(sprintf("\tcumulative number of unique fiber elements sampled:  %d\n", nfp))
    }
    if(!is.null(writeToFile)){
      cat(sprintf("%d,%d,%d,%d\n",i, nXA, nfp-lastN, nfp), file=step.f, append=TRUE)
    }
    
    lastN <- nfp
    if(move_method=='runif'){
      if(length(newPts)>0 & runif(1)<weightNew){
        x <-XA$fiber[sample(newPts,1),]  |> as("sparseMatrix") |> t()
      } else {
        x <- fiber_points[sample(1:nfp,1),]  |> as("sparseMatrix") |> t()
      }
    }
    
    if(length(newPts)==0){
      vr <- vr + epsilon
    } else {
      vr <- 0
    }
  }
  
  return(list(fiber=fiber_points, fiber_table=fiber_hash))
}

