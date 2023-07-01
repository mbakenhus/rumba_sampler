require(pracma) # Lcm(), gcd(), rref()
require(Matrix) # sparse matrices
require(numbers) # hermiteNF()


integerREF <- function(A){
  if(class(A)[1]!="matrix" & class(A)[1]!="dgCMatrix"){
    stop("A should be a matrix or dgCMatrix")
  }
  nR <- nrow(A)
  nC <- ncol(A)
  lcmLT <- 1
  for(i in 1:nR){
    # find the pivot in the row
    k <- which(A[i,]!=0)[1]
    
    # if no pivot exist move on
    if(is.na(k)){
      next
    }
    
    # eliminate the non-pivot entries in the pivot column
    for(j in 1:nR){
      if(j!=i & A[j,k]!=0){
        mjik <- Lcm(A[j,k], A[i,k])/A[j,k]
        mijk <- Lcm(A[j,k], A[i,k])/A[i,k]
        A[j,] <- mjik*A[j,] - mijk*A[i,]
      }
    }
    
    # compute the row GCD
    gcdi <- A[i,k]
    if(k < nC){
      for(l in (k+1):nC){
        gcdi <- gcd(gcdi, A[i,l])
      }
    }
    
    # re-scale the row by GCD
    A[i,] <- (1/gcdi)*A[i,]
    
    # running LCM for the pivots
    lcmLT <- Lcm(lcmLT,A[i,k])
  }
  
  # homogenize pivots using the LCM
  for(i in 1:nR){
    k <- which(A[i,]!=0)[1]
    if(!is.na(k)){
      A[i,] <- (lcmLT/A[i,k])*A[i,]
    }
  }
  
  return(A)
}
  


integerBasis <- function(A, intREF=TRUE){
  
  # compute the integer REF of A if specified
  if(intREF){
    X <- integerREF(A)
  } else{
    X <- A
  }
  
  
  n <- nrow(X)
  k <- ncol(X)
  
  # pivot columns
  piv <- apply(X, 1, function(xi){which(xi!=0)[1]})
  
  # for empty rows
  zeroRows <- which(is.na(piv))
  piv <- piv[!(is.na(piv))]
  
  # non-pivot columns correspond to basis vectors
  bv <- (1:k)[-piv]
  
  # initialize empty matrix
  B <- Matrix(rep(0, k), nrow = k, sparse = TRUE)
  
  for(j in bv){
    v <- rep(0, k)
    if(length(zeroRows)==0){
      v[piv] <- -X[,j]
    } else{
      v[piv] <- -X[-zeroRows,j]
    }
    
    v[j] <- X[1,1]
    # The basis vector
    if(all(B[,1]==0)){
      B[,1] <- as(v,"sparseMatrix")
    } else{
      B <- cbind(B,Matrix(v, sparse = TRUE))
    }
  }
  
  return(B)
}


utri_old <- function(A){
  n <- nrow(A)
  m <- ncol(A)
  
  for(i in 1:n){
    # find the pivot in the row
    k <- which(A[i,]!=0)[1]
    
    # if no pivot exist move on
    if(is.na(k)){
      next
    }
  
    if(i <n){
      # eliminate the non-pivot entries in the pivot column
      for(j in (i+1):n){
        if(A[j,k]!=0){
          mjik <- Lcm(A[j,k], A[i,k])/A[j,k]
          mijk <- Lcm(A[j,k], A[i,k])/A[i,k]
          A[j,] <- mjik*A[j,] - mijk*A[i,]
        }
      }
    }
    # compute the row GCD
    gcdi <- A[i,k]
    if(k < m){
      for(l in (k+1):m){
        gcdi <- gcd(gcdi, A[i,l])
      }
    }
    
    # re-scale the row by GCD
    A[i,] <- A[i,]/gcdi
    
    # running LCM for the pivots
    #lcmLT <- Lcm(lcmLT,A[i,k])
    
  }
  return(A)
}


utri <- function(A){
  N <- nrow(A)
  M <- ncol(A)
  
  prow <- 1
  pcol <- 1
  
  for(j in 1:M){
    if(prow == N){
      if(A[prow,j]<0){A[prow,]<- -A[prow,]}
      break
    }
    # find the pivot row
    nz <- which(A[prow:N,j]!=0) +(prow -1)
    
    # if no pivot exist move on
    if(length(nz)==0){
      next
    }
    k <- nz[which.min(abs(A[nz,j]))]
    if(A[k,j]<0){A[k,]<- -A[k,]}
    tmpRow <- A[prow,]
    A[prow,] <- A[k,]
    A[k,] <- tmpRow
    
    
    
    
    if(prow < N){
      for(i in (prow+1):N){
        # eliminate the non-pivot entries in the pivot column
        if(A[i,j]!=0){
          m_pij <- Lcm(A[prow,j], A[i,j])/A[prow,j]
          m_ipj <- Lcm(A[prow,j], A[i,j])/A[i,j]
          A[i,] <- m_pij*A[prow,] - m_ipj*A[i,]
        }
      }
    }
    
   
    # compute the row GCD
    gcdp <- A[prow,j]
    if(j < M){
      for(l in (j+1):M){
        gcdp <- gcd(gcdp, A[prow,l])
      }
    }
    
    # re-scale the row by GCD
    A[prow,] <- A[prow,]/gcdp
    prow <- prow + 1
  }
  return(A)
}



lbasis <- function(A, reduce = TRUE){
  n <- nrow(A)
  m <- ncol(A)
  nB <- m-n
  
  X <-as(cbind(t(A),diag(m)), "sparseMatrix")
  X <- utri(X)
  
  nX <- nrow(X)
  mX <- ncol(X)
  
  B <- as(t(X[(nX-nB+1):nX, (mX-m+1):mX]), "sparseMatrix")
  
  B <- apply(B, 2, function(v){
    gcdv <- v[1]
    for(i in v[-1]){
      gcdv <- gcd(gcdv, i)
    }
    return(v/gcdv)
  })
  
  if(reduce){
    nB <- ncol(B)
    for(v in 2:nB){
      for(u in 1:(v-1)){
        B[,v] <- B[,v]- round(((B[,v]%*%B[,u])/(B[,u]%*%B[,u])))%*%B[,u]
      }
    }
    B <- apply(B, 2, function(v){
      gcdv <- v[1]
      for(i in v[-1]){
        gcdv <- gcd(gcdv, i)
      }
      return(v/gcdv)
    })
  }
  
  return(as(B,"sparseMatrix"))
}

