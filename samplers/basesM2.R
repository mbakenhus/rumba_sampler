require(m2r)
require(algstat)
require(Matrix)

#' sets up the m2 connection in R
#' 
#' @param path A character for the path
#' @param fourTiTwo_path character for 4ti2 path
#' @param restart logical indicating restart
setup_m2 <- function(path="/usr/bin/",fourTiTwo_path="/home/miles/4ti2/bin",restart=TRUE){
# restart m2
if(!is.null(get_m2_procid())) {
  if(!system(paste0('if ps -p ',
                    get_m2_procid(),
                    ' > /dev/null; then return 1; else return 0; fi'))){
    options(m2r=NULL)
    detach("package:algstat", unload = TRUE)
    detach("package:m2r", unload = TRUE)
    library(m2r)
    library(algstat)
    set_m2_path(path)
    
    # load m2 package 4ti2
    m2(
      paste0("needsPackage(\"FourTiTwo\", Configuration=>{\"path\"=>\"", 
             fourTiTwo_path, "\"})")
    ) |> invisible()
  } else if(restart){
    stop_m2()
    set_m2_path(path)
    
    # load m2 package 4ti2
    m2(
      paste0("needsPackage(\"FourTiTwo\", Configuration=>{\"path\"=>\"", 
             fourTiTwo_path, "\"})")
    ) |> invisible()
  }
} else{
  set_m2_path(path)
  
  # load m2 package 4ti2
  m2(
    paste0("needsPackage(\"FourTiTwo\", Configuration=>{\"path\"=>\"", 
           fourTiTwo_path, "\"})")
  ) |> invisible()
}

}

#' Computes the lattice basis for a matrix
#'  
#' @param A A matrix to compute the lattice basis
#' @param timeout integer time in seconds for timeout
#' @param m2_path path to Macaulay2 binaries
#' @returns A lattice basis for A
latticeBasis <- function(A, timeout=Inf, m2_path="/usr/bin"){
  setup_m2(m2_path)
  setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  
  tryCatch({
    A.m2 <- m2_matrix(A)
    
    basis <- paste0("kernel ", m2_name(A.m2)) |>
      m2() |>
      m2_parse() |> t() |> rbind()
    return(basis)
  }, error = function(e) {
    if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
      stop("Basis computation timed out")
    } else {
      stop(e)
    }
  })
}

#' Computes the markov basis for a matrix
#'  
#' @param A A matrix to compute the makrov basis
#' @param timeout integer time in seconds for timeout
#' @param m2_path path to Macaulay2 binaries
#' @returns A markov basis for A
markovBasis <- function(A, timeout=Inf, m2_path="/usr/bin"){
  setup_m2(m2_path)
  setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  
  tryCatch({
    A.m2 <- m2_matrix(A)
    
    basis <- paste0("mb = toricMarkov ", m2_name(A.m2)) |>
      m2() |>
      m2_parse() |> rbind()
    
    return(basis)
  }, error = function(e) {
    if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
      stop("Basis computation timed out")
    } else {
      stop(e)
    }
  })
}

