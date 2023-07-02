# rumba_sampler

This is the R implementation of the RUMBA algorithm prsented in the paper ``[Sampling lattice points in a polytope: a Bayesian biased algorithm with random updates](www.arxiv.org/find)" by Miles Bakenhus and Sonja Petrović. The folder "output" contains data and figures contained in the Simulations section of the paper. 

Function implementaiton of RUMBA in `samplers.R`:

    movingBayes(init_x, basis,
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
                epsilon = 0, writeToFile=NULL)

`init_x`: (numeric matrix) row vector representing the initial feasible point

`basis`: (numeric matrix) matrix with rows that span the lattice 

`init_alpha_pos`: (numeric vector) shape params for positive prior 

`init_alpha_neg`: (numeric vector) shape params for negative prior 

`init_beta_pos`: (numeric vector) rate params for positive prior 

`init_beta_neg`: (numeric vector) rate params for negative prior 

`n_moves`:  (numeric value) indicates the number of moves 

`min_per_itr`: (numeric value) minimum number of samples per iteration 

`max_per_itr`: (numeric value) maximum number of samples per iteration 

`verbose:` (logical value) where indicating verbose output 

`sample_frequency` *NOT IMPLEMENTED*:  (logical value) return the sample frequency 

`move_method`: (character value in `c("runif")`) denotes the method for deciding the next initial point

`track_inits`*NOT IMPLEMENTED*: (logical value) return the initial points 

`track_moves`*NOT IMPLEMENTED*: (logical value) return the list of moves 

`upper_bds`: (numeric vector) upper bounds for fiber element values

`weightNew`: (numeric value) weight between 0 and 1 for sampling next move from new points 

`epsilon`*NOT IMPLEMENTED*: (numeric value) Rate by which to increase the variance
