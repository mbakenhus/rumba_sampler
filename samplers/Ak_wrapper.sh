#!/bin/bash

printf "\nPrinting to the files: Ak_out_k10_single.txt, Ak_out_k10_split.txt\n\n"

# single sample

printf "\tAk_out_k10_single.txt: contains the output of Ak sampling for running \"Ak_sampling.R\"\n"
printf "\ton a SINGLE starting element, x0, using the parameters:\n"
printf "\t\t\tk = 10\n"
printf "\t\t\tnumber of moves = 32\n"
printf "\t\t\tnumber of iterations per move = 8\n"
printf "\t\t\tweight for sampling next move from new points= 1\n"
printf "\t\t\tEpsilon for increasing the variance = 0.05\n"
printf "\t\t\tminimum number of samples per iteration = 1000\n"
printf "\t\t\tminimum number of samples per iteration = 10000\n"
printf "\t\t\tstepwise csv file: \"Ak_single_step_out.csv\"\n"
printf "\t\t\titeration csv file: \"Ak_single_iter_out.csv\"\n\n"
printf "Running single sampler ...  "

# args: k flag.x nmoves niter wt eps minSample maxSample 
#      ... step.f iter.f step.f0 step.f1 iter.f0 iter.f1
Rscript Ak_sampling.R 10 "FALSE" 32 8 1 0.05 1e3 1e4 \
  output/Ak_single_step_out.csv \
  output/Ak_single_iter_out.csv \
  output/Ak_single_step_x0.csv \
  output/Ak_single_step_x1.csv \
  output/Ak_single_iter_x0.csv \
  output/Ak_single_iter_x1.csv &> output/Ak_out_single.txt 
  
# split sample
  
printf "done\n\n"
printf "\tAk_out_k10_split.txt: contains the output of Ak sampling for running \"Ak_sampling.R\"\n"
printf "\ton TWO starting elements, x0 and x1, using the parameters:\n"
printf "\t\t\tk = 10\n"
printf "\t\t\tnumber of moves = 22 (11 for each of x0 and x1)\n"
printf "\t\t\tnumber of iterations per move = 8\n"
printf "\t\t\tweight for sampling next move from new points= 1\n"
printf "\t\t\tEpsilon for increasing the variance = 0.05\n"
printf "\t\t\tminimum number of samples per iteration = 1000\n"
printf "\t\t\tminimum number of samples per iteration = 10000\n"
printf "\t\t\tstepwise csv file: \"Ak_split_step_out.csv\"\n"
printf "\t\t\titeration csv file: \"Ak_split_iter_out.csv\"\n\n"
printf "\nRunning split sampler ... "


# args: k flag.x nmoves niter wt eps minSample maxSample 
#      ... step.f iter.f step.f0 step.f1 iter.f0 iter.f1
Rscript Ak_sampling.R 10 "TRUE" 24 8 1 0.05 1e3 1e4 \
  output/Ak_split_step_out.csv \
  output/Ak_split_iter_out.csv \
  output/Ak_split_step_x0.csv \
  output/Ak_split_step_x1.csv \
  output/Ak_split_iter_x0.csv \
  output/Ak_split_iter_x1.csv &> output/Ak_out_split.txt
  
printf "done\n\n"

