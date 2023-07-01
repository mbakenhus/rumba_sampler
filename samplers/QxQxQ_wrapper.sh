#!/bin/bash

# single sample

printf "\t5x5x5_out_n100_i25_s50.txt: contains the output for running \"QxQxQ_sampling.R\"\n"
printf "\ton an element, x0, using the parameters:\n"
printf "\t\t\tnumber of moves = 50\n"
printf "\t\t\tnumber of iterations per move =25\n"
printf "\t\t\tweight for sampling next move from new points= 1\n"
printf "\t\t\tEpsilon for increasing the variance = 0.05\n"
printf "\t\t\tminimum number of samples per iteration = 100\n"
printf "\t\t\tminimum number of samples per iteration = 100\n"
printf "\t\t\tstepwise csv file: \"5x5x5_step_out_n100_i25_s50.csv\"\n"
printf "\t\t\titeration csv file: \"5x5x5_iter_out_n100_i25_s50.csv\"\n\n"
printf "Running single sampler ...  "


Rscript QxQxQ_sampler.R 5 50 25 1 0.05 1e2 1e2 &> output/5x5x5_out_n100_i25_s50.txt

printf "done\n\n"


printf "\t10x10x10_out_n100_i25_s50.txt: contains the output for running \"QxQxQ_sampling.R\"\n"
printf "\ton an element, x0, using the parameters:\n"
printf "\t\t\tnumber of moves = 50\n"
printf "\t\t\tnumber of iterations per move =25\n"
printf "\t\t\tweight for sampling next move from new points= 1\n"
printf "\t\t\tEpsilon for increasing the variance = 0.05\n"
printf "\t\t\tminimum number of samples per iteration = 100\n"
printf "\t\t\tminimum number of samples per iteration = 100\n"
printf "\t\t\tstepwise csv file: \"10x10x10_step_out_n100_i25_s50.csv\"\n"
printf "\t\t\titeration csv file: \"10x10x10_iter_out_n100_i25_s50.csv\"\n\n"
printf "Running single sampler ...  "


Rscript QxQxQ_sampler.R 10 50 25 1 0.05 1e2 1e2 &> output/10x10x10_out_n100_i25_s50.txt

printf "done\n\n"