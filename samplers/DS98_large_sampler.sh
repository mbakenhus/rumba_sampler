#!/bin/bash

printf "\nPrinting to the files: DS98_out_n1000_i15_s200.txt\n\n"

# single sample

printf "\tDS98_out_n1000_i15_s200.txt: contains the output for running \"DS98_sampling.R\"\n"
printf "\ton an element, x0, using the parameters:\n"
printf "\t\t\tnumber of moves = 200\n"
printf "\t\t\tnumber of iterations per move = 15\n"
printf "\t\t\tweight for sampling next move from new points= 1\n"
printf "\t\t\tEpsilon for increasing the variance = 0.05\n"
printf "\t\t\tminimum number of samples per iteration = 1000\n"
printf "\t\t\tminimum number of samples per iteration = 10000\n"
printf "\t\t\tstepwise csv file: \"DS98_step_out_n1000_i15_s200.csv\"\n"
printf "\t\t\titeration csv file: \"DS98_iter_out_n1000_i15_s200.csv\"\n\n"
printf "Running single sampler ...  "


Rscript DS98_sampling.R 200 15 1 0.05 1e3 1e4 \
output/DS98_step_out_n1000_i15_s200.csv \
output/DS98_iter_out_n1000_i15_s200.csv &> output/DS98_out_n1000_i15_s200.txt

printf "done\n\n"