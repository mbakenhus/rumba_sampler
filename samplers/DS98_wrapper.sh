#!/bin/bash

printf "\nPrinting to the files: DS98_out_n100_i5_s5.txt, DS98_out_n1000_i5_s5.txt\n"
printf "\t\t\t DS98_out_n100_i10_s5.txt, DS98_out_n100_i5_s10.txt\n\n"

# single sample

printf "\tDS98_out_n100_i5_s5.txt: contains the output for running \"DS98_sampling.R\"\n"
printf "\ton an element, x0, using the parameters:\n"
printf "\t\t\tnumber of moves = 5\n"
printf "\t\t\tnumber of iterations per move = 5\n"
printf "\t\t\tweight for sampling next move from new points= 1\n"
printf "\t\t\tEpsilon for increasing the variance = 0.05\n"
printf "\t\t\tminimum number of samples per iteration = 100\n"
printf "\t\t\tminimum number of samples per iteration = 1000\n"
printf "\t\t\tstepwise csv file: \"DS98_step_out_n100_i5_s5.csv\"\n"
printf "\t\t\titeration csv file: \"DS98_iter_out_n100_i5_s5.csv\"\n\n"
printf "Running single sampler ...  "


Rscript DS98_sampling.R 5 5 1 0.05 1e2 1e3 \
output/DS98_step_out_n100_i5_s5.csv \
output/DS98_iter_out_n100_i5_s5.csv &> output/DS98_out_n100_i5_s5.txt

printf "done\n\n"

printf "\tDS98_out_n1000_i5_s5.txt: contains the output for running \"DS98_sampling.R\"\n"
printf "\ton an element, x0, using the parameters:\n"
printf "\t\t\tnumber of moves = 5\n"
printf "\t\t\tnumber of iterations per move = 5\n"
printf "\t\t\tweight for sampling next move from new points= 1\n"
printf "\t\t\tEpsilon for increasing the variance = 0.05\n"
printf "\t\t\tminimum number of samples per iteration = 1000\n"
printf "\t\t\tminimum number of samples per iteration = 10000\n"
printf "\t\t\tstepwise csv file: \"DS98_step_out_n1000_i5_s5.csv\"\n"
printf "\t\t\titeration csv file: \"DS98_iter_out_n1000_i5_s5.csv\"\n\n"
printf "Running single sampler ...  "


Rscript DS98_sampling.R 5 5 1 0.05 1e3 1e4 \
output/DS98_step_out_n1000_i5_s5.csv \
output/DS98_iter_out_n1000_i5_s5.csv &> output/DS98_out_n1000_i5_s5.txt

printf "done\n\n"

printf "\tDS98_out_n100_i10_s5.txt: contains the output for running \"DS98_sampling.R\"\n"
printf "\ton an element, x0, using the parameters:\n"
printf "\t\t\tnumber of moves = 5\n"
printf "\t\t\tnumber of iterations per move = 10\n"
printf "\t\t\tweight for sampling next move from new points= 1\n"
printf "\t\t\tEpsilon for increasing the variance = 0.05\n"
printf "\t\t\tminimum number of samples per iteration = 100\n"
printf "\t\t\tminimum number of samples per iteration = 1000\n"
printf "\t\t\tstepwise csv file: \"DS98_step_out_n100_i10_s5.csv\"\n"
printf "\t\t\titeration csv file: \"DS98_iter_out_n100_i10_s5.csv\"\n\n"
printf "Running single sampler ...  "


Rscript DS98_sampling.R 5 10 1 0.05 1e2 1e3 \
output/DS98_step_out_n100_i10_s5.csv \
output/DS98_iter_out_n100_i10_s5.csv &> output/DS98_out_n100_i10_s5.txt

printf "done\n\n"

printf "\tDS98_out_n100_i5_s10.txt: contains the output for running \"DS98_sampling.R\"\n"
printf "\ton an element, x0, using the parameters:\n"
printf "\t\t\tnumber of moves = 10\n"
printf "\t\t\tnumber of iterations per move = 5\n"
printf "\t\t\tweight for sampling next move from new points= 1\n"
printf "\t\t\tEpsilon for increasing the variance = 0.05\n"
printf "\t\t\tminimum number of samples per iteration = 100\n"
printf "\t\t\tminimum number of samples per iteration = 1000\n"
printf "\t\t\tstepwise csv file: \"DS98_step_out_n100_i5_s10.csv\"\n"
printf "\t\t\titeration csv file: \"DS98_iter_out_n100_i5_s10.csv\"\n\n"
printf "Running single sampler ...  "


Rscript DS98_sampling.R 10 5 1 0.05 1e2 1e3 \
output/DS98_step_out_n100_i5_s10.csv \
output/DS98_iter_out_n100_i5_s10.csv &> output/DS98_out_n100_i5_s10.txt

printf "done\n\n"