source("./sampler_plotting.R")


{
height <- 1080
width <- round((16/9)*height)+40
res <- 120
fsz <- 22
a <- 0.6
palette <-  c("#1E88E5","#D81B60","#FFC107") # colorblind friendly-ish  
  
odir <- "output/"
f.iter10 <- "10x10x10_iter_output_n100_i25_s50.csv"
f.step10 <- "10x10x10_step_output_n100_i25_s50.csv"

f.iter5 <- "5x5x5_iter_output_n100_i25_s50.csv"
f.step5 <- "5x5x5_step_output_n100_i25_s50.csv"


f.sparse35_iter10 <- "sparse35_10x10x10_iter_output_n100_i25_s50.csv"
f.sparse35_step10 <- "sparse35_10x10x10_step_output_n100_i25_s50.csv"

f.sparse35_iter5 <- "sparse35_5x5x5_iter_output_n100_i25_s50.csv"
f.sparse35_step5 <- "sparse35_5x5x5_step_output_n100_i25_s50.csv"

f.sparse65_iter10 <- "sparse65_10x10x10_iter_output_n100_i25_s50.csv"
f.sparse65_step10 <- "sparse65_10x10x10_step_output_n100_i25_s50.csv"

f.sparse65_iter5 <- "sparse65_5x5x5_iter_output_n100_i25_s50.csv"
f.sparse65_step5 <- "sparse65_5x5x5_step_output_n100_i25_s50.csv"
}


step_ylim <- list(0,1e5)
new_step_ylim <- list(0, 2200)
iter_ylim <- list(0,100)

# plots
{
p_10 <- plotQxQxQ(f.iter10, f.step10, odir, step_ylim, new_step_ylim, iter_ylim,
                  a, palette, 10)
p_5 <- plotQxQxQ(f.iter5, f.step5,odir, step_ylim, new_step_ylim, iter_ylim,
                 a, palette, 5)
png(filename=paste0(odir,"QxQxQ_sample.png"), 
    width = width, height = height, res = res)
do.call(grid.arrange, 
        append(append(
          p_5,
          p_10), 
          list(
            ncol=4,
            top=textGrob(
"QxQxQ table samples
50 steps, 25 iterations/step, 100 samples/iteration",
              gp=gpar(fontsize=fsz,font=3)))))
dev.off()
}

{
#step_ylim <- list(0,25000)
#new_step_ylim <- list(0, 2000)
#iter_ylim <- list(0,75) 

p_10 <- plotQxQxQ(f.sparse35_iter10, f.sparse35_step10, 
                  odir, step_ylim, new_step_ylim, iter_ylim,
                  a, palette, 10)
p_5 <- plotQxQxQ(f.sparse35_iter5, f.sparse35_step5,
                 odir, step_ylim, new_step_ylim, iter_ylim,
                 a, palette, 5)

png(filename=paste0(odir,"sparse35_QxQxQ_sample.png"),
    width = width, height = height, res = res)
do.call(grid.arrange, 
        append(append(
          p_5,
          p_10), 
          list(
            ncol=4,
            top=textGrob(
"Sparse (%35 non-zero) QxQxQ table samples
50 steps, 25 iterations/step, 100 samples/iteration",
              gp=gpar(fontsize=fsz,font=3)))))
dev.off()
}


{
  #step_ylim <- list(0,1e5)
  #new_step_ylim <- list(0, 2200)
  #iter_ylim <- list(0,100)  
  
  p_10 <- plotQxQxQ(f.sparse65_iter10, f.sparse65_step10, 
                    odir, step_ylim, new_step_ylim, iter_ylim,
                    a, palette, 10)
  p_5 <- plotQxQxQ(f.sparse65_iter5, f.sparse65_step5,
                   odir, step_ylim, new_step_ylim, iter_ylim,
                   a, palette, 5)
  
  png(filename=paste0(odir,"sparse65_QxQxQ_sample.png"),
      width = width, height = height, res = res)
  do.call(grid.arrange, 
          append(append(
            p_5,
            p_10), 
            list(
              ncol=4,
              top=textGrob(
                "Sparse (%65 non-zero) QxQxQ table samples
50 steps, 25 iterations/step, 100 samples/iteration",
                gp=gpar(fontsize=fsz,font=3)))))
  dev.off()
}
