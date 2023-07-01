source("./sampler_plotting.R")


{
  height <- 1080
  width <- round((16/9)*height)+40
  res <- 120
  fsz <- 22
  a <- 0.6
  palette <-  c("#1E88E5","#D81B60","#FFC107") # colorblind friendly-ish  
  
  odir <- "output/"
  f.iter.split.x0 <- "Ak_split_iter_x0.csv"
  f.iter.split.x1 <- "Ak_split_iter_x1.csv"
  
  f.step.split.x0 <- "Ak_split_step_x0.csv"
  f.step.split.x1 <- "Ak_split_step_x1.csv"
  
  f.iter.single <- "Ak_single_iter_out.csv"
  
  f.step.single <- "Ak_single_step_out.csv"
}


step_ylim <- list(0,2.1e3)
new_step_ylim <- list(0, 300)
iter_ylim <- list(0,200)

# plots
{
  p_x0<- plotAk(f.iter.split.x0, f.step.split.x0, odir, step_ylim, new_step_ylim, iter_ylim,
                    a, palette, "x0")
  p_x1<- plotAk(f.iter.split.x1, f.step.split.x1, odir, step_ylim, new_step_ylim, iter_ylim,
                a, palette, "x1")
  png(filename=paste0(odir,"Ak_split_sample.png"), 
      width = width, height = height, res = res)
  do.call(grid.arrange, 
          append(append(
            p_x0,
            p_x1), 
            list(
              ncol=4,
              top=textGrob(
                "Ak sample
22 steps (11 each), 8 iterations/step, 1000 samples/iteration",
                gp=gpar(fontsize=fsz,font=3)))))
  dev.off()
}

{
  #step_ylim <- list(0,25000)
  #new_step_ylim <- list(0, 2000)
  #iter_ylim <- list(0,75) 
  
  p_single <- plotAk(f.iter.single, f.step.single, 
                    odir, step_ylim, new_step_ylim, iter_ylim,
                    a, palette, 10)
  
  png(filename=paste0(odir,"Ak_single_sample.png"),
      width = width, height = height/2, res = res)
  do.call(grid.arrange, append(p_single, 
            list(ncol=4,top=textGrob(
                "Ak sample
32 steps, 8 iterations/step, 1000 samples/iteration",
                gp=gpar(fontsize=fsz,font=3)))))
  dev.off()
}
