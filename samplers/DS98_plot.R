source("./sampler_plotting.R")


{
  height <- 1080
  width <- round((16/9)*height)+40
  res <- 120
  fsz <- 22
  a <- 0.6
  palette <-  c("#1E88E5","#D81B60","#FFC107") # colorblind friendly-ish  
  
  odir <- "output/"
  
  f.step.n100_i5_s5 <- "DS98_step_out_n100_i5_s5.csv"
  f.iter.n100_i5_s5 <- "DS98_iter_out_n100_i5_s5.csv"
  
  f.step.n100_i10_s5 <- "DS98_step_out_n100_i10_s5.csv"
  f.iter.n100_i10_s5 <- "DS98_iter_out_n100_i10_s5.csv"
  
  f.step.n100_i5_s10 <- "DS98_step_out_n100_i5_s10.csv"
  f.iter.n100_i5_s10 <- "DS98_iter_out_n100_i5_s10.csv"
  
  f.step.n1000_i5_s5 <- "DS98_step_out_n1000_i5_s5.csv"
  f.iter.n1000_i5_s5 <- "DS98_iter_out_n1000_i5_s5.csv"
  
  f.step.n1000_i15_s200 <- "DS98_step_out_n1000_i15_s200.csv"
  f.iter.n1000_i15_s200 <- "DS98_iter_out_n1000_i15_s200.csv"
}




# plots
{
  
  step_ylim <- list(0,3.5e3)
  new_step_ylim <- list(0, 750)
  iter_ylim <- list(0,1e2)
  
  p_n100_i5_s5<- plotDS98(f.iter.n100_i5_s5, f.step.n100_i5_s5, odir, step_ylim, new_step_ylim, iter_ylim,
                a, palette,  c(100,5,5))
  p_n100_i10_s5<- plotDS98(f.iter.n100_i10_s5, f.step.n100_i10_s5, odir, step_ylim, new_step_ylim, iter_ylim,
                        a, palette,  c(100,10,5))
  p_n100_i5_s10<- plotDS98(f.iter.n100_i5_s10, f.step.n100_i5_s10, odir, step_ylim, new_step_ylim, iter_ylim,
                        a, palette,  c(100,5,10))
  

  png(filename=paste0(odir,"DS98_compare.png"), 
      width = width, height = height*1.25, res = res)
  do.call(grid.arrange, 
          append( mapply(c,list(p_n100_i5_s5,
                        p_n100_i10_s5,
                        p_n100_i5_s10)), 
            list(
              ncol=4,
              top=textGrob(
                "DS98 Fiber Samples",
                gp=gpar(fontsize=fsz,font=3)))))
  dev.off()
}

{
  step_ylim <- list(2e3,1.2e4)
  new_step_ylim <- list(2e3, 3e3)
  iter_ylim <- list(200,800)
  p_n1000_i5_s5<- plotDS98(f.iter.n1000_i5_s5, f.step.n1000_i5_s5, odir, step_ylim, new_step_ylim, iter_ylim,
                           a, palette,  c(1000,5,5))
  png(filename=paste0(odir,"DS98_n1000.png"), 
      width = width, height = height/2, res = res)
  do.call(grid.arrange, 
          append( p_n1000_i5_s5, 
                  list(
                    ncol=4,
                    top=textGrob(
                      "DS98 Fiber Samples",
                      gp=gpar(fontsize=fsz,font=3)))))
  dev.off()
}

{
  step_ylim <- list(0,1.3e6)
  new_step_ylim <- list(4e3, 8e3)
  iter_ylim <- list(0,1000) 
  
  p_large <- plotDS98(f.iter.n1000_i15_s200, f.step.n1000_i15_s200, 
                     odir, step_ylim, new_step_ylim, iter_ylim,
                     a, palette, c(1000,15,200))
  
  png(filename=paste0(odir,"DS98_large.png"),
      width = width, height = height/2, res = res)
  do.call(grid.arrange, append(p_large, 
                               list(ncol=4,top=textGrob(
                                 "DS98 Fiber Sample",
                                 gp=gpar(fontsize=fsz,font=3)))))
  dev.off()
}
