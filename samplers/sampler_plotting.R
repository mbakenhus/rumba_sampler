require(ggplot2)
require(grid)
require(gridExtra)

getPlots <- function(f.iter, f.step, odir, step_ylim, new_step_ylim, iter_ylim,
                     a, palette, subtitle){
  d.iter <-read.csv(paste0(odir,f.iter))
  d.step <-read.csv(paste0(odir,f.step))
  
  p.theme <- theme(
    title = element_text(size=12,face="bold"),
    axis.title = element_text(size = 14,face="bold"),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14))
  
  p0 <- ggplot(d.iter, aes(x=i, y=num_sampled, group=t, color=t)) + 
    geom_line(alpha = a)  +
    scale_color_gradientn(colors = palette) +
    labs(title=paste0("Points by Iteration  \n", subtitle),
         x="i", y = "Number of Points", color="t")+ 
    p.theme +
    do.call(ylim, iter_ylim)
  
  p1 <- ggplot(d.iter, aes(x=i, y=num_new_elements, group=t, color=t)) + 
    geom_line(alpha = a)  +
    scale_color_gradientn(colors = palette)+
    labs(title=paste0("Unique Points by Iteration \n", subtitle),
         x="i", y = "Number of Points", color="t")+
    p.theme +
    do.call(ylim, iter_ylim)
  
  p2 <- ggplot(d.step, aes(x=t, y=total_sample_size)) +
    geom_line() +
    labs(title=paste0("Total Points by Step t  \n", subtitle),
         x="t", y = "Number of Points")+
    p.theme +
    do.call(ylim, step_ylim)
  
  p3 <- ggplot(d.step, aes(x=t, y=num_new_elements)) +
    geom_line()  +
    labs(title=paste0("New Points by Step \n", subtitle),
         x="t", y = "Number of Points")+
    p.theme +
    do.call(ylim, new_step_ylim)
  
  
  return(list(p0,p1,p2,p3))
}

plotQxQxQ <- function(f.iter, f.step, odir, step_ylim, new_step_ylim, iter_ylim,
                      a, palette, Q){
  
  QxQxQ <- paste0("(",Q,"x",Q,"x",Q,")")
  
  
  return(getPlots(f.iter, f.step, odir, step_ylim, new_step_ylim, iter_ylim,
                  a, palette, QxQxQ))
  
}

plotAk <- function(f.iter, f.step, odir, step_ylim, new_step_ylim, iter_ylim,
                      a, palette, split_var){
  
  if(!is.na(split_var)){
    Ak <- paste0("(Split Ak for k=10 from ", split_var, ")")
  } else{
    Ak <- "(Single Ak for k=10)"
  }
 
  return(getPlots(f.iter, f.step, odir, step_ylim, new_step_ylim, iter_ylim,
                  a, palette, Ak))
  
}

plotDS98 <- function(f.iter, f.step, odir, step_ylim, new_step_ylim, iter_ylim,
                   a, palette, params){
  
  n <- params[1]
  i <- params[2]
  s <- params[3]
  
  ds98 <- paste0("(J=",n,",  I=",i, ", T=", s,")")
  
  return(getPlots(f.iter, f.step, odir, step_ylim, new_step_ylim, iter_ylim,
                  a, palette, ds98))
  
}
