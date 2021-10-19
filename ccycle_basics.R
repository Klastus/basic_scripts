package.list <- list("ggplot2")
try({package.list
  package.load <- sapply(package.list, function(package.name){
    package.exist <- require(package.name, character.only = TRUE)
    if(!package.exist){
      install.packages(package.name)
      return(library(package.name, character.only = TRUE))
    }
    return(package.exist)
  })
})

theme_trajectories <- function(border.thickness = 0.5,
                               axis.num.size = 10, 
                               axis.name.size = 11,
                               aspect.ratio = FALSE){
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=border.thickness, fill=NA),
        panel.background = element_blank(),
        axis.ticks = element_line(colour = "black", size = border.thickness),
        axis.text.x = element_text(colour = "black", size = axis.num.size),
        axis.text.y = element_text(colour = "black", size = axis.num.size),
        strip.background = element_blank(),
        axis.title = element_text(face="plain", size = axis.name.size),
        plot.title = element_text(hjust = 0.5),
        legend.key=element_blank())+
    if(aspect.ratio != FALSE){
      theme(aspect.ratio = aspect.ratio)
    } else {theme()}
}

distinguish <- function(data, 
                        borders, 
                        var.column, 
                        factor.list){
  c.phase <- c()
  for (i in data[[var.column]]) {
    for(discriminated in 1:length(borders)){
      if (i <= borders[[discriminated]]){
        c.phase <- c(c.phase, factor.list[[discriminated]])
        break
      } else if(discriminated == length(borders)){
        c.phase <- c(c.phase, factor.list[[1]])
      }
    }
  }
  return(factor(c.phase, levels = paste(factor.list)))
}

dna.histogram <- function(data, channel, bins = 100, 
                          border.list, title = "random title", 
                          xlab = "DAPI", ylab = "count", xlim,
                          thickness = 1, 
                          aspect.ratio = 1,
                          linetype = 2){
  g <- ggplot(data, aes_string(x=channel))+
    geom_histogram(bins=bins)+
    geom_vline(xintercept = border.list[[1]], col="red", 
               size = thickness, linetype = linetype)+
    geom_vline(xintercept = border.list[[2]], col="orange", 
               size = thickness, linetype = linetype)+
    geom_vline(xintercept = border.list[[3]], col="green", 
               size = thickness, linetype = linetype)+
    geom_vline(xintercept = border.list[[4]], col="blue", 
               size = thickness, linetype = linetype)+
    ggtitle(title)+
    scale_x_continuous(expand = c(0, 0), name = xlab, limits = xlim)+
    scale_y_continuous(expand = c(0, 0), name = ylab)+
    theme_trajectories(aspect.ratio = aspect.ratio)+
    ylab(ylab)
  return(g)
}
