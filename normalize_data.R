normalize_data <- function(data,
                           normalize_factor = 65535){
  data.intensity_colnames <- grepl("Intensity", colnames(data)) & 
    !grepl("Location", colnames(data))
  data[, data.intensity_colnames] <- data[, data.intensity_colnames] * normalize_factor
  return(list(data = data))
}


cc.assign <- function(dane, cc.borders, DAPI.column){
  c.phase <- c()
  for (i in dane[[DAPI.column]]) {
    if (i < cc.borders[1]){
      c.phase <- c(c.phase, "outliers")
    } else if (i < cc.borders[2]){
      c.phase <- c(c.phase, "G1")
    } else if (i < cc.borders[3]){
      c.phase <- c(c.phase, "S")
    } else if (i < cc.borders[4]){
      c.phase <- c(c.phase, "G2/M")
    } else { 
      c.phase <- c(c.phase, "outliers")
    }
  }
  return(c.phase)
}

dna.histogram <- function(data, channel, bins, 
                          border.list, title, 
                          xlab, ylab, xlim, ylim=c(0, 0)){
  g <- ggplot(data, aes_string(x=channel))+
    geom_histogram(bins=bins)+
    geom_vline(xintercept = border.list[[1]], col="red")+
    geom_vline(xintercept = border.list[[2]], col="orange")+
    geom_vline(xintercept = border.list[[3]], col="green")+
    geom_vline(xintercept = border.list[[4]], col="blue")+
    ggtitle(title)+
    theme_jetka()+
    xlab(xlab)+
    ylab(ylab)+
    xlim(xlim)+
    if(ylim[2]){ 
      ylim(ylim)
    }
  return(g)
}

cc.to.factor <- function(phases=c("G1","S","G2/M"), df){
  df.subset <- df[df$phase %in% phases, ]
  df.subset$phase <- factor(df.subset$phase, levels = phases)
  return(df.subset)
}
variable.subset <- function(data, columns, new.columns){
  data.2 <- data[, colnames(data) %in% columns]
  colnames(data.2) <- new.columns
  return(data.2)
}
randomRows <- function(df, n){
  return(df[sample(nrow(df), n), ])
}

library(gridExtra)
library(flowCore)
library(reshape2)
library(ggplot2)
library(deamer)
library(foreach)
library(doParallel)
library(dplyr)
