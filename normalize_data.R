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

discriminate <- function(data, borders, var.column, factor.list){
  c.phase <- c()
  for (i in data[[var.column]]) {
    if (i < borders[[1]]){
      c.phase <- c(c.phase, factor.list[[1]])
    } else if (i < borders[[2]]){
      c.phase <- c(c.phase, factor.list[[2]])
    } else if (i < borders[[3]]){
      c.phase <- c(c.phase, factor.list[[3]])
    } else if (i < borders[[4]]){
      c.phase <- c(c.phase, factor.list[[4]])
    } else { 
      c.phase <- c(c.phase, factor.list[[1]])
    }
  }
  return(c.phase)
}

dna.histogram <- function(data, channel, bins = 100, 
                          border.list, title = "random title", 
                          xlab = "DAPI", ylab = "count", xlim){
  g <- ggplot(data, aes_string(x=channel))+
    geom_histogram(bins=bins)+
    geom_vline(xintercept = border.list[[1]], col="red")+
    geom_vline(xintercept = border.list[[2]], col="orange")+
    geom_vline(xintercept = border.list[[3]], col="green")+
    geom_vline(xintercept = border.list[[4]], col="blue")+
    ggtitle(title)+
    theme_sysbiosig()+
    xlab(xlab)+
    ylab(ylab)+
    xlim(xlim)
  return(g)
}

cc.to.factor <- function(phases=c("G1","S","G2/M"), df){
  df.subset <- df[df$phase %in% phases, ]
  df.subset$phase <- factor(df.subset$phase, levels = phases)
  return(df.subset)
}

variable.subset <- function(data, columns, new.columns = 0){
  data.2 <- data[, colnames(data) %in% columns]
  if(new.columns[1]!=0){
    colnames(data.2) <- new.columns
    }
  return(data.2)
}

randomRows <- function(df, n){
  return(df[sample(nrow(df), n), ])
}


CV <- function(x){
  return(sd(x)/mean(x))
}

push.dir <- function(folder.name){
  if(!dir.exists(folder.name)){
    dir.create(folder.name, recursive = TRUE)
  }
}

zero.time.multiply <- function(data.zero, data.no.zero, 
                               time.of.zero = 30,
                               stimulation.of.zero = 0){
  data.zero.unchanged <- data.zero
  data.zero.unchanged$duplication <- "no"
  data.zero.unchanged$time.1.1 <- time.of.zero
  data.zero.unchanged$stimulation.1.1 <- stimulation.of.zero
  data.zeros <- data.frame()
  
  stims <- unique(data.no.zero$stimulation.1.1)
  
  for(stim in stims){
    data.zero$stimulation.1.1 <- stim
    data.zero$time.1.1 <- 0
    data.zeros <- rbind(data.zeros, data.zero)
  }
  data.zeros$duplication <- "yes"
  data.no.zero$duplication <- "no"
  
  if(stimulation.of.zero == -1){
    return(rbind(data.no.zero, data.zeros))
  } else {
  return(rbind(data.zero.unchanged, data.no.zero, data.zeros))
  }
}


package.list <- list("ggplot2", 
                     "gridExtra",
                     "reshape2",
                     "ggplot2",
                     "deamer",
                     "foreach",
                     "doParallel",
                     "dplyr",
                     "SysBioSigTheme",
                     "scales",
                     "RColorBrewer",
                     "ggforce",
                     "rlist",
                     "extrafont")
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


copy.IPIQA <- function(ipiqa.path = "D:/IPIQA/analysis_output/2019-08-27/",
                       core.destination = "D:/Piotrek/Experiments/ICF/",
                       experiment.full.name,
                       csv.name.list = c("ShrinkedNuclei.csv"),
                       project,
                       shift.value = FALSE) {
  
  string <- tail(strsplit(experiment.full.name, "-")[[1]], 1)
  exper.last.name <- regmatches(string, regexpr("PT|JW\\d+", string))
  
  exper.path = list.dirs(paste(ipiqa.path, experiment.full.name, sep = ""),
                         recursive = FALSE, full.names = TRUE) [1]
  
  normalizations <- list.dirs(exper.path, recursive = FALSE, full.names = FALSE)
  for(normaliz in normalizations){
    for(csv.name in csv.name.list){
    csv.path <- paste(exper.path, 
                      normaliz, 
                      "data_quantify", 
                      csv.name,
                      sep = "/")
    destination.path <- paste(core.destination, 
                              exper.last.name, 
                              "/R/", 
                              project, 
                              "/input/", 
                              normaliz, 
                              "/",
                              if(shift.value != FALSE){
                              shift.value},
                              sep = "")
    push.dir(destination.path)
    file.copy(from = csv.path,
              to = destination.path,
              overwrite = TRUE)
    }
  }
}

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

theme.itrc <- function(){
  theme_itrc <-theme(axis.title = element_text(size= 11,
                                               face="plain",
                                               vjust = 0.5),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     axis.line.x = element_blank(),
                     axis.line.y = element_blank(),
                     panel.background = element_blank(),
                     strip.background = element_blank(),
                     strip.text =
                       element_text(size= 10,
                                    face="plain",
                                    vjust = 0.5#,lineheight = theme.text_size*3
                       ))
}


nice.colors <- c("forestgreen", "steelblue", "coral3", "cadetblue", "mediumorchid4")
# IFN.pal <- c(brewer.pal(5, "PuBu"))[c(1,2,3,5)]
IFN.pal <- c(brewer.pal(8, "PuBu"))[c(3,5,7,8)]
TNF.pal <- c(brewer.pal(5, "OrRd"))[c(1,2,3,5)]
OSM.pal <- c("#cbdeda", "#63bda3", "#218845", "#114320")
overlaps.pal <- c(brewer.pal(5, "BuGn"))[c(1,2,3,5)]

pdf.push <- function(file, ...){
    if(!dir.exists(dirname(file))){
      dir.create(dirname(file), recursive = TRUE)
    }
  pdf(file, ...)
}

extract.MK <- function(exp.name){ 
  base.path <- "D:/Piotrek/Experiments/ICF/"
  rds.path <- paste(base.path, "MK_all/R/input/data_ffc_filtered_oranized.RDS", sep = "")
  poster.data.list <- readRDS(file = rds.path)
  
  F1=poster.data.list[[exp.name]]
  
  F1$time <- factor(F1$time)
  F1$Intensity_MeanIntensity_Alexa488 <- F1$Intensity_MeanIntensity_Alexa
  F1$stimulation <- as.factor(F1$stimulation)
  return(F1)
}

pbs.reference <- 19.6605
pbs.normalize <- function(data, 
                          pbs.camcor, 
                          pbs.reference = 19.6605, 
                          column = "Intensity_MeanIntensity_Alexa488"){
  data[[column]] <- data[[column]] * pbs.reference / pbs.camcor
  return(data)
}

load.bio <- function(bio.path,
                     columns = c("Intensity_IntegratedIntensity_Alexa488",
                                        "Intensity_MeanIntensity_Alexa488")){
  
  bio.nuc <- read.table(bio.path, header=TRUE, sep=",")
  
  bio.nuc <- normalize_data(bio.nuc)$data
  
  from <- which(colnames(bio.nuc) == grep(".1.1", colnames(bio.nuc), value = TRUE)[1])
  to <- length(bio.nuc[1 ,])
  
  if(columns == "all"){
    columns.all <- colnames(bio.nuc)
  } else {
  columns.basic <- c("AreaShape_Area",
                     "AreaShape_Center_X",
                     "AreaShape_Center_Y")
  columns.all <- c(columns.basic, columns, 
                   colnames(bio.nuc)[from:to])
  }
  bio.nuc <- bio.nuc[, columns.all]
  
  return(bio.nuc)
}

# how to install and use fonts:
# 1. download the font from any webpage (ttf format) and install it (right click on windows)
# 2. load font in R: windowsFonts("LM Roman 10"=windowsFont("LM Roman 10"))
# 3. e.g. family = "LM Roman 10" in theme() or geom_xxx
# 4. save cairo_pdf()
# in case of troubles try the package extrafont

# on mac: run loadfonts() before
# loadfonts()
# font_import(pattern = "08635755*") 
# but it doesn't work anyway
