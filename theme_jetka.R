#' theme_rcc
#'
#' @param ...
#'
#' @return ...
#'
#' @examples ...
#'
theme_jetka_new <-  function (
  theme.base_size = 12,
  theme.base_family = "sans",
  theme.title_size = 18,
  theme.text_size = 3*theme.title_size/4,
  theme.margins = c(1,1, 1, 1),
  legend.position="right",
  base_size = NULL,
  base_family = NULL,
  text_size = NULL,
  text_size_2 = NULL,
  ...)
{
  if(!is.null(base_size)){
    theme.base_size <- base_size
  }
  if(!is.null(base_family)){
    theme.base_family <- base_family
  }
  
  if(!is.null(text_size)){
    theme.title_size <- text_size
    text_size_2 <- 3*text_size/4 
    theme.text_size <- text_size_2
  }
  
  if(!is.null(text_size_2)){
    theme.text_size <- text_size_2
  }
  
  if(packageVersion("ggthemes") >= 4) {
    return((ggthemes::theme_foundation(
      base_size = theme.base_size,
      base_family = theme.base_family) +
        ggplot2::theme(line = ggplot2::element_line(),
                       rect = ggplot2::element_rect(fill =
                                                      ggthemes::ggthemes_data$fivethirtyeight$value[3],
                                                    linetype = 0, colour = NA),
                       plot.title = ggplot2::element_text(colour =
                                                            ggthemes::ggthemes_data$fivethirtyeight$value[1],
                                                          vjust = 1, hjust = 0.5,
                                                          size=theme.title_size,
                                                          face="bold"),
                       text = ggplot2::element_text(colour = ggthemes::ggthemes_data$fivethirtyeight$value[1]),
                       axis.title = ggplot2::element_text(size=theme.title_size),
                       axis.title.y = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size=theme.text_size, face="bold"),
                       axis.text.x = ggplot2::element_text(angle = 90),
                       axis.ticks = ggplot2::element_blank(),
                       axis.line.x = ggplot2::element_line(),
                       axis.line.y = ggplot2::element_blank(),
                       legend.position=legend.position,
                       legend.background = ggplot2::element_rect(fill = "white"),
                       panel.grid = ggplot2::element_line(colour = NULL),
                       panel.grid.major =
                         ggplot2::element_line(colour = ggthemes::ggthemes_data$fivethirtyeight$value[2]),
                       panel.grid.minor =
                         ggplot2::element_blank(),
                       panel.background = ggplot2::element_rect(fill = "white"),
                       # plot.title = ggplot2::element_text(hjust = 0, size = rel(1.75), face = "bold"),
                       plot.margin = unit(theme.margins, "lines"),
                       plot.background = ggplot2::element_rect(fill = "white"),
                       
                       strip.background = ggplot2::element_rect(fill = "white"),
                       strip.text =
                         ggplot2::element_text(size= theme.text_size,
                                               face="bold",
                                               vjust = 0.5,
                                               lineheight = theme.text_size*3))))
  } else {
    return((theme_foundation(base_size = theme.base_size, base_family = theme.base_family) + 
              theme(line = element_line(), 
                    rect = element_rect(fill = ggthemes_data$fivethirtyeight["ltgray"], linetype = 0, colour = NA),
                    #plot.title = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"], vjust = 1, hjust = 0.5, size=theme.title_size, face="bold"), 
                    text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]), 
                    axis.title = element_text(size=theme.title_size-7),
                    axis.title.y = element_text(angle=90),
                    axis.text = element_text(size=theme.text_size, face="bold"),
                    axis.text.x = element_text(angle = 90),
                    axis.ticks = element_blank(),
                    axis.line.x = element_line(),
                    axis.line.y = element_blank(), 
                    legend.position="right",
                    legend.background = element_rect(fill = "white"),
                    panel.grid = element_line(colour = NULL), 
                    panel.grid.major = element_line(colour = ggthemes_data$fivethirtyeight["medgray"]), 
                    panel.grid.minor = element_blank(), 
                    panel.background = element_rect(fill = "white"),
                    plot.title = element_text(hjust = 0.5, size = rel(1.75), face = "bold"), 
                    plot.margin = unit(c(1,1, 1, 1), "lines"), 
                    plot.background = element_rect(fill = "white"),
                    
                    strip.background = element_rect(fill = "white"),
                    strip.text = element_text(size= theme.text_size, face="bold", vjust = 0.5, lineheight = theme.text_size*3))))
  }
}
