#' @title smx_biomass_plot
#' 
#' @export
#' 
#' @param data A data.frame
smx_biomass_plot <- function (data) 
{
  
  lengd <- unique(data$l)
  p <- ggplot(data,aes(year, Bp/1e3, fill = sur)) + 
    theme_bw() +
    geom_ribbon(aes(year, ymin = Bp/1e3 * (1 - cvBp), ymax = Bp/1e3 * (1 + cvBp)), alpha = I(0.4)) + 
    geom_pointrange(aes(year,ymin = Bp/1e3 * (1 - cvBp), ymax = Bp/1e3 * (1 + cvBp), col = sur)) +
    expand_limits(y = 0) + 
    labs(x="",y="",title = paste("biomass >", lengd, " cm", sep = "")) + 
    scale_color_brewer(palette='Set1') +
    scale_fill_brewer(palette='Set1') +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 1),
          plot.margin = unit(c(0,0,0,0),"mm")) +
    scale_x_continuous(minor_breaks = 1)
  return(p)
}

#' @title smx_abundance_plot
#' 
#' @export
#' 
#' @param data A data.frame
smx_abundance_plot <- function (data) 
{

  lengd <- unique(data$l)
  p <- ggplot(data,aes(year, Nm/1e3, fill = sur)) + 
    theme_bw() +
    geom_ribbon(aes(year,ymin = Nm/1e3 * (1 - cvNm), ymax = Nm/1e3 * (1 + cvNm)), alpha = I(0.4)) +
    geom_pointrange(aes(year,ymin = Nm/1e3 * (1 - cvNm), ymax = Nm/1e3 * (1 + cvNm), col = sur)) + 
    expand_limits(y = 0) + 
    labs(x="",y="",title = paste("abundance <", lengd, " cm", sep = "")) + 
    scale_color_brewer(palette='Set1') +
    scale_fill_brewer(palette='Set1') +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 1),
          plot.margin = unit(c(0,0,0,0),"mm")) +
    scale_x_continuous(minor_breaks = 1)
  return(p)
}

#' @title smx_4plot
#' 
#' @export
#' 
#' @param tegund character, like "torskur", "ysa", ....
#' @param dat data.frame
#' @param std data.frame containing some specs
smx_4plot <- function(tegund, dat, std=smx_standards) {
  std <- std[std$tegund == tegund,]
  x <- dat[dat$spe %in% std$spe & dat$l %in% std$a.length,]
  bioA <- smx_biomass_plot(dat[dat$spe %in% std$spe & dat$l %in% std$a.length,])
  bioB <- smx_biomass_plot(dat[dat$spe %in% std$spe & dat$l %in% std$b.length,])
  bioC <- smx_biomass_plot(dat[dat$spe %in% std$spe & dat$l %in% std$c.length,])
  numD <- smx_abundance_plot(dat[dat$spe %in% std$spe & dat$l %in% std$d.length,])
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(2,2)))
  vplayout <- function(x,y) viewport(layout.pos.row=x,layout.pos.col=y)
  print(numD,vp=vplayout(1,1))
  print(bioA,vp=vplayout(1,2))
  print(bioB,vp=vplayout(2,1))
  print(bioC,vp=vplayout(2,2))
}