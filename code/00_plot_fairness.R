require(dplyr)
require(tidyr)
require(ggplot2)
require(ggpubr)

plotFairnessDensity <- function(v1,v2,title,delta=TRUE) {
  plotDF <- bind_rows(data.frame(value = v1, Group = "v1"),
                      data.frame(value = v2, Group = "v2")
  )
  if(delta) {
    labelVec = c('Gain designation', 'Lose designation')
    xLabel = "Population (%)"
  } else {
    labelVec = c('Old Model', 'New Model')
    xLabel =  "Population (%)"
  }
  ggplot(plotDF, aes(x=value*100, fill=Group)) +
    geom_density(alpha=.25) + ggtitle(title) +
    scale_fill_discrete(NULL, labels=labelVec,type=c("#4DBBD5B2","#E64B35B2")) +
    scale_x_continuous(xLabel, expand = c(0,0), limits = c(0,100)) +
    scale_y_continuous("Density", expand = c(0,0)) +
    theme_classic(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.margin = margin(1,.4, .5, .4, "cm")) 
}


plotFairnessDensity2D <- function(df, racial_group, xlabel, poverty_type, xlimits = c(NA,NA), ylimits = c(NA,NA), plotpoints = FALSE, return_legend = FALSE, expand_amount = 0.015) {
  if(poverty_type == "race"){
    df$y <- df$rPoverty
    ylabel = "Tract-level poverty within racial group (%)"
  } else if(poverty_type == "tract"){
    df$y <- df$pPoverty
    ylabel = "Tract-level poverty (%)"
  } else{
    print("Error: poverty_type must be c('race','tract')")
    return(-1)
  }
  
  add_2d_theme_h <-  function(g, xlimits, ylimits, xlabel, ylabel, expand_amount){
    g + 
      scale_fill_manual(values = pal, guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
      scale_color_manual(values = pal) +
      scale_x_continuous(breaks = pretty_breaks(), limits = xlimits, expand = c(0,0)) +
      scale_y_continuous(expand = expansion(mult = c(0, expand_amount)), breaks = pretty_breaks(), limits = ylimits) +
      scale_alpha_discrete("Percentage of tracts\nbounded by contour", labels = probs.lab, guide = guide_legend(title.position = "top", title.hjust = 0.5)) + 
      labs(fill = "Census tract status", x = xlabel, y = ylabel) + 
      theme_classic(base_size = 14) + 
      guides(color = "none") +
      theme(strip.text.x = element_text(size = 14),
            legend.direction = "horizontal",
            legend.box = "horizontal") +
      coord_cartesian(clip = "off")
  }
  
  add_2d_theme_v <-  function(g, xlimits, ylimits, xlabel, ylabel, expand_amount){
    g + 
      scale_fill_manual(values = pal) +
      scale_color_manual(values = pal) +
      scale_x_continuous(breaks = pretty_breaks(), limits = xlimits, expand = c(0,0)) +
      scale_y_continuous(expand = expansion(mult = c(0, expand_amount)), breaks = pretty_breaks(), limits = ylimits) +
      scale_alpha_discrete("Percentage of tracts\nbounded by contour", labels = probs.lab) + 
      labs(fill = "Census tract status", x = xlabel, y = ylabel) + 
      theme_classic(base_size = 14) + 
      guides(color = "none") +
      theme(strip.text.x = element_text(size = 14),
            legend.direction = "vertical",
            legend.box = "vertical") +
      coord_cartesian(clip = "off")
  }
  
  if(return_legend %in% c(FALSE, "h")){
    add_2d_theme <- add_2d_theme_h
  } else{
    add_2d_theme <- add_2d_theme_v
  }
  
  g <- ggplot(df %>% filter(race == racial_group), 
              aes(x=rProp*100, y = y*100, fill = Group, color = Group)) 
  if(plotpoints) g <- g + geom_point(alpha = 0.7, size = 0.7, shape = 20)
  
  g <- g + geom_hdr(probs = probs, method = method_kde(adjust = 1))
  
  g <- add_2d_theme(g, xlimits, ylimits, xlabel, ylabel, 0)
  
  g.r <- ggplot(df %>% filter(race == racial_group), 
                aes(x=rProp*100, fill = Group, color = Group)) +
    geom_density(alpha = 0.25)
  
  g.r <- add_2d_theme(g.r, xlimits, c(NA, NA), NULL, NULL, expand_amount) + theme_void()
  
  g.p <- ggplot(df %>% filter(race == racial_group), 
                aes(x=y*100, fill = Group, color = Group)) +
    geom_density(alpha = 0.25)
  
  g.p <- add_2d_theme(g.p, xlimits, c(NA, NA), NULL, NULL, expand_amount) + theme_void() + rotate()
  
  g.leg = get_legend(g)
  if(return_legend != FALSE) return(g.leg)
  
  g.combined <- ggarrange(
    g.r, NULL, NULL, NULL, NULL,NULL, g, NULL, g.p, ncol = 3, nrow = 3,
    align = "hv", widths = c(4, -0.3, 1),
    heights = c(1, -0.3, 4),
    common.legend = TRUE, legend = "none"
  ) +
    theme(plot.margin = margin(1,1,1,1, "cm")) 
  
  g.combined <- annotate_figure(g.combined, fig.lab = racial_group, fig.lab.pos = "top", fig.lab.size = 16)
  return(g.combined)
}