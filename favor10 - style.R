
library(extrafont)
if (0) {
  font_import(pattern="[T/t]imes")
  font_import(pattern="[G/g]eorgia")
  fonts()
}
loadfonts(device="win")

plot_style_1 = function (p) {
  if (1) {
    cols = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')
    if (1) {
      cols = cols[c(-6,-11)]
    } else if (0) {
      cols = cols[c(7:10)]
    }
    c1 = "beige"
    c2 = "#EFEFCA"
    c1 = "#F0EBDE"
    c2 = "#E9DDB4"
    p +
      theme_igray(base_size=13) +
      guides(colour = guide_legend(override.aes = list(size=4))) + 
      theme(panel.margin=unit(2,"lines")) + 
      scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65)) +
      theme(plot.background = element_rect(fill = c1)) + 
      theme(legend.background = element_rect(fill = c1)) + 
      theme(strip.background = element_rect(fill = c2)) + 
      theme(panel.grid.major = element_line(colour = c1)) +
      scale_colour_manual(values = cols) +
      theme(text = element_text(family="Georgia"))
  } else {
    p +
      theme_igray(base_size=13) +
      guides(colour = guide_legend(override.aes = list(size=4))) + 
      theme(panel.margin=unit(2,"lines")) + 
      scale_y_continuous(breaks=seq(0, 100, 10),limits=NA)
  }
}
