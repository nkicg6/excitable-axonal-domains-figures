# Plotting defaults for R figures
# Author: Nicholas George
# last updated: 2021-01-22
library(ggplot2)

# call sink("file/to/write/to.txt") before this, then do all results, and close with sink()
pretty_print_results <- function(name, stuff) {
  print(stringr::str_glue("* {name} \n"))
  print(stuff)
}

theme_set(theme_classic())

custom_annotation_size <- 8
occl_single_color<- "#88419d"
open_single_color <-"#00688B"
black <- "#000000"
grey <- "#808080"


theme_and_axis_nolegend <- theme(legend.position = "None",
                                 text = element_text(size=25, face = "bold"),
                                 axis.text = element_text(size = 18, face = "bold", color = black),
                                 axis.line = element_line(color = black, size = 0.6))


theme_and_axis_legend <- theme(text = element_text(size=25, face = "bold"),
                               legend.title = element_blank(),
                               legend.background = element_blank(),
                               axis.text = element_text(size = 18, face = "bold", color = black),
                               axis.line = element_line(color = black, size = 0.6))


ctrl_color <- scale_color_manual(values = c(black, grey))
ctrl_fill <- scale_fill_manual(values = c(black, grey))
occl_color <- scale_color_manual(values = c(open_single_color, occl_single_color))
occl_fill <- scale_fill_manual(values = c(open_single_color, occl_single_color))

three_color <- scale_color_manual(values = c(black, open_single_color, occl_single_color))
three_color_ctrl_occl_open <-scale_color_manual(values = c(black, occl_single_color, open_single_color))
three_fill <- scale_fill_manual(values = c(black, open_single_color, occl_single_color))

control_vs_occl_color <- scale_color_manual(values = c(black, occl_single_color))
control_vs_occl_fill <- scale_fill_manual(values = c(black, occl_single_color))

color_color <- scale_color_manual(values = c(black,grey,open_single_color, occl_single_color))
fill_fill <- scale_fill_manual(values = c(black,grey,open_single_color, occl_single_color))

oligo_lineage_fill <-  scale_fill_manual(values = c("#00BFFF", "magenta"))
oligo_lineage_color <-  scale_color_manual(values = c("#00BFFF", "magenta"))

pt_alpha <- 0.6
pt_stroke <- 1
line_size <- 1.5
ecdf_pt_size <- 5
pt_size <- 2
ecdf_pad <- FALSE
narrow_jitter_width <- 0.25
barplot_width <- 0.5

insert_theme <- theme(legend.position = "None",
      text = element_text(size=25, face = "bold"),
      axis.text = element_text(size = 18, face = "bold", color = black),
      axis.text.x = element_blank(),
      axis.line = element_line(color = black, size = 0.6),
      axis.ticks.x = element_blank())
