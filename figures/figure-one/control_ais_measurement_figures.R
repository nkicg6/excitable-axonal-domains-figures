# ais plots for characterization figure
library(dplyr)
library(ggplot2)
library(Cairo)

SAVEALL <- FALSE
source("../plotting_defaults/ggplot_theme_defaults.R")

img_save_rt <- "../node_ais_characterization_figure/ais_figures/"

#### read and organize data ####

aisrawd <- readr::read_csv("../../../analysis_and_data/immunohistochemistry/data/summary_data/ais_length_201812to201907/aislen_summary_201912.csv")



###### histogram of control only
aisrawd %>%
  filter(group == "Control") %>%
  ggplot(aes(x = PathLength, fill = group)) +
  scale_fill_manual(values = c("grey")) +
  geom_histogram(color = "black", size = line_size, binwidth = 2) +
  geom_vline(aes(xintercept = mean(PathLength)), linetype = "longdash", size = line_size) +
  labs(x = "Length (\u03BCm)", y = "Number of initial semgents") +
  theme_and_axis_nolegend +
  coord_cartesian(xlim = c(0, 45), expand = F) +
  scale_x_continuous(breaks = seq(5, 42, 5)) +
  theme(plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"))

ggsave(file.path(img_save_rt, "ais-length-histogram-control.pdf"), width = 8, height = 5, dpi = 300, device = cairo_pdf)

ais_control_stats <- aisrawd %>%
  filter(group == "Control") %>%
  summarize(m = mean(PathLength), median_pl = median(PathLength), sd_len = sd(PathLength), n = n())

#### Stats ####
sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/ais_characterization.org")
pretty_print_results("AIS measurements", ais_control_stats)
sink()
