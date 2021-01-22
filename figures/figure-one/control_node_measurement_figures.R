# node plots for characterization figure
library(dplyr)
library(ggplot2)
library(Cairo)

SAVEALL <- FALSE
source("../plotting_defaults/ggplot_theme_defaults.R")
img_save_rt <- "../node_ais_characterization_figure/node_figures/"

#### read and organize data ####

rawd <- readr::read_csv("../../../analysis_and_data/immunohistochemistry/data/summary_data/node_length_manual_2018to2019/manual_node_length_summarized_202004.csv")

# summary
select(rawd, animal_id, type, side) %>%
  group_by(animal_id, type, side) %>%
  tally()
# >> 4 control 4 experimental
select(rawd, side) %>%
  group_by(side) %>%
  tally()

###### histogram of control only
rawd %>%
  filter(type == "Control") %>%
  ggplot(aes(x = fwhm, fill = type)) +
  scale_fill_manual(values = c("grey")) +
  geom_histogram(color = "black", size = line_size, binwidth = 0.2) +
  geom_vline(aes(xintercept = mean(fwhm)), linetype = "longdash", size = line_size) +
  labs(x = "Length (\u03BCm)", y = "Number of nodes") +
  theme_and_axis_nolegend +
  coord_cartesian(xlim = c(0, 2.8), expand = F) +
  scale_x_continuous(breaks = seq(0.5, 2.5, 0.5))

ggsave(file.path(img_save_rt, "node-length-histogram-control.pdf"), width = 8, height = 5, dpi = 300, device = cairo_pdf)

control_stats <- rawd %>%
  filter(type == "Control") %>%
  summarize(
    mean_nodes = mean(fwhm), median_nodes = median(fwhm),
    sd_nodes = sd(fwhm), n_nodes = n(), min_nodes = min(fwhm), max_nodes = max(fwhm)
  )

rawd %>%
  filter(type == "Control") %>%
  group_by(animal_id, section) %>%
  summarize(
    mean_nodes = mean(fwhm), median_nodes = median(fwhm),
    sd_nodes = sd(fwhm), n_nodes = n(), min_nodes = min(fwhm), max_nodes = max(fwhm)
  )

#### Stats ####
sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/node_characterization.org")
pretty_print_results("Node measurements", control_stats)
sink()
