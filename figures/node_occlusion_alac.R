# node plots for figure
library(dplyr)
library(ggplot2)
library(cowplot)
library(Cairo)

#### setup ####

SAVEALL <- FALSE

source("../plotting_defaults/ggplot_theme_defaults.R")

img_save_rt <- "../naris_occlusion_node_ais_figure/alac_occlusion_figures/"
data_path <- "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/immunohistochemistry/data/summary_data/node_length_manual_alac/alac-node-data.csv"

alac_raw <- readr::read_csv(data_path) %>%
  mutate(., treatment = case_when(
    treatment == "Control" ~ "Control",
    treatment == "Occluded" ~ "Naris Occlusion",
    TRUE ~ "Unknown"
  ))

alac_raw$treatment <- forcats::fct_relevel(alac_raw$treatment, c("Control", "Naris Occlusion"))

alac_nodes_grouped <- alac_raw %>%
  group_by(treatment, animal_id, slice) %>%
  summarize(sliceMean = mean(node_length, na.rm = T), n = n()) %>%
  group_by(treatment, animal_id) %>%
  summarize(animalMean = mean(sliceMean, na.rm = T), n = n())

alac_nodes_grouped_one_mean <- alac_nodes_grouped %>%
  group_by(treatment) %>%
  summarize(ml = mean(animalMean))

ttest_animal_grouped <- t.test(animalMean ~ treatment, alac_nodes_grouped)

occl_nodes <- filter(alac_raw, treatment == "Naris Occlusion")$node_length
occl_ecdf <- ecdf(occl_nodes)
ctrl_nodes <- filter(alac_raw, treatment == "Control")$node_length
ctrl_ecdf <- ecdf(ctrl_nodes)

ctrl_occl_ks_test <- ks.test(occl_nodes, ctrl_nodes)

alac_nodes_grouped_ecdf <- alac_nodes_grouped %>%
  mutate(ecdf_val = case_when(
    treatment == "Control" ~ ctrl_ecdf(animalMean),
    treatment == "Naris Occlusion" ~ occl_ecdf(animalMean)
  ))



base_plot <- ggplot(alac_raw, aes(x = node_length, color = treatment)) +
  stat_ecdf(geom = "step", pad = ecdf_pad, size = line_size) +
  geom_point(
    data = alac_nodes_grouped_ecdf, inherit.aes = F,
    aes(x = animalMean, y = ecdf_val, color = treatment),
    size = ecdf_pt_size, alpha = pt_alpha, show.legend = FALSE
  ) +
  control_vs_occl_color +
  annotate("text",
    label = paste("p = ", signif(ctrl_occl_ks_test$p.value, 2)),
    x = 1.3, y = 1.05, size = custom_annotation_size
  ) +
  labs(x = "Node of Ranvier length (\u03BCm)", y = "Probability") +
  theme_and_axis_legend +
  theme(legend.position = c(0.65, 0.75)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(0, 2.5, 0.5), expand = c(0, 0.3))

insert_bar <- ggplot(
  alac_nodes_grouped,
  aes(x = treatment, y = animalMean, color = treatment)
) +
  control_vs_occl_color +
  geom_bar(
    data = alac_nodes_grouped_one_mean,
    aes(x = treatment, y = ml, color = treatment), stat = "identity",
    width = barplot_width, fill = "white", size = line_size
  ) +
  geom_point(alpha = pt_alpha, size = pt_size) +
  annotate("text",
    label = paste("p = ", signif(ttest_animal_grouped$p.value, 2)),
    x = 1.6, y = 1.4, size = custom_annotation_size
  ) +
  insert_theme +
  labs(x = "", y = "Length (\u03BCm)") +
  coord_cartesian(expand = F, ylim = c(0, 1.5), xlim = c(0.1, 3))

ggdraw(base_plot) +
  draw_plot(insert_bar, x = 0.55, y = 0.15, width = 0.3, height = 0.5, scale = 1)



if (SAVEALL) {
  ggsave(file.path(img_save_rt, "nodes-alac-naris-occlusion-cdf-with-barplot.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}
