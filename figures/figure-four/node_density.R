# Laetitia's node density data
library(dplyr)
library(ggplot2)
library(Cairo)

#### setup ####

SAVEALL <- TRUE

source("../plotting_defaults/ggplot_theme_defaults.R")

img_save_rt <- "../naris_occlusion_node_ais_figure/node_density"

data_path <- "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/immunohistochemistry/node_density/laetitia-node-data_202110.csv"

noded <- readr::read_csv(data_path)

noded$side <- forcats::fct_relevel(noded$side, c("Left", "Right", "Open", "Occluded"))
noded$treatment <- forcats::fct_relevel(noded$treatment, c("Control", "Naris Occlusion"))

ttest_treatments <- t.test(density_mm2 ~ treatment, data = noded)

ttest_sides_control <- t.test(density_mm2 ~ side, data = filter(noded, treatment == "Control"))

ttest_sides_occluded <- t.test(density_mm2 ~ side, data = filter(noded, treatment != "Control"))

by_side_animal <- noded %>%
  group_by(animal_id, treatment, side) %>%
  summarize(mean_density = mean(density_mm2))

by_side <- noded %>%
  group_by(treatment, side) %>%
  summarize(mean_density = mean(density_mm2))

by_treatment <- noded %>%
  group_by(treatment) %>%
  summarize(mean_density = mean(density_mm2))

by_treatment_and_animal <- noded %>%
  group_by(animal_id, treatment) %>%
  summarize(mean_density = mean(density_mm2))

ggplot(filter(by_side_animal, treatment == "Control"), aes(x = side, y = mean_density)) +
  geom_bar(
    data = filter(by_side, treatment == "Control"),
    aes(x = side, y = mean_density, color = side), stat = "identity",
    width = barplot_width, fill = "white", size = line_size
  ) +
  geom_jitter(alpha = pt_alpha, size = pt_size, width = 0.2) +
  theme_and_axis_nolegend +
  ctrl_color +
  labs(x = "", y = "Node Density (mm\u00B2)") +
  facet_grid(~treatment) +
  annotate("text",
    label = paste("p = ", signif(ttest_sides_control$p.value, 3)),
    x = 1.6, y = 180000, size = custom_annotation_size
  )

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "node-density-control.pdf"),
    width = 4, height = 5, dpi = 300, device = cairo_pdf
  )
}


ggplot(filter(by_side_animal, treatment == "Naris Occlusion"), aes(x = side, y = mean_density)) +
  geom_bar(
    data = filter(by_side, treatment == "Naris Occlusion"),
    aes(x = side, y = mean_density, color = side), stat = "identity",
    width = barplot_width, fill = "white", size = line_size
  ) +
  geom_jitter(alpha = pt_alpha, size = pt_size, width = 0.2) +
  theme_and_axis_nolegend +
  occl_color +
  labs(x = "", y = "Node Density (mm\u00B2)") +
  facet_grid(~treatment) +
  annotate("text",
    label = paste("p = ", signif(ttest_sides_occluded$p.value, 3)),
    x = 1.6, y = 180000, size = custom_annotation_size
  )

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "node-density-occlusion.pdf"),
    width = 4, height = 5, dpi = 300, device = cairo_pdf
  )
}

ggplot(by_treatment_and_animal, aes(x = treatment, y = mean_density)) +
  geom_bar(
    data = by_treatment,
    aes(x = treatment, y = mean_density, color = treatment), stat = "identity",
    width = barplot_width, fill = "white", size = line_size
  ) +
  geom_jitter(alpha = pt_alpha, size = pt_size, width = 0.2) +
  theme_and_axis_nolegend +
  control_vs_occl_color +
  labs(x = "", y = "Node Density (mm\u00B2)") +
  annotate("text",
    label = paste("p = ", signif(ttest_treatments$p.value, 3)),
    x = 1.6, y = 180000, size = custom_annotation_size
  )

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "node-density-treatment.pdf"),
    width = 5, height = 5, dpi = 300, device = cairo_pdf
  )
}
