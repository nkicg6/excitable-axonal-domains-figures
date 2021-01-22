# node plots for figure
library(dplyr)
library(ggplot2)
library(cowplot)
library(Cairo)

#### setup ####

SAVEALL <- FALSE 

source("../plotting_defaults/ggplot_theme_defaults.R")

img_save_rt <- "../naris_occlusion_node_ais_figure/node_occlusion_figures/"

#### plot with insert fn ####
cdf_with_node_insert_plot <- function(data, grouped_data, grouped_ecdfs, group_str,
                                      color_palette, ks_cdf, legend_pos_vec) {
  data_filtered <- filter(data, type == group_str)
  grouped_ecdfs_filtered <- filter(grouped_ecdfs, type == group_str)
  grouped_filtered <- filter(grouped_data, type == group_str)
  grouped_filtered_means <- grouped_filtered %>%
    group_by(side) %>%
    summarize(ml = mean(mean_node_len))

  ttestres <- t.test(mean_node_len ~ side, data = grouped_filtered, paired = T)

  base_cdf <-
    ggplot(data_filtered, aes(x = fwhm, color = side)) +
    stat_ecdf(geom = "step", pad = ecdf_pad, size = line_size) +
    geom_point(
      data = grouped_ecdfs_filtered, inherit.aes = F,
      aes(x = ml, y = ecdf_prob, color = side),
      size = ecdf_pt_size, alpha = pt_alpha,
      show.legend = FALSE
    ) +
    color_palette +
    annotate("text",
      label = paste("p = ", signif(ks_cdf$p.value, 3)),
      x = 1.3, y = 1, size = custom_annotation_size
    ) +
    labs(x = "Node of Ranvier length (\u03BCm)", y = "Probability") +
    theme_and_axis_legend +
    theme(legend.position = legend_pos_vec) +
    scale_x_continuous(breaks = seq(0, 3, 0.5)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) + # !!!
    coord_cartesian(xlim = c(0, 2.6)) +
    facet_grid(~type)

  insert_paired <- ggplot(grouped_filtered, aes(
    x = side,
    y = mean_node_len,
    color = side
  )) +
    color_palette +
    geom_bar(
      data = grouped_filtered_means,
      aes(x = side, y = ml, color = side),
      stat = "identity", width = barplot_width, fill = "white", size = line_size
    ) +
    geom_point(alpha = pt_alpha, size = pt_size) +
    geom_line(aes(group = animal_id), color = "black") +
    annotate("text",
      label = paste("p = ", signif(ttestres$p.value, 3)),
      x = 1.6, y = 1.4, size = custom_annotation_size
    ) +
    insert_theme +
    labs(x = "", y = "Length (\u03BCm)") +
    coord_cartesian(expand = F, ylim = c(0, 1.5), xlim = c(0.1, 3)) + # !!!
    scale_y_continuous(breaks = seq(0, 2, 0.5)) # !!!

  ggdraw(base_cdf) +
    draw_plot(insert_paired,
      x = 0.65, y = 0.15, width = 0.3,
      height = 0.5, scale = 1
    )
}
#### end fn ####

#### Data processing ####
raw_node_data_path <- "../../../analysis_and_data/immunohistochemistry/data/summary_data/node_length_manual_2018to2019/manual_node_length_summarized_202004.csv"
rawd_nodes <- readr::read_csv(raw_node_data_path)

rawd_nodes <- mutate(rawd_nodes, ctrlcmp = case_when(
  type == "Control" ~ "Control",
  type == "Naris Occlusion" ~ side,
  TRUE ~ "Unknown"
))

rawd_nodes$ctrlcmp <- forcats::fct_relevel(rawd_nodes$ctrlcmp, c("Control", "Open", "Occluded"))

rawd_nodes$side <- forcats::fct_relevel(rawd_nodes$side, c("Left", "Right", "Open", "Occluded"))

rawd_nodes_grouped <- rawd_nodes %>%
  group_by(type, animal_id, side, section) %>%
  summarize(
    sliceLenMean = mean(fwhm, na.rm = T), n = n(),
    count_nodes = length(fwhm)
  ) %>%
  group_by(type, animal_id, side) %>%
  summarize(
    mean_node_len = mean(sliceLenMean),
    sd_node_len = sd(sliceLenMean), n = n()
  )

rawd_nodes_occl_control_by_animal <- rawd_nodes_grouped %>%
  group_by(type, animal_id) %>%
  summarize(animal_node_len = mean(mean_node_len))

ttest_naris_occl_control_barplot <- t.test(animal_node_len ~ type, rawd_nodes_occl_control_by_animal)

fligner_naris_occl_control_grouped <- fligner.test(animal_node_len ~ type, rawd_nodes_occl_control_by_animal)

occl_vs_control_variance_by_animal <- fligner.test(animal_node_len ~ type, rawd_nodes_occl_control_by_animal)

#### KS test statistics ####

occl_side <- filter(rawd_nodes, type == "Naris Occlusion", side == "Occluded")$fwhm
open_side <- filter(rawd_nodes, type == "Naris Occlusion", side == "Open")$fwhm

occl_vs_open_variance_rawd <- fligner.test(fwhm ~ side, filter(rawd_nodes, type == "Naris Occlusion"))
left_vs_right_variance_rawd <- fligner.test(fwhm ~ side, filter(rawd_nodes, type == "Control"))
control_vs_occluded_variance_rawd <- fligner.test(fwhm ~ type, rawd_nodes)

occl_open_ks <- ks.test(occl_side, open_side)

left_side <- filter(rawd_nodes, type == "Control", side == "Left")$fwhm
right_side <- filter(rawd_nodes, type == "Control", side == "Right")$fwhm

left_right_ks <- ks.test(left_side, right_side)

occl <- filter(rawd_nodes, type == "Naris Occlusion")$fwhm
control <- filter(rawd_nodes, type == "Control")$fwhm

naris_occl_vs_control_ks <- ks.test(occl, control)

# compute naris occl and open ECDF
occl_ecdf <- ecdf(occl_side)
open_ecdf <- ecdf(open_side)
left_ecdf <- ecdf(left_side)
right_ecdf <- ecdf(right_side)

control_ecdf <- ecdf(control)
naris_occl_ecdf <- ecdf(occl)


grouped_ecdfs <- rawd_nodes_grouped %>%
  group_by(type, animal_id, side) %>%
  summarize(ml = mean(mean_node_len)) %>%
  mutate(ecdf_prob = case_when(
    side == "Left" ~ left_ecdf(ml),
    side == "Right" ~ right_ecdf(ml),
    side == "Open" ~ open_ecdf(ml),
    side == "Occluded" ~ occl_ecdf(ml)
  ))

grouped_ecdfs_occl_ctrl_by_animal <- rawd_nodes_grouped %>%
  group_by(type, animal_id) %>%
  summarize(ml = mean(mean_node_len)) %>%
  mutate(ecdf_prob = case_when(
    type == "Control" ~ control_ecdf(ml),
    type == "Naris Occlusion" ~ naris_occl_ecdf(ml)
  ))

just_grp_mean_nodes <- rawd_nodes_grouped %>%
  group_by(type, side) %>%
  summarize(ml_nodes = mean(mean_node_len))

control_naris_occl_barplot <- just_grp_mean_nodes %>%
  group_by(type) %>%
  summarize(ml = mean(ml_nodes))

#### KS test naris occlusion CDF plot ####

rawd_nodes %>%
  filter(type == "Naris Occlusion") %>%
  ggplot(aes(x = fwhm, color = side)) +
  stat_ecdf(geom = "step", pad = ecdf_pad, size = line_size) +
  occl_color +
  annotate("text",
    label = paste("p = ", signif(occl_open_ks$p.value, 3)),
    x = 1.4, y = 1, size = custom_annotation_size
  ) +
  labs(x = "Node of Ranvier length (\u03BCm)", y = "Probability") +
  theme_and_axis_legend +
  theme(legend.position = c(0.5, 0.5)) +
  facet_grid(~type)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "nodes-naris-occl-cdf.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### with barplot insert ####

cdf_with_node_insert_plot(
  rawd_nodes, rawd_nodes_grouped, grouped_ecdfs,
  "Naris Occlusion", occl_color, occl_open_ks, c(0.15, 0.5)
)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "nodes-naris-occl-cdf-with-insert.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### KS test control CDF plot ####

rawd_nodes %>%
  filter(type == "Control") %>%
  ggplot(aes(x = fwhm, color = side)) +
  stat_ecdf(geom = "step", pad = ecdf_pad, size = line_size) +
  ctrl_color +
  annotate("text",
    label = paste("p = ", signif(left_right_ks$p.value, 3)),
    x = 1.4, y = 1, size = custom_annotation_size
  ) +
  labs(x = "Node of Ranvier length (\u03BCm)", y = "Probability") +
  theme_and_axis_legend +
  theme(legend.position = c(0.5, 0.5)) +
  facet_grid(~type)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "nodes-control-cdf.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### with barplot insert ####

cdf_with_node_insert_plot(
  rawd_nodes, rawd_nodes_grouped, grouped_ecdfs,
  "Control", ctrl_color, left_right_ks, c(0.15, 0.5)
)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "nodes-control-cdf-with-insert.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### KS test control vs naris occlusion CDF plot ####

# rawd_nodes %>%
ggplot(rawd_nodes, aes(x = fwhm, color = type)) +
  stat_ecdf(geom = "step", pad = ecdf_pad, size = line_size) +
  control_vs_occl_color +
  annotate("text",
    label = paste("p = ", signif(naris_occl_vs_control_ks$p.value, 3)),
    x = 1.4, y = 1, size = custom_annotation_size
  ) +
  labs(x = "Node of Ranvier length (\u03BCm)", y = "Probability") +
  theme_and_axis_legend +
  theme(legend.position = c(0.55, 0.5))

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "nodes-control-vs-naris-occlusion-cdf.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### with barplot insert ####

base_both <- rawd_nodes %>%
  ggplot(aes(x = fwhm, color = type)) +
  stat_ecdf(geom = "step", pad = ecdf_pad, size = line_size) +
  geom_point(
    data = grouped_ecdfs_occl_ctrl_by_animal, inherit.aes = F,
    aes(x = ml, y = ecdf_prob, color = type), size = ecdf_pt_size, alpha = pt_alpha,
    show.legend = FALSE
  ) +
  control_vs_occl_color +
  annotate("text",
    label = paste("p = ", signif(naris_occl_vs_control_ks$p.value, 3)),
    x = 1.4, y = 1.01, size = custom_annotation_size
  ) +
  labs(x = "Node of Ranvier length (\u03BCm)", y = "Probability") +
  theme_and_axis_legend +
  theme(legend.position = c(0.7, 0.76)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(1, 2.5, 0.5), expand = c(0, 0.3))

insert_both <- ggplot(
  rawd_nodes_occl_control_by_animal,
  aes(x = type, y = animal_node_len, color = type)
) +
  control_vs_occl_color +
  geom_bar(
    data = control_naris_occl_barplot,
    aes(x = type, y = ml, color = type), stat = "identity",
    width = barplot_width, fill = "white", size = line_size
  ) +
  geom_point(alpha = pt_alpha, size = pt_size) +
  annotate("text",
    label = paste("p = ", signif(ttest_naris_occl_control_barplot$p.value, 3)),
    x = 1.6, y = 1.4, size = custom_annotation_size
  ) +
  insert_theme +
  labs(x = "", y = "Length (\u03BCm)") +
  coord_cartesian(expand = F, ylim = c(0, 1.5), xlim = c(0.1, 3))

ggdraw(base_both) +
  draw_plot(insert_both, x = 0.55, y = 0.15, width = 0.3, height = 0.5, scale = 1)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "nodes-control-vs-naris-occlusion-cdf-with-insert.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### Writing summaries ####

rawd_nodes_by_type <- rawd_nodes %>%
  group_by(type) %>%
  summarize(
    n = n(),
    mean_node = mean(fwhm),
    median_node = median(fwhm),
    sd_node = sd(fwhm)
  )

rawd_nodes_by_type_and_side <- rawd_nodes %>%
  group_by(type, side) %>%
  summarize(
    n = n(),
    mean_node = mean(fwhm),
    median_node = median(fwhm),
    sd_node = sd(fwhm)
  )
summarized_rawd_nodes_sides_and_type_grouped <- rawd_nodes_grouped %>%
  group_by(type, side) %>%
  summarize(
    mean_node_len_grp = mean(mean_node_len),
    median_node_len_grp = median(mean_node_len),
    sd_node_len_grp = sd(mean_node_len)
  )

summarized_rawd_nodes_type_grouped <- rawd_nodes_grouped %>%
  group_by(type) %>%
  summarize(
    mean_node_len_grp = mean(mean_node_len),
    median_node_len_grp = median(mean_node_len),
    sd_node_len_grp = sd(mean_node_len)
  )

ttest_paired_left_vs_right_control <- t.test(mean_node_len ~ side, data = filter(rawd_nodes_grouped, type == "Control"), paired = T)
ttest_paired_open_vs_occl_naris_occlusion <- t.test(mean_node_len ~ side, data = filter(rawd_nodes_grouped, type == "Naris Occlusion"), paired = T)
fligner_results_left_vs_right_control_grouped <- fligner.test(mean_node_len ~ side, data = filter(rawd_nodes_grouped, type == "Control"))
fligner_results_open_vs_occl_naris_occlusion <- fligner.test(mean_node_len ~ side, data = filter(rawd_nodes_grouped, type == "Naris Occlusion"))

sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/node_occlusion_results.org")
pretty_print_results("All nodes by type", rawd_nodes_by_type)
pretty_print_results("All nodes by type and side", rawd_nodes_by_type_and_side)
pretty_print_results("KS test open vs occluded", occl_open_ks)
pretty_print_results("Occluded vs open variance", occl_vs_open_variance_rawd)
pretty_print_results("KS test left vs right", left_right_ks)
pretty_print_results("Left vs right variance", left_vs_right_variance_rawd)
pretty_print_results("KS test control vs Naris occlusion", naris_occl_vs_control_ks)
pretty_print_results("Control vs Naris Occlusion variance", control_vs_occluded_variance_rawd)
"---- t-test and grouped data ----"
pretty_print_results("Summarized grouped data by side and type", summarized_rawd_nodes_sides_and_type_grouped)
pretty_print_results("Summarized grouped data by type", summarized_rawd_nodes_type_grouped)

pretty_print_results("T-test paried left vs right", ttest_paired_left_vs_right_control)
pretty_print_results("Homogenous variance left vs right", fligner_results_left_vs_right_control_grouped)
pretty_print_results("T-test paired open vs occl", ttest_paired_open_vs_occl_naris_occlusion)
pretty_print_results("Homogenous variance open vs occl", fligner_results_open_vs_occl_naris_occlusion)
pretty_print_results("T-test unpaired control vs naris occlusion", ttest_naris_occl_control_barplot)
pretty_print_results("Homogenous variance control vs naris occlusion", fligner_naris_occl_control_grouped)

"---- End of statistics ----"
sink()
