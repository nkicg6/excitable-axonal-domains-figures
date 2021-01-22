# oligo progenitor and mature analysis

library(dplyr)
library(ggplot2)
library(tidyr)
library(Cairo)

#### preamble ####

SAVEALL <- FALSE

source("../plotting_defaults/ggplot_theme_defaults.R")

img_save_rt <- "../oligos_and_myelin_occlusion_figure/oligo_occlusion_figures"

data <- readr::read_csv("~/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/immunohistochemistry/data/summary_data/oligo_lineage/lineage_summary_202008.csv") %>%
  rename("Olig2+ PDGFR\u03b1+" = "prog", "Olig2+ CC1+" = "mature") %>%
  mutate(
    region = case_when(region == "gcl" ~ "GCL", region == "lot" ~ "LOT", TRUE ~ "unknown"),
    density_prog = `Olig2+ PDGFRα+` / (roi_area_um2 * (41 * 0.3)), # n steps * optical section size * XY area
    density_mature = `Olig2+ CC1+` / (roi_area_um2 * (41 * 0.3)), # n steps * optical section size * XY area
    total_cells = (`Olig2+ PDGFRα+` + `Olig2+ CC1+`)
  ) %>%
  filter(side != "unknown")

data$side <- forcats::fct_relevel(data$side, c("Left", "Right", "Open", "Occluded"))
data$treatment <- forcats::fct_relevel(data$treatment, c("Control", "Naris Occlusion"))


by_animal_cell_count <- data %>%
  group_by(treatment, side, region, animal_id) %>%
  summarize(
    mean_prog_density = mean(density_prog),
    mean_mature_density = mean(density_mature),
    mean_total_cells = mean(total_cells),
    mean_mature_n = mean(`Olig2+ CC1+`),
    mean_prog_n = mean(`Olig2+ PDGFRα+`),
    mean_roi_area = mean(roi_area_um2)
  )

by_animal_density <- pivot_longer(by_animal_cell_count, c("mean_prog_density", "mean_mature_density"),
  names_to = "stage", values_to = "cell_density"
) %>%
  select(-mean_total_cells, -mean_mature_n, -mean_prog_n, -mean_roi_area) %>%
  mutate(dummy_name = case_when(
    stage == "mean_prog_density" ~ "Olig2+ PDGFR\u03b1+",
    stage == "mean_mature_density" ~ "Olig2+ CC1+"
  ))

ttest_annot <- function(df, pair) {
  model <- t.test(cell_density ~ side, data = df, paired = pair)
  p_value <- paste("p = ", signif(model$p.value, 3), sep = "")
  t_stat <- sprintf("t(%.2f) = %.3f", model$parameter, model$statistic)
  max_buffered_value <- (max(df$cell_density) * 0.15 + max(df$cell_density))
  df_ret <- data.frame(
    tstatistic = t_stat, p_text = p_value,
    dataname = model$data.name, max_y = max_buffered_value,
    stringsAsFactors = F
  )
  return(df_ret)
}

make_stat_df <- function(df, treatment_str, region_str, paired_test) {
  df %>%
    filter(treatment == treatment_str, region == region_str) %>%
    group_by(dummy_name) %>%
    do(ttest_annot(., paired_test))
}

# awesome from: https://r-graphics.org/recipe-annotate-facet

make_cell_n_plot <- function(df, treatment_str, region_str, color_palette, paired_test) {
  filtered_df <- filter(df, treatment == treatment_str, region == region_str)
  barplot_df <- filtered_df %>%
    group_by(side, dummy_name) %>%
    summarize(mean_den = mean(cell_density))
  labs_df <- filtered_df %>%
    group_by(dummy_name) %>%
    do(ttest_annot(., pair = paired_test))
  ggplot(filtered_df, aes(x = side, y = cell_density, color = side)) +
    geom_bar(
      data = barplot_df,
      aes(x = side, y = mean_den, color = side),
      stat = "identity", width = barplot_width,
      fill = "white", size = line_size
    ) +
    geom_line(aes(group = animal_id), color = black) +
    geom_point(size = pt_size, alpha = pt_alpha) +
    geom_text(aes(label = p_text),
      x = 1.5,
      y = max(labs_df$max_y),
      data = labs_df, inherit.aes = F,
      size = custom_annotation_size
    ) +
    coord_cartesian(ylim = c(0, max(labs_df$max_y))) +
    facet_grid(~dummy_name) +
    labs(x = "", y = "Cells/\u03BCm\u00B3") +
    theme_and_axis_nolegend +
    color_palette
}

ttest_two_group_annot <- function(df) {
  model <- t.test(mean_cell_density ~ treatment, data = df, paired = F)
  p_value <- paste("p = ", signif(model$p.value, 3), sep = "")
  t_stat <- sprintf("t(%.2f) = %.3f", model$parameter, model$statistic)
  max_buffered_value <- (max(df$mean_cell_density) * 0.15 + max(df$mean_cell_density))
  df_ret <- data.frame(
    tstatistic = t_stat, p_text = p_value,
    dataname = model$data.name, max_y = max_buffered_value,
    stringsAsFactors = F
  )
  return(df_ret)
}

make_two_group_stat_df <- function(df, region_str) {
  df %>%
    filter(region == region_str) %>%
    group_by(dummy_name) %>%
    do(ttest_two_group_annot(.))
}

make_ctrl_occl_plot <- function(df, region_str) {
  filtered_df <- filter(df, region == region_str)
  barplot_df <- filtered_df %>%
    group_by(treatment, stage, dummy_name) %>%
    summarize(mean_treat_density = mean(mean_cell_density))
  labs_df <- filtered_df %>%
    group_by(dummy_name) %>%
    do(ttest_two_group_annot(.))
  ggplot(filtered_df, aes(x = treatment, y = mean_cell_density, color = treatment)) +
    geom_bar(
      data = barplot_df, aes(x = treatment, y = mean_treat_density, color = treatment),
      stat = "identity", width = barplot_width, fill = "white", size = line_size
    ) +
    geom_jitter(size = pt_size, width = 0.1, alpha = pt_alpha) +
    geom_text(aes(label = p_text),
      x = 1.5, y = max(labs_df$max_y),
      data = labs_df, inherit.aes = F,
      size = custom_annotation_size
    ) +
    coord_cartesian(ylim = c(0, max(labs_df$max_y))) +
    control_vs_occl_color +
    labs(x = "", y = "Cells/\u03BCm\u00B3") +
    theme_and_axis_nolegend +
    facet_grid(~dummy_name)
}


summary_table <- by_animal_density %>%
  group_by(treatment, side, region, dummy_name) %>%
  summarize(
    mean_density = mean(cell_density),
    sd_density = sd(cell_density), n = n()
  ) %>%
  drop_na()

gcl_summary <- summary_table %>% filter(region == "GCL")
lot_summary <- summary_table %>% filter(region == "LOT")


make_cell_n_plot(by_animal_density, "Control", "LOT", ctrl_color, T)
if (SAVEALL) {
  ggsave(file.path(img_save_rt, "control_lot_counts.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}
control_left_right_ttest_lot <- make_stat_df(by_animal_density, "Control", "LOT", T)
make_cell_n_plot(by_animal_density, "Control", "GCL", ctrl_color, T)
if (SAVEALL) {
  ggsave(file.path(img_save_rt, "control_gcl_counts.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

control_left_right_ttest_gcl <- make_stat_df(by_animal_density, "Control", "GCL", T)

make_cell_n_plot(by_animal_density, "Naris Occlusion", "LOT", occl_color, T)
if (SAVEALL) {
  ggsave(file.path(img_save_rt, "naris_occlusion_lot_counts.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}
naris_occlusion_open_occl_ttest_lot <- make_stat_df(by_animal_density, "Naris Occlusion", "LOT", T)
make_cell_n_plot(by_animal_density, "Naris Occlusion", "GCL", occl_color, T)
if (SAVEALL) {
  ggsave(file.path(img_save_rt, "naris_occlusion_gcl_counts.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}
naris_occlusion_open_occl_ttest_gcl <- make_stat_df(by_animal_density, "Naris Occlusion", "GCL", T)

animal_treatment_density <- by_animal_density %>%
  group_by(treatment, animal_id, region, stage, dummy_name) %>%
  summarize(
    mean_cell_density = mean(cell_density),
    sd_cell_density = sd(cell_density)
  )

treatment_density <- animal_treatment_density %>%
  group_by(treatment, region, stage, dummy_name) %>%
  summarize(
    mean_treat_density = mean(mean_cell_density),
    sd_treat_density = sd(mean_cell_density)
  )


make_ctrl_occl_plot(animal_treatment_density, "LOT")
if (SAVEALL) {
  ggsave(file.path(img_save_rt, "naris_occlusion_control_lot_counts.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

ctrl_vs_occl_lot <- make_two_group_stat_df(animal_treatment_density, "LOT")

make_ctrl_occl_plot(animal_treatment_density, "GCL")
if (SAVEALL) {
  ggsave(file.path(img_save_rt, "naris_occlusion_control_gcl_counts.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}
ctrl_vs_occl_gcl <- make_two_group_stat_df(animal_treatment_density, "GCL")

#### Statistics ####

sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/oligo_lineage_occlusion_results.org")
pretty_print_results("Control left right ttest LOT", control_left_right_ttest_lot)
pretty_print_results("Control left right ttest GCL", control_left_right_ttest_gcl)
pretty_print_results("Naris Occlusion open occl ttest LOT", naris_occlusion_open_occl_ttest_lot)
pretty_print_results("Naris Occlusion open occl ttest GCL", naris_occlusion_open_occl_ttest_gcl)
pretty_print_results("GCL summary", gcl_summary)
pretty_print_results("LOT summary", lot_summary)
pretty_print_results("Control vs occluded summary", treatment_density)
pretty_print_results("Naris Occlusion vs Control ttest LOT", ctrl_vs_occl_lot)
pretty_print_results("Naris Occlusion vs Control ttest GCL", ctrl_vs_occl_gcl)
"---- END STATISTICS ----"
sink()
