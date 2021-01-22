# TH analysis
library(dplyr)
library(tidyr)
library(ggplot2)
library(Cairo)

SAVEALL <- TRUE
source("../plotting_defaults/ggplot_theme_defaults.R")
img_save_rt <- "../methods_exp_design_figure/th_figures/"

#### Process data ####

data <- readr::read_csv("~/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/immunohistochemistry/data/summary_data/th_intensity_summary_202009/th_summary_202009.csv") %>%
  mutate(total_int = mean_pixel * area)
data <- group_by(data, animal_id, channel, treatment, side, slice) %>%
  summarize(mean_int = sum(total_int) / sum(area)) # collapse multiple ROIs to one per slice

data$treatment <- forcats::fct_relevel(data$treatment, c("Control", "Naris Occlusion"))
data$side <- forcats::fct_relevel(data$side, c("Left", "Right", "Open", "Occluded"))

summary_data <- data %>%
  group_by(treatment, side, animal_id) %>%
  summarise(mean_intensity = mean(mean_int))

summary_barplot <- summary_data %>%
  group_by(treatment, side) %>%
  summarize(mean_group_intensity = mean(mean_intensity))

#### Create relative size summary ####

wide <- pivot_wider(summary_data,
  id_cols = c(treatment, animal_id),
  names_from = side,
  values_from = mean_intensity
)

ctrl_summary <- wide %>%
  filter(treatment == "Control") %>%
  mutate(relative = Right / Left) %>%
  select(treatment, animal_id, relative)

no_summary <- wide %>%
  filter(treatment == "Naris Occlusion") %>%
  mutate(relative = Occluded / Open) %>%
  select(treatment, animal_id, relative)

relative_summarized <- bind_rows(ctrl_summary, no_summary)
relative_barplot_summarized <- group_by(relative_summarized, treatment) %>%
  summarize(
    mean_relative = mean(relative),
    median_relative = median(relative),
    sd_relative = sd(relative)
  )

#### Tests ####
ttest_paired_control <- t.test(mean_intensity ~ side, paired = T, data = filter(summary_data, treatment == "Control"))
ttest_paired_occl <- t.test(mean_intensity ~ side, paired = T, data = filter(summary_data, treatment != "Control"))
ttest_relative <- t.test(relative ~ treatment, data = relative_summarized)

#### Plots ####

ggplot(relative_summarized, aes(x = treatment, y = relative, color = treatment)) +
  control_vs_occl_color +
  theme_and_axis_nolegend +
  geom_bar(
    data = relative_barplot_summarized, stat = "identity", fill = "white",
    size = line_size, width = barplot_width,
    aes(x = treatment, y = mean_relative, color = treatment)
  ) +
  geom_jitter(width = 0.1, size = pt_size) +
  geom_hline(
    yintercept = 1, alpha = pt_alpha,
    linetype = "longdash", size = line_size
  ) +
  annotate("text", x = 1.5, y = 1.3, label = paste("p = ", signif(ttest_relative$p.value, 3)), size = custom_annotation_size) +
  labs(x = "", y = "TH intensity occluded / open")

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "th_relative_figure.pdf"),
    width = 6, height = 6, dpi = 300, device = cairo_pdf
  )
}


ggplot(summary_data, aes(x = side, y = mean_intensity / 100, color = side)) +
  geom_bar(
    data = summary_barplot, inherit.aes = F,
    stat = "identity", size = line_size, width = barplot_width,
    fill = "white", aes(x = side, y = mean_group_intensity / 100, color = side)
  ) +
  geom_line(
    data = summary_data, alpha = pt_alpha, color = black, size = line_size,
    aes(x = side, y = mean_intensity / 100, group = animal_id)
  ) +
  geom_point(
    data = summary_data, size = pt_size,
    aes(x = side, y = mean_intensity / 100, color = side, group = animal_id)
  ) +
  color_color +
  theme_and_axis_nolegend +
  labs(x = "", y = "Fluoro. intensity (a.u.)") +
  annotate("text", x = 1.5, y = 260, label = paste("p = ", signif(ttest_paired_control$p.value, 3)), size = custom_annotation_size) +
  annotate("text", x = 3.5, y = 260, label = paste("p = ", signif(ttest_paired_occl$p.value, 3)), size = custom_annotation_size)


if (SAVEALL) {
  ggsave(file.path(img_save_rt, "th_sides_figure.pdf"),
    width = 6, height = 6, dpi = 300, device = cairo_pdf
  )
}

#### Statistics ####
sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/th_results.org")
pretty_print_results("By animal summary", summary_data)
pretty_print_results("Side group summary", summary_barplot)
pretty_print_results("relative summary", relative_barplot_summarized)
pretty_print_results("T-test paired control", ttest_paired_control)
pretty_print_results("T-test paired occluded", ttest_paired_occl)
pretty_print_results("T-test relative control vs occl", ttest_relative)
sink()
