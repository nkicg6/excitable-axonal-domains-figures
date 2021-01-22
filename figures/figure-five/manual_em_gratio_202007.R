# Manual EM quantification naris occlusion
library(dplyr)
library(ggplot2)
library(cowplot)
library(Cairo)

SAVEALL <- FALSE

#### preamble ####

source("../plotting_defaults/ggplot_theme_defaults.R")
img_save_rt <- "../oligos_and_myelin_occlusion_figure/em_occlusion_figures"

#### Grouping and processing ####

gr <- readr::read_csv("../../../analysis_and_data/EM/data/summary_data/em_gratio_manual_2020-07/summary_gratio_em_manual_202007_with_perimeters.csv") %>%
  mutate(
    ctrlcmp = case_when(
      treatment == "Control" ~ "Control",
      treatment == "Naris Occlusion" ~ side,
      TRUE ~ "unknown"
    ),
    lm_comp = case_when(
      side == "Left" ~ "Left",
      side == "Right" ~ "Right",
      side == "Open" ~ "Left",
      side == "Occluded" ~ "Right"
    ),
    gratio_perim = ax_perimeter / my_perimeter
  )

gr$treatment <- forcats::fct_relevel(gr$treatment, c("Control", "Naris Occlusion"))
gr$ctrlcmp <- forcats::fct_relevel(gr$ctrlcmp, c("Control", "Open", "Occluded"))
gr$side <- forcats::fct_relevel(gr$side, c("Left", "Right", "Open", "Occluded"))
gr$lm_comp <- forcats::fct_relevel(gr$lm_comp, c("Left", "Right"))


#### Summarize ####

gr_treatment_summary <- gr %>%
  group_by(treatment) %>%
  summarize(
    mean_gr = mean(gratio_perim),
    median_gr = median(gratio_perim),
    sd_gr = sd(gratio_perim),
    mean_ax_diam = mean(ax_diam),
    median_ax_diam = median(ax_diam),
    sd_ax_diam = sd(ax_diam),
    n = n()
  )

gr_side_summary <- gr %>%
  group_by(treatment, side) %>%
  summarize(
    mean_gr = mean(gratio_perim),
    median_gr = median(gratio_perim),
    sd_gr = sd(gratio_perim),
    mean_ax_diam = mean(ax_diam),
    median_ax_diam = median(ax_diam),
    sd_ax_diam = sd(ax_diam),
    n = n()
  )

ttest_control_sides <- t.test(gratio_perim ~ side, data = filter(gr, treatment == "Control"))
ttest_occluded_sides <- t.test(gratio_perim ~ side, data = filter(gr, treatment != "Control"))
ttest_occl_vs_control <- t.test(gratio_perim ~ treatment, data = gr)

# linear models
simple_lm_diam_treatment <- lm(gratio_perim ~ ax_diam + treatment, data = gr)
summary(simple_lm_diam_treatment)

simple_lm_diam_treatment_side <- lm(gratio_perim ~ ax_diam + treatment + lm_comp, data = gr)
summary(simple_lm_diam_treatment_side)

lm_gr_treatment_and_side_interaction <- lm(gratio_perim ~ ax_diam + treatment + lm_comp + treatment:lm_comp, data = gr)
summary(lm_gr_treatment_and_side_interaction)

car::Anova(lm_gr_treatment_and_side_interaction, type = "II")

anova_lm_gr_treatment_side_ineraction <- anova(lm_gr_treatment_and_side_interaction)

gratio_side_plot <- function(df, treatment_str, colors) {
  df_filtered <- filter(df, treatment == treatment_str)
  max_filt <- max(df_filtered$gratio_perim)
  y_ttest <- max_filt + (0.1 * max_filt)
  mean_df <- group_by(df_filtered, side) %>%
    summarize(
      mean_group = mean(gratio_perim),
      sd_group = sd(gratio_perim),
      n = n()
    )
  ttest_res <- t.test(gratio_perim ~ side, data = df_filtered)
  print(ttest_res)
  main_plot <- ggplot(df_filtered, aes(x = ax_diam, y = gratio_perim, color = side)) +
    geom_point(
      shape = 21, size = pt_size,
      show.legend = F
    ) +
    geom_smooth(method = "lm") +
    theme_and_axis_legend +
    labs(x = "Axon diameter (\u03BCm)", y = "G-ratio") +
    facet_grid(~treatment) +
    theme(legend.position = c(0.15, 0.9)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(0, 4, 0.5)) +
    coord_cartesian(xlim = c(0, 4), ylim = c(0.3, 1)) +
    colors

  insert_plot <- ggplot(data = mean_df, aes(x = side, y = mean_group, color = side)) +
    geom_bar(stat = "identity", width = barplot_width, fill = "white", size = line_size) +
    geom_errorbar(aes(
      ymin = mean_group - sd_group,
      ymax = mean_group + sd_group
    ),
    size = line_size, width = 0.2
    ) +
    colors +
    labs(x = "", y = "Mean g-ratio") +
    annotate("text",
      label = paste("p = ", signif(ttest_res$p.value, 2)),
      x = 1.5, y = y_ttest, size = custom_annotation_size
    ) +
    scale_y_continuous(breaks = seq(0, 1, 0.5)) +
    coord_cartesian(ylim = c(0, 1.3)) +
    insert_theme
  ggdraw(main_plot) +
    draw_plot(insert_plot,
      x = 0.6, y = 0.15,
      width = 0.3, height = 0.5, scale = 1
    )
}


gratio_treatment_plot <- function(df) {
  max_filt <- max(df$gratio_perim)
  y_ttest <- max_filt + (0.1 * max_filt)
  mean_df <- group_by(df, treatment) %>%
    summarize(
      mean_group = mean(gratio_perim),
      sd_group = sd(gratio_perim),
      n = n()
    )
  ttest_res <- t.test(gratio_perim ~ treatment, data = df)
  print(ttest_res)
  main_plot <- ggplot(df, aes(x = ax_diam, y = gratio_perim, color = treatment)) +
    geom_point(
      shape = 21, size = pt_size,
      show.legend = F
    ) +
    geom_smooth(method = "lm") +
    theme_and_axis_legend +
    labs(x = "Axon diameter (\u03BCm)", y = "G-ratio") +
    theme(legend.position = c(0.2, 0.9)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(0, 4, 0.5)) +
    coord_cartesian(xlim = c(0, 4), ylim = c(0.3, 1)) +
    control_vs_occl_color

  insert_plot <- ggplot(data = mean_df, aes(x = treatment, y = mean_group, color = treatment)) +
    geom_bar(stat = "identity", width = barplot_width, fill = "white", size = line_size) +
    geom_errorbar(aes(
      ymin = mean_group - sd_group,
      ymax = mean_group + sd_group
    ),
    size = line_size, width = 0.2
    ) +
    control_vs_occl_color +
    labs(x = "", y = "Mean g-ratio") +
    annotate("text",
      label = paste("p = ", signif(ttest_res$p.value, 2)),
      x = 1.5, y = y_ttest, size = custom_annotation_size
    ) +
    scale_y_continuous(breaks = seq(0, 1, 0.5)) +
    coord_cartesian(ylim = c(0, 1.3)) +
    insert_theme
  ggdraw(main_plot) +
    draw_plot(insert_plot,
      x = 0.6, y = 0.15,
      width = 0.3, height = 0.5, scale = 1
    )
}

gratio_side_plot(gr, "Control", ctrl_color)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "gratio_control_lm.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}
gratio_side_plot(gr, "Naris Occlusion", occl_color)
if (SAVEALL) {
  ggsave(file.path(img_save_rt, "gratio_naris_occl_lm.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}
gratio_treatment_plot(gr)
if (SAVEALL) {
  ggsave(file.path(img_save_rt, "gratio_naris_occl_vs_control_lm.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

ggplot(gr, aes(x = ax_diam, fill = treatment)) +
  geom_histogram(aes(y = ..density..),
    binwidth = 0.2,
    position = "dodge",
    alpha = 0.8
  ) +
  theme_and_axis_legend +
  control_vs_occl_fill +
  labs(x = "Axon diameter (\u03BCm)", y = "Density")

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "axon_diameter_histogram.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/myelin_occlusion_results.org")
pretty_print_results("G-ratio and axon diameter treatment", gr_treatment_summary)
pretty_print_results("G-ratio and axon diameter by side", gr_side_summary)
pretty_print_results("t-test g-ratio control left vs right", ttest_control_sides)
pretty_print_results("t-test g-ratio occluded open vs occluded", ttest_occluded_sides)
pretty_print_results("t-test g-ratio occluded vs control", ttest_occl_vs_control)
sink()
