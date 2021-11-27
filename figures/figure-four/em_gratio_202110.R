library(dplyr)
library(ggplot2)
library(cowplot)
library(Cairo)

SAVEALL <- TRUE

source("../plotting_defaults/ggplot_theme_defaults.R")
img_save_rt <- "../oligos_and_myelin_occlusion_figure/em_occlusion_figures"

#### grouping and processing ####
all_data_path <- "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/EM/data/em-all-automated/all_data.json"
gr <- jsonlite::fromJSON(all_data_path) %>%
  mutate(
    treatment = case_when(
      treatment == "sham" ~ "Control",
      treatment == "occl" ~ "Naris Occlusion",
      TRUE ~ "unknown"
    ),
    ctrlcmp = case_when(
      treatment == "Control" ~ "Control",
      treatment == "Naris Occlusion" ~ side,
      TRUE ~ "unknown"
    ),
    ctrlcmp = case_when(
      ctrlcmp == "Left" ~ "Open",
      ctrlcmp == "Right" ~ "Occluded",
      ctrlcmp == "Control" ~ "Control",
      TRUE ~ "unknown"
    ),
    lm_comp = case_when(
      treatment == "Control" ~ side,
      side == "Left" ~ "Open",
      side == "Right" ~ "Occluded"
    ),
    side = case_when(
      treatment == "Naris Occlusion" & side == "Left" ~ "Open",
      treatment == "Naris Occlusion" & side == "Right" ~ "Occluded",
      treatment == "Control" ~ side
    ),
  ) %>%
  filter(gratio < 1) %>%
  filter(axon_diam > 0.4)

gr$treatment <- forcats::fct_relevel(gr$treatment, c("Control", "Naris Occlusion"))
gr$ctrlcmp <- forcats::fct_relevel(gr$ctrlcmp, c("Control", "Open", "Occluded"))
gr$side <- forcats::fct_relevel(gr$side, c("Left", "Right", "Open", "Occluded"))
gr$lm_comp <- forcats::fct_relevel(gr$lm_comp, c("Left", "Right"))


#### Summarize ####

gr_treatment_summary <- gr %>%
  group_by(treatment) %>%
  summarize(
    mean_gr = mean(gratio),
    median_gr = median(gratio),
    sd_gr = sd(gratio),
    mean_ax_diam = mean(axon_diam),
    median_ax_diam = median(axon_diam),
    sd_ax_diam = sd(axon_diam),
    n = n()
  )

gr_side_summary <- gr %>%
  group_by(treatment, side) %>%
  summarize(
    mean_gr = mean(gratio),
    median_gr = median(gratio),
    sd_gr = sd(gratio),
    mean_ax_diam = mean(axon_diam),
    median_ax_diam = median(axon_diam),
    sd_ax_diam = sd(axon_diam),
    n = n()
  )

ttest_control_sides <- t.test(gratio ~ side, data = filter(gr, treatment == "Control"))
ttest_occluded_sides <- t.test(gratio ~ side, data = filter(gr, treatment != "Control"))
ttest_occl_vs_control <- t.test(gratio ~ treatment, data = gr)

#### linear models ####
simple_lm_diam_treatment <- lm(gratio ~ axon_diam + treatment, data = gr)
summary(simple_lm_diam_treatment)

simple_lm_diam_treatment_side <- lm(gratio ~ axon_diam + treatment + lm_comp, data = gr)
summary(simple_lm_diam_treatment_side)

lm_gr_treatment_and_side_interaction <- lm(gratio ~ axon_diam + treatment + lm_comp + treatment:lm_comp, data = gr)
summary(lm_gr_treatment_and_side_interaction)

car::Anova(lm_gr_treatment_and_side_interaction, type = "II")

anova_lm_gr_treatment_side_ineraction <- anova(lm_gr_treatment_and_side_interaction)

#### Plot within treatment ####

gratio_side_plot <- function(df, treatment_str, colors) {
  df_filtered <- filter(df, treatment == treatment_str)
  max_filt <- max(df_filtered$gratio)
  y_ttest <- max_filt + (0.1 * max_filt)
  mean_df <- group_by(df_filtered, side) %>%
    summarize(
      mean_group = mean(gratio),
      sd_group = sd(gratio),
      n = n()
    )
  ttest_res <- t.test(gratio ~ side, data = df_filtered)
  print(ttest_res)
  main_plot <- ggplot(df_filtered, aes(x = axon_diam, y = gratio, color = side)) +
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
    scale_x_continuous(breaks = seq(0, 3, 0.5)) +
    coord_cartesian(xlim = c(0, 3), ylim = c(0.3, 1)) +
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
    scale_y_continuous(breaks = seq(0.5, 1, 0.2)) +
    coord_cartesian(ylim = c(0.5, 1)) +
    insert_theme
  ggdraw(main_plot) +
    draw_plot(insert_plot,
      x = 0.7, y = 0.15,
      width = 0.3, height = 0.5, scale = 1
    )
}
gratio_side_plot(gr, "Control", ctrl_color)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "gratio_control_lm_revision.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

gratio_side_plot(gr, "Naris Occlusion", occl_color)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "gratio_naris_occl_lm_revision.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### Plotting function between treatment ####

gratio_treatment_plot <- function(df) {
  max_filt <- max(df$gratio)
  y_ttest <- max_filt + (0.1 * max_filt)
  mean_df <- group_by(df, treatment) %>%
    summarize(
      mean_group = mean(gratio),
      sd_group = sd(gratio),
      n = n()
    )
  ttest_res <- t.test(gratio ~ treatment, data = df)
  print(ttest_res)
  main_plot <- ggplot(df, aes(x = axon_diam, y = gratio, color = treatment)) +
    geom_point(
      shape = 21, size = pt_size,
      show.legend = F
    ) +
    geom_smooth(method = "lm") +
    theme_and_axis_legend +
    labs(x = "Axon diameter (\u03BCm)", y = "G-ratio") +
    theme(legend.position = c(0.2, 0.9)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(0, 3, 0.5)) +
    coord_cartesian(xlim = c(0, 3), ylim = c(0.3, 1)) +
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
      x = 1.55, y = y_ttest, size = custom_annotation_size
    ) +
    scale_y_continuous(breaks = seq(0.5, 1, 0.2)) +
    coord_cartesian(ylim = c(0.5, 1)) +
    insert_theme
  ggdraw(main_plot) +
    draw_plot(insert_plot,
      x = 0.6, y = 0.15,
      width = 0.36, height = 0.5, scale = 1
    )
}

gratio_treatment_plot(gr)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "gratio_naris_occl_vs_control_lm_revision.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

t.test(gratio ~ treatment, data = gr)
ggplot(gr, aes(x = axon_diam, fill = treatment)) +
  geom_histogram(aes(y = ..density..),
    binwidth = 0.2,
    position = "dodge",
    alpha = 0.8
  ) +
  theme_and_axis_legend +
  control_vs_occl_fill +
  labs(x = "Axon diameter (\u03BCm)", y = "Density")
