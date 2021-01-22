# ais figures
# node plots for characterization figure
library(dplyr)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(Cairo)

SAVEALL <- FALSE 

source("../plotting_defaults/ggplot_theme_defaults.R")
img_save_rt <- "../naris_occlusion_node_ais_figure/ais_occlusion_figures/"

raw_data_path <- "../../../analysis_and_data/immunohistochemistry/data/summary_data/ais_length_201812to201907/aislen_summary_201912.csv"

#### plot and insert fn ####

cdf_with_insert_plot <- function(data, grouped_data, grouped_ecdfs, group_str, color_palette,
                                 ks_cdf, legend_pos_vec, coord_cart) {
  data_filtered <- filter(data, group == group_str)
  grouped_ecdfs_filtered <- filter(grouped_ecdfs, group == group_str)
  grouped_filtered <- filter(grouped_data, group == group_str)
  max_filt <- max(grouped_filtered$animalLenMean)
  y_ttest <- (max_filt * 0.1) + max_filt
  max_buf <- (max_filt * 0.18) + max_filt

  grouped_filtered_means <- grouped_filtered %>%
    group_by(occlusionStatus) %>%
    summarize(ml = mean(animalLenMean))

  ttestres <- t.test(animalLenMean ~ occlusionStatus, data = grouped_filtered, paired = T)

  base_cdf <-
    ggplot(data_filtered, aes(x = PathLength, color = occlusionStatus)) +
    stat_ecdf(geom = "step", pad = ecdf_pad, size = line_size) +
    geom_point(
      data = grouped_ecdfs_filtered, inherit.aes = F,
      aes(x = ml, y = ecdf_prob, color = occlusionStatus),
      size = ecdf_pt_size, alpha = pt_alpha,
      show.legend = FALSE
    ) +
    color_palette +
    annotate("text",
      label = paste("p = ", signif(ks_cdf$p.value, 3)),
      x = 24, y = 1, size = custom_annotation_size
    ) +
    labs(x = "AIS length (\u03BCm)", y = "Probability") +
    theme_and_axis_legend +
    theme(legend.position = legend_pos_vec) +
    coord_cart +
    scale_x_continuous(breaks = seq(0, 70, 10)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) + # !!!
    facet_grid(~group)


  insert_paired <- ggplot(grouped_filtered, aes(
    x = occlusionStatus,
    y = animalLenMean,
    color = occlusionStatus
  )) +
    color_palette +
    geom_bar(
      data = grouped_filtered_means,
      aes(x = occlusionStatus, y = ml, color = occlusionStatus),
      stat = "identity", width = barplot_width, fill = "white", size = line_size
    ) +
    geom_point(alpha = pt_alpha, size = pt_size) +
    geom_line(aes(group = animal), color = "black") +
    annotate("text",
      label = paste("p = ", signif(ttestres$p.value, 3)),
      x = 1.5, y = y_ttest, size = custom_annotation_size
    ) +
    insert_theme +
    labs(x = "", y = "Length (\u03BCm)") +
    coord_cartesian(expand = F, ylim = c(0, max_buf), xlim = c(0.1, 3)) + # !!!
    scale_y_continuous(breaks = seq(0, 30, 10)) # !!!

  ggdraw(base_cdf) +
    draw_plot(insert_paired,
      x = 0.6, y = 0.15, width = 0.3,
      height = 0.5, scale = 1
    )
}
#### end fn ####


#### data processing ####

rawd <- readr::read_csv(raw_data_path) %>% filter(PathLength <= 60)

rawd <- mutate(rawd, ctrlcmp = case_when(
  group == "Control" ~ "Control",
  group == "Naris Occlusion" ~ occlusionStatus,
  TRUE ~ "Unknown"
))


rawd_grouped <- rawd %>%
  group_by(group, animal, occlusionStatus, slice) %>%
  summarize(
    sliceLenMean = mean(PathLength, na.rm = T),
    n = n(), count_ais = length(PathLength)
  ) %>%
  group_by(group, animal, occlusionStatus) %>%
  summarize(
    animalLenMean = mean(sliceLenMean),
    animalLenSd = sd(sliceLenMean), n = n()
  )

rawd_grouped <- mutate(rawd_grouped, ctrlcmp = case_when(
  group == "Control" ~ "Control",
  group == "Naris Occlusion" ~ occlusionStatus,
  TRUE ~ "Unknown"
))

no_ctrl <- filter(rawd_grouped, group != "Control")

three_comparison_grouped <- rawd_grouped %>%
  filter(ctrlcmp == "Control") %>%
  group_by(animal) %>%
  summarize(animalLenMean = mean(animalLenMean)) %>%
  mutate(group = "Control", ctrlcmp = "Control") %>%
  bind_rows(., no_ctrl)


rawd$occlusionStatus <- forcats::fct_relevel(rawd$occlusionStatus, c("Left", "Right", "Open", "Occluded"))

rawd$ctrlcmp <- forcats::fct_relevel(rawd$ctrlcmp, c("Control", "Open", "Occluded"))

rawd_grouped$occlusionStatus <- forcats::fct_relevel(rawd_grouped$occlusionStatus, c("Left", "Right", "Open", "Occluded"))

rawd_grouped$ctrlcmp <- forcats::fct_relevel(rawd_grouped$ctrlcmp, c("Control", "Open", "Occluded"))

#### statistics ####

occl_side <- filter(rawd, group == "Naris Occlusion", occlusionStatus == "Occluded")$PathLength
open_side <- filter(rawd, group == "Naris Occlusion", occlusionStatus == "Open")$PathLength

occl_open_ks <- ks.test(occl_side, open_side)

left_side <- filter(rawd, group == "Control", occlusionStatus == "Left")$PathLength
right_side <- filter(rawd, group == "Control", occlusionStatus == "Right")$PathLength

left_right_ks <- ks.test(left_side, right_side)

naris_occluded_all <- filter(rawd, group != "Control")$PathLength
control_all <- filter(rawd, group == "Control")$PathLength

control_vs_occluded_ks_all <- ks.test(naris_occluded_all, control_all)

grouped_normal_shapiro <- shapiro.test(three_comparison_grouped$animalLenMean) # grouped data are normal
ggqqplot(three_comparison_grouped$animalLenMean) # nicely represented with qq plot too.

together_comps <- list(c("Open", "Occluded"), c("Control", "Occluded"), c("Control", "Open"))

open_vs_occl_grouped <- filter(three_comparison_grouped, ctrlcmp != "Control") %>%
  t.test(animalLenMean ~ occlusionStatus, .)

control_vs_occl_grouped <- filter(three_comparison_grouped, ctrlcmp != "Open") %>%
  t.test(animalLenMean ~ ctrlcmp, .)

control_vs_open_grouped <- filter(three_comparison_grouped, ctrlcmp != "Occluded") %>%
  t.test(animalLenMean ~ ctrlcmp, .)

grouped_aov_model <- aov(animalLenMean ~ ctrlcmp, data = three_comparison_grouped)

anova_res_grouped_model <- anova(grouped_aov_model)

grouped_tukey_results <- TukeyHSD(grouped_aov_model)

together_comps_pvals <- c(open_vs_occl_grouped$p.value, control_vs_occl_grouped$p.value, control_vs_open_grouped$p.value)
fdr_adjusted_together_comps_pvals <- p.adjust(together_comps_pvals, "fdr")
fdr_adjusted_together_comps_for_plot <- compare_means(animalLenMean ~ ctrlcmp,
  data = three_comparison_grouped,
  method = "t.test",
  p.adjust.method = "fdr"
)

#### paired, grouped statistics ####

ctrl_grouped_paired <- filter(rawd_grouped, group == "Control")
occl_grouped_paired <- filter(rawd_grouped, group != "Control")

ctrl_grouped_paired_t_test <- t.test(animalLenMean ~ occlusionStatus, data = ctrl_grouped_paired, paired = T)
occl_grouped_paired_t_test <- t.test(animalLenMean ~ occlusionStatus, data = occl_grouped_paired, paired = T)

# compute naris occl and open ECDF
occl_ecdf <- ecdf(occl_side)
open_ecdf <- ecdf(open_side)
left_ecdf <- ecdf(left_side)
right_ecdf <- ecdf(right_side)

grouped_ecdfs <- rawd_grouped %>%
  group_by(group, animal, occlusionStatus) %>%
  summarize(ml = mean(animalLenMean)) %>%
  mutate(ecdf_prob = case_when(
    occlusionStatus == "Left" ~ left_ecdf(ml),
    occlusionStatus == "Right" ~ right_ecdf(ml),
    occlusionStatus == "Open" ~ open_ecdf(ml),
    occlusionStatus == "Occluded" ~ occl_ecdf(ml)
  ))

#### KS test naris occlusion CDF plot ####

rawd %>%
  filter(group == "Naris Occlusion") %>%
  ggplot(aes(x = PathLength, color = occlusionStatus)) +
  stat_ecdf(geom = "step", pad = ecdf_pad, size = line_size) +
  occl_color +
  annotate("text",
    label = paste("p = ", signif(occl_open_ks$p.value, 3)),
    x = 24, y = 1, size = custom_annotation_size
  ) +
  labs(x = "AIS length (\u03BCm)", y = "Probability") +
  theme_and_axis_legend +
  theme(legend.position = c(0.7, 0.5)) +
  coord_cartesian(xlim = c(5, 50), ylim = c(0, 1.)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  facet_grid(~group)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "ais-naris-occl-cdf.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

### with barplot insert ####

cdf_with_insert_plot(
  rawd, rawd_grouped, grouped_ecdfs, "Naris Occlusion",
  occl_color, occl_open_ks, c(0.15, 0.5),
  coord_cartesian(xlim = c(5, 55), ylim = c(0, 1.))
)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "ais-naris-occl-animal-insert-cdf.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### KS test control CDF plot ####

rawd %>%
  filter(group == "Control") %>%
  ggplot(aes(x = PathLength, color = occlusionStatus)) +
  stat_ecdf(geom = "step", pad = ecdf_pad, size = 1.5) +
  ctrl_color +
  annotate("text",
    label = paste("p = ", signif(left_right_ks$p.value, 3)),
    x = 24, y = 1., size = custom_annotation_size
  ) +
  labs(x = "AIS length (\u03BCm)", y = "Probability") +
  theme_and_axis_legend +
  theme(legend.position = c(0.65, 0.5)) +
  coord_cartesian(xlim = c(5, 50), ylim = c(0, 1.)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  facet_grid(~group)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "ais-control-cdf.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

### with barplot insert ####

cdf_with_insert_plot(
  rawd, rawd_grouped, grouped_ecdfs, "Control",
  ctrl_color, left_right_ks, c(0.1, 0.5),
  coord_cartesian(xlim = c(5, 55), ylim = c(0, 1.))
)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "ais-control-animal-insert-cdf.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### KS test control vs naris occlusion CDF plot ####

rawd %>%
  ggplot(aes(x = PathLength, color = group)) +
  stat_ecdf(geom = "step", pad = ecdf_pad, size = 1.5) +
  control_vs_occl_color +
  annotate("text",
    label = paste("p = ", signif(control_vs_occluded_ks_all$p.value, 3)),
    x = 24, y = 1., size = custom_annotation_size
  ) +
  labs(x = "AIS length (\u03BCm)", y = "Probability") +
  theme_and_axis_legend +
  theme(legend.position = c(0.75, 0.5)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 1.)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2))

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "ais-control-vs-occl-all-cdf.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### Compare grouped per animal ####

ggplot(
  three_comparison_grouped,
  aes(x = ctrlcmp, y = animalLenMean, color = ctrlcmp)
) +
  geom_boxplot(size = 1.5) +
  geom_jitter(size = ecdf_pt_size, width = 0.1, alpha = pt_alpha) +
  three_color_ctrl_occl_open +
  labs(y = "AIS length (\u03BCm)", x = "") +
  stat_pvalue_manual(fdr_adjusted_together_comps_for_plot,
    label = "p = {p.adj}",
    y.position = c(28, 30, 29), size = custom_annotation_size
  ) +
  stat_compare_means(size = custom_annotation_size, label.x = 0.7, label.y = 31., method = "anova") +
  coord_cartesian(ylim = c(20, 31)) +
  scale_y_continuous(breaks = seq(15, 30, 2)) +
  theme_and_axis_nolegend

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "ais-by-animal-boxplot.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

#### plotting range of AIS lengths IN PROGRESS/EXPERIMENTAL! ####

sum_var <- rawd %>%
  group_by(group) %>%
  summarise(
    min_v = range(PathLength)[1],
    max_v = range(PathLength)[2],
    mean_v = mean(PathLength),
    sd_v = sd(PathLength),
    var_v = var(PathLength)
  ) %>%
  tidyr::pivot_longer(., cols = c(min_v, max_v, mean_v)) %>%
  select(-sd_v, var_v)

ggplot(rawd, aes(x = PathLength, y = group, color = group)) +
  stat_summary(
    geom = "ribbon", fun.data = mean_cl_boot,
    alpha = 0.6, linetype = "dashed", size = line_size,
    colour = "grey"
  ) +
  geom_line(inherit.aes = F, data = sum_var, aes(y = group, x = value, color = group)) +
  labs(x = "", y = "") +
  theme_and_axis_nolegend #+
coord_flip()


#### Writing summaries ####


# summaries

grouped_summary_df <- rawd_grouped %>%
  group_by(ctrlcmp) %>%
  summarize(
    mean_length = mean(animalLenMean),
    median_length = median(animalLenMean),
    sd_length = sd(animalLenMean)
  )

rawd_by_group_summarized <- rawd %>%
  group_by(group) %>%
  summarize(
    mean_length = mean(PathLength),
    median_length = median(PathLength),
    sd_length = sd(PathLength),
    max_len = max(PathLength),
    min_len = min(PathLength),
    n = n()
  )

rawd_by_side_summarized <- rawd %>%
  group_by(group, occlusionStatus) %>%
  summarize(
    mean_length = mean(PathLength),
    median_length = median(PathLength),
    sd_length = sd(PathLength),
    max_len = max(PathLength),
    min_len = min(PathLength),
    n = n()
  )

# possibly useful?

quantile(control_all, c(0.25, 0.5, 0.75, 0.95, 0.99))
quantile(naris_occluded_all, c(0.25, 0.5, 0.75, 0.95, 0.99))
homogenous_variance_animal_grouped_by_group <- fligner.test(animalLenMean ~ group, data = rawd_grouped)
homogenous_variance_animal_grouped_by_occlusion_status <- fligner.test(animalLenMean ~ occlusionStatus, data = rawd_grouped)
sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/ais_occlusion_results.org")

pretty_print_results("Control left vs right homogenous variance", fligner.test(PathLength ~ occlusionStatus,
  data = filter(rawd, group == "Control")
))
pretty_print_results("Naris Occlusion occl vs open homogenous variance", fligner.test(PathLength ~ occlusionStatus,
  data = filter(rawd, group != "Control")
))
pretty_print_results("Control vs Naris Occlusion  homogenous variance", fligner.test(PathLength ~ group, data = rawd))
pretty_print_results("Open vs occl KS test", occl_open_ks)
pretty_print_results("Left vs right KS test", left_right_ks)
pretty_print_results("Control vs occl KS test all", control_vs_occluded_ks_all)
pretty_print_results("Normality test three group data", grouped_normal_shapiro)
pretty_print_results("Summary Grouped data", grouped_summary_df)
pretty_print_results("Raw data by group summarized", rawd_by_group_summarized)
pretty_print_results("Raw data by group and side summarized", rawd_by_side_summarized)
pretty_print_results("ANOVA grouped summary", anova_res_grouped_model)
pretty_print_results("Homogenous variance group data by group", homogenous_variance_animal_grouped_by_group)
pretty_print_results("Homogenous variance group data by side", homogenous_variance_animal_grouped_by_occlusion_status)
pretty_print_results("T-test open vs occluded grouped", open_vs_occl_grouped)
pretty_print_results("T-test control vs occluded grouped", control_vs_occl_grouped)
pretty_print_results("T-test control vs open grouped", control_vs_open_grouped)
pretty_print_results("FDR adjusted three class comparison", fdr_adjusted_together_comps_for_plot)
pretty_print_results("Paired t-test control left-right", ctrl_grouped_paired_t_test)
pretty_print_results("Paired t-test naris occluded open-occluded", occl_grouped_paired_t_test)

# possibly useful?

pretty_print_results("Control quantiles", quantile(control_all, c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99)))
pretty_print_results("Naris Occlusion quantiles", quantile(naris_occluded_all, c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99)))
"---- End of statistics ----"
sink()
