# Plot of % neurons with dendrites
library(dplyr)
library(ggplot2)
library(Cairo)

source("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/figures/2020_04_paper_draft_figures/plotting_defaults/ggplot_theme_defaults.R")
img_save_rt <- "current_step_figures_version2/"

data <- readr::read_csv("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/patch_clamp/data/classify_patch_clamp_histology/classify_dendrites_patch_clamp.csv") %>%
  tidyr::pivot_longer(., cols = c("full_dendrite?", "ais_present?"), names_to = "type") %>%
  mutate(type = case_when(
    type == "full_dendrite?" ~ "Has primary dendrite",
    type == "ais_present?" ~ "Has AIS",
    TRUE ~ "unknown"
  ))

data_summarized <- data %>%
  group_by(type) %>%
  summarize(n = n(), s = sum(value), proportion = s / n)

data_summarized_group <- data %>%
  group_by(treatment, type) %>%
  summarize(n = n(), s = sum(value), proportion = s / n)

ggplot(data_summarized, aes(type, proportion, color = type)) +
  geom_bar(
    stat = "identity", width = 0.5,
    position = "identity", fill = "white", size = line_size + 1
  ) +
  theme_and_axis_nolegend +
  scale_color_manual(values = c("#006600", "magenta")) +
  geom_text(data = data_summarized, color = "black", size = 7, aes(
    x = type, y = proportion,
    label = round(proportion, digits = 2),
    family = "Helvetica",
    fontface = "bold",
  ), nudge_y = 0.04) +
  labs(x = "", y = "Proportion")

ggsave(file.path(img_save_rt, "proportion_plot.pdf"), device = cairo_pdf, width = 8, height = 8)

sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/dendrite_ais_classification.org")
pretty_print_results("Classification table", data_summarized)
pretty_print_results("Classification table treatment", data_summarized_group)
sink()
