library(dplyr)
library(ggplot2)
library(Cairo)

SAVEALL <- FALSE

#### preamble ####

source("../plotting_defaults/ggplot_theme_defaults.R")
img_save_rt <- "../oligos_and_myelin_characterization_figure/em_figures"

#### grouping and processing ####

gr <- readr::read_csv("../../../analysis_and_data/EM/data/summary_data/em_gratio_manual_2020-07/summary_gratio_em_manual_202007_with_perimeters.csv") %>%
  mutate(
    ctrlcmp = case_when(
      treatment == "Control" ~ "Control",
      treatment == "Naris Occlusion" ~ side,
      TRUE ~ "unknown"
    ),
    gratio_perim = ax_perimeter / my_perimeter
  ) %>%
  filter(ctrlcmp == "Control")

gr %>%
  ggplot(aes(x = gratio, fill = treatment)) +
  scale_fill_manual(values = c("grey")) +
  geom_histogram(color = "black", size = line_size, binwidth = 0.025) +
  geom_vline(aes(xintercept = mean(gratio)), linetype = "longdash", size = line_size) +
  labs(x = "G-ratio", y = "Number of myelinated axons") +
  theme_and_axis_nolegend +
  coord_cartesian(xlim = c(0.55, 0.95), expand = F) +
  scale_x_continuous(breaks = seq(0.6, 0.95, 0.1))

# if (SAVEALL) {
#   ggsave(file.path(img_save_rt, "gratio-histogram-control.pdf"),
#          width = 8, height = 5, dpi = 300, device = cairo_pdf)
# }

gr %>%
  ggplot(aes(x = gratio_perim, fill = treatment)) +
  scale_fill_manual(values = c("grey")) +
  geom_histogram(color = "black", size = line_size, binwidth = 0.025) +
  geom_vline(aes(xintercept = mean(gratio_perim)), linetype = "longdash", size = line_size) +
  labs(x = "G-ratio", y = "Number of \nmyelinated axons") +
  theme_and_axis_nolegend +
  coord_cartesian(xlim = c(0.55, 0.95), expand = F) +
  scale_x_continuous(breaks = seq(0.6, 0.95, 0.1)) +
  theme( # plot.margin = margin(1.2, 0.25, 0.25, 0.25, "cm"),
    axis.text = element_text(size = 25, face = "bold", color = black),
    text = element_text(size = 30, face = "bold")
  ) # fix text writing off


if (SAVEALL) {
  ggsave(file.path(img_save_rt, "gratio-histogram-perimeter-control.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

gr %>%
  ggplot(aes(x = ax_diam, fill = treatment)) +
  scale_fill_manual(values = c("grey")) +
  geom_histogram(color = "black", size = line_size, binwidth = 0.2) +
  geom_vline(aes(xintercept = mean(ax_diam)), linetype = "longdash", size = line_size) +
  labs(x = "Axon diameter (\u03BCm)", y = "Number of axons") +
  theme_and_axis_nolegend +
  coord_cartesian(xlim = c(0.0, 4.5), expand = F) +
  scale_x_continuous(breaks = seq(0.5, 4, 0.5)) +
  theme(
    plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"),
    axis.text = element_text(size = 25, face = "bold", color = black),
    text = element_text(size = 30, face = "bold")
  ) # fix text writing off


if (SAVEALL) {
  ggsave(file.path(img_save_rt, "axdiam-histogram-control.pdf"),
    width = 8, height = 5, dpi = 300, device = cairo_pdf
  )
}

gr_stats <- gr %>%
  summarize(
    gr_med = median(gratio),
    gr_mean = mean(gratio),
    gr_perim_med = median(gratio_perim),
    gr_perim_mean = mean(gratio_perim),
    ax_med = median(ax_diam),
    ax_mean = mean(ax_diam),
    ax_sd = sd(ax_diam),
    gr_sd = sd(gratio),
    gr_perim_sd = sd(gratio_perim),
    n = n()
  )

#### play maybe include ####
ggplot(gr, aes(x = ax_diam, y = gratio_perim)) +
  geom_point() +
  geom_smooth(method = "lm")


ggplot(gr, aes(x = ax_diam, y = my_diam)) +
  geom_point() +
  geom_smooth(method = "lm")


#### statistics ####

# linear models (not used)
lm_gr <- lm(gratio_perim ~ ax_diam, data = gr)
summary(lm_gr)

ggplot(gr, aes(x = ax_diam, y = gratio_perim)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation()

sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/myelin_characterization_results.org")
pretty_print_results("G-ratio and axon diameter results", gr_stats)
sink()
