# three case analysis

library(dplyr)
library(ggplot2)
library(ggpubr)

source("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/figures/2020_04_paper_draft_figures/plotting_defaults/ggplot_theme_defaults.R")
DB_PATH <- "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/patch_clamp/data/summary_data/spiking_201912-202001/patch_data_batch.db"
img_save_rt <- "ap_figures/"
SAVEALL <- FALSE

#### Read and fmt ####
con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
ap_table <- DBI::dbReadTable(con, "ap_features") %>%
  filter(cell_side != "open-check") %>%
  filter(cell_side != "occl-check")
DBI::dbDisconnect(con)

ap_table <- ap_table %>%
  mutate(
    cell_side = case_when(
      cell_side == "left" ~ "Left",
      cell_side == "right" ~ "Right",
      cell_side == "open" ~ "Open",
      cell_side == "occl" ~ "Occluded",
      TRUE ~ cell_side
    ),
    side = case_when(
      cell_side == "Left" ~ "Left",
      cell_side == "Right" ~ "Right",
      cell_side == "Open" ~ "Left",
      cell_side == "Occluded" ~ "Right",
      TRUE ~ "unknown"
    ),
    treatment = case_when(
      treatment == "occl" ~ "Naris Occlusion",
      treatment == "sham" ~ "Control",
      TRUE ~ "unknown"
    ),
    three_case = case_when(
      treatment == "Control" ~ "Control",
      cell_side == "Open" ~ "Open",
      cell_side == "Occluded" ~ "Occluded",
      TRUE ~ "unknown"
    ),
    current = case_when(
      sweep == 0 ~ -50,
      sweep == 1 ~ -25,
      sweep == 2 ~ 0,
      sweep == 3 ~ 25,
      sweep == 4 ~ 50,
      sweep == 5 ~ 75,
      sweep == 6 ~ 100,
      sweep == 7 ~ 125,
      sweep == 8 ~ 150,
      sweep == 9 ~ 175,
      sweep == 10 ~ 200,
      sweep == 11 ~ 225,
      sweep == 12 ~ 250,
      sweep == 13 ~ 275,
      sweep == 14 ~ 300,
      sweep == 15 ~ 325,
      sweep == 16 ~ 350,
      sweep == 17 ~ 375,
      sweep == 18 ~ 400,
      sweep == 19 ~ 425,
      sweep == 20 ~ 450,
      sweep == 21 ~ 475,
      sweep == 22 ~ 500
    )
  )

ap_table$cell_side <- forcats::fct_relevel(ap_table$cell_side, c("Left", "Right", "Open", "Occluded"))
ap_table$treatment <- forcats::fct_relevel(ap_table$treatment, c("Control", "Naris Occlusion"))
ap_table$three_case <- forcats::fct_relevel(ap_table$three_case, c("Control", "Open", "Occluded"))


#### ttest fn ####
ret_3 <- function(df, cur, formula) {
  res <- df %>%
    filter(current == cur) %>%
    t.test(formula, data = .)
  c(res$p.value, res$statistic, res$parameter)
}

ttest_sides_currs <- function(df, thing, side1, side2) {
  currs <- unique(df$current)
  new_p <- c()
  new_t <- c()
  new_df <- c()
  formula <- as.formula(paste(thing, "~", "three_case", sep = " "))
  fil_df <- df %>%
    filter(three_case == side1 | three_case == side2)
  for  (cur in currs) {
    p_res <- tryCatch(ret_3(fil_df, cur, formula), error = function(e) {
      c(NA, NA, NA)
    })
    new_p <- append(new_p, p_res[1])
    new_t <- append(new_t, p_res[2])
    new_df <- append(new_df, p_res[3])
  }
  fdr_p <- p.adjust(new_p, method = "fdr")
  new_df <- data.frame(
    "Current" = currs, "p_value" = new_p, "FDR" = fdr_p,
    "t_value" = new_t, "DF" = new_df,
    "Groups" = paste(side1, side2, sep = " vs "),
    "Measurement_variable" = thing
  )
  new_df %>%
    mutate("signif" = case_when(
      fdr_p <= 0.001 ~ "***",
      fdr_p > 0.001 & fdr_p < 0.01 ~ "**",
      fdr_p > 0.01 & fdr_p <= 0.05 ~ "*",
      fdr_p > 0.05 ~ "ns",
      is.na(fdr_p) ~ "NA"
    ))
}

three_case_aps <- ap_table %>%
  group_by(treatment, mouse_id, three_case, cell_n, current) %>%
  summarize(
    mean_fwhm = mean(FWHM),
    mean_amplitude = mean(ap_amplitude),
    mean_threshold = mean(firing_threshold_voltage),
    mean_dydx = mean(max_dydx),
    n = n()
  )

n_cells_3_aps <- three_case_aps %>%
  group_by(three_case, current) %>%
  filter(current == 50) %>%
  tally()

#### FWHM ####

control_fwhm_glm <- glm(FWHM ~ current * cell_side,
  data = filter(ap_table, treatment == "Control"),
  family = gaussian
)
summary(control_fwhm_glm) # ns, only intercept and current

naris_occlusion_fwhm_glm <- glm(FWHM ~ current * cell_side,
  data = filter(ap_table, treatment != "Control"),
  family = gaussian
)

summary(naris_occlusion_fwhm_glm) # ns, only intercept and current

fwhm_glm <- glm(mean_fwhm ~ current * three_case, data = three_case_aps, family = gaussian)
fwhm_glm_constant <- glm(mean_fwhm ~ 1, data = three_case_aps, family = gaussian)
fwhm_glm_current <- glm(mean_fwhm ~ current, data = three_case_aps, family = gaussian)

fwhm.aov <- anova(fwhm_glm, test = "F")
fwhm.aov.constant <- anova(fwhm_glm_constant, fwhm_glm, test = "F")
fwhm.aov.current <- anova(fwhm_glm_current, fwhm_glm, test = "F")


ap_table %>%
  ggplot(aes(x = current, y = FWHM * 1000, color = cell_side)) +
  color_color +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  labs(x = "Current step (pA)", y = "AP width (ms)") +
  theme_and_axis_nolegend +
  scale_x_continuous(breaks = seq(50, 500, 50)) +
  facet_grid(~treatment)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "fwhm_steps_facet.pdf"), width = 12, height = 8)
}

three_case_aps %>%
  ggplot(aes(x = current, y = mean_fwhm * 1000, color = three_case)) +
  three_color +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  labs(x = "Current step (pA)", y = "AP width (ms)") +
  scale_x_continuous(breaks = seq(50, 500, 50)) +
  theme_and_axis_nolegend


if (SAVEALL) {
  ggsave(file.path(img_save_rt, "fwhm_steps_three_case.pdf"), width = 8, height = 6)
}

#### AP Amplitude ####


control_amplitude_glm <- glm(ap_amplitude ~ current * cell_side,
  data = filter(ap_table, treatment == "Control"),
  family = gaussian
)

summary(control_amplitude_glm) # ns, only intercept and current

naris_occlusion_amplitude_glm <- glm(ap_amplitude ~ current * cell_side,
  data = filter(ap_table, treatment != "Control"),
  family = gaussian
)

summary(naris_occlusion_amplitude_glm) # ns, only intercept and current

amplitude_glm <- glm(mean_amplitude ~ current * three_case,
  data = three_case_aps, family = gaussian
)
amplitude_glm_constant <- glm(mean_amplitude ~ 1, data = three_case_aps, family = gaussian)
amplitude_glm_current <- glm(mean_amplitude ~ current, data = three_case_aps, family = gaussian)

amplitude.aov <- anova(amplitude_glm, test = "F")
amplitude.aov.constant <- anova(amplitude_glm_constant, amplitude_glm, test = "F")
amplitude.aov.current <- anova(amplitude_glm_current, amplitude_glm, test = "F")

ap_table %>%
  ggplot(aes(x = current, y = ap_amplitude, color = cell_side)) +
  color_color +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  labs(x = "Current step (pA)", y = "AP amplitude (mV)") +
  theme_and_axis_nolegend +
  scale_x_continuous(breaks = seq(50, 500, 50)) +
  facet_grid(~treatment)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "amplitude_steps_facet.pdf"), width = 12, height = 8)
}

three_case_aps %>%
  ggplot(aes(x = current, y = mean_amplitude, color = three_case)) +
  three_color +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  labs(x = "Current step (pA)", y = "AP amplitude (mV)") +
  scale_x_continuous(breaks = seq(50, 500, 50)) +
  theme_and_axis_nolegend


if (SAVEALL) {
  ggsave(file.path(img_save_rt, "amplitude_steps_three_case.pdf"), width = 8, height = 6)
}

#### AP firing threshold ####

control_threshold_glm <- glm(firing_threshold_voltage ~ current * cell_side,
  data = filter(ap_table, treatment == "Control"),
  family = gaussian
)

summary(control_threshold_glm) # ns, only intercept and current

naris_occlusion_threshold_glm <- glm(firing_threshold_voltage ~ current * cell_side,
  data = filter(ap_table, treatment != "Control"),
  family = gaussian
)

summary(naris_occlusion_threshold_glm) # side occluded signif no interaction

threshold_glm <- glm(mean_threshold ~ current * three_case,
  data = three_case_aps, family = gaussian
)
threshold_glm_constant <- glm(mean_threshold ~ 1, data = three_case_aps, family = gaussian)
threshold_glm_current <- glm(mean_threshold ~ current, data = three_case_aps, family = gaussian)

threshold.aov <- anova(threshold_glm, test = "F")
threshold.aov.constant <- anova(threshold_glm_constant, threshold_glm, test = "F")
threshold.aov.current <- anova(threshold_glm_current, threshold_glm, test = "F")


ap_table %>%
  ggplot(aes(x = current, y = firing_threshold_voltage, color = cell_side)) +
  color_color +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  labs(x = "Current step (pA)", y = "AP threshold (mV)") +
  theme_and_axis_nolegend +
  scale_x_continuous(breaks = seq(50, 500, 50)) +
  facet_grid(~treatment)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "threshold_steps_facet.pdf"), width = 12, height = 8)
}

three_case_aps %>%
  ggplot(aes(x = current, y = mean_threshold, color = three_case)) +
  three_color +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  labs(x = "Current step (pA)", y = "AP threshold (mV)") +
  scale_x_continuous(breaks = seq(50, 500, 50)) +
  theme_and_axis_nolegend

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "threshold_steps_three_case.pdf"), width = 8, height = 6)
}

#### Max rise rate ####

control_rise_rate_glm <- glm(max_dydx ~ current * cell_side,
  data = filter(ap_table, treatment == "Control"),
  family = gaussian
)
summary(control_rise_rate_glm) # ns, only intercept and current

naris_occlusion_rise_rate_glm <- glm(max_dydx ~ current * cell_side,
  data = filter(ap_table, treatment != "Control"),
  family = gaussian
)

summary(naris_occlusion_rise_rate_glm) # ns, only current and intercept

three_case_rise_rate_glm <- glm(mean_dydx ~ current * three_case,
  data = three_case_aps, family = gaussian
)

summary(three_case_rise_rate_glm) # occluded is a significant covariate


ap_table %>%
  ggplot(aes(x = current, y = max_dydx, color = cell_side)) +
  color_color +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  labs(x = "Current step (pA)", y = "Max AP rise rate (V/s)") +
  scale_x_continuous(breaks = seq(50, 500, 50)) +
  theme_and_axis_nolegend +
  facet_grid(~treatment)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "max_rise_rate_four_case.pdf"), width = 12, height = 8)
}

three_case_aps %>%
  ggplot(aes(x = current, y = mean_dydx, color = three_case)) +
  three_color +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  labs(x = "Current step (pA)", y = "Max AP rise rate (V/s)") +
  scale_x_continuous(breaks = seq(50, 500, 50)) +
  theme_and_axis_nolegend

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "max_rise_rate_three_case.pdf"), width = 8, height = 6)
}

all_comps <- bind_rows(list(
  ttest_sides_currs(three_case_aps, thing = "mean_fwhm", "Control", "Occluded"),
  ttest_sides_currs(three_case_aps, thing = "mean_fwhm", "Control", "Open"),
  ttest_sides_currs(three_case_aps, thing = "mean_fwhm", "Occluded", "Open"),
  ttest_sides_currs(three_case_aps, thing = "mean_threshold", "Control", "Occluded"),
  ttest_sides_currs(three_case_aps, thing = "mean_threshold", "Control", "Open"),
  ttest_sides_currs(three_case_aps, thing = "mean_threshold", "Occluded", "Open"),
  ttest_sides_currs(three_case_aps, thing = "mean_amplitude", "Control", "Occluded"),
  ttest_sides_currs(three_case_aps, thing = "mean_amplitude", "Control", "Open"),
  ttest_sides_currs(three_case_aps, thing = "mean_amplitude", "Occluded", "Open")
))

#### print stats ####
readr::write_csv(all_comps, "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/supplemental-results/ap_ttests_supplemental_data_4.csv")

sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/action_potential_three_case.org")
pretty_print_results("N cells 3 group APs", n_cells_3_aps)
pretty_print_results("Control FWHM GLM", summary(control_fwhm_glm))
pretty_print_results("Naris Occlusion FWHM GLM", summary(naris_occlusion_fwhm_glm))
pretty_print_results("Three group FWHM GLM", summary(fwhm_glm))

pretty_print_results("Three case GLM FWHM ANOVA table", fwhm.aov)
pretty_print_results("Three case GLM FWHM ANOVA vs constant table", fwhm.aov.constant)
pretty_print_results("Three case GLM FWHM ANOVA vs current table", fwhm.aov.current)

pretty_print_results("Control amplitude GLM", summary(control_amplitude_glm))
pretty_print_results("Naris Occlusion amplitude GLM", summary(naris_occlusion_amplitude_glm))
pretty_print_results("Three group amplitude GLM", summary(amplitude_glm))

pretty_print_results("Three case GLM amplitude ANOVA table", amplitude.aov)
pretty_print_results("Three case GLM amplitude ANOVA vs constant table", amplitude.aov.constant)
pretty_print_results("Three case GLM amplitude ANOVA vs current table", amplitude.aov.current)

pretty_print_results("Control threshold GLM", summary(control_threshold_glm))
pretty_print_results("Naris Occlusion threshold GLM", summary(naris_occlusion_threshold_glm))
pretty_print_results("Three group threshold GLM", summary(threshold_glm))

pretty_print_results("Three case GLM threshold ANOVA table", threshold.aov)
pretty_print_results("Three case GLM threshold ANOVA vs constant table", threshold.aov.constant)
pretty_print_results("Three case GLM threshold ANOVA vs current table", threshold.aov.current)

sink()
