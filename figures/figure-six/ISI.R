# ISI
# patch clamp ISI figures
library(dplyr)
library(ggplot2)

source("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/figures/2020_04_paper_draft_figures/plotting_defaults/ggplot_theme_defaults.R")
DB_PATH <- "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/patch_clamp/data/summary_data/spiking_201912-202001/patch_data_batch.db"
img_save_rt <- "isi_step_figures/"
SAVEALL <- FALSE

con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
steps_table <- DBI::dbReadTable(con, "peak_times") %>%
  mutate(
    treatment = case_when(
      treatment == "occl" ~ "Naris Occlusion",
      treatment == "sham" ~ "Control",
      TRUE ~ "unknown"
    ),
    cell_side = case_when(
      cell_side == "left" ~ "Left",
      cell_side == "right" ~ "Right",
      cell_side == "open" ~ "Open",
      cell_side == "occl" ~ "Occluded",
      TRUE ~ "unknown"
    ),
    three_case = case_when(
      treatment == "Control" ~ "Control",
      cell_side == "Open" ~ "Open",
      cell_side == "Occluded" ~ "Occluded"
    ),
    current = current * 1000
  ) %>%
  filter(cell_side != "unknown") %>%
  mutate(has_peak = if_else(!is.na(peak_time), 1, 0))

membrane_potential_raw <- DBI::dbReadTable(con, "RMP_CC01") %>%
  left_join(steps_table, ., by = c("fpath", "fname", "sweep")) %>%
  filter(sweep == 0)

DBI::dbDisconnect(con)

steps_table$treatment <- forcats::fct_relevel(steps_table$treatment, c("Control", "Naris Occlusion"))
steps_table$cell_side <- forcats::fct_relevel(steps_table$cell_side, c("Left", "Right", "Open", "Occluded"))
steps_table$three_case <- forcats::fct_relevel(steps_table$three_case, c("Control", "Open", "Occluded"))


#### Membrane potential ####

membrane_potential_sides_df <- membrane_potential_raw %>%
  group_by(treatment, mouse_id, cell_side, cell_n) %>%
  summarize(mrmp = mean(measured_mean_rmp), n = n()) %>%
  group_by(treatment, cell_side) %>%
  summarize(mean_rmp = mean(mrmp), sd_rmp = sd(mrmp), n = n())

membrane_potential_treatment_df <- membrane_potential_raw %>%
  group_by(treatment, mouse_id, cell_side, cell_n) %>%
  summarize(mrmp = mean(measured_mean_rmp), n = n()) %>%
  group_by(treatment) %>%
  summarize(mean_rmp = mean(mrmp), sd_rmp = sd(mrmp), n = n())

#### Membrane potential at holding for recording ttests ####
membrane_potential_ttest_side_control <- membrane_potential_raw %>%
  group_by(treatment, mouse_id, cell_side, cell_n) %>%
  summarize(mrmp = mean(measured_mean_rmp), n = n()) %>%
  filter(treatment == "Control") %>%
  t.test(mrmp ~ cell_side, data = .)

membrane_potential_ttest_side_naris_occlusion <- membrane_potential_raw %>%
  group_by(treatment, mouse_id, cell_side, cell_n) %>%
  summarize(mrmp = mean(measured_mean_rmp), n = n()) %>%
  filter(treatment != "Control") %>%
  t.test(mrmp ~ cell_side, data = .)

membrane_potential_ttest_treatment <- membrane_potential_raw %>%
  group_by(treatment, mouse_id, cell_side, cell_n) %>%
  summarize(mrmp = mean(measured_mean_rmp), n = n()) %>%
  t.test(mrmp ~ treatment, data = .)

#### ISI ####

isi <- steps_table %>%
  filter(current >= 0) %>%
  select(-fpath, -sweep, -membrane_potential, -protocol) %>%
  group_by(fname, mouse_id, cell_n, treatment, current) %>%
  arrange(peak_time, .by_group = T) %>%
  mutate(ISI_ms = (peak_time - lag(peak_time)) * 1000) # peak time is in seconds. So this is ms

#### Summarize data, group together by cell ####

isi_summary <- isi %>%
  group_by(fname, treatment, mouse_id, cell_side, cell_n, current) %>%
  summarize(
    mean_isi = mean(ISI_ms, na.rm = T),
    sd_isi = sd(ISI_ms, na.rm = T),
    median_isi = median(ISI_ms, na.rm = T),
    coeff_var = (sd(ISI_ms, na.rm = T) / mean(ISI_ms, na.rm = T)),
    n = n()
  )

ggplot(isi_summary, aes(x = current, y = mean_isi, color = cell_side)) +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  color_color +
  theme_and_axis_nolegend +
  labs(x = "Current step (pA)", y = "Inter-spike interval (ms)") +
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  facet_grid(~treatment)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "four_case_isi.pdf"), width = 12, height = 8)
}


isi_four_side_sum_glm <- glm(mean_isi ~ current * cell_side,
  data = isi_summary, family = gaussian
)
summary(isi_four_side_sum_glm)

#### Group by three-class side, don't double count Control ####

isi_three_summary <- isi %>%
  group_by(fname, treatment, mouse_id, three_case, cell_n, current) %>%
  summarize(
    mean_isi = mean(ISI_ms, na.rm = T),
    sd_isi = sd(ISI_ms, na.rm = T),
    median_isi = median(ISI_ms, na.rm = T),
    coeff_var = (sd(ISI_ms, na.rm = T) / mean(ISI_ms, na.rm = T)),
    n = n()
  )

ggplot(isi_three_summary, aes(x = current, y = mean_isi, color = three_case)) +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  three_color +
  labs(x = "Current step (pA)", y = "Inter-spike interval (ms)") +
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  theme_and_axis_nolegend

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "three_case_isi.pdf"), width = 8, height = 6)
}

isi_glm <- glm(mean_isi ~ current * three_case,
  data = isi_three_summary, family = gaussian
)
isi_glm_constant <- glm(mean_isi ~ 1, data = isi_three_summary, family = gaussian)
isi_glm_current <- glm(mean_isi ~ current, data = isi_three_summary, family = gaussian)

isi.aov <- anova(isi_glm, test = "F")
isi.aov.constant <- anova(isi_glm_constant, isi_glm, test = "F")
isi.aov.current <- anova(isi_glm_current, isi_glm, test = "F")

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



ctrl_vs_occluded_ttest <- ttest_sides_currs(isi_three_summary, "mean_isi", "Control", "Occluded")
ctrl_vs_open_ttest <- ttest_sides_currs(isi_three_summary, "mean_isi", "Control", "Open")
open_vs_occluded_ttest <- ttest_sides_currs(isi_three_summary, "mean_isi", "Open", "Occluded")
isi_csv <- bind_rows(list(ctrl_vs_occluded_ttest, ctrl_vs_open_ttest, open_vs_occluded_ttest))

#### print stats ####
readr::write_csv(isi_csv, "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/supplemental-results/isi_ttest_supplemental_data_2.csv")

sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/isi.org")
pretty_print_results("Mean RMP at holding by treatment", membrane_potential_treatment_df)
pretty_print_results("Mean RMP at holding by treatment ttest", membrane_potential_ttest_treatment)
pretty_print_results("Mean RMP at holding by side", membrane_potential_sides_df)
pretty_print_results("Mean RMP at holding by side Control ttest", membrane_potential_ttest_side_control)
pretty_print_results("Mean RMP at holding by side Naris Occlusion ttest", membrane_potential_ttest_side_naris_occlusion)
pretty_print_results("Mean RMP at holding by treatment ttest", membrane_potential_ttest_treatment)
pretty_print_results("Four case GLM ISI", summary(isi_four_side_sum_glm))
pretty_print_results("Three case GLM ISI", summary(isi_glm))
pretty_print_results("Three case GLM ISI ANOVA table", isi.aov)
pretty_print_results("Three case GLM ISI ANOVA vs constant table", isi.aov.constant)
pretty_print_results("Three case GLM ISI ANOVA vs current table", isi.aov.current)
sink()
