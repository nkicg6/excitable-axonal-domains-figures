# Current clamp steps analysis

library(dplyr)
library(ggplot2)
library(Cairo)

source("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/figures/2020_04_paper_draft_figures/plotting_defaults/ggplot_theme_defaults.R")
DB_PATH <- "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/patch_clamp/data/summary_data/spiking_201912-202001/patch_data_batch.db"
img_save_rt <- "current_step_figures_version2/"
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

DBI::dbDisconnect(con)

steps_table$treatment <- forcats::fct_relevel(steps_table$treatment, c("Control", "Naris Occlusion"))
steps_table$cell_side <- forcats::fct_relevel(steps_table$cell_side, c("Left", "Right", "Open", "Occluded"))
steps_table$three_case <- forcats::fct_relevel(steps_table$three_case, c("Control", "Open", "Occluded"))



####
#### Membrane Potential ####
####
RMP_CURRENT <- 150

membrane_potential_df <- steps_table %>%
  filter(current >= 0, membrane_potential != 0) %>%
  group_by(fname, mouse_id, treatment, cell_side, cell_n, current) %>%
  summarize(rmp = mean(membrane_potential)) %>%
  group_by(mouse_id, treatment, cell_side, cell_n, current) %>%
  summarize(mrmp = mean(rmp)) %>%
  mutate(three_case = case_when(
    treatment == "Control" ~ "Control",
    cell_side == "Open" ~ "Open",
    cell_side == "Occluded" ~ "Occluded",
    TRUE ~ "unknown"
  )) %>%
  mutate(unique_id = paste(mouse_id, cell_n, three_case, sep = "_")) %>%
  filter(mrmp < -40) # ONLY LESS THAN 40mV!

rmp_treatment_mean <- membrane_potential_df %>%
  filter(current == RMP_CURRENT) %>%
  group_by(treatment) %>%
  summarize(
    mean_group_rmp = mean(mrmp),
    sdmrmp = sd(mrmp), n = n(),
    semrmp = sdmrmp / n
  )

rmp_treatment_mean_ttest <- membrane_potential_df %>%
  filter(current == RMP_CURRENT) %>%
  t.test(mrmp ~ treatment, data = .)

membrane_potential_df_three <- membrane_potential_df %>%
  filter(current == RMP_CURRENT) %>%
  group_by(treatment, mouse_id, three_case, cell_n, current) %>%
  summarize(mmrmp = mean(mrmp))

anova_rmp_three_case <- anova(aov(mmrmp ~ three_case, data = membrane_potential_df_three))

rmp_treatment_mean_three_case <- membrane_potential_df_three %>%
  filter(current == RMP_CURRENT) %>%
  group_by(three_case) %>%
  summarize(
    mean_group_rmp = mean(mmrmp),
    sdmrmp = sd(mmrmp), n = n(), semrmp = sdmrmp / n
  )

sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/RMP_patch_clamp_version2.org")
pretty_print_results("Three Group RMP summary", rmp_treatment_mean_three_case)
pretty_print_results("Treatment RMP summary", rmp_treatment_mean)
pretty_print_results("Treatment RMP t-test", rmp_treatment_mean_ttest)
pretty_print_results("Anova three group ", anova_rmp_three_case)
sink()


membrane_potential_df %>%
  filter(current == RMP_CURRENT) %>%
  ggplot(aes(mrmp, fill = three_case)) +
  geom_density(alpha = pt_alpha) +
  geom_vline(
    data = rmp_treatment_mean_three_case, size = line_size, show.legend = F,
    aes(xintercept = mean_group_rmp, color = three_case)
  ) +
  theme_and_axis_legend +
  three_color +
  three_fill +
  labs(x = "Resting membrane potential (mV)", y = "Density") +
  coord_cartesian(xlim = c(-74, -35), expand = F)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "rmp_density_plot_three.pdf"),
    device = cairo_pdf, width = 8, height = 6
  )
}

membrane_potential_df %>%
  filter(current == RMP_CURRENT) %>%
  ggplot(aes(mrmp, fill = treatment)) +
  geom_density(alpha = pt_alpha) +
  geom_vline(
    data = rmp_treatment_mean, size = line_size, show.legend = F,
    aes(xintercept = mean_group_rmp, color = treatment)
  ) +
  theme_and_axis_legend +
  control_vs_occl_fill +
  control_vs_occl_color +
  labs(x = "Resting membrane potential (mV)", y = "Density") +
  coord_cartesian(xlim = c(-74, -35), expand = F)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "rmp_density_plot_treatment.pdf"),
    device = cairo_pdf, width = 8, height = 6
  )
}

#### END Membrane Potential ####

spikes <- steps_table %>%
  filter(current >= 0) %>% # only positive currents
  group_by(fname, mouse_id, treatment, cell_side, cell_n, current) %>%
  summarize(n_peaks = sum(has_peak)) %>%
  # if a cell was recorded twice, average them
  group_by(mouse_id, treatment, cell_side, cell_n, current) %>%
  summarize(mn_peaks = mean(n_peaks), n = n()) %>%
  mutate(three_case = case_when(
    treatment == "Control" ~ "Control",
    cell_side == "Open" ~ "Open",
    cell_side == "Occluded" ~ "Occluded",
    TRUE ~ "unknown"
  ))

#### Group by three-class side, don't double count Control ####

spikes_three_summary <- spikes %>%
  group_by(treatment, mouse_id, three_case, cell_n, current) %>%
  summarize(
    mean_spikes = mean(mn_peaks, na.rm = T),
    sd_spikes = sd(mn_peaks, na.rm = T),
    spike_coeff_var = sd_spikes / mean_spikes
  )

#### ISI ####
isi <- steps_table %>%
  filter(current >= 0) %>%
  select(-fpath, -sweep, -membrane_potential, -protocol) %>%
  group_by(fname, mouse_id, cell_n, treatment, current) %>%
  arrange(peak_time, .by_group = T) %>%
  mutate(ISI_ms = (peak_time - lag(peak_time)) * 1000) # peak time is in seconds. So this is ms

#### Summarize data, group together by cell ####

isi_summary <- isi %>%
  group_by(treatment, mouse_id, three_case, cell_n, current) %>%
  summarize(
    mean_isi = mean(ISI_ms, na.rm = T),
    sd_isi = sd(ISI_ms, na.rm = T),
    median_isi = median(ISI_ms, na.rm = T),
    coeff_var = sd_isi / mean_isi,
    n = n()
  )

#### ISI GLM ####

coeff_var_isi_glm <- glm(coeff_var ~ current * three_case, data = isi_summary, family = gaussian)
coeff_var_isi_glm_constant <- glm(coeff_var ~ 1, data = isi_summary, family = gaussian)
coeff_var_isi_glm_current <- glm(coeff_var ~ current, data = isi_summary, family = gaussian)

isi.coeff.aov.constant <- anova(coeff_var_isi_glm_constant, coeff_var_isi_glm, test = "F")
isi.coeff.aov.current <- anova(coeff_var_isi_glm_current, coeff_var_isi_glm, test = "F")
isi.coeff.aov <- anova(coeff_var_isi_glm, test = "F")

coeff_var_isi_treatment_glm <- glm(coeff_var ~ current * treatment, data = isi_summary, family = gaussian)
coeff_var_isi_treatment_glm.aov <- anova(coeff_var_isi_treatment_glm, test = "F")

#### ttest ####
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


ctrl_vs_occluded_ttest <- ttest_sides_currs(isi_summary, "coeff_var", "Control", "Occluded")
ctrl_vs_open_ttest <- ttest_sides_currs(isi_summary, "coeff_var", "Control", "Open")
open_vs_occluded_ttest <- ttest_sides_currs(isi_summary, "coeff_var", "Open", "Occluded")
isi_csv <- bind_rows(list(ctrl_vs_occluded_ttest, ctrl_vs_open_ttest, open_vs_occluded_ttest))

# ISI CV glm #
readr::write_csv(isi_csv, "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/supplemental-results/isi_coeff_var_ttest_supplemental_data_3.csv")

sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/isi_cv_patch_clamp.org")
pretty_print_results("Three case GLM ISI Coeff variation", summary(coeff_var_isi_glm))
pretty_print_results("Three case GLM ISI Coeff variation ANOVA table", isi.coeff.aov)
pretty_print_results("Three case GLM ISI Coeff variation ANOVA vs constant table", isi.coeff.aov.constant)
pretty_print_results("Three case GLM ISI Coeff variation ANOVA vs current table", isi.coeff.aov.current)
pretty_print_results("Three case GLM ISI by treatment coeff variation", summary(coeff_var_isi_treatment_glm))
pretty_print_results("Three case GLM ISI by treatment coeff variation ANOVA table", coeff_var_isi_treatment_glm.aov)

sink()


ggplot(isi_summary, aes(x = current, y = coeff_var, color = three_case)) +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  three_color +
  labs(x = "Current step (pA)", y = "Inter-spike interval CV") +
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  theme_and_axis_nolegend

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "isi_three_case.pdf"), device = cairo_pdf, width = 8, height = 6)
}

####
#### Single AP ####
####

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



three_case_aps <- ap_table %>%
  group_by(treatment, mouse_id, three_case, cell_n, current) %>%
  summarize(
    mean_fwhm = mean(FWHM, na.rm = T),
    mean_amplitude = mean(ap_amplitude, na.rm = T),
    mean_threshold = mean(firing_threshold_voltage, na.rm = T),
    mean_dydx = mean(max_dydx, na.rm = T),
    n = n()
  ) %>%
  mutate(unique_id = paste(mouse_id, cell_n, three_case, sep = "_"))

#### Join the membrane potential and ap table ####

mp_join <- membrane_potential_df_three %>%
  mutate(unique_id = paste(mouse_id, cell_n, three_case, sep = "_")) %>%
  ungroup() %>%
  filter(current == 150) %>%
  select(unique_id, mmrmp)

ap_join <- three_case_aps %>%
  filter(current == 150)

ap_mp_df <- left_join(ap_join, mp_join, by = "unique_id")

ggplot(ap_mp_df, aes(x = mmrmp, y = mean_amplitude, color = three_case)) +
  geom_smooth(method = "lm", size = line_size + 1, se = F) +
  geom_point(size = pt_size + 2) +
  three_color +
  theme_and_axis_nolegend +
  labs(x = "Resting membrane potential (mV)", y = "AP amplitude (mV)")

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "amplitude_by_rmp.pdf"), device = cairo_pdf, width = 8, height = 6)
}

linear_model_amp_rmp <- lm(mean_amplitude ~ mmrmp * three_case, data = ap_mp_df)

summary(linear_model_amp_rmp)

summary(lm(mean_amplitude ~ mmrmp, data = filter(ap_mp_df, three_case == "Control")))
summary(lm(mean_amplitude ~ mmrmp, data = filter(ap_mp_df, three_case == "Open")))
summary(lm(mean_amplitude ~ mmrmp, data = filter(ap_mp_df, three_case == "Occluded")))



ctrl <- ap_mp_df %>%
  filter_all(all_vars(!is.nan(.))) %>%
  filter(three_case == "Control")

open <- ap_mp_df %>%
  filter_all(all_vars(!is.nan(.))) %>%
  filter(three_case == "Open")

occluded <- ap_mp_df %>%
  filter_all(all_vars(!is.nan(.))) %>%
  filter(three_case == "Occluded")

ctrl_corr <- cor.test(ctrl$mean_amplitude, ctrl$mmrmp)
open_corr <- cor.test(open$mean_amplitude, open$mmrmp)
occl_corr <- cor.test(occluded$mean_amplitude, occluded$mmrmp)

summary_table_ap_rmp <- ap_mp_df %>%
  group_by(three_case) %>%
  filter_all(all_vars(!is.nan(.))) %>%
  summarize(
    mean_rmp = mean(mmrmp, na.rm = T),
    sd_rmp = sd(mmrmp, na.rm = T), n = n(),
    sem_rmp = sd_rmp / n
  )

# membrane potential amplitude #
sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/ap_amp_membpot_patch_clamp.org")
pretty_print_results("Linear model Amplitude by RMP", summary(linear_model_amp_rmp))
pretty_print_results("AP amplitude at 150pA step summary", summary_table_ap_rmp)
pretty_print_results("Control correlation Amp vs RMP", ctrl_corr)
pretty_print_results("Open correlation Amp vs RMP", open_corr)
pretty_print_results("Occluded correlation Amp vs RMP", occl_corr)
sink()
