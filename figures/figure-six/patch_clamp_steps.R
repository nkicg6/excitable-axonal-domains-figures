# Patch clamp steps analysis
library(dplyr)
library(ggplot2)

source("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/figures/2020_04_paper_draft_figures/plotting_defaults/ggplot_theme_defaults.R")

DB_PATH <- "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/patch_clamp/data/summary_data/spiking_201912-202001/patch_data_batch.db"
img_save_rt <- "current_step_figures/"
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

ttest_sides_currs <- function(df, thing, side1, side2) {
  currs <- unique(df$current)
  new_p <- c()
  new_t <- c()
  new_df <- c()
  formula <- as.formula(paste(thing, "~", "three_case", sep = " "))
  fil_df <- df %>%
    filter(three_case == side1 | three_case == side2)
  for  (cur in currs) {
    p_res <- fil_df %>%
      filter(current == cur) %>%
      t.test(formula, data = .)
    new_p <- append(new_p, p_res$p.value)
    new_t <- append(new_t, p_res$statistic)
    new_df <- append(new_df, p_res$parameter)
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
      fdr_p > 0.05 ~ "ns"
    ))
}


#### Test to ensure we are getting the quantities we think. ####
tmp <- steps_table %>%
  filter(current >= 0) %>% # only positive currents
  group_by(fname, mouse_id, treatment, cell_side, cell_n, current) %>%
  summarize(n_peaks = sum(has_peak)) %>%
  filter(mouse_id == "180753-none", cell_n == 5)

t2 <- tmp %>%
  group_by(mouse_id, treatment, cell_side, cell_n, current) %>%
  summarize(mn_peaks = mean(n_peaks))

# View(tmp)
# View(t2)

#### end test ####

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
    mean_spikes = mean(mn_peaks),
    sd_spikes = sd(mn_peaks)
  )

#### spikes analysis ####

four_spikes <- glm(mn_peaks ~ current * cell_side, data = spikes, family = gaussian)
summary(four_spikes)
four_spikes.aov <- anova(four_spikes, test = "F")

three_spikes <- glm(mean_spikes ~ current * three_case, data = spikes_three_summary, family = gaussian)
summary(three_spikes)

three.aov <- anova(three_spikes, test = "F")

three_spikes_glm_current <- glm(mean_spikes ~ current, data = spikes_three_summary, family = gaussian)
three_spikes_glm_constant <- glm(mean_spikes ~ 1, data = spikes_three_summary, family = gaussian)

three.aov.constant <- anova(three_spikes_glm_constant, three_spikes, test = "F")
three.aov.current <- anova(three_spikes_glm_current, three_spikes, test = "F")

ctrl_vs_occluded_ttest <- ttest_sides_currs(
  spikes_three_summary, "mean_spikes",
  "Control", "Occluded"
)
ctrl_vs_open_ttest <- ttest_sides_currs(
  spikes_three_summary, "mean_spikes",
  "Control", "Open"
)
occluded_vs_open_ttest <- ttest_sides_currs(
  spikes_three_summary, "mean_spikes",
  "Occluded", "Open"
)

spikes_ttest_csv <- bind_rows(list(ctrl_vs_occluded_ttest, ctrl_vs_open_ttest, occluded_vs_open_ttest))

spikes_three_summary %>%
  ggplot(aes(x = current, y = mean_spikes, color = three_case)) +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  labs(x = "Current step (pA)", y = "Spiking frequency (Hz)") +
  three_color +
  theme_and_axis_nolegend +
  scale_x_continuous(breaks = seq(0, 500, 50))

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "three_class_spikes_steps.pdf"), width = 8, height = 6)
}

spikes %>%
  ggplot(aes(x = current, y = mn_peaks, color = cell_side)) +
  stat_summary(fun.data = "mean_se", size = pt_size, show.legend = F) +
  stat_summary(fun.data = "mean_se", geom = "line", size = line_size) +
  #  geom_point(alpha = pt_alpha) +
  labs(x = "Current step (pA)", y = "Spiking frequency (Hz)") +
  theme_and_axis_legend +
  color_color +
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  facet_grid(~treatment)

if (SAVEALL) {
  ggsave(file.path(img_save_rt, "four_class_spikes_steps.pdf"), width = 12, height = 8)
}

n_cells_3 <- spikes_three_summary %>%
  group_by(three_case, current) %>%
  filter(current == 0) %>%
  tally()
n_cells_4 <- spikes %>%
  group_by(treatment, cell_side, current) %>%
  filter(current == 0) %>%
  tally()

#### print stats ####
readr::write_csv(spikes_ttest_csv, "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/supplemental-results/spikes_ttest_supplemental_data_1.csv")

sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/spikes_three_case.org")
pretty_print_results("Three group spikes GLM", summary(three_spikes))
pretty_print_results("Three group spikes ANOVA table", three.aov)
pretty_print_results("Three group spikes ANOVA vs Constant table", three.aov.constant)
pretty_print_results("Three group spikes ANOVA vs Current table", three.aov.current)
pretty_print_results("Four group spikes GLM", summary(four_spikes))
pretty_print_results("Four group spikes ANOVA table", four_spikes.aov)
pretty_print_results("N cells 3 group", n_cells_3)
pretty_print_results("N cells 4 group", n_cells_4)
sink()
