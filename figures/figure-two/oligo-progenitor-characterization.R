# manual Oligo progenitor calculation
library(dplyr)
library(ggplot2)
library(tidyr)
library(Cairo)

#### preamble ####

SAVEALL <- TRUE

source("../plotting_defaults/ggplot_theme_defaults.R")

img_save_rt <- "../oligos_and_myelin_characterization_figure/oligos_figures/"

control_data <- readr::read_csv("~/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/immunohistochemistry/data/summary_data/oligo_lineage/lineage_summary_202008.csv") %>%
  rename("Olig2+ PDGFR\u03b1+" = "prog", "Olig2+ CC1+" = "mature") %>%
  mutate(
    roi_area_mm2 = roi_area_um2 / 1e6,
    region = case_when(region == "gcl" ~ "GCL", region == "lot" ~ "LOT", TRUE ~ "unknown"),
    density_prog = `Olig2+ PDGFRα+` / (roi_area_mm2 * (41 * (0.3/1000))), # n steps * optical section size * XY area
    density_mature = `Olig2+ CC1+` / (roi_area_mm2 * (41 * (0.3/1000))), # n steps * optical section size * XY area
    total_cells = (`Olig2+ PDGFRα+` + `Olig2+ CC1+`)
  ) %>%
  filter(side != "unknown", treatment == "Control")

control_data$side <- forcats::fct_relevel(control_data$side, c("Left", "Right"))

control_by_animal_cell_count <- control_data %>%
  group_by(treatment, region, animal_id) %>%
  summarize(
    mean_prog_density = mean(density_prog),
    mean_mature_density = mean(density_mature),
    mean_total_cells = mean(total_cells),
    mean_mature_n = mean(`Olig2+ CC1+`),
    mean_prog_n = mean(`Olig2+ PDGFRα+`),
    mean_roi_area = mean(roi_area_um2)
  )

control_by_animal_density <- pivot_longer(control_by_animal_cell_count, c("mean_prog_density", "mean_mature_density"),
  names_to = "stage", values_to = "cell_density"
) %>%
  select(-mean_total_cells, -mean_mature_n, -mean_prog_n, -mean_roi_area) %>%
  mutate(dummy_name = case_when(
    stage == "mean_prog_density" ~ "Olig2+ PDGFR\u03b1+",
    stage == "mean_mature_density" ~ "Olig2+ CC1+"
  ))

control_by_animal_density$dummy_name <- forcats::fct_relevel(control_by_animal_density$dummy_name, c("Olig2+ PDGFRα+","Olig2+ CC1+"))
stage_and_region_means <- group_by(control_by_animal_density, treatment, region, dummy_name) %>%
  summarize(
    mean_region_cell = mean(cell_density),
    median_region_cell = median(cell_density),
    sd_region_cell = sd(cell_density)
  )



#### Plots ####

ggplot(control_by_animal_density, aes(x = region, y = cell_density, color = dummy_name)) +
  labs(x = "", y = "Cells/mm\u00B3") +
  theme_and_axis_legend +
  geom_bar(
    data = stage_and_region_means,
    aes(x = region, y = mean_region_cell, color = dummy_name),
    fill = "white",
    stat = "identity", width = barplot_width, position = "dodge",
    size = line_size
  ) +
  ctrl_color +
  geom_jitter(
    alpha = pt_alpha, size = pt_size, # shape = 21,
    stroke = pt_stroke,
    position = position_dodge(width = barplot_width),
    show.legend = F
  ) +
  theme(legend.position = "bottom")




ggplot(control_by_animal_density, aes(x = region, y = cell_density, fill = dummy_name)) +
  labs(x = "", y = "Cells/mm\u00B3") +
  theme_and_axis_legend +
  geom_bar(
    data = stage_and_region_means,
    aes(x = region, y = mean_region_cell, fill = dummy_name),
    stat = "identity", width = barplot_width, position = "dodge",
    size = line_size
  ) +
  oligo_lineage_fill +
  geom_jitter(
    alpha = pt_alpha, size = pt_size, shape = 21,
    stroke = pt_stroke,
    position = position_dodge(width = barplot_width),
    show.legend = F
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 25, face = "bold", color = black),
    axis.text = element_text(size = 25, face = "bold", color = black)
  )


if (SAVEALL) {
  ggsave(file.path(img_save_rt, "characterization-count-progenitors-by-region.pdf"),
    width = 7, height = 8, device = cairo_pdf
  )
}


#### statistics
sink("/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/figures/oligo_lineage_characterization_results.org")
pretty_print_results("By animal density", control_by_animal_density)
pretty_print_results("Region/stage density", stage_and_region_means)
sink()
