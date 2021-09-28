# node plots for figure
library(dplyr)
library(ggplot2)
library(cowplot)
library(Cairo)

#### setup ####

SAVEALL <- FALSE

source("../plotting_defaults/ggplot_theme_defaults.R")

img_save_rt <- "../naris_occlusion_node_ais_figure/node_occlusion_figures/"
data_path <- "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/immunohistochemistry/data/summary_data/node_length_manual_alac/alac-node-data.csv"

alac_raw <- readr::read_csv(data_path)