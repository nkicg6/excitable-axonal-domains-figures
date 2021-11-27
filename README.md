# Utility scripts and figure analysis code for my thesis work

Initial submission date: 2021-01-25 

Resubmission date (second journal): 2021-11-21

bioRxiv link: https://www.biorxiv.org/content/10.1101/2021.01.25.428132v1

See [excitable-axonal-domains-physiology](https://github.com/nkicg6/excitable-axonal-domains-physiology) for code used to analyze electrophysiology data. 

Analysis code was primarily developed with R on macOS Mojave (version `10.14.6`):

```
# r --version
R version 3.6.1 (2019-07-05) -- "Action of the Toes"
Copyright (C) 2019 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin15.6.0 (64-bit)
```

# Scripts Key

Please note, these scripts are provided as-is and will not directly run to generate the figures. This is largely due to the fact that they contain hard coded variabes of relative file paths as they did when they were used for the primary analysis. Others were used as part of semi-automated pipelines and bound to key strokes for faster and consistent analysis and data saving (e.g. `th_intensity`). The `.py` files in the Fiji directory are meant to be run with the built in jython interpreter included with [Fiji](https://fiji.sc). The Fiji version at the time of submission was: 

```
Version: 2.1.0/1.53c
Build: 5f23140693
Date: 2020-08-02T03:43:22+0000
```
## `figures/*`

R scripts used to do data wrangling, statistics, and generate plots for figures. 

## `utils/rename_gather_multipoint_a1r.py`

Script to rename and organize files gathered from Nikon A1R confocal microscope in multipoint mode. 

Files are stored in an `experiment` directory together and ready for blinding. This script accepts two command line options:
- `-d` input directory, required
- `--dry-run` prints output so you can verify what it is doing. 

`python3 -d path/to/files --dry-run` for a test run which will print where the files will be moved to
`python3 -d path/to/files` for a run to rename and move all files

## `fiji/th_intensity.py`

Jython script run after tracing ROIs in a tyrosine hydroxylase image. Script will save the ROIs (from the ROI Manager), extract the values, and save them as a CSV. 

Used to make Figure 3. 

## `fiji/node_intensity_analysis.py`

Jython script to extract fluoro intensity from pre-traced ROI's and source images. 

## `fiji/format_AMT_EM_Images.py`

Jython script to automatically crop and calibrate EM images from CU Anschutz Core EM scope.

## `fiji/grab_em_measurements.py`

Jython script to grab the measurements from the currently opened image, write them to a file, close the image, and re-open the next image. 

**NOTE** Paths are hard-coded in this script. It is provided as an example for how the process was semi-automated. 

To use this shortcut script, I copied it to the `Fiji.app/plugins/` and then bound it to a keyboard shortcut. I run the script after tracing all the images in a file. 
