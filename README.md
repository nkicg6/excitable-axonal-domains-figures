# Utility scripts and figure analysis code for my thesis work

Initial submission date: TBD 

BioRxiv link: TBD

See [excitable-axonal-domains-physiology](https://github.com/nkicg6/excitable-axonal-domains-physiology) for code used to analyze electrophysiology data. 

# Scripts Key

Please note, these scripts are provided as-is and will not directly run to generate the figures. This is largely due to the fact that they contain hard coded variabes of relative file paths as they did when they were used for the primary analysis. Others were used as part of semi-automated pipelines and bound to key strokes for faster and consistent analysis and data saving (e.g. `th_intensity`). The `.py` files in the Fiji directory are meant to be run with the built in jython interpreter included with Fiji. The Fiji version at the time of submission was: 

```
Version: 2.1.0/1.53c
Build: 5f23140693
Date: 2020-08-02T03:43:22+0000
```
## `figures/*`

Contains R scripts used to do data wrangling, statistics, and generate plots for figures. 

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


