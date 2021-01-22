# Utility scripts and figure analysis code for my thesis work.

* Key

** `utils/rename_gather_multipoint_a1r.py`

Script to rename and organize files gathered from Nikon A1R confocal microscope in multipoint mode. 

Files are stored in an `experiment` directory together and ready for blinding. This script accepts two command line options:
- `-d` input directory, required
- `--dry-run` prints output so you can verify what it is doing. 

`python3 -d path/to/files --dry-run` for a test run which will print where the files will be moved to
`python3 -d path/to/files` for a run to rename and move all files


