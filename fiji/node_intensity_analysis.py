# Jython, must be run via Fiji's Jython interpreter
# Jython script to get node of Ranvier length
# Author: Nick George

import os
import re
import time
import json
from ij import IJ
from ij.io import Opener
from ij.gui import ProfilePlot, PlotWindow
from ij.plugin.frame import RoiManager
from ij.measure import CurveFitter
from java.lang import Double

start_t = time.time()


def getXY(image):
    image.setC(2)
    pp = ProfilePlot(image).getPlot()
    pw = PlotWindow(image, pp)
    x, y = pw.getXValues(), pw.getYValues()
    # convert to java double for use in CurveFitter() class
    xd = [Double(i) for i in x]
    yd = [Double(i) for i in y]
    return xd, yd


def readRois(path_):
    RoiManager().close()
    rm = RoiManager()
    if rm is not None:
        rm.getInstance()
    rm.runCommand("Open", path_)
    return rm


def getFWHM(fit):
    sd_pat = re.compile("d = (\d.*)")
    r2_pat = re.compile("R\^2: (\d.*)")
    r2 = float(r2_pat.search(fit).group(1))
    try:
        res = float(sd_pat.search(fit).group(1))  # regex for searching
        fwhm = 2.355 * res
        return fwhm, r2
    except Exception as e:
        print("DIDNT FIND SD! RETURNING string None")
        print(e)
        return "None", "None"


def fit_curve(x, y):
    """fits curve of a single gaussian, returns
	FWHM, R^2, and the fn args.
	"""
    cf = CurveFitter(x, y)
    cf.doFit(12)
    params = cf.getResultString()
    fwhm, r2 = getFWHM(params)
    return fwhm, r2, params


def iter_rois_fwhm(roi_path, img_path):
    roi = readRois(roi_path)
    img = Opener.openUsingBioFormats(img_path)
    title = str(img.getShortTitle())
    all_out = []
    for i in roi.getIndexes():
        roi.select(img, i)
        x, y = getXY(img)
        fwhm, r2, params = fit_curve(x, y)
        all_out.append(
            {
                "fwhm": fwhm,
                "r2": r2,
                "roi_id": i,
                "ch_n": 2,
                "img_name": title,
                "ch_name": "NaV1.6",
            }
        )
    roi.close()
    img.close()
    return all_out


def write_json(dict_, out_dir, suffix):
    base_name = dict_[0]["img_name"] + "_" + suffix + ".json"
    new_path = os.path.join(out_dir, base_name)
    if not os.path.exists(new_path):
        print("\n\nWriting data to {}\n\n").format(new_path)
        with open(new_path, "w") as js:
            json.dump(dict_, js)
    else:
        print("\n\npath {} exists! printing data to stdout instead\n\n").format(
            new_path
        )
        print(data_)


def get_real_files(path_, ending):
    listoffiles = [str(f) for f in os.listdir(path_) if f.endswith(ending)]
    # ignore automatically generated hidden files that start with .
    real_names = [os.path.join(path_, f) for f in listoffiles if not f.startswith(".")]
    return real_names


def make_dir(workingDir):
    """
	make new file directory for data files
	"""
    new_dir = os.path.join(workingDir, "data")
    if os.path.exists(new_dir):
        print("\n[INFO] path {} exists. Returning path name.".format(new_dir))
        return new_dir
    else:
        os.mkdir(new_dir)
        return new_dir


def get_roi_file(nd2_path):
    roi_name = nd2_path.replace(".nd2", "-ROIs.zip")
    assert os.path.exists(roi_name), "ROI file {} for {} not found.".format(
        roi_name, nd2_path
    )
    return roi_name


#### main run ####
# CONSTANTS
INPUT_DIR = "/Users/nick/Dropbox/lab-data/nodes-alac/example-img/"
# get files
FILE_LIST = get_real_files(INPUT_DIR, ".nd2")
OUT_DIR = make_dir(INPUT_DIR)
FULL_LEN = len(FILE_LIST) - 1

for n, nd2 in enumerate(FILE_LIST):
    print("[INFO] Processing file {}\n".format(nd2))
    print("[PROGRESS] File {} out of {}\n".format(n, FULL_LEN))
    roi_file = get_roi_file(nd2)
    out = iter_rois_fwhm(roi_file, nd2)
    json.dumps(out)
    write_json(out, OUT_DIR, "fwhm-ch")


elapsed = time.time() - start_t
print("---\n\n\n---")
print("that took {} seconds".format(elapsed))
print("Done")
