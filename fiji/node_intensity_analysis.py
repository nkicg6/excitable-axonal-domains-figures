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


def getXY(image, channel):
    image.setC(channel)
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
	FWHM
	"""
    cf = CurveFitter(x, y)
    cf.doFit(12)
    params = cf.getResultString()
    fwhm, r2 = getFWHM(params)
    return fwhm, r2, params


def iter_rois_fwhm(roi_path, img_path, channel, ch_key):
    assert (
        channel == 2 or channel == 3
    ), "Only functions on channel 1 or 2, channel {} given".format(channel)
    roi = readRois(roi_path)
    img = Opener.openUsingBioFormats(img_path)
    fwhm_list = []
    r2_list = []
    ch_id_list = []
    roi_list = []
    title_list = []
    full_ch_name = []
    ch_name = ch_key[channel]
    title = img.getShortTitle()
    for i in roi.getIndexes():
        roi.select(img, i)
        x, y = getXY(img, channel)
        fwhm, r2, params = fit_curve(x, y)
        fwhm_list.append(fwhm)
        r2_list.append(r2)
        roi_list.append(i)
        ch_id_list.append(channel)
        title_list.append(title)
        full_ch_name.append(ch_name)
    roi.close()
    img.close()
    return {
        "fwhm": fwhm_list,
        "r2": r2_list,
        "roi_id": roi_list,
        "ch_id": ch_id_list,
        "img_name": title_list,
        "ch_name": full_ch_name,
    }


def iter_rois_caspr(roi_path, img_path, ch_key):
    """ NOT WORKING
	bad json encoding during write, likely a problem using Java Double
	which is returned by getXY. """
    channel = 3
    roi = readRois(roi_path)
    img = IJ.openImage(img_path)
    ch_id_list = []
    roi_list = []
    title_list = []
    full_ch_name = []
    ch_name = ch_key[channel]
    X = []
    Y = []
    title = img.getShortTitle()
    for i in roi.getIndexes():
        roi.select(img, i)
        x, y = getXY(img, channel)
        X = X + x
        Y = Y + y
        roi_list = roi_list + [i] * len(x)
        ch_id_list = ch_id_list + [channel] * len(x)
        title_list = title_list + [title] * len(x)
        full_ch_name = full_ch_name + [ch_name] * len(x)
    roi.close()
    img.close()
    return {
        "X": X,
        "Y": Y,
        "roi_id": roi_list,
        "ch_id": ch_id_list,
        "img_name": title_list,
        "ch_name": full_ch_name,
    }


def merge_similar_dicts(d1, d2):
    new = {}
    for k in d1.keys():
        if k in d2.keys():
            newl = d1[k] + d2[k]
            new[k] = newl
        else:
            new[k] = d1[k]
    return new


def run_two_channel(roi_path, img_path, channel_key):
    c1 = iter_rois_fwhm(roi_path, img_path, 2, channel_key)
    c2 = iter_rois_fwhm(roi_path, img_path, 3, channel_key)
    new = merge_similar_dicts(c1, c2)
    return new


def write_json(dict_, out_dir, suffix):
    base_name = dict_["img_name"][0] + "_" + suffix + ".json"
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


print("starting")


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
    roi_name = nd2_path.replace(".nd2", "-rois.zip")
    assert os.path.exists(roi_name), "ROI file {} for {} not found.".format(
        roi_name, nd2_path
    )
    return roi_name


#### main run ####
# CONSTANTS
CHANNEL_KEY = {2: "AnkyrinG", 3: "Nav1.6", 4: "Caspr"}
input_dir = "/Users/nick/Desktop/node_analysis/blinded/"
# get files
f_list = get_real_files(input_dir, ".nd2")
output_dir = make_dir(input_dir)
full_len = len(f_list)

for nd2 in f_list:
    ind = f_list.index(nd2)
    print("[INFO] Processing file {}\n".format(nd2))
    print("[PROGRESS] File {} out of {}\n".format(ind, full_len))
    roi_file = get_roi_file(nd2)
    two_ch = run_two_channel(roi_file, nd2, CHANNEL_KEY)
    write_json(two_ch, output_dir, "fwhm-ch")


elapsed = time.time() - start_t
print("---\n\n\n---")
print("that took {} seconds".format(elapsed))
print("Done")
# that took 1725.39300013 seconds


### testing ###
testimg = "/Users/nick/Dropbox/lab-data/ALAC nodes all zip files"
testrois = "/Users/nick/Desktop/node_analysis/blinded/testing_script/testimg-rois.zip"

roi = readRois(testrois)
img = IJ.openImage(testimg)
# roi.select(img, 1)

# x,y = getXY(img, 3)
# fwhm,params = fit_curve(x,y)
### main run
# c1 = iter_rois_fwhm(testrois, testimg, 1, CHANNEL_KEY)
# c2 = iter_rois_fwhm(testrois, testimg, 1, CHANNEL_KEY)
