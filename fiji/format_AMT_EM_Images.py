# process AMTCamera system images by cropping out the info bar, and adding calibration to the image metadata. 
# Author: Nick George
# Contact: nicholas.m.george@cuanschutz.edu
# License: GNU GPL v3 or later
#@ File (label="Choose the image directory to calibrate", style="directory") directory
#@ String (label="What is the file ending of your image files?", choices={".tif",".jpg",".jpeg",".png", ".pdf", ".czi", ".lsm", ".nd2", ".lif", ".oib"}, style="listBox") fileend

from ij import IJ
from ij import ImagePlus
from ij.measure import Calibration
import os
import re
import shutil


def make_dir(folder):
	"""
	make new file directory for extracted tifs
	if folder exists, just return the path.
	"""
	new_dir = os.path.join(folder, "calibrated")
	if os.path.exists(new_dir):
		return new_dir
	else:
		os.mkdir(new_dir)
		return new_dir

def save_file(imageplus, uncalibratedpath):
	oldparts = os.path.split(uncalibratedpath)
	calibratedpath = os.path.join(oldparts[0], "calibrated",oldparts[-1].replace(" ", "_"))
	if os.path.exists(calibratedpath):
		message = str("\n >> *file:* " + calibratedpath + " exists, continuing")
		IJ.log(message)
		print(message)
		return
	else:
		message = str("\n >> *uncalibrated file* \n --> "+ uncalibratedpath+ "\n >> *saving as* \n --> " + calibratedpath)
		print(message)
		IJ.log(message)
		IJ.saveAs(imageplus, "Tiff", calibratedpath)


def get_calibration(imageplus):
    #old_str = "XpixCal=(?P<xpix>\d+?\.\d+).YpixCal=(?P<ypix>\d+?\.\d+).Unit=(?P<unit>\w\w)"
    regex_string = re.compile(r"ImageDescription: (?P<id>AMT Camera System).+XpixCal=(?P<xpix>\d+?\.\d+).YpixCal=(?P<ypix>\d+?\.\d+).Unit=(?P<unit>\w\w)")
    img_metadata = imageplus.getProperties().getProperty("Info")
    grouped = regex_string.search(img_metadata)
    assert grouped.group("xpix") == grouped.group("ypix"), "Units are not equal in x and y direction, cannot set the scale. x-pixels = {}, y-pixels = {}".format(grouped.group("xpix"), grouped.group("ypix"))
    if grouped.group("id") != "AMT Camera System":
        print("Did not detect AMT Camera Systems signature in tif. Exiting...")
        sys.exit()
    metadata_dict = {"xpix": float(grouped.group("xpix")),
                     "ypix": float(grouped.group("ypix")),
                     "unit": grouped.group("unit")}
    return metadata_dict

def set_calibration_obj(metadata_dict):
    cal = Calibration()
    cal.setUnit(metadata_dict["unit"])
    cal.pixelWidth = 1/metadata_dict["xpix"]
    cal.pixelHeight = 1/metadata_dict["ypix"]
    return cal

def format_name(imageplus, path_to_image):
    formatted_title = imgeplus.getTitle().replace(" ", "_")
    new_path = os.path.join(path_to_image, "calibrated", formatted_title)
    return new_path

def apply_calibration(imageplus):
    meta_dict = get_calibration(imageplus)
    cal_obj = set_calibration_obj(meta_dict)
    imageplus.setCalibration(cal_obj)
    #return imageplus # java obj is mutable, no need to return. 

def get_files(folder, fileend):
    listoffiles = [f for f in os.listdir(folder) if f.endswith(fileend)]
    # ignore hidden files starting with .
    return [os.path.join(folder,f) for f in listoffiles if not f.startswith(".")]

def make_roi(imageplus):
    x=0
    y=0
    width = img.getWidth()
    computed_dif = img.getHeight() - img.getWidth()
    height = img.getHeight()- computed_dif
    img.setRoi(x,y,width, height)
    

def crop_roi(img):
    new = img.crop()
    return new

########### script begin ############# 

folder = str(directory)
assert os.path.exists(folder), "Couldnt find the directory. Try using 'Browse' next time"

files_ = get_files(folder, fileend)
new_dir = make_dir(folder)


for uncalibrated in files_:
    img = ImagePlus(uncalibrated)
    print("old calibration: {}".format(img.getCalibration()))
    apply_calibration(img)
    # make ROI 
    make_roi(img)
    print("cropping image")
    # crop to ROI 
    new = crop_roi(img)
    print("new calibration: {}".format(new.getCalibration()))
    save_file(new, uncalibrated)
    img.close()
    new.close()

"""
# testing 
img = ImagePlus(files_[0])
img.show()
make_roi(img)
new = crop_roi(img)
new.show()
"""
"""
regex_string = re.compile(r"ImageDescription: (?P<id>AMT Camera System).+XpixCal=(?P<xpix>\d+?\.\d+).YpixCal=(?P<ypix>\d+?\.\d+).Unit=(?P<unit>\w\w)")
img_metadata = img.getProperties().getProperty("Info")
print(img_metadata)
grouped = regex_string.search(img_metadata)
try:
    print(grouped.group("id"))
    print(grouped.group("xpix"))
    print(grouped.group("ypix"))
    print(grouped.group("unit"))
    assert grouped.group("id") == "AMT Camera System"
except:
    print(grouped)
"""
print("DONE")