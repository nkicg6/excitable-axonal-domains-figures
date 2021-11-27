from ij import IJ
from ij import ImagePlus
from ij import WindowManager
from ij.plugin.frame import RoiManager
import os
import shutil
import csv

blinded_base = "/Users/nick/Desktop/em-round-2/calibrated-blinded" 
data = "/Users/nick/Desktop/em-round-2/done"

img = WindowManager.getCurrentImage()
image_title = img.getShortTitle()

# make paths for moving when done.
full_img_path_orig = os.path.join(blinded_base, image_title+".tif")
full_img_path_done = os.path.join(data, image_title+".tif")

# get roi manager
roim = RoiManager().getRoiManager()

# make savepaths for csv and rois
roimsave = os.path.join(data, image_title + "_rois.zip")
csvsave = os.path.join(data, image_title + "_data.csv")

csv_ = [["image", "roi_name", "uncalibrated_length"]]

for roi,_ in enumerate(roim.getRoisAsArray()):
    target = roim.getRoi(roi)
    csv_.append([image_title, target.getName(), target.getLength()])

# write and save csv
with open(csvsave, "w") as c:
    writer = csv.writer(c)
    for l in csv_:
        writer.writerow(l)

# save rois
roim.runCommand("Deselect")
roim.runCommand("Save", roimsave)

# close both
roim.close()
img.close()

# move the image to done directory
shutil.move(full_img_path_orig, full_img_path_done)

l = sorted([os.path.join(blinded_base, f) for f in os.listdir(blinded_base) if f.endswith(".tif")])
remaining = len(l)

# prep the next image.
if remaining != 0:
    new = ImagePlus(l[0])
    new.show()
    print("{} images left to analyze".format(remaining -1))
else:
    print("No images left!")

print("Done")
