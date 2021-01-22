# script runner
# import ij.plugin
# ij.plugin.Macro_Runner().run("/Users/nick/personal_projects/thesis/thesis_fiji_scripts/measure/th_intensity.py");
import csv
import os
import shutil

# java class imports
from ij import IJ
from ij import ImagePlus
from ij.plugin.frame import RoiManager

# constant vars
target_dir = "/Users/nick/Desktop/th-analysis/all_th/blinded"
out_dir = "/Users/nick/Desktop/th-analysis/all_th/blinded/done"


def get_stats(channel, roi, image):
    image.setC(channel)
    image.setRoi(roi)
    stats = image.getStatistics()
    output = {
        "title": image.getShortTitle(),
        "channel": channel,
        "roi": roi.getName(),
        "mean_pixel": stats.mean,
        "area": stats.area,
        "pixel_height": image.getCalibration().pixelHeight,
        "pixel_unit": image.getCalibration().getUnit(),
    }
    return output


def make_path(base, imagename, fileend):
    fend = imagename + fileend
    newpath = os.path.join(base, fend)
    # assert not os.path.exists(newpath)
    return newpath


def move_file(old, new):
    print("moving {} to {}".format(old, new))
    shutil.move(old, new)


def save_rois(roi_instance, out_path):
    roi_instance.runCommand("Deselect")
    roi_instance.runCommand("Save", out_path)
    print("saving rois to {}".format(out_path))


def write_csv(write_list, out_path):
    with open(out_path, "w") as thing:
        writer = csv.DictWriter(
            thing,
            fieldnames=[
                "title",
                "roi",
                "channel",
                "mean_pixel",
                "area",
                "pixel_height",
                "pixel_unit",
            ],
        )
        writer.writeheader()
        writer.writerows(current)
    print("wrote results to {}".format(out_path))


# imp = IJ.openImage(th_files[0])
# imp.show()

# get open image
imp = IJ.getImage()
image_name = imp.getShortTitle()

# get roi manager
roi_manager = RoiManager().getInstance()
all_rois = roi_manager.getRoisAsArray()

csv_name = make_path(out_dir, image_name, ".csv")
roi_name = make_path(out_dir, image_name, "_rois.zip")
old_name = make_path(target_dir, image_name, ".tif")
new_name = make_path(out_dir, image_name, ".tif")

current = []
for roi in all_rois:
    res = get_stats(2, roi, imp)
    current.append(res)

# write csv
write_csv(current, csv_name)
# save all rois
save_rois(roi_manager, roi_name)
roi_manager.close()
# move image open next
move_file(old_name, new_name)
imp.changes = False  # prevent the "are you sure? dialogue"
imp.close()

th_files = sorted(
    [os.path.join(target_dir, i) for i in os.listdir(target_dir) if i.endswith(".tif")]
)
if th_files:
    print("{} files left to analyze!".format(len(th_files)))
    print("opening {}".format(th_files[0]))
    next_image = IJ.openImage(th_files[0])
    next_image.show()
else:
    print("you are done!")
