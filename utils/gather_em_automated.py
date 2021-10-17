import csv
import os

from openpyxl import load_workbook


# newest data: /Users/nick/Dropbox/lab-data/em-round-2/calibrated-blinded/sorted_KEY.csv # DONE!
# /Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/EM/data/em_set1_2019-11/calibrated_for_analysis/KEY.csv # DONE!
# /Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/EM/data/em_set2_2020-02/blinded/KEY.csv # IP

base_path = "done/"
include_files = [
    os.path.join(base_path, i) for i in os.listdir(bp) if i.endswith("_include.txt")
]


def _fmt_name(name):
    return os.path.split(name)[-1].replace(".tif", "")


def fmt_key(path):
    formatted = []
    with open(path, "r") as f:
        csvread = csv.reader(f)
        for real, blind in csvread:
            formatted.append([_fmt_name(real), _fmt_name(blind)])
    return formatted[1:]


def write_new_key(key_path, data):
    new_path = key_path.replace("KEY", "sorted_KEY")
    with open(new_path, "w") as f:
        csvwrite = csv.writer(f)
        for row in data:
            csvwrite.writerow(row)


def get_needed(include: list, data: dict) -> dict:
    out = {}
    for axon in data.keys():
        if axon in include:
            out[axon] = data[axon]
    return out


def parse_include(path):
    with open(path, "r") as f:
        data = f.read()
    split_up = data.strip("\n").split(",")
    int_list = []
    try:
        for prob_int in split_up:
            int_list.append(int(prob_int))
    except Exception as e:
        print(f"tried to parse '{prob_int}' as an integer, failed with {e}")
        return []
    return list(set(int_list))


def parse_excel(path):
    wb = load_workbook(path)
    sheet1 = wb["Sheet1"]
    axons_measures = {}
    for n, things in enumeratee(sheet1.values):
        axons_measures[n] = things
    wb.close()
    return axons_measures


def get_needed(include: list, data: dict) -> dict:
    out = {}
    for axon in data.keys():
        if axon in include:
            out[axon] = data[axon]
    return out


def read_key(path: string) -> dict:
    with open(path, "r") as f:
        pass


key = {}
