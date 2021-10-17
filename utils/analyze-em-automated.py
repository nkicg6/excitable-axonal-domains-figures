import csv
import json
import os

from openpyxl import load_workbook


def filter_files(base: str, end: str) -> list:
    include_files = [os.path.join(base, i) for i in os.listdir(base) if i.endswith(end)]
    return include_files


def key_to_dict(path: str) -> dict:
    formatted = {}
    with open(path, mode="r", encoding="utf-8-sig") as f:
        csvread = csv.reader(f)
        for real, blind, _ in csvread:
            formatted[blind] = real
    return formatted


def read_json_key(path: str) -> dict:
    out_dict = {}
    with open(path, "r") as f:
        data = json.load(f)
    for entry in data:
        out_dict[entry["animal"]] = entry
    return out_dict


def merge_metadata(main_key: dict, animal_key: dict, include_dict: dict) -> dict:
    all_data = {}
    for key in main_key.keys():
        target = main_key[key].replace("R", "").replace("L", "").split("_")[0]
        all_data[key] = animal_key[target]
        all_data[key]["animal"] = main_key[key]
        try:
            all_data[key]["include"] = include_dict[key]
        except KeyError as e:
            print(f"Key '{key}' does not exist in include_dict. Deleting key.")
            all_data.pop(key, None)
    return all_data


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


def make_include_map(list_of_includes: list) -> dict:
    out = {}
    for path in list_of_includes:
        base_name = os.path.split(path)[-1].replace("_include.txt", "")
        out[base_name] = parse_include(path)
    return out


def return_full_path(target_string: str, path_list: list) -> str:
    for full_path in path_list:
        if target_string in full_path:
            return full_path
    print(f"FULL PATH FOR {target_string} NOT PRESENT")
    return None


def parse_excel(path: str) -> dict:
    if not path:
        return None
    wb = load_workbook(path)
    sheet1 = wb["Sheet1"]
    axons_measures = {}
    for n, things in enumerate(sheet1.values):
        axons_measures[n] = things
    wb.close()
    return axons_measures


base_path = "/Users/nick/Dropbox/lab_notebook/projects_and_data/mnc/analysis_and_data/EM/data/em-all-automated/"
blind_key = key_to_dict(base_path + "sorted-key-main-merged.csv")
animal_key = read_json_key(base_path + "animal_info.json")
include_paths = filter_files(base_path, "_include.txt")
include_map = make_include_map(include_paths)
xlsx_paths = filter_files(base_path, ".xlsx")
merged_metadata = merge_metadata(blind_key, animal_key, include_map)


def make_excel_map(excel_row):
    return {
        "gratio": excel_row[3],
        "axon_area": excel_row[4],
        "axon_perimeter": excel_row[5],
        "myelin_area": excel_row[6],
        "axon_diam": excel_row[7],
        "myelin_thickness": excel_row[8],
        "axonmyelin_area": excel_row[9],
        "axonmyelin_perimeter": excel_row[10],
        "solidity": excel_row[11],
    }


real_header = (
    None,
    "x0",
    "y0",
    "gratio",
    "axon_area",
    "axon_perimeter",
    "myelin_area",
    "axon_diam",
    "myelin_thickness",
    "axonmyelin_area",
    "axonmyelin_perimeter",
    "solidity",
    "eccentricity",
    "orientation",
)


def test_excel_map(real_header):
    excel_map = make_excel_map(real_header)
    for item in excel_map.keys():
        assert item == excel_map[item]
    print("PASS")
    return


def side_from_name(name: str) -> str:
    remove_stuff = name.split("_")[0]
    if remove_stuff.lower().endswith("r"):
        return "Right"
    if remove_stuff.lower().endswith("l"):
        return "Left"
    raise AssertionError(f"Couldn't identify side for name '{name}'!")


#### Example protocol ####
measurements = []
current_key = "7f74f87d54564ca684bdcd1eb7a9b477"
target_xlsx = return_full_path(current_key, xlsx_paths)
excel_data = parse_excel(target_xlsx)
for ind in merged_metadata[current_key]["include"]:
    row = make_excel_map(excel_data[ind])
    row["treatment"] = merged_metadata[current_key]["treatment"]
    row["sex"] = merged_metadata[current_key]["sex"]
    row["animal"] = merged_metadata[current_key]["animal"]
    row["side"] = side_from_name(row["animal"])
    measurements.append(row)
