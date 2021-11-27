ak = {
    "133598F3": {
        "animal": "133598F3",
        "sex": "female",
        "cage_card": 133598,
        "treatment": "occl",
    },
    "NG002": {
        "animal": "NG002",
        "sex": "male",
        "cage_card": 188956,
        "treatment": "occl",
    },
    "NG001": {
        "animal": "NG001",
        "sex": "male",
        "cage_card": 188956,
        "treatment": "sham",
    },
}

bk = {
    "5656544307564645919f5244af06e70c": "133598F3L_06",
    "467ab002424d416cb668f066ed514930": "NG002L_02",
    "227e46b8dab44b2b9c6cb61ebad9494b": "NG001L_05",
}

want = {
    "5656544307564645919f5244af06e70c": {
        "animal": "133598F3L_06",
        "sex": "female",
        "cage_card": 133598,
        "treatment": "occl",
    },
    "467ab002424d416cb668f066ed514930": {
        "animal": "NG002L_02",
        "sex": "male",
        "cage_card": 188956,
        "treatment": "occl",
    },
    "227e46b8dab44b2b9c6cb61ebad9494b": {
        "animal": "NG001L_05",
        "sex": "male",
        "cage_card": 188956,
        "treatment": "sham",
    },
}

#### Example tests to be run interactively ####

# def test_match():
#     got = match(bk, ak)
#     assert got == want


# def test_all_things():
#     all_ = match(blind_key, animal_key)
#     for thing in all_.keys():
#         animal_target = all_[thing]["animal"]
#         try:
#             assert animal_target == blind_key[thing]
#         except Exception as e:
#             print(f"problem with {thing}")
#             print(f"Merged is '{animal_target}', Actual is '{blind_key[thing]}")
