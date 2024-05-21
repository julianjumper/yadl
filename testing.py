import os

import pytest
import subprocess
import filecmp


def parse_yadl(filename):
    test_cfg = {
        "run": "yadl-interpreter " + filename,
        "out": [],
        "file-eq": [],
        "remove": []
    }

    with open(filename, "r") as file:
        for line in file:
            line = line.strip()
            tokens = line.split(sep=" ")

            # only comments are of interest
            if not tokens[0] == "//":
                continue

            # %s is a placeholder for the filename
            for i, token in enumerate(tokens):
                if token == "%s":
                    tokens[i] = filename

            # fill the config by checking for keywords
            # define (alternative) command to run
            if tokens[1] == "RUN:":
                test_cfg["run"] = " ".join(tokens[2:])
            # check output
            elif tokens[1] == "CHECK-OUT:":
                test_cfg["out"].append(" ".join(tokens[2:]))
            # check if two files are equal
            elif tokens[1] == "CHECK-FILE-EQ:":
                test_cfg["file-eq"].append(tokens[2:4])
            # remove a file after the test
            elif tokens[1] == "REMOVE:":
                test_cfg["remove"].append(tokens[2])

    return test_cfg


def run_test(test_cfg):
    result = subprocess.run(test_cfg["run"], capture_output=True, text=True)

    # check output
    output = result.stdout.strip().split("\n")
    assert output == test_cfg["out"]

    # check file equalities
    for files in test_cfg["file-eq"]:
        assert filecmp.cmp(files[0], files[1], shallow=False)

    # remove files
    for file in test_cfg["remove"]:
        os.remove(file)


configurations = []
for file_name in os.listdir("tests"):
    configurations.append(parse_yadl("tests/"+file_name))


@pytest.mark.parametrize("config", configurations)
def test_config(config):
    run_test(config)
