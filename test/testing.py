import os

import pytest
import subprocess
import filecmp
from pathlib import Path

DEFAULT_RUN_COMMAND = "yadl-interpreter %s"


def parse_comment(comment_line: str):
    if not comment_line.startswith("//"):
        raise SyntaxError("Expected commented line")
    if not comment_line.startswith("// "):
        raise SyntaxError("Expected space after slashes")
    comment_line = comment_line.split("// ")[1:].join("// ")
    comment_line.split(":")
    if len(comment_line) < 2:
        raise SyntaxError("Expected command")
    cmd = comment_line[0]
    comment_line.join(":")

    if not comment_line.startswith(" "):
        raise SyntaxError("Expected Space after colon")

    return {"cmd": cmd, "payload": comment_line}


def parse_yadl(filepath):
    test_cfg = {
        "run": "yadl-interpreter " + filepath,
        "out": [],
        "file-eq": [],
        "remove": [],
    }

    with open(filepath, "r") as file:
        lines = file.readlines()

        if len(lines) == 0:
            raise SyntaxError("Expected non empty yadl file")

        for line in lines:
            line = line.strip()
            tokens = line.split(sep=" ")

            # only comments are of interest
            if not tokens[0] == "//":
                continue

            # %s is a placeholder for the filename
            for i, token in enumerate(tokens):
                if token == "%s":
                    tokens[i] = filepath

            # fill the config by checking for keywords
            # define (alternative) command to run
            if tokens[1] == "RUN:":
                assert (
                    "run" not in test_cfg
                ), f'RUN command found multiple times in file "{filepath}"'

                if " ".join(tokens[2]) == "DEFAULT":
                    test_cfg["run"] = DEFAULT_RUN_COMMAND.replace("%s", filepath)
                else:
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
    # assert False, test_cfg['run']
    result = subprocess.run(test_cfg["run"], capture_output=True, shell=True, text=True)

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
TEST_DIR = os.path.abspath("test/tests")
for posix_path in Path(TEST_DIR).rglob("*.yadl"):
    full_path = os.path.join(os.path.dirname(TEST_DIR), posix_path)
    configurations.append(parse_yadl(str(full_path)))


@pytest.mark.parametrize("config", configurations)
def test_config(config):
    run_test(config)
