"""renames and organizes files from A1R multipoint experiments."""
import argparse
import functools
import os
import shutil


def make_dest_dir(cwd: str) -> str:
    """makes experiment dir if one does not exist"""
    new_dir = os.path.join(cwd, "experiment")
    if not os.path.exists(new_dir):
        os.mkdir(new_dir)
    if os.path.exists(new_dir):
        return new_dir


def get_files(folder: str, fend: str) -> list:
    """gathers files ending with fend"""
    files_in_dir = [
        os.path.join(folder, i) for i in os.listdir(folder) if i.endswith(fend)
    ]
    return sorted(files_in_dir)


def make_new_name(path: str, new_dest_dir: str) -> tuple:
    """makes new names for files based on old names/paths"""
    assert os.path.exists(path), f"path {path} does not exist."
    base, name = os.path.split(path)
    _, n_base = os.path.split(base)
    new_name = n_base + "_" + name
    return (path, os.path.join(new_dest_dir, new_name))


def move_files(src: str, dst: str, dry_run: bool = True) -> None:
    """moves files and prints progress. Just prints in dry_run"""
    if not dry_run:
        shutil.move(src, dst)
        print(f"moving {src} to {dst}")
        return
    if dry_run:
        print(f"DRY RUN moving {src} to {dst}")
        return


def main(dry, target):
    """script runner"""
    new_dest_dir = make_dest_dir(target)
    dirs_here = [
        os.path.join(target, item)
        for item in os.listdir(target)
        if os.path.isdir(os.path.join(target, item))
    ]
    print(dirs_here)
    for directory in dirs_here:
        d_files = get_files(directory, ".nd2")
        old_and_new = list(
            map(functools.partial(make_new_name, new_dest_dir=new_dest_dir), d_files)
        )
        for pair in old_and_new:
            move_files(pair[0], pair[1], dry_run=dry)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="rename files based on parent directory and image name"
    )
    parser.add_argument("--dry-run", action="store_true", dest="dry_run")
    parser.add_argument("-d", dest="target")
    arguments = parser.parse_args()
    main(arguments.dry_run, arguments.target)
    print("DONE")
