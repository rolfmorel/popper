#!/usr/bin/env bash

archivename="popper-$(git show HEAD --pretty=format:%h --no-patch)"

rsync -ar --exclude ".*" --exclude "__pycache__/" popper popper.py README.md requirements.txt examples "$archivename"

zip -r "$archivename".zip "$archivename"
rm -rf "$archivename"

