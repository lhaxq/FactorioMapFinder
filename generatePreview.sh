#!/bin/sh

../Standalone/factorio.app/Contents/MacOS/factorio -c ../Standalone/config/config.ini --map-settings map-settings.json --map-gen-settings map-gen-settings.json --generate-map-preview phase2-previews-detailed/$1.png --map-gen-seed $1 --map-preview-size $2
