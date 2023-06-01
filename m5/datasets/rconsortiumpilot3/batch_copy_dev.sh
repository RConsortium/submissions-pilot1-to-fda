#!/bin/bash

cd /cloud/project/m5/datasets/rconsortiumpilot3

git clone https://github.com/RConsortium/submissions-pilot3-adam.git --branch batch_copy_test --single-branch ./submission-pilot3-adam --depth 1

mv ./submission-pilot3-adam/submission/sdtm/*.xpt ./tabulations/sdtm
mv ./submission-pilot3-adam/submission/adam/*.xpt ./analysis/adam
mv ./submission-pilot3-adam/submission/programs/ad*.R ./submission-pilot3-adam/submission/programs/tlf*.R ./analysis/programs
mv ./submission-pilot3-adam/submission/output/* ./analysis/output

rm -rf ./submission-pilot3-adam
