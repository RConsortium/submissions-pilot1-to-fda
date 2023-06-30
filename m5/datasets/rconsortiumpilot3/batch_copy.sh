#!/bin/bash

cd ./m5/datasets/rconsortiumpilot3

git clone https://github.com/RConsortium/submissions-pilot3-adam.git --branch main --single-branch ./submission-pilot3-adam --depth 1

mv ./submission-pilot3-adam/submission/sdtm/*.xpt ./submission-pilot3-adam/submission/sdtm/define.* ./tabulations/sdtm
mv ./submission-pilot3-adam/submission/adam/*.xpt ./submission-pilot3-adam/submission/adam/define* './submission-pilot3-adam/submission/adam/ADaM - Pilot 3.xlsx' ./analysis/adam
mv ./submission-pilot3-adam/renv.lock ./submission-pilot3-adam/submission/programs/ad*.r ./submission-pilot3-adam/submission/programs/tlf*.r ./analysis/programs
mv ./submission-pilot3-adam/submission/output/* ./analysis/output

rm -rf ./submission-pilot3-adam
