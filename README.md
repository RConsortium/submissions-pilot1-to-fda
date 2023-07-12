> Do not include `README.md` and `.gitignore` files into the final submission. 

## Overview

The objective of the R Consortium R Submission Pilot 3 Project is to 
test the concept that an R-language based submission package can meet 
the needs and the expectations of the FDA reviewers, 
including assessing code review and analyses reproducibility. 
All submission materials and communications from this pilot are publicly available, 
with the aim of providing a working example for future R-language based FDA submissions.
This is an FDA-industry collaboration through the non-profit organisation R Consortium.

The [RConsortium/submissions-pilot3-adam-to-fda](https://github.com/RConsortium/submissions-pilot3-adam-to-fda)
repo demonstrates the eCTD submission package based on the [RConsortium/submissions-pilot3-adam](https://github.com/RConsortium/submissions-pilot3-adam) repo.  

The [RConsortium/submissions-pilot3-adam](https://github.com/RConsortium/submissions-pilot3-adam) repo demonstrates an approach to organise an R-language based submission, including ADaMs, tables and figures. This is supported by internally-developed R functions, contained within an R package in the [RConsortium/submissions-pilot3-utilities](https://github.com/RConsortium/submissions-pilot3-utilities) repo.

To learn more about other pilots, visit the [R Consortium R Submission Working Group website](https://rconsortium.github.io/submissions-wg/) and the [R Consortium Working Groups webpage](https://www.r-consortium.org/all-projects/isc-working-groups).

## FDA Response 

- Initial submission
  + version: [v0.1.0] (**link TBC**)
  + [Cover letter](https://github.com/RConsortium/submissions-pilot3-adam-to-fda/blob/main/m1/us/cover-letter.pdf) (**Draft version**)
  
## Re-run analysis 

To re-run analysis, you can follow the steps described in the 
[ADRG] (**link TBC**) based on the 
[programs saved in module 5](https://github.com/RConsortium/submissions-pilot3-adam-to-fda/tree/main/m5/datasets/rconsortiumpilot1/analysis/adam/programs). 

## Folder Structure 

The folder is organized as a demo eCTD package following ICH guidance. 

eCTD package: 

- `m1/`: module 1 of the eCTD package

```
m1
└── us
    ├── cover-letter.pdf  # Submission cover letter
    └── report-tlf.pdf    # Submission TLFs 
```

> Note: the TLF format in `report-tlf.pdf` is not consistent, 
> because it reflects different table layouts in different organizations. 

- `m5/`: module 5 of the eCTD package

```
m5
└── datasets
    └── rconsortiumpilot3
        ├── tabulations
        |   ├── sdtm
        |   │   ├── blankcrf.pdf
        |   │   ├── define-v1-updated-html.xsl
        |   │   ├── define.pdf
        |   │   ├── define.xml
        |   │   ├── pinnacle21-report-2023-05-19T10-33-03-557.xlsx
        |   │   ├── ae.xpt                      # SDTM datasets in XPT format
        |   │   ├── cm.xpt
        |   │   ├── dm.xpt
        |   │   ├── ds.xpt
        |   │   ├── ex.xpt
        |   │   ├── lb.xpt
        |   │   ├── mh.xpt
        |   │   ├── qs.xpt
        |   │   ├── relrec.xpt
        |   │   ├── sc.xpt
        |   │   ├── se.xpt
        |   │   ├── suppae.xpt
        |   │   ├── suppdm.xpt
        |   │   ├── suppds.xpt
        |   │   ├── supplb.xpt
        |   │   ├── sv.xpt
        |   │   ├── ta.xpt
        |   │   ├── te.xpt
        |   │   ├── ti.xpt
        |   │   ├── ts.xpt
        |   │   ├── tv.xpt
        |   │   └── vs.xpt
        └── analysis
            ├── adam
            |   ├── ADaM - Pilot 3.xlsx
            |   ├── define.xml
            │   ├── adadas.xpt                    # ADaM datasets in XPT format
            │   ├── adae.xpt
            │   ├── adlbc.xpt
            │   ├── adsl.xpt
            │   ├── adtte.xpt
            │   └── define2-0-0.xsl
            ├── output
            │   ├── tlf-demographic-pilot3.out    # output files for TLFs. 
            │   ├── tlf-efficacy-pilot3.rtf
            │   ├── tlf-kmplot-pilot3.pdf
            │   └── tlf-primary-pilot3.rtf
            └── programs
                ├── adadas.R                      # analysis R code for TLFs.   
                ├── adae.R
                ├── adlbc.R
                ├── adsl.R
                ├── adtte.R
                ├── tlf-demographic.R
                ├── tlf-efficacy.R
                ├── tlf-kmplot.R
                ├── tlf-primary.R
                └── renv.lock
```
Other files: (**Do not include in eCTD package**)

- `.gitignore`: git ignore file
- `README.md`: readme file for github repo

## News

## Questions 

Report issues in <https://github.com/RConsortium/submissions-pilot3-adam-to-fda/issues>.
