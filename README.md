> Do not includ `README.md` and `.gitignore` files into the final submission. 

## Overview

The objective of the R Consortium R submission Pilot 1 Project is to 
test the concept that a R-language based submission package can meet 
the needs and the expectations of the FDA reviewers, 
including assessing code review and analyses reproducibility. 
All submission materials and communications from this pilot are publicly available, 
with the aim of providing a working example for future R language based FDA submissions.
This is a FDA-industry collaboration through the non-profit organization R consortium.

The [RConsortium/submissions-pilot1-to-fda](https://github.com/RConsortium/submissions-pilot1-to-fda)
repo demonstrates the eCTD submission package based on the [RConsortium/submissions-pilot1](https://github.com/RConsortium/submissions-pilot1) repo.  

The [RConsortium/submissions-pilot1](https://github.com/RConsortium/submissions-pilot1) repo demonstrates an approach to organize internal developed R function and 
table, listing, figure generation program using an R package. 

## Re-run analysis 

To re-run analysis, you can follow the steps described in the 
[ADRG document](https://github.com/RConsortium/submissions-pilot1-to-fda/blob/main/m5/datasets/rconsortiumpilot1/analysis/adam/datasets/adrg.pdf) based on the 
[program saved in the module 5](https://github.com/RConsortium/submissions-pilot1-to-fda/tree/main/m5/datasets/rconsortiumpilot1/analysis/adam/programs). 

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
    └── rconsortiumpilot1
        └── analysis
            └── adam
                ├── datasets              # ADaM datasets in XPT format
                │   ├── adadas.xpt
                │   ├── adae.xpt
                │   ├── adcibc.xpt
                │   ├── adlbc.xpt
                │   ├── adlbcpv.xpt
                │   ├── adlbh.xpt
                │   ├── adlbhpv.xpt
                │   ├── adlbhy.xpt
                │   ├── adnpix.xpt
                │   ├── adrg.pdf          # Analysis Data Reviewer's Guide
                │   ├── adsl.xpt
                │   ├── adtte.xpt
                │   ├── advs.xpt
                │   ├── define.xml        # ADaM data define file
                │   └── define2-0-0.xsl
                └── programs
                    ├── r0pkg.txt         # Proprietary R package in txt format
                    ├── tlf-demographic.r # analysis R code for TLFs. 
                    ├── tlf-efficacy.r
                    ├── tlf-kmplot.r
                    └── tlf-primary.r
```
Other files: (**Do not include in eCTD package**)

- `.gitignore`: git ignore file
- `README.md`: readme file for github repo

## News

- [Successful R-based Test Package Submitted to FDA](https://www.r-consortium.org/blog/2021/12/08/successful-r-based-test-package-submitted-to-fda)

## Questions 

Report issues in <https://github.com/RConsortium/submissions-pilot1-to-fda/issues>

