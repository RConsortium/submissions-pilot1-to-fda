# Note to Reviewer
# To rerun the code below, please refer ADRG appendix.
# After required package are installed.
# The path variable needs to be defined by using example code below
#
# nolint start
# path = list(adam = "path/to/esub/analysis/adam/datasets")    	# Modify path to the actual location
# path$output = "."                                             # Output saved in current folder
# nolint end

## ------------------------------------------------------------------------------------------------------------------------------
# Working directory requires write permission
if (file.access(".", 2) != 0) {
  warning(
    "The working directory '", normalizePath("."), "' is not writable.\n",
    "Please change it to a location with write permission."
  )
}


## ----setup, message=FALSE------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# CRAN package, please using install.packages() to install
library(dplyr)
library(haven)
library(r2rtf)
library(emmeans)
library(pilot3)


## ------------------------------------------------------------------------------------------------------------------------------
adsl <- read_xpt(file.path("submission/adam/", "adsl.xpt"))
adlb <- read_xpt(file.path("submission/adam/", "adlbc.xpt"))


## ------------------------------------------------------------------------------------------------------------------------------

itt <- adsl[adsl[["ITTFL"]] == "Y", c("STUDYID", "USUBJID")]

adlb1 <- adlb %>%
  dplyr::right_join(itt, by = c("STUDYID", "USUBJID")) %>%
  subset(TRTPN %in% c(0, 81) & PARAMCD == "GLUC" & !is.na(AVISITN)) %>%
  mutate(TRTPN = ifelse(TRTPN == 0, 99, TRTPN)) # change treatment order for pairwise comparison

## Fit data for linear model
gluc_lmfit <- adlb1 %>%
  filter(AVISITN == 20) %>%
  lm(CHG ~ BASE + TRTPN, data = .)

## Raw summary statistics
t10 <- adlb1 %>%
  filter(AVISITN == 0) %>%
  group_by(TRTPN, TRTP) %>%
  summarise(
    N = n(),
    mean_bl = mean(BASE),
    sd_bl = sd(BASE)
  )

t11 <- adlb1 %>%
  filter(AVISITN == 20, !is.na(CHG), !is.na(BASE)) %>%
  group_by(TRTPN, TRTP) %>%
  summarise(
    N_20 = n(),
    mean_chg = mean(CHG),
    sd_chg = sd(CHG),
    mean = mean(AVAL),
    sd = sd(AVAL)
  )

## Calculate LS mean
t12 <- emmeans(gluc_lmfit, "TRTPN")

## Merge and format data for reporting
apr0ancova1 <- merge(t10, t11) %>%
  merge(t12) %>%
  mutate(emmean_sd = SE * sqrt(df)) %>%
  mutate(
    Trt = c("Xanomeline High Dose", "Placebo"),
    N1 = N,
    Mean1 = pilot3::fmt_est(mean_bl, sd_bl),
    N2 = N_20,
    Mean2 = pilot3::fmt_est(mean, sd),
    N3 = N_20,
    Mean3 = pilot3::fmt_est(mean_chg, sd_chg),
    CI = pilot3::fmt_ci(emmean, lower.CL, upper.CL)
  ) %>%
  select(Trt:CI)

apr0ancova1


## ------------------------------------------------------------------------------------------------------------------------------
t2 <- data.frame(pairs(t12))

## Treatment Comparison
apr0ancova2 <- t2 %>%
  mutate(
    lower = estimate - 1.96 * SE,
    upper = estimate + 1.96 * SE
  ) %>%
  mutate(
    comp = "Xanomeline High Dose vs. Placebo",
    mean = pilot3::fmt_ci(estimate, lower, upper),
    p = pilot3::fmt_pval(p.value)
  ) %>%
  select(comp:p)

apr0ancova2


## ------------------------------------------------------------------------------------------------------------------------------
### Calculate root mean square and save data in output folder
apr0ancova3 <- data.frame(rmse = paste0(
  "Root Mean Squared Error of Change = ",
  formatC(sqrt(mean((gluc_lmfit$residuals)^2)), digits = 2, format = "f", flag = "0")
))

apr0ancova3


## ------------------------------------------------------------------------------------------------------------------------------
tbl_1 <- apr0ancova1 %>%
  rtf_title(
    title = "ANCOVA of Change from Baseline at Week 20"
  ) %>%
  rtf_colheader(
    colheader = " | Baseline{^a} | Week 20 | Change from Baseline",
    col_rel_width = c(4, 3.5, 3.5, 7.5)
  ) %>%
  rtf_colheader(
    colheader = "Treatment | N | Mean (SD) | N | Mean (SD) | N | Mean (SD) | LS Mean (95% CI){^b}",
    col_rel_width = c(4, 1, 2.5, 1, 2.5, 1, 2.5, 4)
  ) %>%
  rtf_body(
    col_rel_width = c(4, 1, 2.5, 1, 2.5, 1, 2.5, 4),
    text_justification = c("l", rep("c", 7)),
    last_row = FALSE
  ) %>%
  rtf_footnote(
    footnote = c(
      "{^a} Table is based on participants who have observable data at Baseline and Week 20.",
      "{^b} Based on an Analysis of covariance (ANCOVA) model with treatment and baseline value as covariates",
      "CI = Confidence Interval, LS = Least Squares, SD = Standard Deviation"
    )
  ) %>%
  rtf_footnote(
    footnote = c(
      paste("Table generated on:", Sys.time())
    ),
    as_table = FALSE,
    text_justification = "c"
  ) %>%
  rtf_source(
    source = "Source: [pilot3: adam-adsl; adlbc]",
    text_justification = "c"
  )


## ------------------------------------------------------------------------------------------------------------------------------
tbl_2 <- apr0ancova2 %>%
  rtf_colheader(
    colheader = "Pairwise Comparison | Difference in LS Mean (95% CI){^b} | p-Value",
    text_justification = c("l", "c", "c"),
    col_rel_width = c(7.5, 7, 4)
  ) %>%
  rtf_body(
    col_rel_width = c(7.5, 7, 4),
    text_justification = c("l", "c", "c"),
    last_row = FALSE
  )


## ------------------------------------------------------------------------------------------------------------------------------
tbl_3 <- apr0ancova3 %>%
  rtf_body(
    as_colheader = FALSE,
    text_justification = "l"
  )


## ------------------------------------------------------------------------------------------------------------------------------
tbl <- list(tbl_1, tbl_2, tbl_3)
tbl %>%
  rtf_encode() %>%
  write_rtf(file.path("submission/output/", "tlf-efficacy-pilot3.rtf"))
