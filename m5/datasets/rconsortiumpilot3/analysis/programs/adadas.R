###########################################################################
#' developers : Kangjie Zhang
#' date: 29NOV2022
#' modification History:
#' program ADADAS
###########################################################################

## setup
library(dplyr)
library(tidyr)
library(admiral)
library(metacore)
library(metatools)
library(stringr)
library(xportr)
library(pilot3)

dm <- haven::read_xpt(file.path("submission/sdtm", "dm.xpt"))
qs <- haven::read_xpt(file.path("submission/sdtm", "qs.xpt"))
adsl <- haven::read_xpt(file.path("submission", "adam", "adsl.xpt"))

dm <- convert_blanks_to_na(dm)
qs <- convert_blanks_to_na(qs)
adsl <- convert_blanks_to_na(adsl)


## origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("adam/TDF_ADaM - Pilot 3 Team updated.xlsx", where_sep_sheet = FALSE)
# Get the specifications for the dataset we are currently building
adadas_spec <- metacore %>%
  select_dataset("ADADAS")
# Pull together all the predecessor variables
adadas_pred <- build_from_derived(adadas_spec,
  ds_list = list("ADSL" = adsl, "QS" = qs, "DM" = dm)
)

## ADT/ADY
adas1 <- adadas_pred %>%
  derive_vars_merged(
    dataset_add = qs,
    new_vars = exprs(QSDTC, QSSTRESN, QSTEST), # Get QS vars required for derivations
    by_vars = exprs(STUDYID, USUBJID, QSSEQ)
  ) %>%
  # subset to interested PARAMCD(QSTESTCD)
  filter(PARAMCD %in%
    c(str_c("ACITM", str_pad(1:14, 2, pad = "0")), "ACTOT")) %>%
  # ADT
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = QSDTC
  ) %>%
  # ADY
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

## mutate AVISIT/AVAL/PARAM, assign AVISITN/PARAMN based on codelist from define
adas2 <- adas1 %>%
  mutate(
    AVISIT = case_when(
      ADY <= 1 ~ "Baseline",
      ADY >= 2 & ADY <= 84 ~ "Week 8",
      ADY >= 85 & ADY <= 140 ~ "Week 16",
      ADY > 140 ~ "Week 24",
      TRUE ~ NA_character_
    ),
    AVAL = QSSTRESN,
    PARAM = QSTEST %>% str_to_title()
  ) %>%
  create_var_from_codelist(adadas_spec, AVISIT, AVISITN) %>% # derive AVISITN from codelist
  create_var_from_codelist(adadas_spec, PARAM, PARAMN) # derive PARAMN from codelist

# derive PARAMCD=ACTOT, DTYPE=LOCF
# A dataset with combinations of PARAMCD, AVISIT which are expected.
actot_expected_obsv <- tibble::tribble(
  ~PARAMCD, ~AVISITN, ~AVISIT,
  "ACTOT", 0, "Baseline",
  "ACTOT", 8, "Week 8",
  "ACTOT", 16, "Week 16",
  "ACTOT", 24, "Week 24"
)

adas_locf <- derive_locf_records(
  data = adas2,
  dataset_expected_obs = actot_expected_obsv,
  by_vars = exprs(
    STUDYID, SITEID, SITEGR1, USUBJID, TRTSDT, TRTEDT,
    TRTP, TRTPN, AGE, AGEGR1, AGEGR1N, RACE, RACEN, SEX,
    ITTFL, EFFFL, COMP24FL, PARAMCD
  ),
  order = exprs(AVISITN, AVISIT),
  keep_vars = exprs(VISIT, VISITNUM, ADY, ADT, PARAM, PARAMN, QSSEQ)
)

## derive AWRANGE/AWTARGET/AWTDIFF/AWLO/AWHI/AWU
aw_lookup <- tribble(
  ~AVISIT, ~AWRANGE, ~AWTARGET, ~AWLO, ~AWHI,
  "Baseline", "<=1", 1, NA_integer_, 1,
  "Week 8", "2-84", 56, 2, 84,
  "Week 16", "85-140", 112, 85, 140,
  "Week 24", ">140", 168, 141, NA_integer_
)

adas3 <- derive_vars_merged(
  adas_locf,
  dataset_add = aw_lookup,
  by_vars = exprs(AVISIT)
) %>%
  mutate(
    AWTDIFF = abs(AWTARGET - ADY),
    AWU = "DAYS"
  )


## baseline information BASE/CHG/PCHG
adas4 <- adas3 %>%
  # Calculate BASE
  derive_var_base(
    by_vars = exprs(STUDYID, USUBJID, PARAMCD),
    source_var = AVAL,
    new_var = BASE
  ) %>%
  # Calculate CHG
  restrict_derivation(
    derivation = derive_var_chg,
    filter = is.na(ABLFL)
  ) %>%
  # Calculate PCHG
  restrict_derivation(
    derivation = derive_var_pchg,
    filter = is.na(ABLFL)
  )

## ANL01FL
adas5 <- adas4 %>%
  mutate(diff = AWTARGET - ADY) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID, PARAMCD, AVISIT),
      order = exprs(AWTDIFF, diff),
      new_var = ANL01FL,
      mode = "first"
    ),
    filter = !is.na(AVISIT)
  )

## out to XPT
adas5 %>%
  drop_unspec_vars(adadas_spec) %>% # only keep vars from define
  order_cols(adadas_spec) %>% # order columns based on define
  set_variable_labels(adadas_spec) %>% # apply variable labels based on define
  xportr_format(adadas_spec$var_spec %>%
    mutate_at(c("format"), ~ replace_na(., "")), "ADADAS") %>%
  xportr_write("submission/adam/adadas.xpt",
    label = "ADAS-COG Analysis Dataset"
  )
