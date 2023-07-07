# Note to Reviewer
# To rerun the code below, please refer ADRG appendix.
# After required package are installed.
# The path variable needs to be defined by using example code below
#
# nolint start
# path <- list(
#   sdtm = "path/to/esub/tabulations/sdtm",   # Modify path to the sdtm location
#   adam = "path/to/esub/analysis/adam"       # Modify path to the adam location
# )
# nolint end

###########################################################################
#' developers : Phani Tata/Joel Laxamana
#' date: 07FEB2023
#' modification History:
#' program: adae.R
###########################################################################

library(admiral)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(xportr)
library(metacore)
library(metatools)
library(haven)

# read in AE
# ----------
ae <- read_xpt(file.path(path$sdtm, "ae.xpt"))
suppae <- read_xpt(file.path(path$sdtm, "suppae.xpt"))


# read in ADSL
# ------------
adsl <- read_xpt(file.path(path$adam, "adsl.xpt"))


# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values
#----------------------------------------------------------------------------------------
ae <- convert_blanks_to_na(ae)
adsl <- convert_blanks_to_na(adsl)


# ADAE derivation start
# Read in specifications from define
#----------------------------------------------------------------------------------------
## placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore(file.path(path$adam, "ADaM - Pilot 3.xlsx"), where_sep_sheet = FALSE, quiet = TRUE)
adae_spec <- metacore %>% select_dataset("ADAE") # Get the specifications for the dataset we are currently building


# Get list of ADSL vars
#----------------------------------------------------------------------------------------
adsl_vars <- exprs(
  TRTSDT,
  TRTEDT,
  STUDYID,
  SITEID,
  TRT01A,
  TRT01AN,
  AGE,
  AGEGR1,
  AGEGR1N,
  RACE,
  RACEN,
  SEX,
  SAFFL,
  TRTSDT,
  TRTEDT
)

# Merge adsl to ae
#----------------------------------------------------------------------------------------
adae0 <- ae %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by = exprs(STUDYID, USUBJID)
  ) %>%
  # Set TRTA and TRTAN from ADSL
  #----------------------------------------------------------------------------------------
  rename(
    TRTA = TRT01A,
    TRTAN = TRT01AN
  ) %>%
  # Derive analysis start time
  #----------------------------------------------------------------------------------------
  derive_vars_dtm(
    dtc = AESTDTC,
    new_vars_prefix = "AST",
    highest_imputation = "D"
  ) %>%
  # Derive analysis end time
  #----------------------------------------------------------------------------------------
  derive_vars_dtm(
    dtc = AEENDTC,
    new_vars_prefix = "AEN",
    highest_imputation = "h",
    max_dates = NULL
  ) %>%
  # Derive analysis start & end dates
  #----------------------------------------------------------------------------------------
  derive_vars_dtm_to_dt(exprs(ASTDTM, AENDTM)) %>%
  # Duration of AE
  #----------------------------------------------------------------------------------------
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = exprs(TRTSDT, ASTDT, AENDT)
  ) %>%
  derive_vars_duration(
    new_var = ADURN,
    new_var_unit = ADURU,
    start_date = ASTDT,
    end_date = AENDT,
    out_unit = "days"
  ) %>%
  # Treatment Emergent Analysis flag
  #----------------------------------------------------------------------------------------
  restrict_derivation(
    derivation = derive_var_trtemfl,
    args = params(
      start_date = ASTDT,
      end_date = AENDT,
      trt_start_date = TRTSDT
    ),
    filter = !is.na(ASTDT)
  ) %>%
  # AOCCFL - 1st Occurrence of Any AE Flag
  #----------------------------------------------------------------------------------------
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID),
      order = exprs(ASTDT, AESEQ),
      new_var = AOCCFL,
      mode = "first"
    ), filter = TRTEMFL == "Y"
  ) %>%
  # AOCCSFL - 1st Occurrence of SOC Flag
  #----------------------------------------------------------------------------------------
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID, AEBODSYS),
      order = exprs(ASTDT, AESEQ),
      new_var = AOCCSFL,
      mode = "first"
    ), filter = TRTEMFL == "Y"
  ) %>%
  # AOCCPFL - 1st Occurrence of Preferred Term Flag
  #----------------------------------------------------------------------------------------
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID, AEBODSYS, AEDECOD),
      order = exprs(ASTDT, AESEQ),
      new_var = AOCCPFL,
      mode = "first"
    ), filter = TRTEMFL == "Y"
  ) %>%
  # AOCC02FL - 1st Occurrence 02 Flag for Serious
  #----------------------------------------------------------------------------------------
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID),
      order = exprs(ASTDT, AESEQ),
      new_var = AOCC02FL,
      mode = "first"
    ), filter = TRTEMFL == "Y" & AESER == "Y"
  ) %>%
  # AOCC03FL - 1st Occurrence 03 Flag for Serious SOC
  #----------------------------------------------------------------------------------------
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID, AEBODSYS),
      order = exprs(ASTDT, AESEQ),
      new_var = AOCC03FL,
      mode = "first"
    ), filter = TRTEMFL == "Y" & AESER == "Y"
  ) %>%
  # AOCC04FL - 1st Occurrence 04 Flag for Serious PT
  #----------------------------------------------------------------------------------------
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID, AEBODSYS, AEDECOD),
      order = exprs(ASTDT, AESEQ),
      new_var = AOCC04FL,
      mode = "first"
    ), filter = TRTEMFL == "Y" & AESER == "Y"
  ) %>%
  # CQ01NAM - Customized Query 01 Name
  #----------------------------------------------------------------------------------------
  mutate(CQ01NAM = ifelse(str_detect(AEDECOD, "APPLICATION") |
    str_detect(AEDECOD, "DERMATITIS") |
    str_detect(AEDECOD, "ERYTHEMA") |
    str_detect(AEDECOD, "BLISTER") |
    str_detect(AEBODSYS, "SKIN AND SUBCUTANEOUS TISSUE DISORDERS") &
      !str_detect(AEDECOD, "COLD SWEAT|HYPERHIDROSIS|ALOPECIA"),
  "DERMATOLOGIC EVENTS",
  NA_character_
  )) %>%
  # AOCC01FL - 1st Occurrence 01 Flag for CQ01
  #----------------------------------------------------------------------------------------
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID),
      order = exprs(ASTDT, AESEQ),
      new_var = AOCC01FL,
      mode = "first"
    ), filter = TRTEMFL == "Y" & CQ01NAM == "DERMATOLOGIC EVENTS"
  )

# ADAE derivation end
#
# Check variables against define &
# Assign dataset labels, var labels and formats
#----------------------------------------------------------------------------------------
adae <- adae0 %>%
  drop_unspec_vars(adae_spec) %>% # Check all variables specified are present and no more
  check_ct_data(adae_spec, na_acceptable = TRUE) %>% # Checks all variables with CT only contain values within the CT
  order_cols(adae_spec) %>% # Orders the columns according to the spec
  sort_by_key(adae_spec) %>%
  xportr_df_label(adae_spec) %>% # dataset label
  xportr_label(adae_spec) %>% # variable labels
  convert_blanks_to_na() # blanks to NA


# NOTE : When reading in original ADAE dataset to check against, it
# seems the sas.format attributes set to DATE9. are changed to DATE9,
# i.e. without the dot[.] at the end. So when calling diffdf() the
# workaround is to also remove the dot[.] in the sas.format in the
# dataset generated here. This will make the sas.format comparisons
# equal in diffdf(). See code below for work around.
#----------------------------------------------------------------------------------------
adae <- adae %>%
  xportr_format(adae_spec$var_spec %>%
    mutate_at(c("format"), ~ replace_na(., "")), "ADAE")


# Export to xpt
#----------------------------------------------------------------------------------------
adae %>%
  xportr_write(file.path(path$adam, "adae.xpt"),
    label = "Adverse Events Analysis Dataset"
  )
