###########################################################################
#' developers : Steven Haesendonckx/Bingjun Wang/Ben Straub
#' date: 13NOV2022
#' modification History:
#'
###########################################################################

# Set up ------------------------------------------------------------------

library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(metacore)
library(metatools)
library(pilot3)
library(xportr)

# read source -------------------------------------------------------------

adsl <- read_xpt(file.path("submission", "adam", "adsl.xpt"))
adae <- read_xpt(file.path("submission", "adam", "adae.xpt"))
ds <- convert_blanks_to_na(read_xpt(file.path("submission/sdtm", "ds.xpt")))


## placeholder for origin=predecessor, use metatool::build_from_derived()

metacore <- spec_to_metacore("adam/TDF_ADaM - Pilot 3 Team updated.xlsx", where_sep_sheet = FALSE)

# Get the specifications for the dataset we are currently building

adtte_spec <- metacore %>%
  select_dataset("ADTTE")

# First dermatological event (ADAE.AOCC01FL = 'Y' and ADAE.CQ01NAM != '')

event <- event_source(
  dataset_name = "adae",
  filter = AOCC01FL == "Y" & CQ01NAM == "DERMATOLOGIC EVENTS" & SAFFL == "Y",
  date = ASTDT,
  set_values_to = exprs(
    EVNTDESC = "Dematologic Event Occured",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)


# Censor events ---------------------------------------------------------

## discontinuation, completed, death
ds00 <- ds %>%
  select(STUDYID, USUBJID, DSCAT, DSDECOD, DSSTDTC) %>%
  derive_vars_dt(
    .,
    dtc = DSSTDTC,
    new_vars_prefix = "DSST"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds00,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE" & DSDECOD != "FINAL LAB VISIT"
  ) %>%
  # Analysis uses DEATH date rather than discontinuation when subject dies even if discontinuation occurs before death
  # Observed through QC - However not described in specs
  mutate(EOS2DT = case_when(
    DCDECOD == "DEATH" ~ as.Date(RFENDTC),
    DCDECOD != "DEATH" ~ EOSDT
  ))

censor <- censor_source(
  dataset_name = "adsl",
  date = EOS2DT,
  set_values_to = exprs(
    EVNTDESC = "Study Completion Date",
    SRCDOM = "ADSL",
    SRCVAR = "RFENDT"
  )
)


adtte_pre <- derive_param_tte(
  dataset_adsl = adsl,
  start_date = TRTSDT,
  event_conditions = list(event),
  censor_conditions = list(censor),
  source_datasets = list(adsl = adsl, adae = adae),
  set_values_to = exprs(PARAMCD = "TTDE", PARAM = "Time to First Dermatologic Event")
) %>%
  derive_vars_duration(
    new_var = AVAL,
    start_date = STARTDT,
    end_date = ADT
  ) %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = exprs(
      AGE, AGEGR1, AGEGR1N, RACE, RACEN, SAFFL, SEX, SITEID, TRT01A,
      TRT01AN, TRTDURD, TRTEDT, TRT01P, TRTSDT
    ),
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  rename(
    TRTA = TRT01A,
    TRTAN = TRT01AN,
    TRTDUR = TRTDURD,
    TRTP = TRT01P
  )

adtte <- adtte_pre %>%
  drop_unspec_vars(adtte_spec) %>% # only keep vars from define
  order_cols(adtte_spec) %>% # order columns based on define
  set_variable_labels(adtte_spec) %>% # apply variable labels based on define
  # xportr_type(adtte_spec, "ADTTE") %>%
  # xportr_length(adtte_spec, "ADTTE") %>%
  # unresolved issue in xportr_length due to:
  # https://github.com/tidyverse/haven/issues/699
  # no difference found by diffdf after commenting out xportr_length()
  xportr_format(adtte_spec$var_spec %>%
    mutate_at(c("format"), ~ replace_na(., "")), "ADTTE") %>%
  xportr_write("submission/adam/adtte.xpt",
    label = "AE Time To 1st Derm. Event Analysis"
  )
