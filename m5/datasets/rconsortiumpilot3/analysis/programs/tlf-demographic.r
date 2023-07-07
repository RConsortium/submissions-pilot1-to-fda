# Note to Reviewer
# To rerun the code below, please refer ADRG appendix.
# After required package are installed.
# The path variable needs to be defined by using example code below
#
# nolint start
# path <- list(
#   adam = "path/to/esub/analysis/adam",      # Modify path to the adam location
#   output = "path/to/esub/analysis/output"   # Modify path to the output location
# )
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
# CRAN package, please using install.packages() to install
library(haven)
library(dplyr)
library(rtables)



## ------------------------------------------------------------------------------------------------------------------------------
adsl <- read_xpt(file.path(path$adam, "adsl.xpt"))
adsl_labels <- var_labels(adsl)


## ------------------------------------------------------------------------------------------------------------------------------
adsl <- adsl %>%
  dplyr::filter(
    STUDYID == "CDISCPILOT01",
    ITTFL == "Y"
  ) %>%
  dplyr::mutate(
    TRT01P = factor(TRT01P, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")),
    AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
    RACE = factor(RACE, levels = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE"))
  )


## ------------------------------------------------------------------------------------------------------------------------------
# Table layout
vars <- c("AGE", "AGEGR1", "RACE", "HEIGHTBL", "WEIGHTBL", "BMIBL", "MMSETOT")
lyt <- basic_table(
  title = "Protocol: CDISCPILOT01",
  subtitles = "Population: Intent-to-Treat",
  main_footer = paste0("Program: tlf_demographic.R \n", Sys.time())
) %>%
  split_cols_by("TRT01P") %>%
  add_colcounts() %>%
  analyze(vars, function(x, ...) {
    if (is.numeric(x)) {
      in_rows(
        "Mean (sd)" = c(mean(x), sd(x)),
        "Median" = median(x),
        "Min - Max" = range(x),
        .formats = c("xx.xx (xx.xx)", "xx.xx", "xx.xx - xx.xx")
      )
    } else if (is.factor(x) || is.character(x)) {
      in_rows(.list = list_wrap_x(table)(x))
    } else {
      stop("type not supproted")
    }
  },
  var_labels = adsl_labels[vars]
  )

# Table build
tbl <- build_table(lyt, adsl)

tbl


## ------------------------------------------------------------------------------------------------------------------------------
# Output .out file
tbl %>%
  toString() %>%
  writeLines(con = file.path(path$output, "tlf-demographic-pilot3.out"))
