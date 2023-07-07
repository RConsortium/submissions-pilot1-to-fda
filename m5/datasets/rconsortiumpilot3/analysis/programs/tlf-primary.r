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

## ----setup, message=FALSE------------------------------------------------------------------------------------------------------
# CRAN package, please using install.packages() to install
library(tidyr)
library(dplyr)
library(Tplyr)
library(pharmaRTF)
library(pilot3)

## ------------------------------------------------------------------------------------------------------------------------------
options(huxtable.add_colnames = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------
adas <- haven::read_xpt(file.path(path$adam, "adadas.xpt"))
adsl <- haven::read_xpt(file.path(path$adam, "adsl.xpt"))


## ------------------------------------------------------------------------------------------------------------------------------
adas <- adas %>%
  filter(
    EFFFL == "Y",
    ITTFL == "Y",
    PARAMCD == "ACTOT",
    ANL01FL == "Y"
  )


## ------------------------------------------------------------------------------------------------------------------------------
t <- tplyr_table(adas, TRTP) %>%
  set_pop_data(adsl) %>%
  set_pop_treat_var(TRT01P) %>%
  set_pop_where(EFFFL == "Y" & ITTFL == "Y") %>%
  set_distinct_by(USUBJID) %>%
  set_desc_layer_formats(
    "n" = f_str("xx", n),
    "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd),
    "Median (Range)" = f_str("xx.x (xxx;xx)", median, min, max)
  ) %>%
  add_layer(
    group_desc(AVAL, where = AVISITN == 0, by = "Baseline")
  ) %>%
  add_layer(
    group_desc(AVAL, where = AVISITN == 24, by = "Week 24")
  ) %>%
  add_layer(
    group_desc(CHG, where = AVISITN == 24, by = "Change from Baseline")
  )

hdr <- adas %>%
  distinct(TRTP, TRTPN) %>%
  arrange(TRTPN) %>%
  pull(TRTP)
hdr_ext <- sapply(hdr, FUN = function(x) paste0("|", x, "\\line(N=**", x, "**)"), USE.NAMES = FALSE)
hdr_fin <- paste(hdr_ext, collapse = "")
# Want the header to wrap properly in the RTF file
hdr_fin <- stringr::str_replace_all(hdr_fin, "\\|Xanomeline ", "|Xanomeline\\\\line ")

sum_data <- t %>%
  build() %>%
  nest_rowlabels() %>%
  select(row_label, var1_Placebo, `var1_Xanomeline Low Dose`, `var1_Xanomeline High Dose`) %>%
  add_column_headers(
    hdr_fin,
    header_n(t)
  )


## ------------------------------------------------------------------------------------------------------------------------------
model_portion <- efficacy_models(adas, "CHG", 24)


## ------------------------------------------------------------------------------------------------------------------------------
final <- bind_rows(sum_data, model_portion)

ht <- huxtable::as_hux(final, add_colnames = FALSE) %>%
  huxtable::set_bold(1, seq_len(ncol(final)), TRUE) %>%
  huxtable::set_align(1, seq_len(ncol(final)), "center") %>%
  huxtable::set_valign(1, seq_len(ncol(final)), "bottom") %>%
  huxtable::set_bottom_border(1, seq_len(ncol(final)), 1) %>%
  huxtable::set_width(1.3) %>%
  huxtable::set_escape_contents(FALSE) %>%
  huxtable::set_col_width(c(.4, .2, .2, .2))
cat(huxtable::to_screen(ht))


## ------------------------------------------------------------------------------------------------------------------------------
doc <- rtf_doc(ht) %>%
  set_font_size(10) %>%
  set_ignore_cell_padding(TRUE) %>%
  set_column_header_buffer(top = 1) %>%
  add_titles(
    hf_line(
      "Protocol: CDISCPILOT01",
      "PAGE_FORMAT: Page %s of %s",
      align = "split",
      bold = TRUE,
      italic = TRUE
    ),
    hf_line(
      "Population: Efficacy",
      align = "left",
      bold = TRUE,
      italic = TRUE
    ),
    hf_line(
      "Table 14-3.01",
      bold = TRUE,
      italic = TRUE
    ),
    hf_line(
      "Primary Endpoint Analysis: ADAS Cog (11) - Change from Baseline to Week 24 - LOCF",
      bold = TRUE,
      italic = TRUE
    )
  ) %>%
  add_footnotes(
    hf_line(
      "[1] Based on Analysis of covariance (ANCOVA) model with treatment and site group as factors and baseline value as a covariate.",
      align = "left",
      italic = TRUE
    ),
    hf_line(
      "[2] Test for a non-zero coefficient for treatment (dose) as a continuous variable",
      align = "left",
      italic = TRUE
    ),
    hf_line(
      "[3] Pairwise comparison with treatment as a categorical variable: p-values without adjustment for multiple comparisons.",
      align = "left",
      italic = TRUE
    ),
    hf_line(
      "FILE_PATH: Source: %s",
      "DATE_FORMAT: %H:%M %A, %B %d, %Y",
      align = "split",
      italic = TRUE
    )
  )

# Write out the RTF
write_rtf(doc, file = file.path(path$output, "tlf-primary-pilot3.rtf"))
