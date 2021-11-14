# Note to Reviewer 
# To rerun the code below, please refer ADRG appendix.
# After required package are installed. 
# The path variable needs to be defined by using example code below
#
# path = list(adam = "path/to/esub/analysis/adam/datasets")    	# Modify path to the actual location
# path$outtable = path$outgraph = "."                           # Output saved in current folder

## ----setup, message=FALSE-----------------------------------------------------------------------------------------------------------
library(dplyr)
library(Tplyr)
library(pharmaRTF)
library(pilot1wrappers)
options(huxtable.add_colnames = FALSE)


## -----------------------------------------------------------------------------------------------------------------------------------
adas  <- haven::read_xpt(file.path(path$adam, "adadas.xpt")) 
adsl  <- haven::read_xpt(file.path(path$adam, "adsl.xpt")) 


## -----------------------------------------------------------------------------------------------------------------------------------
adas <- adas %>%
  filter(
    EFFFL == "Y",
    ITTFL=='Y',
    PARAMCD == 'ACTOT',
    ANL01FL == 'Y'
  )


## -----------------------------------------------------------------------------------------------------------------------------------
t <- tplyr_table(adas, TRTP) %>% 
  set_pop_data(adsl) %>% 
  set_pop_treat_var(TRT01P) %>% 
  set_pop_where(EFFFL == "Y" & ITTFL == "Y") %>% 
  set_distinct_by(USUBJID) %>% 
  set_desc_layer_formats(
    'n' = f_str('xx', n),
    'Mean (SD)' = f_str('xx.x (xx.xx)', mean, sd),
    'Median (Range)' = f_str('xx.x (xxx;xx)', median, min, max)
  ) %>% 
  add_layer(
    group_desc(AVAL, where= AVISITN ==  0, by = "Baseline")
  ) %>% 
  add_layer(
    group_desc(AVAL, where= AVISITN == 24, by = "Week 24")
  ) %>% 
  add_layer(
    group_desc(CHG,  where= AVISITN == 24, by = "Change from Baseline")
  )
  
sum_data <- t %>% 
  build() %>% 
  nest_rowlabels() %>% 
  select(-starts_with('ord')) %>% 
  add_column_headers(
     paste0("|Placebo\\line(N=**Placebo**)| Xanomeline Low Dose\\line(N=**Xanomeline Low Dose**) ", 
           "| Xanomeline High Dose\\line(N=**Xanomeline High Dose**)"),
     header_n(t)
  )


## -----------------------------------------------------------------------------------------------------------------------------------
model_portion <- efficacy_models(adas, 'CHG', 24)


## -----------------------------------------------------------------------------------------------------------------------------------
final <- bind_rows(sum_data, model_portion)

ht <- huxtable::as_hux(final, add_colnames = FALSE) %>%
  huxtable::set_bold(1, 1:ncol(final), TRUE) %>%
  huxtable::set_align(1, 1:ncol(final), 'center') %>%
  huxtable::set_valign(1, 1:ncol(final), 'bottom') %>%
  huxtable::set_bottom_border(1, 1:ncol(final), 1) %>%
  huxtable::set_width(1.2) %>%
  huxtable::set_escape_contents(FALSE) %>%
  huxtable::set_col_width(c(.5, 1/6, 1/6, 1/6))
ht


## -----------------------------------------------------------------------------------------------------------------------------------
doc <- rtf_doc(ht) %>% 
  set_font_size(10) %>%
  set_ignore_cell_padding(TRUE) %>%
  set_column_header_buffer(top=1) %>% 
  add_titles(
    hf_line(
      "Protocol: CDISCPILOT01",
      "PAGE_FORMAT: Page %s of %s",
      align='split',
      bold=TRUE,
      italic=TRUE
    ),
    hf_line(
      "Population: Efficacy",
      align="left",
      bold=TRUE,
      italic=TRUE
    ),
    hf_line(
      "Table 14-3.01",
      bold=TRUE,
      italic=TRUE
    ),
    hf_line(
      "Primary Endpoint Analysis: ADAS Cog (11) - Change from Baseline to Week 24 - LOCF",
      bold=TRUE,
      italic=TRUE
    )
  ) %>% 
  add_footnotes(
    hf_line(
      "[1] Based on Analysis of covariance (ANCOVA) model with treatment and site group as factors and baseline value as a covariate.",
      align = "left",
      italic=TRUE
    ),
    hf_line(
      "[2] Test for a non-zero coefficient for treatment (dose) as a continuous variable",
      align = "left",
      italic=TRUE
    ),
    hf_line(
      "[3] Pairwise comparison with treatment as a categorical variable: p-values without adjustment for multiple comparisons.",
      align = "left",
      italic=TRUE
    ),
    hf_line(
      "FILE_PATH: Source: %s",
      "DATE_FORMAT: %H:%M %A, %B %d, %Y",
      align = "split",
      italic=TRUE
    )
  )

# Write out the RTF
write_rtf(doc, file=file.path(path$output, 'tlf-primary.rtf'))


## ---- out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"--------------------------------------------------
knitr::include_graphics("pdf/tlf-primary.pdf")

