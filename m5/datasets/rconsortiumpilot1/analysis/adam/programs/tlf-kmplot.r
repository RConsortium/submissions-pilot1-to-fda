# Note to Reviewer 
# To rerun the code below, please refer ADRG appendix.
# After required package are installed. 
# The path variable needs to be defined by using example code below
#
# path = list(adam = "path/to/esub/analysis/adam/datasets")    	# Modify path to the actual location
# path$outtable = path$outgraph = "."                           # Output saved in current folder

library(haven) 
library(dplyr)
library(pilot1wrappers)
library(ggplot2)
library(cowplot)
library(visR)


## -----------------------------------------------------------------------------------------------------------------------------------
adsl <- read_xpt(file.path(path$adam, "adsl.xpt"))
adtte <- read_xpt(file.path(path$adam, "adtte.xpt")) 


## -----------------------------------------------------------------------------------------------------------------------------------
anl <- adsl %>% 
  dplyr::filter(
    SAFFL == "Y",
    STUDYID == "CDISCPILOT01"
  ) %>%
  dplyr::select(STUDYID, USUBJID, TRT01A) %>%
  dplyr::inner_join(
    filter(
      adtte, PARAMCD == "TTDE", STUDYID == "CDISCPILOT01"
    ) %>% select(STUDYID, USUBJID, AVAL, CNSR, PARAM, PARAMCD),
    by = c("STUDYID", "USUBJID")
  ) %>%
  dplyr::mutate(
    TRT01A = factor(TRT01A, levels = c("Placebo", "Xanomeline Low Dose",  "Xanomeline High Dose"))
  )


## -----------------------------------------------------------------------------------------------------------------------------------
# estimate survival
surv_mod <- visR::estimate_KM(data = anl, strata = "TRT01A")

# save plot
ggplot2::theme_set(theme_bw())

pdf.options(reset = TRUE, onefile = FALSE)

pdf(file.path(path$output, "tlf-kmplot.pdf"))

KM <- visR::visr(surv_mod,
           y_label = "Probability of event\n",
           x_label = "Time to First Dermatologic Event (Days)",
           y_ticks = seq(0,1,0.10)) %>%
    add_CNSR() %>%
    add_CI()

KM <- KM + 
    ggplot2::geom_hline(yintercept=0.5, linetype = "dashed")

KM <- KM %>%
    visR::add_risktable(group = "statlist")

title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "KM plot for Time to First Dermatologic Event: Safety population\n",
    fontfamily = "sans",
    fontface = "bold",
    size=10
  ) 

caption <- cowplot::ggdraw() + 
  cowplot::draw_label(
    paste0("\nProgram: tlf_kmplot.Rmd [", Sys.time(), "]"),
    fontfamily = "sans",
    size=10
  ) 

KM <- cowplot::plot_grid(
  title, KM, caption,
  ncol = 1,
  rel_heights = c(0.1,0.8,0.1)
)

print(KM)
dev.off()


## ---- out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"--------------------------------------------------
knitr::include_graphics("pdf/tlf-kmplot.pdf")

