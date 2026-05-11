# This code extracts all the R code from files
library(stringr)
library(glue)
qmd_fls <- list.files(path = here::here(), pattern = "\\.qmd$")
exclude <- c("dimension.qmd", "preface.qmd", "index.qmd", 
             "regression.qmd", "references.qmd", "supervised.qmd", 
             "temporal.qmd", "unsupervised.qmd", 
             "A1-toolbox.qmd", "A2-data.qmd",
             "A3-book-code-and-data.qmd", "A4-glossary.qmd",
             "19-mv-time-series.qmd")
qmd_fls <- qmd_fls[!(qmd_fls %in% exclude)]

for (i in qmd_fls) {
  fl_orig <- i
  fl_code <- glue::glue("code/", str_replace(fl_orig, ".qmd", ".R"))
  knitr::purl(fl_orig, fl_code)                     
  cat("Writing", fl_code, "\n")
}
