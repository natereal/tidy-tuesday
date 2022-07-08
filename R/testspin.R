knitr::spin("22-01-18-chocolate.R")

ezknitr::ezspin(file = "R/22-01-18-chocolate.R",
                out_dir = "output",
                fig_dir = "figs")

rmarkdown::render(input = "R/22-01-18-chocolate.R",
                  output_dir = "render-output",
                  output_format = "md_document",
                  knit_root_dir = "/Users/Nathan/Projects/tidy-tuesday/")

rstudioapi::getActiveProject()


