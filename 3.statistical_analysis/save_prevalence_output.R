library(dplyr)
library(flextable)
library(officer)


## Saving descriptive and inferential output

flextable::save_as_docx(values = c(prevalence_overall_wave_unweighted_stats, 
                          prevalence_wave_location_unweighted_stats), 
                        path = base::file.path(output_Dir, "descriptive_prevalence_unweighted_stats.docx"),
                        align = "center", #left, center (default) or right.
                        pr_section = officer::prop_section(
                          page_size = officer::page_size(orient = "landscape"), #Use NULL (default value) for no content.
                          page_margins = officer::page_mar(), #Use NULL (default value) for no content.
                          type = "nextPage", # "continuous", "evenPage", "oddPage", "nextColumn", "nextPage"
                          section_columns = NULL, #Use NULL (default value) for no content.
                          header_default = NULL, #Use NULL (default value) for no content.
                          header_even = NULL, #Use NULL (default value) for no content.
                          header_first = NULL, #Use NULL (default value) for no content.
                          footer_default = NULL, #Use NULL (default value) for no content.
                          footer_even = NULL, #Use NULL (default value) for no content.
                          footer_first = NULL #Use NULL (default value) for no content.
                          )
                        )

flextable::save_as_docx(values = c(list(prevalence_overall_wave_weighted_stats), 
                          prevalence_wave_location_weighted_stats), 
                        path = base::file.path(output_Dir, "descriptive_prevalence_weighted_stats.docx"),
                        align = "center", #left, center (default) or right.
                        pr_section = officer::prop_section(
                          page_size = officer::page_size(orient = "landscape"), #Use NULL (default value) for no content.
                          page_margins = officer::page_mar(), #Use NULL (default value) for no content.
                          type = "nextPage", # "continuous", "evenPage", "oddPage", "nextColumn", "nextPage"
                          section_columns = NULL, #Use NULL (default value) for no content.
                          header_default = NULL, #Use NULL (default value) for no content.
                          header_even = NULL, #Use NULL (default value) for no content.
                          header_first = NULL, #Use NULL (default value) for no content.
                          footer_default = NULL, #Use NULL (default value) for no content.
                          footer_even = NULL, #Use NULL (default value) for no content.
                          footer_first = NULL #Use NULL (default value) for no content.
                          )
                        )

