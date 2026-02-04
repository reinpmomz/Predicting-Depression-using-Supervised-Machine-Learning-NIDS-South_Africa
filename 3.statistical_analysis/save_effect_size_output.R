library(dplyr)
library(writexl)


## Saving effect size stats Output

if (length(outcome_vars)>0) {
writexl::write_xlsx(effect_size_stats_overall,
                    path = base::file.path(output_Dir, "effect_size_stats_overall.xlsx" )
                    )
 } else {
    print(paste0("No effect size analysis done"))
 }

if (length(outcome_vars)>0) {
writexl::write_xlsx(effect_size_stats_wave,
                    path = base::file.path(output_Dir, "effect_size_stats_wave.xlsx" )
                    )
 } else {
    print(paste0("No effect size analysis done"))
  }


