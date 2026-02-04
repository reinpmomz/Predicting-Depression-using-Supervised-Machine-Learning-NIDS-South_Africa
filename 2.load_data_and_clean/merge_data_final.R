library(dplyr)
library(janitor)
library(labelled)
library(writexl)

## Merging to get one dataset

### Check if output is true
janitor::compare_df_cols_same(df_match_wave_merged_list
                                   )

df_merged_final <- dplyr::bind_rows( df_match_wave_merged_list
                                     ) %>%
  dplyr::mutate(across(c(a_em1prod_c, a_em2prod_c, a_noemsrex, p_em1prod_c), ~as.character(.x) 
                       ) #variables from factor to character
                ) %>% 
  dplyr::left_join(df_wave_merged_link_file %>% 
                     dplyr::select(pid, wave_id, sample_coalesce, cluster_coalesce, hhid_coalesce),
                   by = c("pid", "wave_id")
                   ) %>%
  labelled::set_variable_labels(!!!renamed_wave1_to_5_merged_labels[names(renamed_wave1_to_5_merged_labels) %in% names(.)]
                                ) #labeling variables from data dictionary

### creating data dictionary
attribute <- as.data.frame(labelled::generate_dictionary(df_merged_final, labels = TRUE, values = TRUE)
                           )

### Saving data dictionary
writexl::write_xlsx(attribute,
                    path = base::file.path(output_Dir, paste0("data_dictionary_merged_final.xlsx") )
                    )

## saving merged dataset
# haven::write_dta(data= df_merged_final, 
#                  path = base::file.path(output_Dir, "NIDS_1_2_3_4_5_row_merge.dta")
#                  )

# haven::write_sav(data= df_merged_final, 
#                  path = base::file.path(output_Dir, "NIDS_1_2_3_4_5_row_merge.sav")
#                  )

