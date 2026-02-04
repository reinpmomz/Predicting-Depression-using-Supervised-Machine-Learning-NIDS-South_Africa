library(dplyr)
library(forcats)
library(tibble)
library(janitor)
library(haven)
library(labelled)

## Reading data from local folder

data_files <- list.files(path = data_Dir, pattern = "^nids-", full.names = FALSE)

df_list <- sapply(data_files, function(x){
  nn <- x
  wave <- strtrim(nn, 7)
  wave_num <- gsub("nids-","",wave)
  data_subDir <- file.path(data_Dir, nn)
  data_subfiles <- list.files(path = data_subDir, full.names = FALSE)
  
  out <- sapply(data_subfiles, function(y){
    df_raw <- haven::read_dta(file.path(data_subDir, y))
    
    df <- df_raw %>%
      #janitor::clean_names() %>%
      dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), ~ haven::as_factor(.x)
                                  ) #converts only labelled columns to factors
                    , wave_id = wave_num
                    ) %>%
      labelled::set_variable_labels(wave_id = 'Wave identifier')
  }, simplify=FALSE)
  
 return(out) 
  
}, simplify=FALSE)

## Merging data from 5 waves independently
df_raw_wave_merged_list <- sapply(names(df_list), function(x){
  nn <- x
  wave <- strtrim(nn, 7)
  wave_num <- gsub("nids-w","",wave)
  version <- strtrim(nn, 14)
  version_num <- gsub(paste0("nids-w",wave_num,"-v"),"",version)
  
  adult_attribute <- as.data.frame(
    labelled::generate_dictionary(df_list[[nn]][[paste0("Adult_W",wave_num,"_Anon_V",version_num,".dta")]],
                                  labels = TRUE, values = TRUE
                                  )
    )
  
  child_attribute <- as.data.frame(
    labelled::generate_dictionary(df_list[[nn]][[paste0("Child_W",wave_num,"_Anon_V",version_num,".dta")]],
                                  labels = TRUE, values = TRUE
                                  )
    )
  
  proxy_attribute <- as.data.frame(
    labelled::generate_dictionary(df_list[[nn]][[paste0("Proxy_W",wave_num,"_Anon_V",version_num,".dta")]],
                                  labels = TRUE, values = TRUE
                                  )
  )
  
  household_derived_attribute <- as.data.frame(
    labelled::generate_dictionary(df_list[[nn]][[paste0("hhderived_W",wave_num,"_Anon_V",version_num,".dta")]],
                                  labels = TRUE, values = TRUE
                                  )
  )
  
  ## Creating a combined named vector to quickly assign the new variable labels
  var_labels <- dplyr::bind_rows(adult_attribute,child_attribute,proxy_attribute, household_derived_attribute
                                 ) %>%
    dplyr::select(variable, label)%>%
    tibble::deframe()
  
  ## Merging dataframes from the 5 waves
  out <- dplyr::bind_rows(df_list[[nn]][[paste0("Adult_W",wave_num,"_Anon_V",version_num,".dta")]],
                          df_list[[nn]][[paste0("Child_W",wave_num,"_Anon_V",version_num,".dta")]],
                          df_list[[nn]][[paste0("Proxy_W",wave_num,"_Anon_V",version_num,".dta")]]
                          ) %>% 
    dplyr::left_join(df_list[[nn]][[paste0("HHQuestionnaire_W",wave_num,"_Anon_V",version_num,".dta")]] %>%
                       dplyr::mutate(across(c(paste0("w",wave_num,"_hhid")), ~as.numeric(as.character(.x))
                                            ) #w4_hhid in Wave 4 HHQuestionnaire is factor. Convert it to number
                                     ),
                     by = c(paste0("w",wave_num,"_hhid"), "wave_id")
                     ) %>%
    dplyr::left_join(df_list[[nn]][[paste0("hhderived_W",wave_num,"_Anon_V",version_num,".dta")]] %>%
                       dplyr::mutate(across(c(paste0("w",wave_num,"_hhid")), ~as.numeric(as.character(.x))
                                            ) #w4_hhid in Wave 4 hhderived is factor. Convert it to number
                                     ),
                     by = c(paste0("w",wave_num,"_hhid"), "wave_id")
                     ) %>%
    dplyr::inner_join(df_list[[nn]][[paste0("indderived_W",wave_num,"_Anon_V",version_num,".dta")]] %>%
                       dplyr::mutate(across(c("pid", paste0("w",wave_num,"_hhid")), ~as.numeric(as.character(.x))
                                            ) #pid and w4_hhid in Wave 4 indderived is factor. Convert it to number
                                     ),
                     by = c("pid", paste0("w",wave_num,"_hhid"), "wave_id")
                     ) %>%
    dplyr::inner_join(df_list[[nn]][[paste0("Admin_W",wave_num,"_Anon_V",version_num,".dta")]] %>%
                        dplyr::mutate(across(c(paste0("w",wave_num,"_hhid")), ~as.numeric(as.character(.x))
                                             ) #w4_hhid in Wave 4 Admin is factor. Convert it to number
                                      ),
                      by = c("pid", paste0("w",wave_num,"_hhid"), "wave_id")
                      ) %>%
    dplyr::inner_join(df_list[[nn]][[paste0("HouseholdRoster_W",wave_num,"_Anon_V",version_num,".dta")]],
                     by = c("pid", paste0("w",wave_num,"_hhid"), "wave_id")
                     )  
  
  out_ <- coalesce_multi(out) %>%
    labelled::set_variable_labels(!!!var_labels[names(var_labels) %in% names(.)]
                                  ) #labeling variables from created data dictionary
  
}, simplify=FALSE)


## Renaming variables for row merge and dropping unused factors in merged data
df_renamed_wave_merged_list <- sapply(names(df_raw_wave_merged_list), function(x){
  nn <- x
  wave <- strtrim(nn, 7)
  wave_num <- gsub("nids-w","",wave)
  renaming <- df_raw_wave_merged_list[[nn]] %>%
    dplyr::rename_with(.fn = gsub,
                       .cols = starts_with(paste0("w",wave_num,"_")),
                       pattern = paste0("w",wave_num,"_"),
                       replacement = ""
                       ) %>%
    dplyr::mutate(across(where(is.factor),  ~forcats::fct_drop(.x )
                         ) #drop unused factor levels
                  )
  
}, simplify=FALSE)


