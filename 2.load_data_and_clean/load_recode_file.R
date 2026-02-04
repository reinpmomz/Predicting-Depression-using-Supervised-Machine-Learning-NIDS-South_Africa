library(dplyr)
library(readxl)
library(tibble)
library(stringr)

## Reading the recode file sheet

recode_file <- read_excel_allsheets("./2.load_data_and_clean/nids_recode_file.xlsx")

study_details <- recode_file[["study"]]

wave1_rename_vars_df <- recode_file[["wave1_rename_vars"]] #df for renaming variable labels
wave2_rename_vars_df <- recode_file[["wave2_rename_vars"]] #df for renaming variable labels
wave3_rename_vars_df <- recode_file[["wave3_rename_vars"]] #df for renaming variable labels
wave4_rename_vars_df <- recode_file[["wave4_rename_vars"]] #df for renaming variable labels
wave5_rename_vars_df <- recode_file[["wave5_rename_vars"]] #df for renaming variable labels

wave_merged_rename_vars_df <- recode_file[["wave1_5_merged_rename_vars"]] #df for renaming variable labels

selected_vars_df <- recode_file[["selected_vars"]] #df for choosing variables for analysis and plots

tools_cutoff_df <- recode_file[["tools_cutoff"]] #df for tool cutoff

drop_selected_vars_df <- recode_file[["drop_selected_vars"]] #df for dropping analysis variables not needed for modelling

model_params_df <- recode_file[["model_params"]] #df for model pre-processing

model_names_df <- recode_file[["model_names"]] #df for model names

data_names_train_df <- recode_file[["data_names_train"]] #df for name type of train data

positive_class <- recode_file$positive_class$class

performance_evaluation <- recode_file[["performance_evaluation"]]

## Creating a named vector to quickly assign the new variable labels
rename_vars_df <- sapply(ls(pattern = "_rename_vars_df$"), function(x){
  nn <- x
  df_new <- get(x)
  
  out <- df_new %>%
    dplyr::mutate(new_label = stringr::str_to_sentence(new_label))
  
}, simplify=FALSE)

new_wave_labels <- dplyr::bind_rows(rename_vars_df
                               ) %>%
  dplyr::select(new_variable, new_label) %>%
  dplyr::distinct(new_variable, .keep_all = TRUE) %>%
  tibble::deframe()

new_labels <- wave_merged_rename_vars_df %>%
  dplyr::select(new_variable, new_label) %>%
  dplyr::distinct(new_variable, .keep_all = TRUE) %>%
  tibble::deframe()

