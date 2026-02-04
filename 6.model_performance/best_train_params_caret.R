library(dplyr)
library(lubridate)
library(writexl)
library(tidyr)

working_directory

# Best Model train parameters caret

best_train_params_caret_df <- sapply(ls(pattern = "_train_caret$"), function(x){
  nn <- x
  model_name <- gsub("_train_caret", "", nn)
  
  df <- get(x)
  
  out <- sapply(names(df), function(y){
    
    best_tune <- df[[y]][["bestTune"]]
    
    best_tune_new <- best_tune %>%
      dplyr::mutate(analysis = y
                    , model = model_name
                    , across(everything(), ~as.character(.x))
                    ) %>%
      tidyr::pivot_longer(!c(analysis, model),
                          names_to = "name", values_to = "value"
                          )
    
  }, simplify=FALSE)%>%
    dplyr::bind_rows()
  
  out
  
}, simplify=FALSE) %>%
  dplyr::bind_rows() %>%
  tidyr::pivot_wider(names_from = analysis, values_from = value)

## save Best model train parameters
writexl::write_xlsx(list( best_train_params = best_train_params_caret_df
                          ),
                    path = base::file.path(output_Dir, "best_train_params_caret.xlsx" )
                    )

