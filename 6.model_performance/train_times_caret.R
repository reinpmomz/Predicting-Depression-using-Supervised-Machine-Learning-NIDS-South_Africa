library(dplyr)
library(lubridate)
library(writexl)
library(tidyr)

working_directory

# Model caret training times

train_caret_times_df <- sapply(ls(pattern = "_train_caret_times$"), function(x){
  nn <- x
  model_name <- gsub("_train_caret_times", "", nn)
  
  df <- get(x)
  
  #convert seconds to a period object
  out <- df %>%
    dplyr::mutate(time_period = lubridate::seconds_to_period(time_taken_sec)
                  , time_period = as.character(time_period)
                  ) %>%
    dplyr::select(-c(time_taken_sec))
  
  out$model <- model_name
  
  return(out)
  
}, simplify=FALSE) %>%
  dplyr::bind_rows() %>%
  tidyr::pivot_wider(names_from = dataset, values_from = time_period)


## save best model metrics
writexl::write_xlsx(list( train_caret_times = train_caret_times_df
                          ),
                    path = base::file.path(output_Dir, "train_caret_times.xlsx" )
                    )

