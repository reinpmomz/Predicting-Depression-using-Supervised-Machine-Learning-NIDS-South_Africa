library(rsample)
library(dplyr)
library(caret)


## set seed for reproducibility
set.seed(seed_partition)

## Create training and test datasets
# split2 <- rsample::initial_split(df_drop_feature, 
#                                 prop = model_params_df$test_train_ratio, 
#                                 strata = any_of(outcome_vars)
#                                 )

split2 <- rsample::group_initial_split(df_drop_feature %>% dplyr::arrange(pid)
                                       ,prop = model_params_df$test_train_ratio
                                       ,group = pid
                                       #,strata = any_of(outcome_vars)
                                       )

train2 <- training(split2)
test2 <- testing(split2)


## Create up-sample and Down-sample datasets only on training set based on ratio condition
if (round(prop.table(table(train2[outcome_vars], useNA = "no")),2)[1] > 0.55 | 
    round(prop.table(table(train2[outcome_vars], useNA = "no")),2)[1] < 0.45) {

### Upsampling increases size of minority class by sampling with replacement so that the classes will have the same size
set.seed(seed_imbalance)

train2up <- caret::upSample(x= train2[, !names(train2) %in% outcome_vars], 
                           y= train2[[outcome_vars]],
                           list = FALSE, 
                           yname = names(train2[outcome_vars])
                           ) %>%
  dplyr::relocate(any_of(outcome_vars)) %>%
  dplyr::mutate(across(where(is.factor),  ~as.character(.x ))) %>%
  dplyr::mutate(across(where(is.character),  ~as.factor(.x ))) 

### Downsampling decreases size of majority class to be the same or closer to minority class by taking a random sample
set.seed(seed_imbalance)

train2down <- caret::downSample(x= train2[, !names(train2) %in% outcome_vars], 
                           y= train2[[outcome_vars]],
                           list = FALSE, 
                           yname = names(train2[outcome_vars])
                           ) %>% 
  dplyr::relocate(any_of(outcome_vars))

}
