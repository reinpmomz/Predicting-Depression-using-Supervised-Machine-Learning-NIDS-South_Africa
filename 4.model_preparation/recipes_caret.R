library(dplyr)
library(recipes)
library(caret)
library(labelled)


model_recipe_caret <- sapply(ls(pattern = "^train2"), function(x){
  x <- get(x)
  rows_data <- nrow(x)
  
  #missing_factor_vars <- names(x)[sapply(x, function(col) is.factor(col) & sum(is.na(col))>0 )]
  #missing_numeric_vars <- names(x)[sapply(x, function(col) is.numeric(col) & sum(is.na(col))>0 )]
  corr_cutoff <- corr_threshold
  
  factor_character_vars <- names(x[sapply(x,function(v) is.factor(v) | is.character(v) | is.logical(v))])
  
  ### pull variables with missing greater than 10%
  var_attribute <- as.data.frame(labelled::generate_dictionary(x, labels = TRUE, values = TRUE)
                                    ) %>%
    dplyr::mutate(missing_prop = round((missing/rows_data)*100,2)
                  ) %>%
    dplyr::filter(missing_prop > 10) %>%
    dplyr::pull(variable)
  
  missing_prop_greater_than_10_vars <- factor_character_vars[factor_character_vars %in% var_attribute]
  
  recipe <- recipe(model_form, data=x) %>%
    #update existing role in recipe or assigns an initial role to variables that do not yet have a declared role
    update_role(any_of(c("pid")), new_role = "id variable") %>% 
    #fine tune requirements of the various roles in recipes
    update_role_requirements("id variable", bake = FALSE) %>% 
    #creates a specification of a recipe step that will remove selected variables.
    #step_rm(any_of(c("pid"))) %>% 
    step_novel(all_nominal_predictors(),
               new_level = "new unseen level"
               ) #creates a specification that will assign a previously unseen factor level to "new"
  
  recipe_unknown <- recipe %>%
    #creates a specification that will create bagged tree models to impute missing data
    # step_impute_bag(any_of(!!missing_prop_greater_than_10_vars) 
    #                 ,trees = 10 #default 25
    #                 ,seed_val = 6974
    #                 , impute_with = all_predictors() 
    #                 ) %>%
    #creates a specification that will assign a missing value in a factor level to "unknown"
    step_unknown(all_nominal_predictors()
                 ) 
  
  recipe_impute_numeric <- recipe_unknown %>%
    #creates a specification that will create linear regression models to impute missing data for numeric variables
    # step_impute_linear(all_numeric_predictors()
    #                    , impute_with = all_predictors() 
    #                    ) %>%
    #creates a specification that will impute missing data using nearest neighbors
    step_impute_knn(all_numeric_predictors() 
                    ,neighbors = 10
                    , impute_with = all_predictors() 
                    )
  
  recipe_dummy <- recipe_impute_numeric %>% 
    step_dummy(all_nominal(), -all_outcomes(), one_hot=FALSE, naming = dummy_extract_names
               ) #creates a specification that will convert nominal data (e.g. factors) into one or more numeric binary model terms 
                 #corresponding to the levels of the original data.
  
  recipe_single <- recipe_dummy %>%
    step_zv(all_predictors()) %>% #creates a specification that will remove variables that contain only a single value
    step_nzv(all_predictors()) #creates a specification that will remove variables that are highly sparse and unbalanced
  
  recipe_corr <- if (corr_cutoff > 0) {
    recipe_single %>% 
      step_corr(all_predictors(), 
                threshold=corr_cutoff) ## remove variables that have large absolute correlations with other variables
  } else {
    recipe_single
  }
  
  recipe_norm <- recipe_corr %>%
    #step_normalize(all_numeric_predictors()) %>% ## normalize numeric data to have a sd of one and mean of zero
    step_scale(all_numeric_predictors()) %>% ## normalize numeric data to have a standard deviation of one
    step_center(all_numeric_predictors()) ## normalize numeric data to have a mean of zero
    
  return(recipe_norm)
  
}, simplify=FALSE)

