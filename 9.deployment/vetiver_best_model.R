library(dplyr)
library(vetiver)
library(pins)
library(plumber)

# 1. Read trained model from output folder
best_model <- readRDS("./Output/best_model_caret.rds")

feature_data <- readRDS("./Output/sample_features.rds") %>%
  dplyr::select(-any_of(c("pid", "cesd_depression"))
                )

# 2. Create vetiver model object
v <- vetiver::vetiver_model(model = best_model
                            , model_name = "depression-southafrica"
                            , save_prototype = feature_data
                            )

# 3. Test REST API 
#plumber::pr() %>%
#  vetiver::vetiver_api(v) %>%
#  plumber::pr_run(port = 8080)

# 4. Version model locally for deployment
board <- pins::board_folder("./9.deployment/model_board"
                                  , versioned = TRUE
                                  )

vetiver::vetiver_pin_write(board, v)

# 5. Generate API + Docker assets
## # pin_read() in plumber file returns the latest version by default
vetiver::vetiver_prepare_docker(
  board,
  "depression-southafrica",
  path = "."
)

