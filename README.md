# Predicting-Depression-using-Supervised-Machine-Learning-NIDS-South_Africa
Predicting Depression in South Africa using Supervised Machine Learning

## Background

Mental health problems, particularly depression, are a growing public health concern worldwide, and middle-income countries such as South Africa are disproportionately affected, causing a significant number of depressed persons to remain unidentified and untreated. Existing studies largely use traditional analytical methods, which are often limited by their inability to capture complex, non-linear relationships between risk factors and depression. This study utilizes an advanced analytical method by applying machine learning (ML) to provide accurate detection of depression.

## Setup

We are assuming you have `R Software` and `RStudio IDE` installed. If not, you can download and install [**R software**](https://www.r-project.org/), followed by [**RStudio/Posit IDE**](https://posit.co/download/rstudio-desktop/).

## Data

The data used for analysis is available from the [**Data First Repository**](https://www.datafirst.uct.ac.za/) under the **National Income Dynamics Study (NIDS)** collection via identifiers https://doi.org/10.25828/e7w9-m033, https://doi.org/10.25828/j1h1-5m16, https://doi.org/10.25828/7pgq-q106, https://doi.org/10.25828/f4ws-8a78, https://doi.org/10.25828/fw3h-v708. One needs to register and request permission to utilize the publicly available datasets in the Repository. 

After permission is granted, download the NIDS data for the five waves in form of Stata DTA files and save them in a local folder. Each wave has its individual folder that contains 9 Stata DTA files for waves 2 - 5 (Admin_W*_Anon_V*.dta, Adult_W*_Anon_V*.dta, Child_W*_Anon_V*.dta, hhderived_W*_Anon_V*.dta, HHQuestionnaire_W*_Anon_V*.dta, HouseholdRoster_W*_Anon_V*.dta, indderived_W*_Anon_V*.dta, Proxy_W*_Anon_V*.dta, Link_File_W*_Anon_V*.dta) and 8 Stata DTA files for wave 1 (Admin_W1_Anon_V*.dta, Adult_W1_Anon_V*.dta, Child_W1_Anon_V*.dta, hhderived_W1_Anon_V*.dta, HHQuestionnaire_W1_Anon_V*.dta, HouseholdRoster_W1_Anon_V*.dta, indderived_W1_Anon_V*.dta, Proxy_W1_Anon_V*.dta)

- **Data used for analysis:** Wave 1-5 Folders `nids-w1-v7.0.0-stata14`, `nids-w2-v4.0.0-stata14`, `nids-w3-v3.0.0-stata14`, `nids-w4-v2.0.0-stata14` and `nids-w5-v1.0.0-stata14`

## Tools/Materials

1. Shape files for South Africa obtained from [**OCHA - HUMANITARIAN DATA EXCHANGE(HDX)**](https://data.humdata.org/dataset/cod-ab-zaf) are in the _data_ sub-folder of this repository.

2. The `nids_recode_file.xlsx` file in the [2.load_data_and_clean folder](./2.load_data_and_clean) contains: 
    
    1. CES-D cutoff threshold value in the _tools_cutoff_ sheet.

    2. Data dictionary about data of the 5 waves in the _wave1_rename_vars_, _wave2_rename_vars_, _wave3_rename_vars_, _wave4_rename_vars_, and _wave5_rename_vars_ sheets.

    3. Data dictionary about the final merged variables in the _wave1_5_merged_rename_vars_ sheet.
  
    4. Guide about the cleaned dataset that includes new variables created, variables used for descriptive and inferential analysis, variables for visualization, and variables used in calculating reliability in the _selected_vars_ sheet. 
   
    5. Variables used in descriptive analysis that are not required for further analysis, i.e., Machine learning (ML) in the _drop_selected_vars_ sheet.
    
    6. _model_params_ sheet that contains various seed values, test_train_ratio value, corr_threshold value and ML train fold value.
  
    7. _positive_class_ sheet that contains the category label of the positive class in the dataset to be used in the ML pipeline.
  
    8. _model_names_ sheet that contains a list of abbreviated ML algorithms and their full names used in the analysis.

    9. _data_names_train_ sheet that contains a list of sampling methods used for the train data in the analysis.

    10. _performance_evaluation_ sheet that contains a list of performance metrics used for ML evaluation.

## Run

After cloning the repository or downloading the ZIP, you also need the data files (**Data used for analysis**) in the _data_ sub-folder of _Predicting-Depression-using-Supervised-Machine-Learning-NIDS-South_Africa_ folder.

Open `RStudio` then set your working directory to the _Predicting-Depression-using-Supervised-Machine-Learning-NIDS-South_Africa_ folder. 

- If you get data for this project, it is advisable to work on a high computational power `RStudio Server` setup or a laptop/desktop with preferably 64GB RAM and run individual files. 

- To run individual files, open the `main.R` script, and run from the beginning.

## Model Deployment

We are utilising the R vetiver framework for Machine learning operations (MLOps). MLOps is a set of practices to deploy and maintain machine learning models in production reliably and efficiently. 

The goal of vetiver is to provide fluent tooling to version, share, deploy, and monitor a trained model. Functions handle both recording and checking the model’s input data prototype, and predicting from a remote API endpoint. The vetiver package is extensible, with generics that can support many kinds of models, i.e., `caret`, `tidymodels`.

The file `vetiver_best_model.R` in the [9.deployment folder](./9.deployment) shows the vetiver workflow as follows:

1. Read the best caret-trained model `best_model_caret.rds` in the [Output folder](./Output)
2. Create a feature test data set from `sample_features.rds` in the [Output folder](./Output)
3. Create vetiver model object
4. Testing the rest API (optional)
5. Version model locally using boards for deployment
6. Generate API + Docker assets, i.e., `Dockerfile`, `plumber.R`, `vetiver_renv.lock`.

## GitHub Actions

GitHub Actions is a continuous integration and continuous delivery (CI/CD) platform that allows you to automate your software development workflows directly within your GitHub repository.

This repository uses GitHub Actions to automate the build and deployment of a Vetiver API as a Docker image. See `deploy.yml` in the [github workflows folder](.github/workflows)

## Docker image

Pull the API image from Docker hub
```bash
docker pull reinpmomz/depression-southafrica-api:latest
```

Run the API image in detached mode
```bash
docker run --name depression-southafrica-api -p 8000:8000 -d --restart unless-stopped reinpmomz/depression-southafrica-api:latest
```

Check that the container is running
```bash
docker ps -a
```

Test API
```bash
curl http://localhost:8000
```

Check logs
```bash
docker logs -f depression-southafrica-api
```
