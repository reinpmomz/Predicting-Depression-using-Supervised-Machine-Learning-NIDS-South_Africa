# Predicting-Depression-using-Supervised-Machine-Learning-NIDS-South_Africa
Predicting Depression in South Africa using Supervised Machine Learning

## Background

Mental health problems particularly depression are a growing public health concern worldwide, and middle-income countries such as South Africa are disproportionally affected causing a significant number of depressed persons to remain unidentified and untreated. Existing studies largely use traditional analytical methods which are often limited by their inability to capture complex, non-linear relationships between risk factors and depression. This study utilizes an advanced analytical method by applying machine learning (ML) to provide accurate detection of depression.

## Setup

We are assuming you have `R Software` and `Rstudio IDE` installed. If not you can download and install [**R software**](https://www.r-project.org/) then followed by [**RStudio/Posit IDE**](https://posit.co/download/rstudio-desktop/).

## Data

The data used for analysis is available from the [**Data First Repository**](https://www.datafirst.uct.ac.za/) under the **National Income Dynamics Study (NIDS)** collection via identifiers https://doi.org/10.25828/e7w9-m033, https://doi.org/10.25828/j1h1-5m16, https://doi.org/10.25828/7pgq-q106, https://doi.org/10.25828/f4ws-8a78, https://doi.org/10.25828/fw3h-v708. One needs to register, and request permission to utilize the publicly available datasets in the Repository. 

After permission is granted, download the NIDS data for the five waves in form of Stata DTA files and save them in a local folder. Each wave has its individual folder that contains 9 Stata DTA files for waves 2 - 5 (Admin_W*_Anon_V*.dta, Adult_W*_Anon_V*.dta, Child_W*_Anon_V*.dta, hhderived_W*_Anon_V*.dta, HHQuestionnaire_W*_Anon_V*.dta, HouseholdRoster_W*_Anon_V*.dta, indderived_W*_Anon_V*.dta, Proxy_W*_Anon_V*.dta, Link_File_W*_Anon_V*.dta) and 8 Stata DTA files for wave 1 (Admin_W1_Anon_V*.dta, Adult_W1_Anon_V*.dta, Child_W1_Anon_V*.dta, hhderived_W1_Anon_V*.dta, HHQuestionnaire_W1_Anon_V*.dta, HouseholdRoster_W1_Anon_V*.dta, indderived_W1_Anon_V*.dta, Proxy_W1_Anon_V*.dta)

- **Data used for analysis:** Wave 1-5 Folders `nids-w1-v7.0.0-stata14`, `nids-w2-v4.0.0-stata14`, `nids-w3-v3.0.0-stata14`, `nids-w4-v2.0.0-stata14` and `nids-w5-v1.0.0-stata14`

## Tools/Materials

1. Shape files for South Africa obtained from [**OCHA - HUMANITARIAN DATA EXCHANGE(HDX)**](https://data.humdata.org/dataset/cod-ab-zaf) are in the _data_ sub-folder of this repository.

2. The `nids_recode_file.xlsx` file in the [2.load_data_and_clean folder](./2.load_data_and_clean) contains: 
    
    1. CES-D cutoff threshold value in the _tools_cutoff_ sheet.

    2. Data dictionary about data of the 5 waves in the _wave1_rename_vars_, _wave2_rename_vars_, _wave3_rename_vars_, _wave4_rename_vars_ and _wave5_rename_vars_ sheets.

    3. Data dictionary about the final merged variables in the _wave1_5_merged_rename_vars_ sheet.
  
    4. Guide about the cleaned dataset that includes new variables created, variables used for descriptive and inferential analysis, variables for visualization and variables used in calculating reliability in the _selected_vars_ sheet. 
   
    5. Variables used in descriptive analysis that are not required for further analysis i.e Machine learning (ML) in the _drop_selected_vars_ sheet.
    
    6. _model_params_ sheet that contains various seed values, test_train_ratio value, corr_threshold value and ML train fold value.
  
    7. _positive_class_ sheet that contains the category label of the positive class in the dataset to be used in ML pipeline.
  
    8. _model_names_ sheet that contains a list of abbreviated ML algorithms and their full names used in the analysis.

    9. _data_names_train_ sheet that contains a list of sampling methods used for the train data in the analysis.

    10. _performance_evaluation_ sheet that contains a list of performance metrics used to for ML evaluation.

## Run

After cloning the repository or downloading the ZIP, you also need the data files (**Data used for analysis**) in the _data_ sub-folder of _Predicting-Depression-using-Supervised-Machine-Learning-NIDS-South_Africa_ folder.

Open `RStudio` then set your working directory to the _Predicting-Depression-using-Supervised-Machine-Learning-NIDS-South_Africa_ folder. 

- If you get data for this project, it is advisable to work on a high computational power `RStudio Server` setup or a laptop/desktop with prefarrably 64GB RAM and run individual files. 

- To run individual files, open the `main.R` script, and run from the beginning.