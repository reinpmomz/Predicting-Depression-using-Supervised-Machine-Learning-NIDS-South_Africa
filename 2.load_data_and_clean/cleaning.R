library(dplyr)
library(readr)
library(lubridate)
library(forcats)
library(labelled)
library(tibble)

## Clean dataset

df_clean <- df_merged_final %>%
  dplyr::mutate(across(where(is.factor), ~ forcats::fct_recode(.x, 
                                                               NULL = "Missing"
                                                               , NULL = "-3" #Missing
                                                               , NULL = "Not asked in Phase 2"
                                                               , NULL = "-8" #Refused
                                                               , NULL = "Refused"
                                                               , NULL = "Refuse"
                                                               , NULL = "-9" #Don't Know
                                                               , NULL = "-9. Don't know" #Don't Know
                                                               , NULL = "Don't know"
                                                               , NULL = "Don't Know"
                                                               , NULL = "Dont know"
                                                               , NULL = "-5" #Not Applicable
                                                               , NULL = "Not Applicable"
                                                               , NULL = "Not applicable"
                                                               )
                       ) #recode levels to NULL
                
                , wave_id = as.factor(wave_id) #variable to factor
                , a_intrv_y = as.factor(a_intrv_y) #variable to factor
                , questionnaire = forcats::fct_collapse(questionnaire,
                                                        "Adult" = c("Adult", "1. Adult"),
                                                        "Child" = c("Child", "2. Child"),
                                                        "Proxy" = c("Proxy", "3. Proxy")
                                                        ) #Collapse factor levels for questionnaire type into defined groups
                , r_relhead = forcats::fct_collapse(r_relhead,
                                                    "Resident Head" = c("Resident Head", "Resident head"),
                                                    "Brother or sister-in-law" = c("Brother or sister-in-law", "Brother-or sister-in-law"),
                                                    "Son or daughter-in-law" = c("Son or daughter-in-law", "Son-or daughter-in-law"),
                                                    "Great-grandparent" = c("Great-grandparent", "Great grandparent"),
                                                    "Great-grandchild" = c("Great-grandchild", "Great grandchild"),
                                                    "Father or mother-in-law" = c("Father or mother-in-law", "Father-or mother-in-law"),
                                                    "Step-child" = c("Stepchild", "Step Child"),
                                                    "Step-parent" = c("Stepparent", "Step parent"),
                                                    ) #Collapse factor levels for r_relhead into defined groups
                , across(c(a_intrv_m, c_intrv_m, p_intrv_m, h_intrv_m, best_dob_m, h_mrtdod_m1, h_mrtdod_m2, h_mrtdod_m3,
                           h_mrtdod_m4, h_mrtdod_m5), ~forcats::fct_recode(.x,
                                                                           "1" = "January",
                                                                           "2" = "February",
                                                                           "3" = "March",
                                                                           "4" = "April",
                                                                           "5" = "May",
                                                                           "6" = "June",
                                                                           "7" = "July",
                                                                           "8" = "August",
                                                                           "9" = "September",
                                                                           "10" = "October",
                                                                           "11" = "November",
                                                                           "12" = "December" )
                         ) #recode month levels to number
                , across(c(a_intrv_d, c_intrv_d), ~readr::parse_number(as.character(.x))
                         ) #day columns are factor. Extract number in character
                , across(c(a_intrv_y, p_intrv_y, best_dob_y, h_mrtdod_y1, h_mrtdod_y2, h_mrtdod_y3, h_mrtdod_y4, h_mrtdod_y5,
                           a_intrv_m, c_intrv_m, p_intrv_m, h_intrv_m, best_dob_m, h_mrtdod_m1, h_mrtdod_m2, h_mrtdod_m3,
                           h_mrtdod_m4, h_mrtdod_m5, r_head, h_mrtage1, h_mrtpid1, h_mrtage2, h_mrtpid2, h_mrtage3, h_mrtpid3,
                           h_mrtage4, h_mrtpid4, h_mrtage5, h_mrtpid5 ), ~as.numeric(as.character(.x))
                         ) #year/month/age/pid death columns are factor. Convert to numeric
                , h_mrtcau1_o = if_else(h_mrtcau1_o == "-3"| h_mrtcau1_o == "", NA, h_mrtcau1_o)
                , h_mrtcau1_o = as.factor(h_mrtcau1_o)
                , h_mrtcau1_o = forcats::fct_collapse(h_mrtcau1_o,
                                                      "Cancer" = c("Lung Cancer.", "Cancer Disease", "Cancer Of Throat", "Cancer",
                                                                   "Breast Cancer", "Brain Cancer."),
                                                      "Diabetes" = c("Sugar Diabetes.", "Diabetes", "High Blood Sugar"),
                                                      "Pneumonia" = c("Pnimonia", "Pnuemonia", "Neumonia", "Pneumonia"),
                                                      "Kidney Diseases" = c("Renal Failer", "Kidney Failure."),
                                                      "Brain Diseases" = c("Menengitis", "Meningitis", "Epilepsi", "Apilepcy", "Mennigitis",
                                                                           "Parkensons Disease"
                                                                           ),
                                                      "Lung Diseases" = c("Asthma", "Ashma .", "Lung Infection", "Lung Disases"),
                                                      "Heart Disease" = c("Heart Attack", "Congestive Cardiac Failure", "Cough And Heart Attack"),
                                                      "Suicide" = c("Suicide", "Commite Saucide"),
                                                      "Violence" = c("He Snabbed By Gengstars", "Raped And Killed", "Robbed And Stabbed",
                                                                     "Been Kidnaped And Found After A Month Killed In Other Area Around Bfn."
                                                                     ),
                                                      "Accidental i.e drowning, choking, electric" = c("She Had A Bone Stuck In Her Troght",
                                                                                                       "Eletric", "Drowned",
                                                                                                       "He Died When The Mine He Worked In Collapsed." 
                                                                                                       ),
                                                      "Other" = c("She Could Not Go To The Toilet On Her Own.", "Headach", 
                                                                  "Something Like", "Headache", "Vaal Sick", "Had A Certain Illness",
                                                                  "Arthritis", "Maag Op Gestoot", "Cold", "Appendix", "Ulcers",
                                                                  "Just Got Sick And Past Away", "Back Pain", "Headache And Painning Feet",
                                                                  "Was Sick But Dont Know Exactly", "He Was Bewitched", "Went For Operation",
                                                                  "She Just Became Weak,They Don'T Know The Cause Of Death.",
                                                                  "Had Something Like Sours In His Chest", "He Was Sick.", "Short Illness",
                                                                  "Leg Problems", "He Got Very Sick", "She Was Sick.", "Unknown Disease"
                                                                  )
                                                      ) #Collapse factor levels for a_rel into defined groups
                , a_rel = forcats::fct_collapse(a_rel, "Other (specify)" = c("Other", "Other (specify)")
                                                ) #Collapse factor levels for a_rel into defined groups
                , a_hldes = forcats::fct_collapse(a_hldes, "Very Good" = c("Very Good", "Very good")
                                                ) #Collapse factor levels for a_hldes into defined groups
                , a_wbsat = forcats::fct_collapse(a_wbsat, 
                                                  "Satisfaction level 1 (Very dissatisfied)" = c("Satisfaction level 1 (Very dissatisfied)",
                                                                                                 "Satisfaction level 1"),
                                                  "Satisfaction level 10 (Very satisfied)" = c("Satisfaction level 10 (Very satisfied)",
                                                                                                 "Satisfaction level 10")
                                                  ) #Collapse factor levels for a_wbsat into defined groups
                , across(c(a_height_1:a_bppls_2, best_age_yrs, h_dwlrms), ~as.numeric(as.character(.x))
                         ) #factor variables to numeric
                , across(c(a_hl30fl:a_hl30i, a_hltb, a_hltb_med:a_hlbp, a_hlbp_med:a_hldia, a_hldia_med:a_hlstrk,
                           a_hlstrk_med:a_hlast, a_hlast_med:a_hlhrt, a_hlcan, a_hlvisaid, a_hlhraid, a_hllfsmk), ~forcats::fct_rev(.x)
                         ) #factor reverse symptoms/health condition variables from Yes/No to No/Yes
                , across(c(h_ownd, h_enrgelec, h_telcel, h_ownrad:h_ownvehpri, h_ownmot:h_ownmll, h_tellnd,
                           h_refrem, h_strlght), ~forcats::fct_rev(.x)
                         ) #factor reverse ses variables from Yes/No to No/Yes
                , h_dwltyp = forcats::fct_collapse(h_dwltyp,
                                                   "Informal dwelling/shack not in backyard, e.g. in an informal/ squatter settlement or on farm" =
                                                     c("Informal dwelling/shack not in backyard, e.g. in an informal/ squatter settlement or on farm",
                                                       "Informal dwelling/shack not in backyard")
                                                   ) #Collapse factor levels for type of dwelling into defined groups
                , h_watsrc = forcats::fct_collapse(h_watsrc, 
                                                   "Other (specify)" = c("Other", "Other (specify)", "Other (Specify)",
                                                                         "From neighbours", "Neighbour"
                                                                         )
                                                   ) #Collapse factor levels for h_watsrc into defined groups
                , across(c(h_enrgck, h_enrght), ~forcats::fct_collapse(.x,
                                                                       "Animal dung" = c("Animal dung", "Animal Dung"),
                                                                       "Solar energy" = c("Solar energy", "Solar Energy")
                                                                       )
                         ) #Collapse factor levels for h_enrgck/h_enrght into defined groups
                , h_enrglght = forcats::fct_collapse(h_enrglght, "Solar energy" = c("Solar energy", "Solar Energy")
                                                     ) #Collapse factor levels for h_enrglght into defined groups
                , h_toi = forcats::fct_collapse(h_toi, 
                                                "Flush toilet with onsite disposal (septic tank/soak-away)" = 
                                                  c("Flush toilet with onsite disposal (septic tank/soak-away)",
                                                    "Flush toilet with onsite disposal (septic tank / soak-away)")
                                                ) #Collapse factor levels for h_toi into defined groups
                , across(c(h_nbthmf:h_freqdrug), ~forcats::fct_collapse(.x, "Very Rare" = c("Very Rare", "Very rare")
                                                                        ) #Collapse factor levels 
                         )
                , marital_status = if_else(is.na(r_marstt), best_marstt, r_marstt
                                           ) #coalesce marital status for waves1_2_3 with waves 4_5
                , marital_status = forcats::fct_collapse(marital_status, 
                                                         "Never Married" = c("Never Married", "Never married"),
                                                         "Living with Partner" = c("Living with Partner", "Living with partner"),
                                                         "Divorced or Separated" = c("Divorced or Seperated", "Divorced or separated")
                                                         ) #Collapse factor levels for marital status into defined groups
                , occupation_status = if_else(is.na(empl_stat), empl_stat_inclprox, empl_stat
                                              ) #coalesce employment status for adults only with proxy
                , occupation_status = forcats::fct_collapse(occupation_status, 
                                                            "Unemployed" = c("Unemployed_Discouraged", "Unemployed_Strict")
                                                            ) #Collapse factor levels for occupation_status into defined groups
                , education_status = best_edu
                , education_status = forcats::fct_collapse(education_status,
                                                           "No Schooling" = c("No Schooling", "25. No Schooling"
                                                                              ),
                                                           "Other (Specify)" = c("Other", "Other (Specify)", "Other (specify)",
                                                                                 "24. Other (specify)", "Others (Specify)"
                                                                                 ),
                                                           "Primary School" = c("Grade R/0", "Grade 1/Sub A/Class 1",
                                                                                "Grade 2/Sub B/Class 2", "Grade 3/Std. 1",
                                                                                "Grade 4/Std. 2", "Grade 5/Std. 3","Grade 6/Std. 4",
                                                                                "Grade 7/Std. 5", "Grade 7/ Std. 5", "0. Grade R/0",
                                                                                "1. Grade 1/Sub A/Class 1", "2. Grade 2/Sub B/Class 2",
                                                                                "3. Grade 3/Std. 1", "4. Grade 4/Std. 2",
                                                                                "5. Grade 5/Std. 3", "6. Grade 6/Std. 4",
                                                                                "7. Grade 7/Std. 5", "Grade 1 (Previously Sub A/Class 1)",
                                                                                "Grade 2 (Previously Sub B/Class 2)", "Grade 3 (Std. 1)",
                                                                                "Grade 4 (Std. 2)", "Grade 5 (Std. 3)", "Grade 6 (Std. 4)",
                                                                                "Grade 7 (Std. 5)"
                                                                                ),
                                                           "Secondary School" = c("Grade 8/Std. 6/Form 1", "Grade 9/Std. 7/Form 2",
                                                                                  "Grade 10/Std. 8/Form 3", "Grade 11/Std. 9/Form 4",
                                                                                  "Grade 12/Std. 10/Form 5/Matric/Senior Certificate",
                                                                                  "Grade 8/ Std. 6/Form 1", "Grade 10/ Std. 8/Form 3",
                                                                                  "Grade 11/ Std. 9/Form 4", "8. Grade 8/Std. 6/Form 1",
                                                                                  "9. Grade 9/Std. 7/Form 2", "10. Grade 10/Std. 8/Form 3",
                                                                                  "11. Grade 11/Std. 9/Form 4", 
                                                                                  "12. Grade 12/Std. 10/Form 5/Matric/Senior Certificate",
                                                                                  "Grade 8 (Std. 6/Form 1)", "Grade 9 (Std. 7/Form 2)",
                                                                                  "Grade 10 (Std. 8/Form 3)", "Grade 11 (Std. 9/Form 4)",
                                                                                  "Grade 12 (Std. 10/Matric/Senior Certificate/Form 5)"
                                                                                  ),
                                                           "College" = c("NTC 1", "NTC 2", "NTC 3",
                                                                         "Certificate with less than Grade 12/Std. 10",
                                                                         "Diploma with less than Grade 12/Std. 10",
                                                                         "Certificate with less than Grade 12/Std 10",
                                                                         "Diploma with less than Grade 12/Std 10",
                                                                         "NTC 1 /NCV 2", "NTC 2 /NCV 3", "NTC 3 /NCV 4",
                                                                         "13. NTC 1 /NCV 2", "14. NTC 2 /NCV 3", "15. NTC 3 /NCV 4",
                                                                         "16. Certificate not requiring Grade 12/Std. 10",
                                                                         "17. Diploma not requiring Grade 12/Std. 10",
                                                                         "NTC 2/NCV 3", "NTC 3/NCV 4",
                                                                         "Certificate not requiring Grade 12/Std. 10",
                                                                         "Diploma not requiring Grade 12/Std. 10",
                                                                         "National Certificate Vocational 2 (NCV 2)",
                                                                         "National Certificate Vocational 3 (NCV 3)",
                                                                         "National Certificate Vocational 4 (NCV 4)",
                                                                         "N1 (NATED)/NTC 1", "N2 (NATED)/NTC 2", "N3 (NATED)/NTC 3",
                                                                         "Certificate with Grade 12/Std. 10",
                                                                         "Certificate with Grade 12/Std 10",
                                                                         "18. Certificate requiring Grade 12/Std. 10",
                                                                         "Certificate requiring Grade 12/Std. 10"
                                                                         ),
                                                           "University" = c("Bachelors degree", "Bachelors degree and Diploma",
                                                                            "Honours degree", "Higher degree (Masters, Doctorate)",
                                                                            "Bachelors Degree", "Bachelors Degree and Diploma",
                                                                            "Honours Degree", "Higher Degree (Masters, Doctorate)",
                                                                            "20. Bachelors Degree", "21. Bachelors Degree and Diploma",
                                                                            "22. Honours Degree", "23. Higher Degree (Masters, Doctorate)",
                                                                            "Bachelors Degree and diploma", 
                                                                            "Diploma with Grade 12/Std. 10",
                                                                            "Diploma with Grade 12/Std 10", 
                                                                            "Diploma with  Grade 12/Std. 10",
                                                                            "Diploma with Grade 12/Std. 10",
                                                                            "19. Diploma requiring Grade 12/Std. 10",
                                                                            "Diploma requiring Grade 12/Std. 10"
                                                                            )
                                                           ) #Collapse factor levels for education_status into defined groups
                # , across(c(a_emobth:a_emogo, marital_status, a_height_1:a_bppls_2, r_gen, best_gen, best_race),
                #          ~ forcats::fct_recode(.x, NULL = "Don't know", NULL = "Don't Know")
                #          ) #recode Don't know levels to NULL
                , across(c(a_emohope, a_emohap), ~ forcats::fct_rev(.x)
                         ) #Reverse factor levels of Q5 and Q8 of CES-D
                , cesd_total_score = rowSums(across(c(a_emobth:a_emogo), ~as.numeric(.x)-1
                                                    ), na.rm = TRUE
                                             )
                , cesd_total_score = if_else(rowSums(across(c(a_emobth:a_emogo), ~is.na(.x))) > round(0.2*10,0), NA,
                                             cesd_total_score 
                                             ) #If more than 20% of total columns have missing, replace with NA
                , cesd_depression = if_else(cesd_total_score < tools_cutoff_df$cesd_cutoff, "No", "Yes"
                                            )
                , adult_height = round(rowMeans(across(c(a_height_1:a_height_3)), na.rm = TRUE)/100
                                     ,2
                                     ) #creating average height in meters for adults
                , adult_weight = round(rowMeans(across(c(a_weight_1:a_weight_1)), na.rm = TRUE)
                                       ,1
                                       ) #creating average weight in Kgs for adults
                , adult_bmi = round(adult_weight/((adult_height)^2)
                                    ,2
                                    ) #creating adult BMI
                , adult_bmi_group = ifelse(adult_bmi <18.5, "Underweight (<18.5)",
                                           ifelse(adult_bmi <25, "Normal (18.5–24.9)",
                                                  ifelse(adult_bmi <30, "Overweight (25–29.9)", "Obese (>=30)"
                                                         )
                                                  )
                                           ) #Creating adult BMI group
                , adult_waist = round(rowMeans(across(c(a_waist_1:a_waist_3)), na.rm = TRUE)
                                      ,1
                                      ) #creating average waist in cm for adults
                , adult_bp_systolic = round(rowMeans(across(c(a_bpsys_1, a_bpsys_2)), na.rm = TRUE)
                                            ,0
                                            ) #creating average Systolic BP for adults
                , adult_bp_diastolic = round(rowMeans(across(c(a_bpdia_1, a_bpdia_2)), na.rm = TRUE)
                                             ,0
                                             ) #creating average Diastolic BP for adults
                , adult_bp_pulse = round(rowMeans(across(c(a_bppls_1, a_bppls_2)), na.rm = TRUE)
                                         ,0
                                         ) #creating average Pulse BP for adults
                , physically_handicapped = if_else(a_hlser1 == "Physically handicapped" | a_hlser2 == "Physically handicapped" 
                                                   | a_hlser3 == "Physically handicapped",
                                                   "Yes", "No") #creating physically_handicapped column
                , sight_hearing_speech_problem = if_else(a_hlser1 == "Problems with sight, hearing or speech" 
                                                         | a_hlser2 == "Problems with sight, hearing or speech" 
                                                         | a_hlser3 == "Problems with sight, hearing or speech",
                                                         "Yes", "No") #creating sight_hearing_speech_problem column
                , hiv_aids = if_else(a_hlser1 == "HIV/AIDS" | a_hlser2 == "HIV/AIDS" 
                                                   | a_hlser3 == "HIV/AIDS", "Yes", "No") #creating HIV/AIDS column
                , epilepsy = if_else(a_hlser1 == "Epilepsy/ fits" | a_hlser2 == "Epilepsy/ fits"
                                     | a_hlser3 == "Epilepsy/ fits", "Yes", "No") #creating epilepsy column
                , across(c(physically_handicapped, sight_hearing_speech_problem, hiv_aids, epilepsy), ~if_else(is.na(.x) & a_hlser == "No", "No", .x)
                         )
                , negative_event_death = if_else(h_mrt24mnth == "Yes" | h_negdthf == "Yes" | h_negdthfr == "Yes"
                                                 | h_negdtho == "Yes" , "Yes", "No"
                                                 ) #Creating negative events from death column
                , negative_event_death = if_else(is.na(negative_event_death), h_mrt24mnth, negative_event_death)
                , negative_event_injury_other = if_else(h_negill == "Yes" | h_nego == "Yes" , "Yes", "No"
                                                        ) #Creating negative events from serious injury/other column 
                , negative_event_injury_other = if_else(is.na(negative_event_injury_other), h_nego, negative_event_injury_other)
                , negative_event_loss_property = if_else(h_negstc == "Yes" | h_negcrp == "Yes" | h_negpro == "Yes", "Yes", "No"
                                                         ) #Creating negative events from destruction of property column 
                , across(where(is.numeric), ~replace(.x, is.nan(.x), NA)
                         ) #Replace NAN with NA
                , across(c(h_dwltyp, h_dwlmatroof, h_dwlmatrwll, h_watsrc, h_toi, h_enrgck, h_enrght, h_enrglght,
                           h_dwlmatflr, occupation_status), ~as.character(.x)
                         ) #variables to character class
                , household_head = if_else(r_relhead == "Resident Head", "Yes", "No")
                , occupation_status = if_else(a_ems == "Yes", "Self-employed", occupation_status)
                , across(c(h_enrgck, h_enrght), ~ factor(.x, levels = c("None", "Other (specify)", "Animal dung", "Wood", "Paraffin",
                                                                        "Coal", "Electricity from generator", "Solar energy", "Gas", 
                                                                        "Electricity from mains")
                                                         )
                         ) #columns to manually defined factors
                , h_enrglght = factor(h_enrglght, levels = c("None", "Other (specify)", "Candles", "Paraffin", "Electricity from generator",
                                                             "Solar energy", "Gas", "Electricity from mains") 
                                      ) #h_enrglght column to manually defined factors
                , h_toi = factor(h_toi, levels = c("None", "Other (specify)", "Bucket toilet", "Pit latrine without ventilation pipe",
                                                   "Pit latrine with ventilation pipe (VIP)", "Chemical toilet",
                                                   "Flush toilet with onsite disposal (septic tank/soak-away)",
                                                   "Flush toilet with offsite disposal"
                                                   ) 
                                 ) #h_toi column to manually defined factors
                , h_watsrc = factor(h_watsrc, levels = c("Other (specify)", "Dam/pool/stagnant water", "Flowing water/stream",
                                                         "Spring", "Well", "Water-Carrier/tanker", "Borehole off site/communal",
                                                         "Borehole on site", "Rain-water tank on site", "Public tap", 
                                                         "Piped (tap) water on site or in yard", "Piped (tap) water in dwelling"
                                                         ) 
                                    ) #h_watsrc column to manually defined factors
                , h_dwlmatflr = factor(h_dwlmatflr, levels = c("Mud/Earth", "Wood", "Linoleum/Vinyl", "Carpet", "Concrete", "Tiles") 
                                       ) #h_dwlmatflr column to manually defined factors
                , h_dwlmatroof = factor(h_dwlmatroof, levels = c("Plastic", "Cardboard", "Thatching", "Wood", "Stone and rock",
                                                                 "Wattle and daub", "Mixture of mud and cement", "Corrugated iron/zinc",
                                                                 "Mud bricks", "Bricks", "Asbestos/cement roof sheeting",
                                                                 "Cement block/concrete", "Tiles")
                                        ) #h_dwlmatroof column to manually defined factors
                , h_dwlmatrwll = factor(h_dwlmatrwll, levels = c("Plastic", "Cardboard", "Thatching", "Wood", "Stone and rock",
                                                                 "Wattle and daub", "Mixture of mud and cement", "Corrugated iron/zinc",
                                                                 "Mud bricks", "Tiles", "Asbestos/cement roof sheeting",
                                                                 "Cement block/concrete", "Bricks")
                                        ) #h_dwlmatrwll column to manually defined factors
                , h_dwltyp = factor(h_dwltyp, levels = c("Other (specify)", "Caravan/tent",
                                                         "Traditional dwelling/hut/structure made of traditional materials",
                                                         "Informal dwelling/shack not in backyard, e.g. in an informal/ squatter settlement or on farm",
                                                         "Informal dwelling/shack in backyard", "Room/flatlet", "Unit in retirement village",
                                                         "Dwelling/house or brick structure on a separate stand or yard or on farm",
                                                         "Dwelling/house/flat/room in backyard",
                                                         "Flat or apartment in a block of flats", 
                                                         "Town/cluster/semi-detached house (simplex, duplex or triplex)"
                                                         )
                                    ) #h_dwltyp column to manually defined factors
                , occupation_status = factor(occupation_status, levels = c("Not Economically Active", "Unemployed",
                                                                           "Self-employed","Employed")
                                             ) #occupation_status column to manually defined factors 
                , across(c(cesd_depression, household_head, physically_handicapped, sight_hearing_speech_problem, 
                           hiv_aids, epilepsy, negative_event_death, negative_event_injury_other, negative_event_loss_property),
                         ~ factor(.x, levels = c("No", "Yes"))
                         ) #cesd_depression column to manually defined factors 
                , adult_bmi_group = factor(adult_bmi_group, levels = c("Underweight (<18.5)", "Normal (18.5–24.9)",
                                                                       "Overweight (25–29.9)", "Obese (>=30)")
                                           ), #adult BMI group column to manually defined factors 
                
         ) %>% 
  labelled::set_variable_labels( #creating labels for new variables
    marital_status = "Marital status",
    household_head = "Household head",
    occupation_status = "Occupation status",
    education_status = "Highest level of education",
    cesd_total_score = "CESD-10 total score",
    cesd_depression = "CESD-10 depression",
    adult_height = "Height measure (m)",
    adult_weight = "Weight measure (kg)",
    adult_bmi = "BMI (kg/m2)",
    adult_bmi_group = "BMI (kg/m2) grouped",
    adult_waist = "Waist measure (cm)",
    adult_bp_systolic = "Systolic Blood Pressure (SYS.mmHg)",
    adult_bp_diastolic = "Diastolic Blood Pressure (DIA.mmHg)",
    adult_bp_pulse = "Blood Pressure: Pulse (bpm)",
    physically_handicapped = "Physically handicapped",
    sight_hearing_speech_problem = "Problems with sight, hearing or speech",
    hiv_aids = "Diagnosed with HIV/AIDS",
    epilepsy = "Diagnosed with Epilepsy",
    negative_event_death = "Death of household member/relative/friend in the last 24 months",
    negative_event_injury_other = "Serious illness/injury of household member or any other negative event in the last 24 months",
    negative_event_loss_property = "Loss/destruction of property in the last 24 months"
    ) %>%
  labelled::set_variable_labels(!!!new_labels[names(new_labels) %in% names(.)]
                                ) #labeling variables from data dictionary


## saving clean dataset

