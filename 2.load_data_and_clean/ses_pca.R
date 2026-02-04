library(dplyr)
library(factoextra)
library(labelled)
library(tidyr)
library(tibble)


## group ses variables 
### if empty vector use character()

ses_pca_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "ses pca" & !is.na(selected_vars_df$select_group)]
id_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "hh_id" & !is.na(selected_vars_df$select_group)]
wave_id_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "wave_id" & !is.na(selected_vars_df$select_group)]

if (length(ses_pca_vars)>1) {
  
ses_df_1 <- df_clean %>%
  dplyr::select(any_of(c(id_vars, ses_pca_vars, wave_id_vars)), -c(h_dwlrate, h_dwlmatflr)
                ) %>%
  dplyr::filter(.data[[wave_id_vars]] == "w1") %>%
  dplyr::mutate(across(any_of(ses_pca_vars), ~as.numeric(.x))
         ) %>%
  dplyr::distinct(.data[[id_vars]], .data[[wave_id_vars]], .keep_all = TRUE) %>%
  tidyr::drop_na()

ses_df_2 <- df_clean %>%
  dplyr::select(any_of(c(id_vars, ses_pca_vars, wave_id_vars))
                ) %>%
  dplyr::filter(.data[[wave_id_vars]] != "w1") %>%
  dplyr::mutate(across(any_of(ses_pca_vars), ~as.numeric(.x))
                ) %>%
  dplyr::distinct(.data[[id_vars]], .data[[wave_id_vars]], .keep_all = TRUE) %>%
  tidyr::drop_na()

## Perform PCA

### prcomp() uses the singular value decomposition (SVD) which examines the covariances / correlations between individuals
ses_pca_1 <- stats::prcomp(ses_df_1 %>% dplyr::select(-any_of(c(id_vars, wave_id_vars))),
                           center = FALSE, scale. = TRUE
                           )

ses_pca_2 <- stats::prcomp(ses_df_2 %>% dplyr::select(-any_of(c(id_vars, wave_id_vars))),
                           center = FALSE, scale. = TRUE
                           )

ses_pca_summ_1 <- summary(ses_pca_1)
ses_pca_summ_2 <- summary(ses_pca_2)

print(ses_pca_summ_1)
print(ses_pca_summ_2)

### The first principal component explains the largest proportion of the total variance and it is used as the wealth index to represent 
### the household's wealth
ses_scores_1 <- ses_pca_1$x[, 1, drop=TRUE]
ses_scores_2 <- ses_pca_2$x[, 1, drop=TRUE]

ses_loadings_1 <- ses_pca_summ_1$rotation[, 1, drop=TRUE]
ses_loadings_2 <- ses_pca_summ_2$rotation[, 1, drop=TRUE]

if(min(sign(ses_loadings_1)) != max(sign(ses_loadings_1))){
	stop("PC1 is not a positively signed index - 1")
}

if(min(sign(ses_loadings_2)) != max(sign(ses_loadings_2))){
  stop("PC1 is not a positively signed index- 2")
}

ses_report_1 <- paste0(paste0(ses_pca_vars[ses_pca_vars %in% names(ses_df_1)], collapse=", "), " used to create SES index - 1")
ses_report_2 <- paste0(paste0(ses_pca_vars[ses_pca_vars %in% names(ses_df_2)], collapse=", "), " used to create SES index - 2")
print(ses_report_1)
print(ses_report_2)

## Variance explained plot

### Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
ses_explained_plot_1 <- factoextra::fviz_screeplot(ses_pca_1, addlabels = TRUE)
ses_explained_plot_2 <- factoextra::fviz_screeplot(ses_pca_2, addlabels = TRUE)
print(ses_explained_plot_1)
print(ses_explained_plot_2)

ggsave(plot=ses_explained_plot_1, height = 7, width = 10,
       filename = "ses_scree_plot_1.png", path = output_plots_Dir, bg='white')

ggsave(plot=ses_explained_plot_2, height = 7, width = 10,
       filename = "ses_scree_plot_2.png", path = output_plots_Dir, bg='white')

## Principal Component plots

### Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides 
###of the graph.

ses_pc_plot_1 <- fviz_pca_var(ses_pca_1,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE     # Avoid text overlapping
                            )

ses_pc_plot_2 <- fviz_pca_var(ses_pca_2,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE     # Avoid text overlapping
                            )

print(ses_pc_plot_1)
print(ses_pc_plot_2)

ggsave(plot=ses_pc_plot_1, height = 7, width = 10,
       filename = "ses_pc_plot_1.png", path = output_plots_Dir, bg='white')

ggsave(plot=ses_pc_plot_2, height = 7, width = 10,
       filename = "ses_pc_plot_2.png", path = output_plots_Dir, bg='white')

## adding ses columns to data set

ses_df <- dplyr::bind_rows(ses_df_1 %>%
                             dplyr::select(any_of(c(id_vars, wave_id_vars))) %>%
                             dplyr::mutate(ses_scores = ses_scores_1),
                           ses_df_2 %>%
                             dplyr::select(any_of(c(id_vars, wave_id_vars))) %>%
                             dplyr::mutate(ses_scores = ses_scores_2)
                           )

df_final <- df_clean %>%
  dplyr::left_join(ses_df,
                   by = c( id_vars, wave_id_vars) 
                   ) %>%
  dplyr::mutate(ses_index = cut(ses_scores, breaks = 5, 
                         labels = c("Lowest", "Middle low", "Middle", "Middle high", "Highest"))
                ) %>%
  labelled::set_variable_labels(#labeling created variables
    ses_scores = "Socio-economic status scores",
    ses_index = "Socio-economic status"
  )

## creating data dictionary
final_attribute <- base::as.data.frame(labelled::generate_dictionary(df_final, labels = TRUE, values = TRUE))

## Creating a named vector to quickly assign the variable labels
final_labels <- final_attribute%>%
  dplyr::select(variable, label)%>%
  tibble::deframe()
} else {
  
  df_final <- df_clean
  
  ## creating data dictionary
  final_attribute <- base::as.data.frame(labelled::generate_dictionary(df_final, labels = TRUE, values = TRUE))
  
  ## Creating a named vector to quickly assign the variable labels
  final_labels <- final_attribute %>%
    dplyr::select(variable, label) %>%
    tibble::deframe()
  
}

