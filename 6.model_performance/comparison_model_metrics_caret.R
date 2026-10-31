library(dplyr)
library(rstatix)
library(tibble)

working_directory
## Comparison of Metrics for sampling techniques

### Friedman test
#non-parametric alternative to a one-way repeated-measures ANOVA
#typically used when data violates normality assumptions or is ordinal.

#conduct separate Friedman tests for each performance metric
#uses the reported point estimates, not the confidence interval bounds. 
#compares performance across the models rather than treating the confidence intervals as independent observations.

friedman_test_metrics_df <- sapply(as.character(unique(caret_metrics_all_df$scores)), function(x){
  nn <- x
  
  #data in long format
  df_new <- caret_metrics_all_df %>%
    dplyr::filter(scores %in% nn)
  
  #The ML models serve as the blocks, while sampling methods will be the repeated-measures factor (groups).
  
  # Run the Friedman Test format y ~ groups | blocks or a ~ b | c
  out <- df_new %>%
    rstatix::friedman_test(estimate ~ analysis | label)
  
  result <- tibble(
    metric = nn,
    method = out$method,
    n = out$n,
    friedman_chisq = unname(out$statistic),
    df = unname(out$df),
    p_value = out$p
  ) %>%
  mutate(
    p_value = round(p_value, 4),
    significance = ifelse(p_value < 0.05, "Significant",
                          "Not significant")
  )
  
}, simplify=FALSE)


friedman_test_metrics_all_df <- dplyr::bind_rows(friedman_test_metrics_df)

### Post-hoc pairwise comparisons - Wilcoxon signed-rank tests
#identify which sampling strategies differ.

pairwilcox_test_metrics_df <- sapply(as.character(unique(caret_metrics_all_df$scores)), function(x){
  nn <- x
  
  #data in long format
  df_new <- caret_metrics_all_df %>%
    dplyr::filter(scores %in% nn)
  
  #output is a tibble
  out <- df_new %>%
    rstatix::pairwise_wilcox_test(estimate ~ analysis                             
                                  , paired = TRUE
                                  , p.adjust.method = "holm"
                                  , exact = FALSE
                                  , comparisons = NULL
                                  , ref.group = NULL
                                  , id = "label"
                                  #, effect.size = TRUE
                                  )
  result <- out %>%
    dplyr::mutate(metric = nn)
  
}, simplify=FALSE)


pairwilcox_test_metrics_all_df <- dplyr::bind_rows(pairwilcox_test_metrics_df)

## save caret model metrics
writexl::write_xlsx(list( friedman_test = friedman_test_metrics_all_df
                          , pairwilcox_test = pairwilcox_test_metrics_all_df
                          ),
                    path = base::file.path(output_Dir, "comparison_model_metrics_caret.xlsx" )
                    )
