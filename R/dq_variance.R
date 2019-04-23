#' Data Quality - Variance
#'
#' Variance indicates the variability of data quality within a dataset.
#' To calculate the variance, we first calculated the 95% confidence intervals of pairwise differences
#' between group_id with in the same compare group. Afterwards, the range of confidence intervals is calculated.
#' Weighted difference between group_ids were calculated and sum. The rules are ranked based on the groups.
#'
#' @param rules_results output from \code{dq_check} after applying the rules on the new datasets with the compare group being defined
#' @param compare_group How data quality is assessed between different group_ids.
#'
#' @return
#'  The output is similar as dq_bias except for that rules are ranked based on variance.
#'
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
dq_variance <- function(rules_results,compare_group){
  ### Create a new variable of strata to
  if (length(compare_group) == 1) {
    rules_results$evaluation_group <- unlist(rules_results[,compare_group])
  } else {
    rules_results <- unite_(data_input,"evaluation_group",compare_group,remove = FALSE)
  }

  ############ Variance: Pair-wise difference between hospitals
  rules_results <- rules_results %>%  filter(n_both> 0, n_lhs>0) %>%
    select(rules,rule_category,group_id,n_both,n_lhs,evaluation_group,confidence_new)

  rules_results <- rules_results %>% add_count(rules,rule_category,evaluation_group) %>% mutate(seq=row_number())

  rules_results_1 <- rules_results %>% select(-confidence_new)
  rules_results_2 <- rules_results_1 %>% select(-group_id,-n) %>% rename(n_both_1=n_both,n_lhs_1=n_lhs,seq_1=seq)


  rules_results_pair <- inner_join(rules_results_1,rules_results_2) %>% filter(seq > seq_1) %>% select(-seq,-seq_1)


  rules_results_pair <- rules_results_pair %>%
    mutate(prop_diff = n_both/n_lhs - n_both_1/n_lhs_1,prop_diff_lower = 0,prop_diff_upper=0)

  for (i in 1:dim(rules_results_pair)[1]) {
    res <-  Diff.Score.CI(rules_results_pair$n_both[i],rules_results_pair$n_lhs[i],rules_results_pair$n_both_1[i],
                          rules_results_pair$n_lhs_1[i], alpha = 0.05,t=rules_results_pair$n[i])
    rules_results_pair$prop_diff_lower[i] <- res[1]
    rules_results_pair$prop_diff_upper[i] <- res[2]
  }

  rules_results_pair <- rules_results_pair %>% mutate(range= prop_diff_upper - prop_diff_lower) %>%
    mutate(weighted_diff = abs(prop_diff)*(2-range))

  rules_results_sum <- rules_results_pair %>% group_by(rules,rule_category,evaluation_group,n) %>%
    summarise(weighted_diff_sum=sum(weighted_diff)) %>% ungroup() %>%
    mutate(weighted_diff_avg=weighted_diff_sum*2/(n*(n-1))) %>%
    arrange(rule_category,evaluation_group,desc(weighted_diff_avg)) %>%
    group_by(rule_category,evaluation_group) %>% mutate(rule_rank=row_number())

  rules_results_sum <- inner_join(rules_results_sum,rules_results[,c('evaluation_group','rules','rule_category','group_id',"confidence_new")])
  return(rules_results_sum)
}
