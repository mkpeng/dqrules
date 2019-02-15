#' Data quality - Bias
#'
#' Bias is calculated using a reference standard. The weighted difference of rules confidence
#' between new dataset and reference standard are calculated and ranked for each category of rules.
#'
#'
#' @param rules_results output from \code{dq_check} after applying the rules on the new datasets with the compare group being defined
#' @param compare_group How data quality is assessed between different group_ids.
#'
#'
#' @details
#' A reference standard is selected for bias calculation.
#' First, confidence intervals of rules in the new datasets are calculated.
#' then the range of confidence intervals is dervied.
#' The difference between rule confidence in the new dataset and reference standard is calcualted and weighted by (1- range).
#' The smaller range, the higher weight. The sum of weighted difference is calculated and ranked based on the compare_group.
#'
#' @return
#'  The rules are ranked based on the bias.
#'  \item{rules}{list of rules}
#'  \item{rule_category }{Category of rules, defined during the rule mining process}
#'  \item{evalulation_group }{Combination of variables specified in the \code{compare_group}, level of unit of rules comparison}
#'  \item{confidence }{Confidence used as the reference standard}
#'  \item{confi_diff_sum }{Weighted sum of rules difference}
#'  \item{rules_rank }{Rank of rules based on the bias (weighted sum of rules difference)}
#'  \item{group_id }{Level of unit for rule application}
#'  \item{confidence_new }{Confidence of rules in the new datasets}
#'
#' @export
#' @importFrom DescTools BinomCI
#' @examples
#'
#'
#'
#'
#'
dq_bias <- function(rules_results=dq_d4,compare_group="hospital_type",confidence_ref="confidence"){

  if (nchar(compare_group)[1]==0) {
    stop("The Compare group is not defined!,Pls create a dummy compare group variables if not available")

  } else if (!(compare_group %in% colnames(rules_results))){
    stop("The variable of", compare_group," does not exist in the data")
  }

  if (length(compare_group) == 1) {
    rules_results$evaluation_group <- unlist(rules_results[,compare_group])
  } else {
    rules_results <- unite_(data_input,"evaluation_group",compare_group,remove = FALSE)
  }

  if (confidence_ref!="confidence"){
    rules_results$confidence = unlist(rules_results[,confidence_ref])
  }

  ### Create a new variable of strata to
  rules_results$confidence_low <- 0
  rules_results$confidence_upper <- 0
  rules_results$range <- 0

  ### Calculate the confidence intervals of rules confidence
  for (i in 1:dim(rules_results)[1]) {
    est_ci <- BinomCI(x=rules_results$n_both[i], n=rules_results$n_lhs[i], method="wilson")
    est_ci <- round(est_ci,3)
    rules_results$confidence_low[i] <- est_ci[2]
    rules_results$confidence_upper[i] <- est_ci[3]
  }
  rules_results$confi_diff <- abs(rules_results$confidence - rules_results$confidence_new)
  rules_results <- mutate(rules_results,range=confidence_upper-confidence_low)
  rules_results <- rules_results %>% filter(!is.nan(confidence_new))%>% mutate(confi_diff_weighted= (1-range)*confi_diff)

  ### Calculate the bias
  rules_results_sum <- rules_results %>% select(confi_diff_weighted,rules,rule_category,evaluation_group,confidence) %>%
    group_by_at(-1) %>% summarise(confi_diff_sum = sum(confi_diff_weighted)) %>% ungroup() %>%
    arrange(evaluation_group,rule_category,desc(confi_diff_sum))
  ### Rank the rules
  rules_results_sum <- rules_results_sum %>% group_by(evaluation_group,rule_category) %>% mutate(rules_rank = row_number())
  rules_results_sum <- inner_join(rules_results_sum,rules_results[,c("evaluation_group",'group_id','rules','rule_category',"confidence_new")])
  return(rules_results_sum)
}
