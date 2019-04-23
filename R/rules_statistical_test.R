#' Statistical association test on the rules
#'
#' Use of chi-square test to test the statistical association between the LHS and RHS of rules.
#' This function used the \code{chisq.test} function and simulated p-values are calculated.
#' The p values are adjusted for multiple comparisons using \code{p.adjust} function. The default method is false discovery rate.
#'
#' @param rules The list of association rules from the \code{rules_mining, rules_clean, rules_test or rules_bootstrap} functions
#' @param data_input The training data used for the rules development
#' @param group_var 1 or more variables used to group rules
#' @param item_id Unique identifier for each transaction
#' @param items The variable indicating the items in a transaction
#' @param method_p_adjustment Methods used to conduct p-value adjustment. See \code{p.adjust} for the list of methods
#'
#' @return
#'  The output is in data.frame format. See functions of \code{rules_mining, rules_clean, rules_test}
#'  for the definition of each column.
#'  \item{support_rhs }{Support for the right hand side of rules}
#'  \item{pvalue_adjusted }{Adjusted P value for chi-square test on the association between the RHS and LHS of rules}
#'
#' @export
#' @examples
#'
#'
#'
#'
#'
rules_statistical_test <- function(rules,data_input,group_var,item_id,items,method_p_adjustment = "fdr"){
  p_value <- NULL
  if (length(group_var) == 1) {
    data_input$rule_category <- unlist(data_input[,group_var])
  } else {
    data_input <- unite_(data_input,"rule_category",group_var,remove = FALSE)
  }

  for(i in 1:dim(rules)[1]){
    temp <- data_input %>% filter(rule_category==rules$rule_category[i])
    ## Right hand side of rules
    id_list <- unique(unlist(temp[,item_id]))
    index_rhs <- unlist(temp[,items]) %in% rules$RHS[i]
    id_rhs <- unique(unlist(temp[index_rhs,item_id]))
    yes_rhs <- id_list %in% id_rhs

    ### Left hand side of rules
    list_lhs <- str_split(rules$LHS,",")
    index_lhs <- unlist(temp[,items]) %in% list_lhs[[i]]
    id_lhs <- unlist(temp[index_lhs,item_id])
    id_lhs <- data.frame(table(id_lhs)) ## Check the frequency of ID to match with length of LHS
    yes_lhs <- as.numeric(as.character(id_lhs[id_lhs$Freq==length(list_lhs[[i]]),1]))
    yes_lhs <- id_list %in% yes_lhs
    table_2by2 <- table(yes_rhs,yes_lhs)
    ## statistical test using fisher.test
    p_value <- c(p_value, chisq.test(table_2by2,simulate.p.value = TRUE)$p.value)
    rules$support_rhs[i] <- length(id_rhs)/length(id_list)
  }
  rules$pvalue_adjusted = p.adjust(p_value,method=method_p_adjustment)
  return(rules)
}
