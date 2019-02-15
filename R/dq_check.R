#' Data quality check using association rules
#'
#' Apply the association rules to calculate the rules confidence on the new dataset.
#' We developed the rules for data quality assessment purposes. The changes of rules confidences at the different comparison level
#' can indicate the issues of consistency and completeness of data quality.
#'
#' @param data_input Transaction data in long format
#' @param rules_input The list of association rules from the \code{rules_mining, rules_clean, rules_test or rules_bootstrap} functions
#' @param compare_group Group indicating how data quality will be assessed
#' @param group_var 1 or more Variables used to group rules
#' @param item_id Unique identifier for each transaction
#' @param items The variable indicating the items in a transaction
#'
#' @return
#'   A dataframe with confidence calculated in the new dataset
#'   \item{rules }{A list of rules being tested}
#'   \item{confidence }{Rules confidence from the training data}
#'   \item{rule_category }{Category of rules, defined during the rule mining process}
#'   \item{group_id }{Level of units for rules application}
#'   \item{n_rhs }{Number of transactions in the right hand side of rules}
#'   \item{n_lhs }{Number of transactions in the left hand side of rules}
#'   \item{n_both }{Number of transactions in both right and left hand sides of rules}
#'   \item{confidence_new }{confidence calculate in the new dataset}
#'
#'
#'
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
#'
dq_check <- function(data_input=dad_test, rules_input=rules_1, compare_group="inst",
                    group_var = "age_categories",item_id = "patient_id",
                    items = "icd10_who_d4"){

  if (length(compare_group) == 1) {
    data_input$group_id <- unlist(data_input[,compare_group])
  } else {
    data_input <- unite_(data_input,"group_id",compare_group,remove = FALSE)
  }
  if (length(group_var) == 1) {
    data_input$rule_category <- unlist(data_input[,group_var])
  } else {
    data_input <- unite_(data_input,"rule_category",group_var,remove = FALSE)
  }

  rhs <- rules_input$RHS
  lhs <- str_split(rules_input$LHS,",")
  output <- NULL

  for (i in 1:length(rhs)) {
    index_1 <- which(unlist(data_input[,items])  %in% rhs[i])
    index_2 <- which(data_input$rule_category  %in% rules_input$rule_category[i])
    index <- intersect(index_1,index_2)
    rhs_df <- data_input[index,c("group_id",item_id,items)]
    colnames(rhs_df)[3] <- "dx_rhs"
    colnames(rhs_df)[2] <- "id"

    ### LHs
    index_1 <- which(unlist(data_input[,items])  %in% lhs[[i]])
    index_2 <- which(data_input$rule_category  %in% rules_input$rule_category[i])
    index <- intersect(index_1,index_2)
    lhs_df <- data_input[index,c("group_id",item_id,items)]
    lhs_df <- lhs_df %>% group_by_at(2) %>% mutate(total=n()) %>% filter(total==length(lhs[[i]]))
    colnames(lhs_df)[2] <- "id"
    temp <- inner_join(rhs_df,lhs_df)
    ###### summary the rules performance at the hopsital lelvel
    inst_rhs <- rhs_df %>% group_by_at(1) %>% summarise(n_rhs = n_distinct(id))
    inst_lhs <- lhs_df %>% group_by_at(1) %>% summarise(n_lhs = n_distinct(id))
    inst_both <- temp %>% group_by_at(1) %>% summarise(n_both = n_distinct(id))

    rules_performance <- full_join(inst_rhs,inst_lhs)
    rules_performance <- full_join(rules_performance,inst_both)
    rules_performance$rule_category = rules_input$rule_category[i]
    rules_performance$rules = rules_input$rules[i]
    output <- bind_rows(output,rules_performance)
  }
  output[is.na(output)] <- 0
  output<- inner_join(rules_input[,c("rules","confidence","rule_category")],output)
  output <- mutate(output,confidence_new = n_both/n_lhs)
  return(output)
}
