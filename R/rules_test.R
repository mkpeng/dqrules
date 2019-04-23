#' Rule test on the test or validation data
#'
#' Apply the list of rules on the test or validation data to calculate support and confidence on the new datasets
#'
#' @param rules The list of association rules from the \code{rules_clean} or \code{rules_mining} functions
#' @param test_data Test or validation dataset
#' @param item_id Unique identifier for each transaction
#' @param group_var 1 or more variables used to group rules
#' @param items The variable indicating the items in a transaction
#'
#' @return
#'   The output is smilar to output from \code{rules_clean} function with support and confidence calculated on the test or validation dataset
#'   \item{support_n_test }{number of transactions with codes from both RHS and LHS for the corresponding rule category}
#'   \item{lhs_n_test }{Number of transactions with codes from the LHS for the corresponding rule category}
#'   \item{confidence_test }{confidence of rules on the test or validation datasets}
#'   \item{support_test }{Support of rules on on the test or validaiton datasets}
#'
#'
#' @export
#'
#' @examples
#'
#'
#'
rules_test <- function(rules,test_data,item_id,group_var,items){

  ##########  configure the output file
  if (length(group_var) == 1) {
    test_data$rule_category <- unlist(test_data[,group_var])
  } else {
    test_data <- unite_(test_data,"rule_category",group_var,remove = FALSE)
  }
  output_test <- matrix(0,dim(rules)[1],4)

  ### Prepare the rules for examination
  #### split the left hand side
  LHS <- strsplit(rules$LHS,",")
  lhs_vector <- unlist(LHS)
  #### List of codes for right hand side
  lhs_list <- unique(lhs_vector)
  RHS <- rules$RHS
  #### list of codes for left hand side
  rhs_list <- unique(RHS)
  ### List of codes in right and left hand side
  codes_list <- union(lhs_list,rhs_list)

  #### loop for calculating the confidence and support in the test DAD
  for (j in 1:dim(rules)[1]){
    temp <- test_data %>% filter(rule_category==rules$rule_category[j])
    id_lhs <- unlist(temp[,items]) %in% LHS[[j]]
    id_lhs <- unlist(temp[id_lhs,item_id])
    id_lhs <- as.character(id_lhs)
    id_lhs <- data.frame(table(id_lhs))
    id_lhs <- filter(id_lhs,Freq>=length(LHS[[j]]))
    id_lhs <- as.character(id_lhs[,1])
    id_rhs <- unlist(temp[,items]) %in% RHS[[j]]
    id_rhs <- unlist(temp[id_rhs,item_id])
    id_rhs <- unique(as.character(id_rhs))
    n_total <- length(unique(unlist(temp[,item_id])))
    #### create the output file
    output_test[j,1] <- sum(id_lhs %in% id_rhs)
    output_test[j,2] <- length(id_lhs)
    output_test[j,3] <- output_test[j,1]/output_test[j,2]
    output_test[j,4] <- output_test[j,1]/n_total
  }
  output_test <- data.frame(output_test)
  colnames(output_test) <- c("support_n_test","lhs_n_test","confidence_test","support_test")
  output <- bind_cols(rules,output_test)
  return(output)
}
