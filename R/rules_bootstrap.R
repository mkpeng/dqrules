#' Rules development using the bootstrap
#'
#' Development of rules using the boostrap method. This function streamlines the rules development process including
#' rules mining, rules clean, rules test.
#'
#' @param data_input Transaction data in long format used for rule development(training data)
#' @param item_id Unique identifier for each transaction
#' @param n_bootstrap Number of bootstrap using sample with replacement
#' @param confidence_threshold Threshold value of confidence for rule development
#' @param support_threshold The minimum number of transactions with items in both left and right sides of a rule
#' @param confidence_diff The minimum difference between nested rules
#' @param group_var 1 or more Variables used to group rules
#' @param items the variable to indicate the items in a transaction
#'
#' @return
#'    The output is in data.frame format. See functions of \code{rules_mining, rules_clean, rules_test} for definition of each column.
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
#'
rules_bootstrap <- function(data_input=dad_train,item_id = "patient_id",n_bootstrap=20,test_data=dad_test,
                            confidence_threshold=0.5,support_threshold=30,confidence_diff = 0.1,group_var = "age_categories",
                            items = "icd10_who_d4"){
  output <- NULL
  id_list <- unique(unlist(data_input[,item_id]))
  #  index_train <- length(unique(data_train$id))
  for(i in 1:n_bootstrap){
    set.seed(i)
    index_train_sub <- sample(id_list,size=length(id_list),replace=TRUE)
    index <- unlist(data_input[,item_id]) %in% index_train_sub
    data_train <- data_input[index,] ## Training data
    data_validation <- data_input[!index,] ## validation data (OOB)
    ########## Rule mining
    rules_train <- rules_mining(data_input = data_train,confidence_threshold = confidence_threshold,support_threshold = support_threshold,
                       group_var = group_var, item_id = item_id,items = items)
    rules_train <- rules_clean(rules = rules_train,confidence_diff = confidence_diff)
    rules_train <- rules_test(rules = rules_train,test_data=test_data, item_id=item_id,
                          group_var = group_var,items=items)
    rules_train$batch <- i
    output <- rbind(output,rules_train)
  }
  return(output)
}
