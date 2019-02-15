#' Association Rule mining
#'
#' @description Conducts the assocation rule mining {X -> Y} on the long format of transaction type of data.
#' In our case, one transaction refers to one hospitalization,
#'
#' @param data_input Transaction data in long format
#' @param confidence_threshold Threshold value of confidence for rule development
#' @param support_threshold The minimum number of transactions with items in both left and right sides of a rule
#' @param group_var 1 or more Variables used to group rules
#' @param item_id Unique identifier for each transaction
#' @param items The variable indicating the items in a transaction
#'
#' @details The function is a wrapper of \code{apriori} function in the \pkg{arules} package.
#' The function works on the data.frame data in long format. A transaction can span several rows
#' if more than 1 item exists for the transaction.
#'
#' @return
#'   a dataframe with a set of rules
#'   \item{rules }{The list of rules with the format of X -> Y where  \code{X} is the  eft hand side (LHS) of a rules and \code{y} is the  right hand side (LHS) of a rule}
#'   \item{support }{Support of a rule}
#'   \item{Confidence }{Confidence of a rule}
#'   \item{lift }{lift of a rule}
#'   \item{count }{Number of items with both RHS and LHS in the corresponding category}
#'   \item{rule_cateogry }{The group of rules}
#'   \item{cases_number} {Total number of transaction in the category of rules}
#'   \item{rule_id }{sequence number for the rules}
#'
#'
#' @export

#' @importFrom arules apriori
#' @examples
#'
#'
#'
#'
#'
rules_mining <- function(data_input=dad2013_mapped,confidence_threshold=0.5,support_threshold=30,
                         group_var = "age_categories",item_id="patient_id",items="icd10_who_d4"){
  #### conf_threshold: threshold of value for confidence
  ### Create the rules_categories variable to uniquely define the rules groups
  data_input <- data_input %>% mutate_if(is.factor, as.character)

  if (length(group_var) == 1) {
    data_input$rule_category <- unlist(data_input[,group_var])
  } else {
    data_input <- unite_(data_input,"rule_category",group_var,remove = FALSE)
  }

  #### support_join: N*p(x,y)=30, number of cases meeting the minimum required cases for P(x,y)
  output <- NULL
  rules_categories <- unique(data_input$rule_category)
  ######  Rules mining at each age categories

  for (j in unique(rules_categories)){
    ################ subset the data based on the rule category
    data_sub <- data_input %>% filter(rule_category==j)
    print(paste("Rule group: ",j))
   ### List of id
    id_list <- unique(unlist(data_sub[,item_id]))
    ######  create the data set format for association rule mining:
    medcode_list  <- list()
    for (i in 1:length(id_list)){
      index_2 <- data_sub[,item_id]==id_list[i]
      temp_1 <- as.character(unlist(data_sub[index_2,items]))
      medcode_list[[i]] <- unique(temp_1)
      if (i %%1000==0) {
        print(i)
      }
    }
    ##### Association rule mining
    medcode_list_mining <- as(medcode_list, "transactions")
    ### The number of P(x,y):
    p_support=support_threshold/length(id_list)
    rules_original<- apriori(medcode_list_mining,parameter =
                               list(support = p_support, confidence = confidence_threshold,target="rules",minlen=2))
    ###  rules processing
    rules_original <- as(rules_original,"data.frame")
    if (nrow(rules_original) >0){
      rules_original <- rules_original[order(-rules_original$support),]
      rules_original$rule_category <- j
      rules_original$cases_number <- length(id_list)
      output <- rbind(output,rules_original)
    }
  }
  output$rule_id <- 1:dim(output)[1]
  return(output)
}
