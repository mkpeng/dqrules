#' Remove nested rules
#'
#' Two rules were considered nested if they have the same RHS while the LHS of one rule is a subset of the LHS on the other.
#' Confidences between two rules are compared.
#' if the confidence difference is less than the selected threshold: \code{confidence_diff}, only the simple rule is kept.
#'
#'
#' @param rules The list of association rules output from the \code{rules_mining} function
#' @param confidence_diff The minimum difference of confidence between the two nested rules
#'
#' @return
#'   The output is smilar to output from \code{rules_mining} function with the nested rules being removed. Only the following two columns are added.
#'   \item{LHS }{Codes in the left hand side of rules, more than 1 code can be presented in the LHS}
#'   \item{RHS }{Code in the right hand side of rules. Only one code can be there for easy interpretation}
#'
#'
#' @export
#' @importFrom stringr str_split_fixed
#' @examples
#'
rules_clean <- function(rules=rules_list,confidence_diff=0.05){
  ####diff_threshold: difference of confidence blow the threshold is deem as ineffective rules
  ###### divide the rules into LHS and RHS
  rules_names <- data.frame(str_split_fixed(rules$rules, " => ", 2))
  colnames(rules_names) <- c("LHS","RHS")

  #####  remove the curly bracket on the LHS and RHS of rules
  rules_names$LHS<- gsub("\\{|\\}", "", rules_names$LHS)
  rules_names$RHS<- gsub("\\{|\\}", "", rules_names$RHS)

  rules_names <- cbind(rules_names,rules)
  rules_names <- rules_names[order(rules_names$RHS),]
  rules_names$rule_id <- 1:dim(rules_names)[1]

  ##### list of code elements in the LHS
  LHS_1 <-  strsplit(rules_names$LHS,split=c(","))
  ###### Calculate the length of LHS
  rules_names$LHS_length  <- as.vector(sapply(LHS_1,length))

  ##### Extract the right side of rules
  RHS <- unique(rules_names$RHS)
  nested_index <- NULL

  for (i in RHS) {
    rules_temp <- filter(rules_names,RHS==i)
    rules_temp <- rules_temp[,c("RHS","LHS","confidence","support","rule_id","LHS_length","rule_category")]
    rules_temp_1 <- rules_temp
    colnames(rules_temp_1)[2:6] <- c("LHS_2","confidence_2","support_2","Seq_2","LHS_2_length")

    rules_merge <- merge(rules_temp,rules_temp_1,by= c("RHS","rule_category"))
    ###  rules_merge <- filter(rules_merge,confidence_2 <=  confidence)
    #####   Based on the difference of left hand side
    rules_merge <- filter(rules_merge,LHS_2_length < LHS_length)

    #####   Calculate the difference between confidence,
    rules_merge <- mutate(rules_merge,diff=confidence-confidence_2,ratio_support=support/support_2)
    rules_merge <- filter(rules_merge,diff < confidence_diff)

    ##### split the LHS to test whether the two element are nested
    LHS_1 <-  strsplit(rules_merge$LHS,split=c(","))
    LHS_2 <-  strsplit(rules_merge$LHS_2,split=c(","))

    if(dim(rules_merge)[1]>1){
      for (j in 1:dim(rules_merge)[1]){
        rules_merge$nested[j]<- all(LHS_2[[j]] %in% LHS_1[[j]])
      }
      rules_merge <- filter(rules_merge,nested==1)
      nested_index <- c(nested_index,unique(rules_merge$rule_id))
    }
  }
  ### Exclude the nested rules
  rules_names_sub <- rules_names[!(rules_names$rule_id %in% nested_index),]
  rownames(rules_names_sub) <- 1:dim(rules_names_sub)[1]
  return(rules_names_sub)
}
