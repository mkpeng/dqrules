#' Remove nested rules using the sequence method
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
#'   \item{lhs_list }{List of codes in the left hand side of rules}
#'   \item{LHS_length }{Number of codes in the left hand side of rules}
#'
#'
#' @export
#' @importFrom stringr str_split_fixed
#' @examples
#'
#'
#'
rules_clean_sequence <- function(rules=rules_list,confidence_diff=0.05){
        ####diff_threshold: difference of confidence blow the threshold is deem as ineffective rules
        ###### divide the rules into LHS and RHS
        rules_final <- data.frame(str_split_fixed(rules$rules, " => ", 2))
        colnames(rules_final) <- c("LHS","RHS")

        #####  remove the curly bracket on the LHS and RHS of rules
        rules_final$LHS<- gsub("\\{|\\}", "", rules_final$LHS)
        rules_final$RHS<- gsub("\\{|\\}", "", rules_final$RHS)

        rules_final <- cbind(rules_final,rules)
        rules_final <- rules_final %>% arrange(RHS,LHS)
        rules_final$rule_id <- 1:dim(rules_final)[1]

        ### create the list of lhs
        rules_final$lhs_list <- strsplit(rules_final$LHS,split=c(","))
        rules_final$LHS_length <- as.vector(sapply(rules_final$lhs_list,length))

        ### Clean the rules
        max_lhs_length <- max(rules_final$LHS_length)

        for(lhs_length in 1:(max_lhs_length-1)){
                rules_1 <- filter(rules_final,LHS_length==lhs_length)
                #dput(paste( 'length=', lhs_length))
                if (length( rules_1 ) > 0 ) { 
                  for(i in 1:dim(rules_1)[1]){
                #          dput(paste( 'i=', i))
                          rhs_code <- rules_1$RHS[i]
                          lhs_code <- rules_1$lhs_list[[i]]
                          confidence_ref <- rules_1$confidence[i]
                          rules_final$lhs_nested <- unlist(lapply(rules_final$lhs_list,
                                                                  function(x) all(lhs_code %in% x)))
                          temp <- rules_final %>%
                                  filter(lhs_nested,LHS_length > lhs_length,RHS==rhs_code,
                                        (confidence - confidence_ref) < confidence_diff) %>% pull(rule_id)
                          rules_final <- rules_final %>% filter( !rule_id %in% temp)
                  }
                }
        }
        rules_final <- rules_final %>% select(-lhs_nested)
        return(rules_final)
}
