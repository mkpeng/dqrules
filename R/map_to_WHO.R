#' Map the ICD-10 coded data into ICD-10 WHO version
#'
#' ICD-10 is the 10th revision of the International Statistical Classification of Diseases and Related Health Problems (ICD), a medical classification list by the World Health Organization (WHO).
#' Several country created their own version of ICD-10 codes based on ICD-10-WHO. This function map country-specific ICD codes to ICD-1o WHO version
#' Decimals in ICD-10 codes are omitted in the current format.
#'
#' @param data_input raw data in long format
#' @param colname_dx colnames indicate the raw ICD-10 codes.
#' @param chapters_excluded Chapters being excluded from analysis: the first digits of ICD-10 codes. In the defaults, the codes from chapters on symptoms, causes of disease or health services related factors are excluded.
#'
#' @return
#'   a data.frame with a WHO version ICD-10 codes
#
#' @export
#'
#' @examples
#'
#'
map_to_WHO <- function(data_input,colname_dx,
                       chapters_excluded= c("R","S","T","U","V","W","X","Y","Z",icd10_who=icd10_who)){
  ## Get the diagnosis code
  diagnosis_code <- unlist(data_input[,colname_dx])
  ## Find the index where the codes would be excluded
  index <- tolower(substr(diagnosis_code,1,1)) %in% tolower(chapters_excluded)
  data_output <- data_input[!index,]
  diagnosis_code <- diagnosis_code[!index]

  ## Map to digital 4 codes
  icd10_who_d4 <- substr(diagnosis_code,1,4)
  index_1 <- substr(diagnosis_code,1,4) %in% icd10_who$icd10_code
  icd10_who_d4[!index_1] <- substr(diagnosis_code[!index_1],1,3)
  ## Map to digital 3 codes
  icd10_who_d3 <- substr(diagnosis_code,1,3)

  data_output$icd10_who_d4 <- icd10_who_d4
  data_output$icd10_who_d3 <- icd10_who_d3
  return(data_output)
}
