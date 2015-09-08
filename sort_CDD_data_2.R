

data_table <- read.csv("WMMC317_4_strains_for_graph-wmass.csv", stringsAsFactors = FALSE)

synonyms <- data_table$Synonyms

get_well_index <- function(syn){
  well_idx_str <- strsplit(syn, "-")
  return(well_idx_str[[1]][2])
}

inv_neg <- function(x) {
  y <- 100-x
  #remove elements less than zero
  if (y > 0){
    return(y)
  }
  else{
    return(0)
  }
} 

order_indexes <- function(df){
  if (well == (A|C|E|G)[0-9]){
    sort
    
  }
}

ordered_indexes <- c("A2", "A3","A4", "A5","A6", "A7","A8", "A9","A10", "A11", "B11","B10","B9","B8","B7","B6","B5","B4", "B3", "B2", "C2", "C3", "C4","C5","C6","C7","C8","C9","C10","C11","D11","D10","D9","D8","D7","D6","D5","D4","D3","D2","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","F11","")

well_idx_vector <- as.vector(sapply(synonyms, get_well_index))

CA_neg <- data_table$CA.Dose.Response.Data....negative.control....
CA_inv_neg <- as.vector(sapply(CA_neg, inv_neg))
CA_data <- data.frame(well_idx_vector, mass=data_table$Mass..mg., volume=data_table$CA.Dose.Response.Data..Volume..nL., neg_control=CA_inv_neg, stringsAsFactors = FALSE)
CA_data_500 <- which(CA_data$volume == 500)
CA_DATA_parsed <- CA_data[CA_data_500,]


EC_neg <- data_table$EC.Dose.Response.Data....negative.control....
EC_inv_neg <- as.vector(sapply(EC_neg, inv_neg))
EC_data <- data.frame(well_idx_vector, mass=data_table$Mass..mg., volume=data_table$EC.Dose.Response.Data..Volume..nL., neg_control=EC_inv_neg, stringsAsFactors = FALSE)
EC_data_500 <- which(EC_data$volume == 500)
EC_DATA_parsed <- EC_data[EC_data_500,]



PA_neg <- data_table$PA.Dose.Response.Data....negative.control....
PA_inv_neg <- as.vector(sapply(PA_neg, inv_neg))
PA_data <- data.frame(well_idx_vector, mass=data_table$Mass..mg., volume=data_table$PA.Dose.Response.Data..Volume..nL., neg_control=PA_inv_neg, stringsAsFactors = FALSE)
PA_data_500 <- which(PA_data$volume == 500)
PA_DATA_parsed <- PA_data[PA_data_500,]

SA_neg <- data_table$SA.Dose.Response.Data....negative.control....
SA_inv_neg <- as.vector(sapply(SA_neg, inv_neg))
SA_data <- data.frame(well_idx_vector, mass=data_table$Mass..mg., volume=data_table$SA.Dose.Response.Data..Volume..nL., neg_control=SA_inv_neg, stringsAsFactors = FALSE)
SA_data_500 <- which(SA_data$volume == 500)
SA_DATA_parsed <- SA_data[SA_data_500,]



parcoord(data_table, col=rainbow(), lty=1, var.label = TRUE)