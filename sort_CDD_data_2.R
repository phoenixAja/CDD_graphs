
#create interactive command line arguments later
data_table <- read.csv("WMMC317_4_strains_for_graph-wmass.csv", stringsAsFactors = FALSE)

synonyms <- data_table$Synonyms

#strip off the first part of the Synonym for well indexes
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

well_idx_vector <- as.vector(sapply(synonyms, get_well_index))

ind <- data.frame(well_idx_vector=paste0(rep(LETTERS[1:8], each=11), c(1:11, 11:1)), stringsAsFactors=FALSE)

CA_neg <- data_table$CA.Dose.Response.Data....negative.control....
CA_inv_neg <- as.vector(sapply(CA_neg, inv_neg))
CA_data <- data.frame(well_idx_vector, mass=data_table$Mass..mg., volume=data_table$CA.Dose.Response.Data..Volume..nL., neg_control=CA_inv_neg, stringsAsFactors = FALSE)
CA_data_500 <- which(CA_data$volume == 500)
CA_DATA_parsed <- CA_data[CA_data_500,]
CA_2 <- merge(ind, CA_DATA_parsed, by="well_idx_vector", sort=FALSE)

EC_neg <- data_table$EC.Dose.Response.Data....negative.control....
EC_inv_neg <- as.vector(sapply(EC_neg, inv_neg))
EC_data <- data.frame(well_idx_vector, mass=data_table$Mass..mg., volume=data_table$EC.Dose.Response.Data..Volume..nL., neg_control=EC_inv_neg, stringsAsFactors = FALSE)
EC_data_500 <- which(EC_data$volume == 500)
EC_DATA_parsed <- EC_data[EC_data_500,]
EC_2 <- merge(ind, EC_DATA_parsed, by="well_idx_vector", sort=FALSE)

PA_neg <- data_table$PA.Dose.Response.Data....negative.control....
PA_inv_neg <- as.vector(sapply(PA_neg, inv_neg))
PA_data <- data.frame(well_idx_vector, mass=data_table$Mass..mg., volume=data_table$PA.Dose.Response.Data..Volume..nL., neg_control=PA_inv_neg, stringsAsFactors = FALSE)
PA_data_500 <- which(PA_data$volume == 500)
PA_DATA_parsed <- PA_data[PA_data_500,]
PA_2 <- merge(ind, PA_DATA_parsed, by="well_idx_vector", sort=FALSE)

SA_neg <- data_table$SA.Dose.Response.Data....negative.control....
SA_inv_neg <- as.vector(sapply(SA_neg, inv_neg))
SA_data <- data.frame(well_idx_vector, mass=data_table$Mass..mg., volume=data_table$SA.Dose.Response.Data..Volume..nL., neg_control=SA_inv_neg, stringsAsFactors = FALSE)
SA_data_500 <- which(SA_data$volume == 500)
SA_DATA_parsed <- SA_data[SA_data_500,]
SA_2 <- merge(ind, SA_DATA_parsed, by="well_idx_vector", sort=FALSE)


#Plot the 4 datasetts

