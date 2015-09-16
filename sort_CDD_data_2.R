

#create interactive command line arguments later
args <- commandArgs(trailingOnly = TRUE)
data_table <- read.csv(args[1], stringsAsFactors=FALSE)

#to get well names 
synonyms <- data_table$Synonyms

#strip off the first part of the Synonym for well indexes
get_strain_info <- function(syn, num){
  well_idx_str <- strsplit(syn, "-")
  return(well_idx_str[[1]][num])
}

#get % Inhibition
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

#get file name
file_name <- unlist(strsplit(args[1], "[.]"))
jpeg(paste0(file_name[1], "landscape.jpg"))

#get strain name and well
well_idx_vector <- as.vector(sapply(synonyms, get_strain_info, num=2))
well_strain_vector <- as.vector(sapply(synonyms, get_strain_info, num=1))

#Organize the data by ascending/descending order
ind <- data.frame(Wells=paste0(rep(LETTERS[1:8], each=10), c("02","03","04","05","06","07","08","09","10","11", "11","10","09","08","07","06","05","04","03","02")), stringsAsFactors=FALSE)

#Plot the 4 dataset in Landscape view
plot_landscape <- function(df_name, Name){
  plot(df_name$neg_control, ylab= "", xlab= "", xaxt='n', 
       main= Name, type = "l", col = "black", ylim=c(0,100), cex=1.0, lwd=2)
  mtext("% Inhibition", side=2, line=2,las=0, font=1)
  par(new=TRUE)
  plot(df_name$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F, 
       lwd=1, ylim=c(0,0.5))
  axis(side=4, col.ticks="red", col.axis="red", las=0)
  axis(side=1, at=c(1:80), labels=ind$Wells, las=2, cex.axis=0.5)
  mtext("Mass (mg)", las=0, side=4,line=2, col="red", cex=0.8) 
}

#Plot all four strains overlayed
plot_overlay <- function(CA,EC,PA,SA, unique_strain){
    #function will overlay all lines crated through % inhibition
    jpeg(paste0(unique_strain, "_overlay.jpg"))
    par(mfrow=c(1,1), mar=c(2.0, 4.0,2.0,4.0), oma=c(0,0,3,1))
    plot(CA$neg_control, ylab= "% inhibition", xlab= "", xaxt='n', main= "Strain Samples at 500nL", type = "l", col = "purple", ylim=c(0,100), cex=1.0, lwd=4)
    par(new=TRUE)
    lines(EC$neg_control, ylab= "% inhibition", xlab= "", xaxt='n',main= "EC Sample",                  
              type = "l", col = "green", ylim=c(0,100), lwd=4)
    par(new=TRUE)
    lines(PA$neg_control, ylab= "", xlab= "",xaxt='n', main= "PA Sample",                  
         type = "l", col = "blue", ylim=c(0,100), lwd=4)
    par(new=TRUE)
    lines(SA$neg_control, ylab= "", xlab= "", xaxt='n',main= "SA Sample",                  
         type = "l", col = "orange", ylim=c(0,100), lwd=4)
    par(new=TRUE)
    plot(PA$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F, lwd=4, ylim=c(0,0.5))
    axis(side=4, col.ticks="red", col.axis="red", las=0)
    axis(side=1, at=c(1:80), labels=ind$Wells, font=0.2, las=2, cex.axis=0.5)
    mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)
    legend("topright", c("CA","EC","SA","PA", "Mass"), col=c("purple", "green", "blue","orange", "red"), cex=0.5, pch=19, pt.cex =0.5)
    dev.off()
}

#Parse data by each unique strain and plot landscape and overlay graphs
get_data <- function(unique_strain, df){
  
  #grep out the unique strain
  unique_selection_1 <- grep(paste0(unique_strain, "-[A-Z][0-9][0-9]"), df$Synonyms)
  unique_2 <- df[unique_selection_1,]
  unique_syns <- unique_2$Synonyms
  wells <- as.vector(sapply(unique_syns, get_strain_info, num=2))
  
  #CA Data
  CA_neg <- unique_2$CA.Dose.Response.Data....negative.control....
  CA_inv_neg <- as.vector(sapply(CA_neg, inv_neg))
  CA_data <- data.frame(Wells=wells, mass=unique_2$Mass..mg., volume=unique_2$CA.Dose.Response.Data..Volume..nL., neg_control=CA_inv_neg, stringsAsFactors = FALSE)
  CA_data_500 <- which(CA_data$volume == 500)
  CA_DATA_parsed <- CA_data[CA_data_500,]
  CA_2 <- merge(ind, CA_DATA_parsed, by="Wells", sort=FALSE)
  #CA_over_fiddy_pos <- which(CA_DATA_parsed$neg_control >= 50.0)
  #CA_over_fiddy <- CA_2[CA_over_fiddy_pos,]
  
  #EC Data
  EC_neg <- unique_2$EC.Dose.Response.Data....negative.control....
  EC_inv_neg <- as.vector(sapply(EC_neg, inv_neg))
  EC_data <- data.frame(Wells=wells, mass=unique_2$Mass..mg., volume=unique_2$EC.Dose.Response.Data..Volume..nL., neg_control=EC_inv_neg, stringsAsFactors = FALSE)
  EC_data_500 <- which(EC_data$volume == 500)
  EC_DATA_parsed <- EC_data[EC_data_500,]
  EC_2 <- merge(ind, EC_DATA_parsed, by="Wells", sort=FALSE)
  #EC_over_fiddy_pos <- which(EC_DATA_parsed$neg_control >= 50.0)
  #EC_over_fiddy <- EC_2[EC_over_fiddy_pos,]
  #write.csv(EC_over_fiddy, file=paste0(file_name[1], "_EC_.csv"))
  
  #PA Data
  PA_neg <- unique_2$PA.Dose.Response.Data....negative.control....
  PA_inv_neg <- as.vector(sapply(PA_neg, inv_neg))
  PA_data <- data.frame(Wells=wells, mass=unique_2$Mass..mg., volume=unique_2$PA.Dose.Response.Data..Volume..nL., neg_control=PA_inv_neg, stringsAsFactors = FALSE)
  PA_data_500 <- which(PA_data$volume == 500)
  PA_DATA_parsed <- PA_data[PA_data_500,]
  PA_2 <- merge(ind, PA_DATA_parsed, by="Wells", sort=FALSE)
  #PA_over_fiddy_pos <- which(PA_DATA_parsed$neg_control >= 50.0)
  #PA_over_fiddy <- PA_2[PA_over_fiddy_pos,]
  #write.csv(PA_over_fiddy, file=paste0(file_name[1], "_PA_.csv"))
  
  #SA Data
  SA_neg <- unique_2$SA.Dose.Response.Data....negative.control....
  SA_inv_neg <- as.vector(sapply(SA_neg, inv_neg))
  SA_data <- data.frame(Wells=wells, mass=unique_2$Mass..mg., volume=unique_2$SA.Dose.Response.Data..Volume..nL., neg_control=SA_inv_neg, stringsAsFactors = FALSE)
  SA_data_500 <- which(SA_data$volume == 500)
  SA_DATA_parsed <- SA_data[SA_data_500,]
  SA_2 <- merge(ind, SA_DATA_parsed, by="Wells", sort=FALSE)
  #SA_over_fiddy_pos <- which(SA_DATA_parsed$neg_control >= 50.0)
  #SA_over_fiddy <- SA_2[SA_over_fiddy_pos,]
  #write.csv(SA_over_fiddy, file=paste0(file_name[1], "_SA_.csv"))
  
  jpeg(paste0(unique_strain, ".jpg"))
  par(mfrow=c(4,1), mar=c(2.0, 4.0,2.0,4.0), oma=c(1,1,3,1))
  plot_landscape(CA_2, "CA")
  plot_landscape(EC_2, "EC")
  plot_landscape(PA_2, "PA")
  plot_landscape(SA_2, "SA")
  mtext(paste0(unique_strain," 500 nL"), side=3, line=1, outer=TRUE, cex=2, font=2)
  dev.off()
  
  plot_overlay(CA_2,EC_2,PA_2,SA_2, unique_strain)
}

unique_strains <- unique(well_strain_vector)
sapply(unique_strains, get_data, df=data_table)  
