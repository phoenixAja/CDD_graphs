
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
ind2 <- as.vector(ind$well_idx_vector)
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


#Plot the 4 dataset

#CA Dataset
  par(mfrow=c(4,1), mar=c(2.0, 4.0,2.0,4.0), oma=c(0,0,3,1))
  plot(CA_2$neg_control, ylab= "% inhibition", xlab= "", xaxt='n', main= "CA sample", type = "l", col = "purple", ylim=c(0,100), cex=1.0)
  par(new=TRUE)
  plot(CA_2$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F)
  axis(side=4, col.ticks="red", col.axis="red", las=0)
  axis(side=1, at=c(10,20,30,40,50,60,70,80), labels=ind2[c(10,20,30,40,50,60,70,80)])
  mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)

#EC Dataset  
  plot(EC_2$neg_control, ylab= "% inhibition", xlab= "", xaxt='n',main= "EC Sample",                  
       type = "l", col = "green", ylim=c(0,100))
  par(new=TRUE)
  plot(EC_2$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F)
  axis(side=4, col.ticks="red", col.axis="red", las=0)
  axis(side=1, at=c(10,20,30,40,50,60,70,80), labels=ind2[c(10,20,30,40,50,60,70,80)])
  mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)	
  
#PA Dataset
  plot(PA_2$neg_control, ylab= "% inhibition", xlab= "",xaxt='n', main= "PA Sample",                  
       type = "l", col = "purple", ylim=c(0,100))
  text(24,60,"C5")
  par(new=TRUE)
  plot(PA_2$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F)
  axis(side=4, col.ticks="red", col.axis="red", las=0)
  axis(side=1, at=c(10,20,30,40,50,60,70,80), labels=ind2[c(10,20,30,40,50,60,70,80)])
  
  mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)	
  
#SA Dataset 
  plot(SA_2$neg_control, ylab= "% inhibition", xlab= "", xaxt='n',main= "SA Sample",                  
       type = "l", col = "purple", ylim=c(0,100))
  par(new=TRUE)
  plot(SA_2$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F)
  axis(side=4, col.ticks="red", col.axis="red", las=0)
  axis(side=1, at=c(10,20,30,40,50,60,70,80), labels=ind2[c(10,20,30,40,50,60,70,80)])
  mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)	    
  
  mtext("500 nL", side=3, line=1, outer=TRUE, cex=2, font=2)    

#Second Plots

#CA Dataset
par(mfrow=c(4,1), mar=c(2.0, 4.0,2.0,4.0), oma=c(1,1,3,1))
plot(CA_2$neg_control, ylab= "", xlab= "", xaxt='n', main= "CA sample", type = "l", col = "purple", ylim=c(0,100), cex=1.0)
par(new=TRUE)
plot(CA_2$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F)
axis(side=4, col.ticks="red", col.axis="red", las=0)
axis(side=1, at=c(10,20,30,40,50,60,70,80), labels=ind2[c(10,20,30,40,50,60,70,80)])
#mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)

#EC Dataset  
plot(EC_2$neg_control, ylab= "", xlab= "", xaxt='n',main= "EC Sample",                  
     type = "l", col = "purple", ylim=c(0,100))
par(new=TRUE)
plot(EC_2$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F)
axis(side=4, col.ticks="red", col.axis="red", las=0)
axis(side=1, at=c(10,20,30,40,50,60,70,80), labels=ind2[c(10,20,30,40,50,60,70,80)])
m#text("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)  

#PA Dataset
plot(PA_2$neg_control, ylab= "", xlab= "",xaxt='n', main= "PA Sample",                  
     type = "l", col = "purple", ylim=c(0,100))
text(24,60,"C5")
par(new=TRUE)
plot(PA_2$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F)
axis(side=4, col.ticks="red", col.axis="red", las=0)
axis(side=1, at=c(10,20,30,40,50,60,70,80), labels=ind2[c(10,20,30,40,50,60,70,80)])

#mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)	

#SA Dataset 
plot(SA_2$neg_control, ylab= "", xlab= "", xaxt='n',main= "SA Sample",                  
     type = "l", col = "purple", ylim=c(0,100))
par(new=TRUE)
plot(SA_2$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F)
axis(side=4, col.ticks="red", col.axis="red", las=0)
axis(side=1, at=c(10,20,30,40,50,60,70,80), labels=ind2[c(10,20,30,40,50,60,70,80)])
#mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)	    

mtext("500 nL", side=3, line=1, outer=TRUE, cex=2, font=2)    
mtext("% Inhibition", side=2, line=1, outer=TRUE, cex=1, font=1) 
mtext("Mass (mg)", col="red", side=4, line=1, outer=TRUE, cex=1.3, font=1.3) 

#Third Graphs
par(mfrow=c(1,1), mar=c(2.0, 4.0,2.0,4.0), oma=c(0,0,3,1))
plot(CA_2$neg_control, ylab= "% inhibition", xlab= "", xaxt='n', main= "Strain Samples at 500nL", type = "l", col = "purple", ylim=c(0,100), cex=1.0, lwd=4)
par(new=TRUE)
lines(EC_2$neg_control, ylab= "% inhibition", xlab= "", xaxt='n',main= "EC Sample",                  
          type = "l", col = "green", ylim=c(0,100), lwd=4)
par(new=TRUE)
lines(PA_2$neg_control, ylab= "", xlab= "",xaxt='n', main= "PA Sample",                  
     type = "l", col = "blue", ylim=c(0,100), lwd=4)
text(24,60,"C5")
par(new=TRUE)
lines(SA_2$neg_control, ylab= "", xlab= "", xaxt='n',main= "SA Sample",                  
     type = "l", col = "orange", ylim=c(0,100), lwd=4)
par(new=TRUE)
plot(PA_2$mass, type="l", col= "red", xlab= NA, ylab=NA, axes=F, lwd=4)
axis(side=4, col.ticks="red", col.axis="red", las=0)
axis(side=1, at=c(10,20,30,40,50,60,70,80), labels=ind2[c(10,20,30,40,50,60,70,80)])
mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)

legend("topright", c("CA","EC","SA","PA", "Mass"), col=c("purple", "green", "blue","orange", "red"), cex=0.5, pch=19, pt.cex =0.5)