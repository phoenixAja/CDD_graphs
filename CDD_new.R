#Author: Phoenix Logan

data <- "CDD_report_6_17_15.csv"
table <- read.csv(data, sep = ",", header = TRUE )
df_count <- as.data.frame(table)

#find the %growth
neg_cont <- df_count$PA.Dose.Response.Data....negative.control....

#function to find the %inhibition removes values less than 0
inv_neg <- function(x) {
  lst <- numeric(length(x))
  for (i in seq_along(x)) {
    y <- 100-x[i]
    #remove elements less than zero
    if (y > 0){
      lst[i] <- y
    }
    else{
      lst[i] <- 0
    }
  }
  return(lst)
}  

inv_pos_control <- sapply(neg_cont, inv_neg)
df_count <- cbind(df_count, inv_pos_control)
#extract the vector with masses
mass <- as.vector(as.numeric(as.character(df_count[,4]))) 

unique_plates <- unique(df_count$Plate.Name)

for (i in seq_along(unique_plates)){
  jpeg(paste0(unique_plates[i], ".jpg"))
	unique_selections_1 <- which(df_count$Plate.Name == unique_plates[i])
	perc_inhibition_1 <- df_count[unique_selections_1,]
	unique_volumes_1 <- which(perc_inhibition_1$PA.Dose.Response.Data..Volume..nL. == 500)	
	df_1 <- perc_inhibition_1[unique_volumes_1,]
	par(mfrow=c(2,2), mar=c(2.0, 4.0,2.0,4.0), oma=c(0,0,3,1))
	plot(df_1$inv_pos_control, ylab= "% inhibition", xlab= "", main= "500 nL Sample", type = "l", col = "purple")
    par(new=TRUE)
    plot(df_1$Mass..mg., type="l", col= "red", xlab= NA, ylab=NA, axes=F)
    axis(side=4, col.ticks="red", col.axis="red", las=0)
    mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)
    	
    #250 mL sample
    unique_volumes_2 <- which(perc_inhibition_1$PA.Dose.Response.Data..Volume..nL. == 250)	
	df_2 <- perc_inhibition_1[unique_volumes_2,]    
	plot(df_2$inv_pos_control, ylab= "% inhibition", xlab= "", main= "250 nL Sample",                  
	type = "l", col = "purple")
    par(new=TRUE)
    plot(df_2$Mass..mg., type="l", col= "red", xlab= NA, ylab=NA, axes=F)
    axis(side=4, col.ticks="red", col.axis="red", las=0)
    mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)	
    
    #100 mL sample
    unique_volumes_3 <- which(perc_inhibition_1$PA.Dose.Response.Data..Volume..nL. == 100)	
	df_3 <- perc_inhibition_1[unique_volumes_3,]    
	plot(df_3$inv_pos_control, ylab= "% inhibition", xlab= "", main= "100 nL Sample",                  
	type = "l", col = "purple")
    par(new=TRUE)
    plot(df_3$Mass..mg., type="l", col= "red", xlab= NA, ylab=NA, axes=F)
    axis(side=4, col.ticks="red", col.axis="red", las=0)
    mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)	
        
    #50 mL sample
    unique_volumes_4 <- which(perc_inhibition_1$PA.Dose.Response.Data..Volume..nL. == 50)	
	df_4 <- perc_inhibition_1[unique_volumes_4,]    
	plot(df_4$inv_pos_control, ylab= "% inhibition", xlab= "", main= "50 nL Sample",                  
	type = "l", col = "purple")
    par(new=TRUE)
    plot(df_4$Mass..mg., type="l", col= "red", xlab= NA, ylab=NA, axes=F)
    axis(side=4, col.ticks="red", col.axis="red", las=0)
    mtext("Mass (mg)", las=0, side=4,line=3, col="red", cex=0.8)	    
    
    mtext(unique_plates[i], side=3, line=1, outer=TRUE, cex=2, font=2)    
    dev.off()
}


unique_volumes_1 <- which(perc_inhibition_1$PA.Dose.Response.Data..Volume..nL. == 500)
unique_volumes_2 <- which(perc_inhibition_1$PA.Dose.Response.Data..Volume..nL. == 250)
unique_volumes_3 <- which(perc_inhibition_1$PA.Dose.Response.Data..Volume..nL. == 100)
df_1 <- perc_inhibition_1[unique_volumes_1,]
df_2 <- perc_inhibition_1[unique_volumes_2,]
df_3 <- perc_inhibition_1[unique_volumes_3,]


