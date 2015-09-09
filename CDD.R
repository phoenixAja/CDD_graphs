data <- "CDD_report_6_17_15.csv"
table <- read.csv(data, sep = ",", header = TRUE )
df_count <- as.data.frame(table)

neg_cont <- df_count$PA.Dose.Response.Data....negative.control....

inv_neg <- function(x) {
  lst <- numeric(length(x))
  for (i in seq_along(x)) {
    y <- 100-x[i]
    if (y > 0){
      lst[i] <- y
    }
    else{
      lst[i] <- 0
    }
  }
  return(lst)
}  



activity_per_gram <- function(activity) {
  stored_data <- vector(mode= "numeric", length = length(activity))
  for (i in seq_along(activity)) {
    a <- activity[i]/mass[i]
    stored_data[i] <- a
  }
  return(stored_data)
}



inv_pos_control <- sapply(neg_cont, inv_neg)
mass <- as.vector(as.numeric(as.character(df_count[,4]))) 
spliced_frame <- rbind(mass, inv_pos_control)
#spliced_frame <- as.data.frame(spliced_frame)

s <- sapply(inv_pos_control, activity_per_gram)
unique_masses <- unique(mass)
#extract out all the different masses

for(i in seq_along(inv_pos_control)){
	pos_mass <- which(df_count$PA.Dose.Response.Data..Volume..nL. == 500)
	volume_500 <- inv_pos_control[pos_mass]
	mass_500 <- mass[pos_mass]
}

for(i in seq_along(inv_pos_control)){
	pos_mass <- which(df_count$PA.Dose.Response.Data..Volume..nL. == 250)
	volume_250 <- inv_pos_control[pos_mass]
}

for(i in seq_along(inv_pos_control)){
	pos_mass <- which(df_count$PA.Dose.Response.Data..Volume..nL. == 100)
	volume_100 <- inv_pos_control[pos_mass]
}

for(i in seq_along(inv_pos_control)){
	pos_mass <- which(df_count$PA.Dose.Response.Data..Volume..nL. == 50)
	volume_50 <- inv_pos_control[pos_mass]
}

unique_doses <- unique(df_count$PA.Dose.Response.Data..Volume..nL.)
#plot different dosage responses
par(mfrow=c(2,2), mar= c(4.3, 4.3, 0.8, 0.8))
plot(volume_500, ylab = "Activity",xlab=NULL, main= "500 nL", type = "h", col = "purple")
plot(volume_250, ylab = "Activity", xlab=NULL, main= "250 nL", type = "h", col = "pink")
plot(volume_100, ylab = "Activity", xlab=NULL, main= "100 nL", type = "h", col = "blue")
plot(volume_50, ylab = "Activity", xlab=NULL, main= "50 nL", type = "h", col = "gray")

#doing two grpahs on one plot
par(mar=c(5, 4, 4, 6) + 0.1)
length_indexes_500 <- 1:length(volume_500)
plot(length_indexes_500,volume_500, ylab = "Activity(% Death)",xlab="", main= "500 nL", type = "h", col = "purple")
#axis(2, ylim=c(min(volume_500, na.rm=TRUE), max(volume_500, na.rm=TRUE)), col="purple", las=1)
box()
par(new=TRUE)
plot(length_indexes_500, mass_500, xlab= "", ylab="", ylim=c(), axes=FALSE, type="l", col="hot pink")
mtext("mass(mg)", side=4, col="hot pink")
axis(4, ylim=c(min(mass_500, na.rm=TRUE), max(mass_500, na.rm=TRUE), col ="hot pink", las=1)

sample_names <- df$CDD.Number
par(mfrow=c(2,1), mar= c(4.3, 4.3, 0.8, 0.8))
plot(inv_pos_control, ylab = "Activity", type = "h", col = "purple")
plot(df_count$Mass..mg. , xlab= "Sample Index", ylab = "Mass (mg)", type = "h", col="coral3")

