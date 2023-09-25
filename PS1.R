#Johanna Nunez
#SOC 756: Problem Set 1

library(tidyverse)
library(socviz)

lifetable.ps1 <- read.csv("/Users/jo/Desktop/Fall 2023/DemTechII/Problem Set 1/ps1_data_F2023.csv", header = TRUE)
#removed commas from values in .csv so all are numeric values
data = lifetable.ps1

#a1. death rate/intervals
data$nMx <- data$nDx/data$nNx
data$n <- c(diff(data$x), 999)

#a2. death probability
data$nqx <- ((data$n*data$nMx)/(1+(data$n-data$nax)*data$nMx))
data$nqx[19] <- 1

#a3. radiux/cohort
data$lx = 100000
for(i in 2:nrow(data)) {
  data$lx[i] <- data$lx[i-1]*(1-data$nqx[i-1])
}

#a4. deaths
data$ndx <- data$lx[1] - data$lx[2]
for(i in 2:nrow(data)) {
  data$ndx[i] <- data$lx[i]-data$lx[i+1]
}
data$ndx[] <- data$lx[19]


#a5. person years in each interval
data$nLx[19] <- data$lx[19]/data$nMx[19]
for(i in 1:18) {
  data$nLx[i] <- data$n[i]*data$lx[i+1]+data$nax[i]*data$ndx[i]
}

#a6. number of person years lived by pop
data$Tx[1] <- sum(data$nLx)
for(i in 2:nrow(data)) {
  data$Tx[i] <- data$Tx[i-1]-data$nLx[i-1]
}

#a7. life expectancy
data$ex <- data$Tx/data$lx

#b1. graph lx
plot(data$lx)
#b2. graoh ndx
plot(data$ndx)
#b3. graoh nMx
plot(data$nMx)


#c. le at 40
le40 = data$ex[10]
#d. probability of survival from birth to 30
s30 = data$lx[8]/data$lx[1]
#e. probability of survival from 30 to 65
s65 = data$lx[15]/data$lx[8]
#f. newborn would die between 50 and 55
d50 = data$ndx[12]/data$lx[1]
#g. newborn expect to live between 15 and 65
l65 = sum(data$nLx[5:14])/data$lx[1]
#i. CBR = 14.02 per 1,000
CBR=(1/data$ex[1])*1000


#extra credit
install.packages("LifeTables")

library(LifeTables)
lt <- lt.mx(nmx = data$nMx, sex = "male", age = c(0, 1, seq(5, 85, 5)))
lt2 = lt.mx(nmx=data$nMx, sex ="male", age = c(0,1,seq(5,85,5)), nax=NULL)