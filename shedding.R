## Assignment 1 ## 
## Renewables in electricity market ## 

## Workspace directory
setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment1/New_try")

## Install packages 
# install.packages("ggplot2")
# install.packages("lpSolve")
# install.packages("lpSolveAPI")


## Read packages if already installed 
library(ggplot2)
library(lpSolve)
library(linprog)
library(lpSolveAPI)


## read data ## 
Avedovre = read.csv("data/Avedovre.csv", header = TRUE, sep =";", skip = 1)
BlueWater = read.csv("data/BlueWater.csv", header = TRUE, sep =";", skip = 1)
FlexiGas = read.csv("data/FlexiGas.csv", header = TRUE, sep =";", skip = 1)
Nuke22 = read.csv("data/Nuke22.csv", header = TRUE, sep =";", skip = 1)
Peako =  read.csv("data/Peako.csv", header = TRUE, sep =";", skip = 1)
RoskildeCHP = read.csv("data/RoskildeCHP.csv", header = TRUE, sep =";", skip = 1)

## Import / export ##
NorwayImport = read.csv("data/NorwayImport.csv", header = TRUE, sep =";", skip = 1)
GermanyExport = read.csv("data/GermanyExport.csv", header = TRUE, sep = ";", skip =0)
SwedenExport = read.csv("data/SwedenExport.csv", header = TRUE, sep = ";", skip =0)

## Prognosis consumption ## 
Consumption = read.csv("data/consumption-prognosis_2018_hourly.csv", header = TRUE, sep =",", skip = 2)
## Prognosis wind power ## 
Wind = read.csv("data/wind-power-dk-prognosis_2018_hourly.csv", header = TRUE, sep =",", skip = 2)
WW1_pp = 0.25 # WestWind1 quantity based on predicted prod
WW2_pp = 0.75 # WestWind2 quantity based on predicted prod
EW1_pp = 1/3 # EastWind1 quantity based on predicted prod
EW2_pp = 2/3 # EastWind2 quantity based on predicted prod

## 
N_simulations = 31*24 # January hourly
transmission_cap = 600 # MWh

## Save the results 
dispatch_hour = matrix(0,nrow = N_simulations, ncol = 20)
price_hour_new = matrix(0,nrow = N_simulations, ncol = 2)
demand_hour = matrix(0,nrow = N_simulations, ncol = 2)

## DK1 = WEST / DK2 = EAST
for (i in 1:N_simulations){
  # i = 454
  lps.model <- make.lp(22,20)
  set.column(lps.model, 1, c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 2, c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 3, c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))
  set.column(lps.model, 4, c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))
  set.column(lps.model, 5, c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 6, c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 7, c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 8, c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 9, c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 10, c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 11, c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1))
  set.column(lps.model, 12, c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1))
  set.column(lps.model, 13, c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1))
  set.column(lps.model, 14, c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1))
  set.column(lps.model, 15, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1))
  set.column(lps.model, 16, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1))
  set.column(lps.model, 17, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1))
  set.column(lps.model, 18, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,-1))
  set.column(lps.model, 19, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0))
  set.column(lps.model, 20, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1))
  
  set.objfn(lps.model, c(0, -17, 0, -25, 70, 64, 153, 82, 89, Nuke22$G6_price[i], Nuke22$G7_price[i], 43, 39, 36, 31, 5, 10, 0, 10000, 10000))
  set.constr.type(lps.model, c(rep("<=", 20),"=", "=") )
  # 
  b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
        400, 330, 345, 390, 510,Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, lsDK1= 10000, lsDK2=10000)
  beq_DK1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
  beq_DK2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
  beq = c(beq_DK1, beq_DK2)
  # 
  # 
  set.rhs(lps.model, c(b,beq))
  set.bounds(lps.model, lower = -600, columns = 18)
  
  solve(lps.model)
  dispatch_hour[i,] <- get.variables(lps.model)
  demand_hour[i,1] = beq_DK1
  demand_hour[i,2] = beq_DK2
  price_hour_new[i,1] <- get.dual.solution(lps.model)[22]
  price_hour_new[i,2] <- get.dual.solution(lps.model)[23]
  
}


########################################################
########################################################
# Post-traitments 
dispatch_daily  = matrix(0,nrow = 31, ncol = 20)
day = seq(1,N_simulations,by = 24)
for (i in 1:length(day)){
  dispatch_daily[i,] = colSums(dispatch_hour[day[i]:(day[i]+23),])
}

########################################################
########################################################
# DK1 let's look at the price evolution 
png('images/Price_DK1.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, price_hour_new[,1], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-20, max(price_hour_new[,1])+50), type = "l", col = "black", lwd = 1.5)
abline( h = max(price_hour_new[,1]), col = "red", lwd = 2)
abline( h = mean(price_hour_new[,1]), col = "red", lty = 3, lwd = 3)
abline(h=0)
legend("topleft", legend= c("DK1", paste0("Max price : ", max(price_hour_new[,1]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour_new[,1])), " €/MWh")),
       col = c("black", "red","red"), lty = c(1,1,3), lwd = 2, cex = 0.75)
dev.off()

# DK2 let's look at the price evolution 
png('images/Price_DK2.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, price_hour_new[,2], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-20, max(price_hour_new[,2])+20), type = "l", col = "black", lwd = 1.5)
abline( h = max(price_hour_new[,2]), col = "red", lwd = 2)
abline( h = mean(price_hour_new[,2]), col = "red", lty = 3, lwd = 3)
abline(h=0)
legend("topleft", legend= c("DK2", paste0("Max price : ", max(price_hour_new[,1]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour_new[,1])), " €/MWh")),
       col = c("black", "red","red"), lty = c(1,1,3), lwd = 2, cex = 0.75)
dev.off()

# DK1 negative prices statistics
negative_on_DK1 = round(sum(price_hour_new[,1]<0)/length(price_hour_new[,1])*100, digits = 1)
negative_off_DK1 = (1 - negative_on_DK1/100)*100
slices_DK1 = c(negative_on_DK1, negative_off_DK1)
lbls_DK1 = c(paste0("Neg. prices : ", negative_on_DK1, " %"), paste0(negative_off_DK1, " %"))

# png('images/pie_wind_curtailment_DK1.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(5, 5,4, 5))
pie(slices_DK1, lbls_DK1, main =" DK1 January prices", col=c("blue", "grey"))
# dev.off()

# DK2 negative prices statistics
negative_on_DK2 = round(sum(price_hour_new[,2]<0)/length(price_hour_new[,2])*100, digits = 1)
negative_off_DK2 = (1 - negative_on_DK2/100)*100
slices_DK2 = c(negative_on_DK2, negative_off_DK2)
lbls_DK2 = c(paste0("Neg. prices : ", negative_on_DK2, " %"), paste0(negative_off_DK2, " %"))

# png('images/pie_wind_curtailment_DK2.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(5, 5,4, 5))
pie(slices_DK2, lbls_DK2, main =" DK2 January prices", col=c("blue", "grey"))
# dev.off()

png('images/summary_prices_DK12.png', width = 886, height = 569, units = "px", pointsize = 12)
par(mfrow=c(2,2))
par(mar = c(2,2,2,2))
pie(slices_DK1, lbls_DK1, main =" DK1 January prices", col=c("blue", "grey"), cex = 1, cex.main = 2)
par(mar = c(2,2,2,4))
pie(slices_DK2, lbls_DK2, main =" DK2 January prices", col=c("blue", "grey"), cex = 1, cex.main = 2)
par(mar=c(2, 4.5, 0, 0.5))
plot(1:N_simulations, price_hour_new[,1], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-20, max(price_hour_new[,1])+50), type = "l", col = "black", lwd = 1.5)
abline( h = max(price_hour_new[,1]), col = "red", lwd = 2)
abline( h = mean(price_hour_new[,1]), col = "red", lty = 3, lwd = 3)
abline(h=0)
legend("topleft", legend= c("DK1", paste0("Max price : ", max(price_hour_new[,1]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour_new[,1])), " €/MWh")),
       col = c("black", "red","red"), lty = c(1,1,3), lwd = 2, cex =1)
par(mar=c(2, 4.5, 0, 0.5))
plot(1:N_simulations, price_hour_new[,2], xlab = "Time [h]", ylab = "",ylim = c(-20, max(price_hour_new[,2])+30), type = "l", col = "black", lwd = 1.5)
abline( h = max(price_hour_new[,2]), col = "red", lwd = 2)
abline( h = mean(price_hour_new[,2]), col = "red", lty = 3, lwd = 3)
abline(h=0)
legend("topleft", legend= c("DK2", paste0("Max price : ", max(price_hour_new[,2]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour_new[,2])), " €/MWh")),
       col = c("black", "red","red"), lty = c(1,1,3), lwd = 2, cex = 1)
dev.off()

########################################################
########################################################
# here we look at the usage of the transmission
count = 0
for (i in 1:N_simulations) if (abs(dispatch_hour[i,18])==600) count = count + 1
cat(count/N_simulations*100) # return the % of time when transmission is fully used

par(mar = c(6, 4.5,2, 2))
png('images/transmission_DK12.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, dispatch_hour[,18], type = 'h', xlab = "Time [h]", ylab = "[MW]", col = "darkgrey", ylim = c(-800,600))
abline(h = transmission_cap, col ="black", lty = 2, lwd = 2)
abline(h = -transmission_cap, col ="black", lty = 2, lwd = 2)
legend("bottomleft", legend = c("(+) : DK1 => DK2", "(-) : DK1 <= DK2"), cex = 0.75, lty = 1, col = "darkgrey", lwd = 2)
title(main = "Transmission", sub = paste0("Congestion occurs : ", round(count/N_simulations*100)," % of the time"), col.sub = "black")
dev.off()

DK12_transmission = DK21_transmission = matrix(0, nrow = 24, ncol = 31)
k = 1
for (i in 1:(N_simulations-1)){
  j = i%%24 
  cat(paste0("j = ",j), "\n")
  cat(paste0("k = ",k), "\n")
  
  if (j != 0){
    k = k
    if (dispatch_hour[i,18] >= 0){
      DK12_transmission[j,k] = dispatch_hour[i,18]/transmission_cap*100 
    } else{
      DK21_transmission[j,k] = -dispatch_hour[i,18]/transmission_cap*100
    }
  }else{
  k=k+1
  if (dispatch_hour[i,18] >= 0){
    DK12_transmission[24,k] = dispatch_hour[i,18]/transmission_cap*100 
  } else{
    DK21_transmission[24,k] = -dispatch_hour[i,18]/transmission_cap*100
  }
  }
}

# How often the transmission is congested 
png('images/congestion_DK12.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(4.5, 4.5,2, 2))
plot(1:24, rowSums(DK12_transmission[1:24,]==100)/31*100, type = 'h', lwd = 10, ylim = c(0,100), xlab = "Time slot", ylab = "[%]")
lines(1:24, rowSums(DK21_transmission[1:24,]==100)/31*100, type = 'h', lwd = 9, ylim = c(0,100), col = "grey")
legend("topleft", legend = c("DK1 => DK2", "DK2 => DK1"), col = c("black", "grey"), lwd = 5, lty = 1, cex = 0.75)
title(main = "Congestion of the transmission cable")
dev.off()

########################################################
########################################################
# We order Generators for DK1 and for DK2 
new_dispatch_hour = matrix(0,nrow = N_simulations, ncol = 17)
new_dispatch_hour[,1:2] = dispatch_hour[,1:2]
new_dispatch_hour[,3:8] = dispatch_hour[,5:10]
new_dispatch_hour[,9:10] = dispatch_hour[,3:4]
new_dispatch_hour[,11:17] = dispatch_hour[,11:17]

# Therefore the participants are : 

new_participants = c("WW1","WW2",
                     "FlexiGas", "FlexiGas", "FlexiGas",
                     "Peako", "Peako","Nuke22",
                     "EW1","EW2", "Nuke22",
                     "RoskildeCHP", "RoskildeCHP",
                     "Avedovre", "Avedovre",
                     "BlueWater", "BlueWater") # "Transmission_line", "Shedding1", "Shedding2")


# Conventional generators scheduled 
png('images/generators_schedule.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
generator_schedule = (colSums(new_dispatch_hour[1:N_simulations,]>0)[1:17])/N_simulations*100
plot(1:17, generator_schedule, type = "h", lend="square", lwd = 10, xlab = "", xaxt = "n", ylab = "[%]", yaxt ="n", ylim = c(0,140))
ylabel <- seq(0, 100, by = 25)
axis(2, at = ylabel)
lines(1:2, generator_schedule[1:2], type = "h", lend="square",col ="blue", lwd =10)
lines(3:4, generator_schedule[3:4], type = "h", lend="square",col ="black", lwd =10)
lines(5, generator_schedule[5], type = "h",lend="square", col ="red", lwd =10)
lines(6:8, generator_schedule[6:8], type = "h", lend="square",col ="black", lwd =10)
lines(9:10, generator_schedule[9:10], type = "h", lend="square",col ="blue", lwd =10)
lines(11, generator_schedule[11], type = "h", lend="square",col ="black", lwd =10)
lines(12, generator_schedule[12], type = "h", lend="square",col ="red", lwd =10)
lines(13:15, generator_schedule[13:15], type = "h", lend="square",col ="black", lwd =10)
lines(16:17, generator_schedule[16:17], type = "h", lend="square",col ="blue", lwd =10)
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 1.2, lwd = 2)
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1")
legend("topright", legend = "DK2" )
legend("top", legend = c("Renewable generators", "Conventional generators","Conventional generators never scheduled"),
       col = c("blue", "black", "red"), lty = 1, lwd = 3, cex = 0.75)
title(main="Generation scheduled over January")
dev.off()

cat(paste0("In average, conventional generators are scheduled : "))

#
eq.price_hour = matrix(0,nrow = N_simulations, ncol = 17) 
eq.price_hour[,1:8] = price_hour_new[,1] # DK1
eq.price_hour[,9:17] = price_hour_new[,2] # DK2

# particular case of support 
eq.price_hour[,2] = eq.price_hour[,2] + 17 # premium
eq.price_hour[,10] = 25 # feed in tariff

revenues_hour = matrix(0,nrow = N_simulations, ncol = 17)
for (i in 1:N_simulations){
  revenues_hour[i,] = eq.price_hour[i,]*new_dispatch_hour[i,]
}

png('images/hourly_revenues_from_wind_DK12.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(5, 5,4, 2))
plot(1:N_simulations,(revenues_hour[,1]+revenues_hour[,2])/1000, type = 'h', xlab = "Time [h]", ylab = "Revenues [k€]", lwd = 5)
lines(1:N_simulations,(revenues_hour[,9]+revenues_hour[,10])/1000, type = 'h', xlab = "Time [h]", ylab = "Revenues [k€]", col = "grey", lwd = 3)
legend("topleft", legend= c("DK1","DK2"), col = c("black", "blue"), lty = 1, cex = 0.75, lwd = 2)
title(main = "Hourly revenues from wind production")
dev.off()

# high wind penetration i = 650
png('images/generator_dispatch_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, new_dispatch_hour[650,], type = "h", lend="square", lwd = 10, xlab = "", xaxt ="n", ylab = "[MWh]" ,ylim = c(0,2800))
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 1.2, lwd=2)
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1 ")
legend("topright", legend = "DK2")
title(main="Generators dispatch when high wind penetration")
dev.off()

png('images/revenues_dispatch_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, revenues_hour[650,]/1000, type = "h",lend="square", lwd = 10, ylab = "[k€]", xlab = "", xaxt ="n")
lines(2,revenues_hour[650,2]/1000, type = "h", col = "red", lwd = 10, lend="square")
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 1.2, lwd= 2)
title(main="Generators revenues when high wind penetration")
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1 ")
legend("topright", legend = "DK2")
mtext(paste0("Eq. price (€) in DK1 - DK2 resp. : ", price_hour_new[650,1], " / ", price_hour_new[650,2]), 1, line=7, col = "blue")
dev.off()

# low wind penetration i = 120
# Generators revenues when low wind penetration 
png('images/generator_dispatch_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, new_dispatch_hour[120,], type = "h",lend="square", lwd = 10, xlab = "", xaxt ="n", ylab = "[MWh]", ylim = c(0,1000) )
axis(1, at=1:20, labels = new_participants[1:20], las = 2, cex.axis = 1.2, lwd = 2)
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1 ")
legend("topright", legend = "DK2")
title(main="Generators dispatch when low wind penetration")
dev.off()


png('images/revenues_dispatch_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, revenues_hour[120,1:17]/1000, type = "h", lend="square", lwd = 10, ylab = "[k€]", xlab = "", xaxt ="n", ylim = c(0,45))
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 1.2, cex.lab=2)
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1 ")
legend("topright", legend = "DK2")
title(main="Generators revenues when low wind penetration")
mtext(paste0("Eq. price (€) in DK1 - DK2 resp. : ", price_hour_new[120, 1], " / ", price_hour_new[120,2]), 1, line=7, col = "blue")
dev.off()


########################################################
########################################################
# Overall revenues
overall_revenues = matrix(0,nrow = 1, ncol = 17)
for (i in 1:17){
  overall_revenues[,i] = sum(revenues_hour[1:N_simulations,i])
}

png('images/overall_revenues.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, overall_revenues[1,1:17]/1000, type = "h", lend="square", lwd = 10, ylab = "[k€]", xlab = "", xaxt ="n", ylim = c(0,35000))
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1 ")
legend("topright", legend = "DK2")
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 1.2, lwd = 2)
title(main = "[January] Overall revenues for each market participant")
dev.off()



########################################################
########################################################
# Curtailement DK1 
png('images/wind_curtailment_DK1.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(5, 5,4, 5))
plot(1:N_simulations, Wind$DK1, type = 'h', lwd = 5, xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, dispatch_hour[,1]+dispatch_hour[,2], col = "grey", type = 'h', lwd = 5)
legend("bottomright", legend = c("Wind offers", "Wind production"), col = c("black", "grey"), lwd = 2, lty = 1, cex = 0.75)
title(main = "Wind Curtailment in DK1")
dev.off()

curtail_DK1 = round(Wind$DK1 - dispatch_hour[,1] - dispatch_hour[,2])
curtail_on = sum(curtail_DK1 !=0)/length(curtail_DK1)
curtail_off = 1 - curtail_on
slices = c(curtail_on, curtail_off)
lbls = c(paste0("wind curtailment : ", round(curtail_on*100), " %"), paste0("full production : ",round(curtail_off*100), " %"))

png('images/pie_wind_curtailment_DK1.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(5, 5,4, 5))
pie(slices, lbls, main =" DK1 January wind production", col=c("blue", "grey"))
dev.off()

# Curtailement DK2 
png('images/wind_curtailment_DK2.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(5, 5,4, 5))
plot(1:N_simulations, Wind$DK2, type = 'h', lwd = 5, xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, dispatch_hour[,3]+dispatch_hour[,4], col = "grey", type = 'h', lwd = 5)
legend("bottomright", legend = c("Wind offers", "Wind production"), col = c("black", "grey"), lwd = 2, lty = 1, cex = 0.75)
title(main = "Wind Curtailment in DK2")
dev.off()

curtail_DK2 = round(Wind$DK2 - dispatch_hour[,3] - dispatch_hour[,4])
curtail_on = sum(curtail_DK2 !=0)/length(curtail_DK2)
curtail_off = 1 - curtail_on
slices = c(curtail_on, curtail_off)
lbls = c(paste0("wind curtailment : ", round(curtail_on*100), " %"), paste0("full production : ", round(curtail_off*100), " %"))

png('images/pie_wind_curtailment_DK2.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(5, 5,4, 5))
pie(slices, lbls, main ="DK2 January wind production", col=c("blue", "grey"))
dev.off()

########################################################
########################################################
# General wind penetration 
par(mar = c(4.5, 4.5,2, 2))
wp = ((dispatch_hour[,1]+dispatch_hour[,2]+dispatch_hour[,3]+dispatch_hour[,4])/(demand_hour[,1]+demand_hour[,2]))*100
mean_wp = mean(wp)

png('images/wp_DK.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, wp, 
     type = 'l', ylim = c(-5,105), xlab = "Time [h]", ylab = "[%]", lwd = 2)
abline(h = mean_wp,
       col = "blue")
legend("bottomright", legend = paste0("Mean penetration over January : ", round(mean_wp), " %" ), lty = 1, col = "blue")
title(main = "Wind penetration in DK")
dev.off()

# DK1 
# General Demand vs wind offers DK1
png('images/demand_vs_offers_DK1.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, demand_hour[,1], type = "l", ylim = c(0,4000), lwd = 2,
     xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, Wind$DK1, col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity Demand DK1", "Wind offers DK1"), col=c("black","blue"),lty = 1, lwd = 2, cex = 0.75)
dev.off()

# Wind production
png('images/demand_vs_windprod_DK1.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, demand_hour[,1], type = 'l', ylim = c(0,4000), xlab = "Time [h]", ylab = "[MW]", lwd = 2)
lines(1:N_simulations,dispatch_hour[,1]+dispatch_hour[,2], col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity demand in DK1", "Wind production in DK1"), col = c("black", "blue"), lty = c(1,1), cex = 0.75, lwd = 2 )
title(main= "Wind production in the electricity market DK1")
dev.off()

# let's look at the wind penetration in this market 
## Wind energy penetration [%] is calculated as the wind energy produced in a time period [hour; day; year] 
## divided by the total electricity consumption in that period
count = 0
for (i in 1:N_simulations){
  if ((dispatch_hour[i,1]+dispatch_hour[i,2])/demand_hour[i,1]*100 >= 100) count = count + 1
}
cat(paste0("100% wind penetration in DK1 : ", round(count/N_simulations*100), " % of the time"))

png('images/wp_DK1.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, (dispatch_hour[,1]+dispatch_hour[,2])/demand_hour[,1]*100, type = 'l', ylim = c(-5,135),
     xlab = "Time [h]", ylab = "[%]", lwd = 2)
title(main = "Wind penetration in DK1")
dev.off()

## DK2
# General Demand vs wind offers DK2
png('images/demand_vs_offers_DK2.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, demand_hour[,2], type = "l", ylim = c(0,3000), lwd = 2,
     xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, Wind$DK2, col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity Demand DK2", "Wind offers DK2"), col=c("black","blue"),lty = 1, lwd = 2, cex = 0.75)
dev.off()

# Wind production
png('images/demand_vs_windprod_DK2.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, demand_hour[,2], type = 'l', ylim = c(0,3000),xlab = "Time [h]", ylab = "[MW]", lwd = 2)
lines(1:N_simulations,dispatch_hour[,3]+dispatch_hour[,4], col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity demand in DK2", "Wind production in DK2"), col = c("black", "blue"), lty = c(1,1), cex = 0.75, lwd = 2 )
title(main= "Wind production in the electricity market DK2")
dev.off()

# Let's look at the wind penetration in this market
## Wind energy penetration [%] is calculated as the wind energy produced in a time period [hour; day; year] 
## divided by the total electricity consumption in that period
count = 0
for (i in 1:N_simulations){
  if ((dispatch_hour[i,3]+dispatch_hour[i,4])/demand_hour[i,2]*100 == 100) count = count + 1
}
cat(paste0("100% wind penetration in DK2 : ", round(count/N_simulations*100), " % of the time"))

png('images/wp_DK2.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, (dispatch_hour[,3]+dispatch_hour[,4])/demand_hour[,2]*100, type = 'l', ylim = c(-5,105),
     xlab = "Time [h]", ylab = "[%]", col ="black", lwd = 2)
title(main = "Wind penetration in DK2")
dev.off()

# Influence of the transmission
# DK1
png('images/usage_transmission.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(4.5, 4.5,2, 2))
plot(620:660, demand_hour[620:660,1], type = 'h', ylim = c(-2500,3500), xlab = "Time [h]", ylab = "[MW]", lwd = 10, cex.lab = 1.5, cex.axis = 1.2)
lines(620:660,dispatch_hour[620:660,1]+dispatch_hour[620:660,2]+dispatch_hour[620:660,5]+dispatch_hour[620:660,6]+dispatch_hour[620:660,7]+dispatch_hour[620:660,8]+dispatch_hour[620:660,9]+dispatch_hour[620:660,10], col = "grey", lwd = 4, type = "h")
lines(620:660, Wind$DK1[620:660],col = "blue", lwd = 4, type = "h" )
lines(620:660,dispatch_hour[620:660,1]+dispatch_hour[620:660,2], col = "darkorange", lwd = 4, type = "h")
lines(620:660, dispatch_hour[620:660,18], type = "h", xlab = "Time [h]", ylab = "[MWh]", col = "green", lwd = 4)
abline(h=-600, col ="red", lty = 2, lwd = 2)
abline(h=600, col ="red", lty = 2, lwd = 2)
legend("bottomleft", legend = c("Electricity demand in DK1", "Electricity prod. in DK1",
                                "Electricity prod. from wind", "Transmission to DK2 (negative = export to DK2)",
                                "Wind availabilities", "Transmission cap"), 
       col = c("black", "grey", "orange","green", "blue","red"), lty = c(1,1,1,1,1,2), cex = 0.75, lwd = 4 )
title(main = "Study case when high wind penetration")
dev.off()

plot(110:130, demand_hour[110:130,1], type = 'h', ylim = c(0,3500), xlab = "Time [h]", ylab = "[MW]", lwd = 10)
lines(110:130,dispatch_hour[110:130,1]+dispatch_hour[110:130,2]+dispatch_hour[110:130,5]+dispatch_hour[110:130,6]+dispatch_hour[110:130,7]+dispatch_hour[110:130,8]+dispatch_hour[110:130,9]+dispatch_hour[110:130,10], col = "grey", lwd = 4, type = "h")
lines(110:130,dispatch_hour[110:130,1]+dispatch_hour[110:130,2], col = "darkorange", lwd = 4, type = "h")
abline(h=600, col ="red", lty = 1, lwd = 2)
points(110:130, dispatch_hour[110:130,18], xlab = "Time [h]", ylab = "[MWh]", col = "green", lwd = 4, pch = 3)
legend("topright", legend = c("Electricity demand in DK1", "Electricity prod. in DK1", "Electricity prod. from wind",
                              "Transmission to DK2 (positive = import from DK2)","Transmission cap"), 
       col = c("black", "grey", "orange","green", "red"), lty = c(1,1,1), pch = c(NaN,NaN,NaN,3), cex = 0.75, lwd = 4 )

########################################################
########################################################
### merit order plot
source("functions/m_order.R")
source("functions/simple_merit_order_plot.R")

i = 650
b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
      400, 330, 345, 390, 510, Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, 10000, 10000)
f.obj = c(0, -17, 0, -25, 70, 64, 153, 82, 89, Nuke22$G6_price[i], Nuke22$G7_price[i], 43, 39, 36, 31, 5, 10, 0, 10000, 10000)

# DK1
quantity = c(b[1:2],b[5:10])
price = c(f.obj[1:2],f.obj[5:10])
demand_dk1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
data_dk1 = data.frame(quantity = quantity, price = price)
data_dk1 = data_dk1[order(data_dk1$price),]
new_quantity = m_order(data_dk1$quantity)
data_dk1[,1] = new_quantity
simple_merit_order_plot(data_dk1, paste0("Market clearing DK1 : hour =  ", i ), TRUE, demand_dk1)

png('images/MC_DK1_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
simple_merit_order_plot(data_dk1, "Market clearing DK1 - high wind penetration", TRUE, demand_dk1)
dev.off()

# DK2
quantity = c(b[3:4],b[11:17])
price = c(f.obj[3:4],f.obj[11:17])
demand_dk2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
data_dk2 = data.frame(quantity = quantity, price = price)
data_dk2 = data_dk2[order(data_dk2$price),]
new_quantity = m_order(data_dk2$quantity)
data_dk2[,1] = new_quantity
simple_merit_order_plot(data_dk2, paste0("Market clearing DK2 : hour =  ", i ), TRUE, demand_dk2)

png('images/MC_DK2_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
simple_merit_order_plot(data_dk2, "Market clearing DK2 - high wind penetration", TRUE, demand_dk2)
dev.off()

## low wind penetration

i = 120 # low penetration
b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
      400, 330, 345, 390, 510, Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, 10000, 10000)
f.obj = c(0, -17, 0, -25, 70, 64, 153, 82, 89, Nuke22$G6_price[i], Nuke22$G7_price[i], 43, 39, 36, 31, 5, 10, 0, 10000, 10000)

# DK1
quantity = c(b[1:2],b[5:10])
price = c(f.obj[1:2],f.obj[5:10])
demand_dk1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
data_dk1 = data.frame(quantity = quantity, price = price)
data_dk1 = data_dk1[order(data_dk1$price),]
new_quantity = m_order(data_dk1$quantity)
data_dk1[,1] = new_quantity
png('images/MC_DK1_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
simple_merit_order_plot(data_dk1, "Market clearing DK1 - low wind penetration", TRUE, demand_dk1)
dev.off()

# DK2
quantity = c(b[3:4],b[11:17])
price = c(f.obj[3:4],f.obj[11:17])
demand_dk2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
data_dk2 = data.frame(quantity = quantity, price = price)
data_dk2 = data_dk2[order(data_dk2$price),]
new_quantity = m_order(data_dk2$quantity)
data_dk2[,1] = new_quantity
png('images/MC_DK2_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
simple_merit_order_plot(data_dk2, "Market clearing DK2 - low wind penetration", TRUE, demand_dk2)
dev.off()

