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
# let's look at the price evolution 
png('images/Price_DK1.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, price_hour_new[,1], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-20, max(price_hour_new[,1])+50), type = "l", col = "black", lwd = 1.5)
abline( h = max(price_hour_new[,1]), col = "red", lwd = 2)
abline( h = mean(price_hour_new[,1]), col = "red", lty = 3, lwd = 3)
abline(h=0)
legend("topleft", legend= c("DK1", paste0("Max price : ", max(price_hour_new[,1]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour_new[,1])), " €/MWh")),
       col = c("black", "red","red"), lty = c(1,1,3), lwd = 2, cex = 0.75)
dev.off()

png('images/Price_DK1.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, price_hour_new[,2], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-20, max(price_hour_new[,2])+20), type = "l", col = "black", lwd = 1.5)
abline( h = max(price_hour_new[,2]), col = "red", lwd = 2)
abline( h = mean(price_hour_new[,2]), col = "red", lty = 3, lwd = 3)
abline(h=0)
legend("topleft", legend= c("DK2", paste0("Max price : ", max(price_hour_new[,1]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour_new[,1])), " €/MWh")),
       col = c("black", "red","red"), lty = c(1,1,3), lwd = 2, cex = 0.75)
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
for (i in 1:24){
  j = i%%24 
  cat(paste0("j = ",j), "\n")
  while (j != 0){
    if (dispatch_hour[i,18] >= 0){
      DK12_transmission[j, ] = dispatch_hour[i,18] 
    } else{
      DK21_transmission[j,] = -dispatch_hour[i,18]
    }}
  
}
########################################################
########################################################
# We order Generators for DK1 and for DK2 
new_dispatch_hour = matrix(0,nrow = N_simulations, ncol = 17)
new_dispatch_hour[,1:2] = dispatch_hour[,1:2]
new_dispatch_hour[,3:8] = dispatch_hour[,5:10]
new_dispatch_hour[,9:10] = dispatch_hour[,3:4]
new_dispatch_hour[,11:17] = dispatch_hour[,11:17]

# Therefore the participants are : 

new_participants = c("WW1_DK1","WW2_DK1",
                     "Peako_DK1", "Peako_DK1",
                     "Nuke22_DK1", "FlexiGas_DK1", "FlexiGas_DK1", "FlexiGas_DK1",
                     "EW1_DK2","EW2_DK2", "Nuke22_DK2",
                     "RoskildeCHP_DK2", "RoskildeCHP_DK2",
                     "Avedovre_DK2", "Avedovre_DK2",
                     "BlueWater_DK2", "BlueWater_DK2") # "Transmission_line", "Shedding1", "Shedding2")

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

# high wind penetration i = 650
# png('images/generator_dispatch_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, new_dispatch_hour[650,], type = "h", lwd = 5, xlab = "", xaxt ="n", ylab = "[MWh]" )
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 0.8)
title(main="Generators dispatch when high wind penetration")
# dev.off()

# png('images/revenues_dispatch_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, revenues_hour[650,]/1000, type = "h", lwd = 5, ylab = "[k€]", xlab = "", xaxt ="n")
lines(2,revenues_hour[650,2]/1000, type = "h", col = "red", lwd = 5)
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 0.8)
title(main="Generators revenues when high wind penetration")
mtext(paste0("Eq. price (€) in DK1 - DK2 resp. : ", price_hour_new[650,1], " / ", price_hour_new[650,2]), 1, line=7, col = "blue")
# dev.off()

# low wind penetration i = 120
# Generators revenues when low wind penetration 
# png('images/generator_dispatch_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, new_dispatch_hour[120,], type = "h", lwd = 5, xlab = "", xaxt ="n", ylab = "[MWh]" )
axis(1, at=1:20, labels = new_participants[1:20], las = 2, cex.axis = 0.8)
title(main="Generators dispatch when low wind penetration")
# dev.off()


# png('images/revenues_dispatch_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, revenues_hour[120,1:17]/1000, type = "h", lwd = 5, ylab = "[k€]", xlab = "", xaxt ="n")
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 0.8)
title(main="Generators revenues when low wind penetration")
mtext(paste0("Eq. price (€) in DK1 - DK2 resp. : ", price_hour_new[120, 1], " / ", price_hour_new[120,2]), 1, line=7, col = "blue")
# dev.off()


########################################################
########################################################
# Overall revenues
overall_revenues = matrix(0,nrow = 1, ncol = 17)
for (i in 1:17){
  overall_revenues[,i] = sum(revenues_hour[1:N_simulations,i])
}

# png('images/overall_revenues.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:17, overall_revenues[1,1:17]/1000, type = "h", lwd = 5, ylab = "[k€]", xlab = "", xaxt ="n")
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 0.8)
title(main = "[January] Overall revenues for each market participant")
# dev.off()


########################################################
########################################################
# General wind penetration 
par(mar = c(4.5, 4.5,2, 2))
wp = ((dispatch_hour[,1]+dispatch_hour[,2]+dispatch_hour[,3]+dispatch_hour[,4])/(demand_hour[,1]+demand_hour[,2]))*100
mean_wp = mean(wp)
plot(1:N_simulations, wp, 
     type = 'l', ylim = c(-5,105), xlab = "Time [h]", ylab = "[%]", lwd = 2)
abline(h = mean_wp,
       col = "blue")
legend("bottomright", legend = paste0("Mean penetration over January : ", round(mean_wp) ), lty = 1, col = "blue")
title(main = "Wind penetration in DK")


# DK1 
# General Demand vs wind offers DK1
par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, demand_hour[,1], type = "l", ylim = c(0,4000), lwd = 2,
     xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, Wind$DK1, col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity Demand DK1", "Wind offers DK1"), col=c("black","blue"),lty = 1, lwd = 2, cex = 0.75)

# Wind production
plot(1:N_simulations, demand_hour[,1], type = 'l', ylim = c(0,4000), xlab = "Time [h]", ylab = "[MW]", lwd = 2)
lines(1:N_simulations,dispatch_hour[,1]+dispatch_hour[,2], col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity demand in DK1", "Wind production in DK1"), col = c("black", "blue"), lty = c(1,1), cex = 0.75, lwd = 2 )
title(main= "Wind production in the electricity market DK1")

# let's look at the wind penetration in this market 
## Wind energy penetration [%] is calculated as the wind energy produced in a time period [hour; day; year] 
## divided by the total electricity consumption in that period
count = 0
for (i in 1:N_simulations){
  if ((dispatch_hour[i,1]+dispatch_hour[i,2])/demand_hour[i,1]*100 >= 100) count = count + 1
}
cat(paste0("100% wind penetration in DK1 : ", round(count/N_simulations*100), " % of the time"))

par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, (dispatch_hour[,1]+dispatch_hour[,2])/demand_hour[,1]*100, type = 'l', ylim = c(-5,135),
     xlab = "Time [h]", ylab = "[%]", lwd = 2)
legend("bottomright", legend = paste0("100% wind penetration in DK1 : ", round(count/N_simulations*100), " % of the time"), cex = 0.75)
title(main = "Wind penetration in DK1")

## DK2
# General Demand vs wind offers DK2
plot(1:N_simulations, demand_hour[,2], type = "l", ylim = c(0,3000), lwd = 2,
     xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, Wind$DK2, col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity Demand DK2", "Wind offers DK2"), col=c("black","blue"),lty = 1, lwd = 2, cex = 0.75)

# Wind production
par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, demand_hour[,2], type = 'l', ylim = c(0,3000),xlab = "Time [h]", ylab = "[MW]", lwd = 2)
lines(1:N_simulations,dispatch_hour[,3]+dispatch_hour[,4], col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity demand in DK2", "Wind production in DK2"), col = c("black", "blue"), lty = c(1,1), cex = 0.75, lwd = 2 )
title(main= "Wind production in the electricity market DK2")

# Let's look at the wind penetration in this market
## Wind energy penetration [%] is calculated as the wind energy produced in a time period [hour; day; year] 
## divided by the total electricity consumption in that period
count = 0
for (i in 1:N_simulations){
  if ((dispatch_hour[i,3]+dispatch_hour[i,4])/demand_hour[i,2]*100 == 100) count = count + 1
}
cat(paste0("100% wind penetration in DK2 : ", round(count/N_simulations*100), " % of the time"))

par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, (dispatch_hour[,3]+dispatch_hour[,4])/demand_hour[,2]*100, type = 'l', ylim = c(-5,105),
     xlab = "Time [h]", ylab = "[%]", col ="black", lwd = 2)
legend("bottomright", legend = paste0("100% wind penetration in DK2 : ", round(count/N_simulations*100), " % of the time"), cex = 0.75)
title(main = "Wind penetration in DK2")

# Influence of the transmission
plot(190:220, demand_hour[190:220,1], type = 'h', ylim = c(-2000,3500), xlab = "Time [h]", ylab = "[MW]", lwd = 10)
lines(190:220,dispatch_hour[190:220,1]+dispatch_hour[190:220,2]+dispatch_hour[190:220,5]+dispatch_hour[190:220,6]+dispatch_hour[190:220,7]+dispatch_hour[190:220,8]+dispatch_hour[190:220,9]+dispatch_hour[190:220,10], col = "grey", lwd = 4, type = "h")
lines(190:220,dispatch_hour[190:220,1]+dispatch_hour[190:220,2], col = "darkorange", lwd = 4, type = "h")
lines(190:220, dispatch_hour[190:220,18], type = "h", xlab = "Time [h]", ylab = "[MWh]", col = "green", lwd = 4)
legend("bottomleft", legend = c("Electricity demand in DK1", "Electricity prod. in DK1", "Electricity prod. from wind", "Transmission to DK2 (negative = export to DK2)"), col = c("black", "grey", "orange","green"), lty = c(1,1,1,1), cex = 0.75, lwd = 4 )

plot(150:160, demand_hour[150:160,1], type = 'h', ylim = c(0,3500), xlab = "Time [h]", ylab = "[MW]", lwd = 10)
lines(150:160,dispatch_hour[150:160,1]+dispatch_hour[150:160,2]+dispatch_hour[150:160,5]+dispatch_hour[150:160,6]+dispatch_hour[150:160,7]+dispatch_hour[150:160,8]+dispatch_hour[150:160,9]+dispatch_hour[150:160,10], col = "grey", lwd = 4, type = "h")
lines(150:160,dispatch_hour[150:160,1]+dispatch_hour[150:160,2], col = "darkorange", lwd = 4, type = "h")
points(150:160, dispatch_hour[150:160,18], xlab = "Time [h]", ylab = "[MWh]", col = "green", lwd = 4, pch = 3)
legend("topleft", legend = c("Electricity demand in DK1", "Electricity prod. in DK1", "Electricity prod. from wind", "Transmission to DK2 (positive = import from DK2)"), col = c("black", "grey", "orange","green"), lty = c(1,1,1), pch = c(NaN,NaN,NaN,3), cex = 0.75, lwd = 4 )


########################################################
########################################################
# Revenues calculation 
eq.price_hour = matrix(0,nrow = N_simulations, ncol = 17) 
eq.price_hour[,1:8] = price_hour_new[,1] # DK1
eq.price_hour[,9:17] = price_hour_new[,2] # DK2

# particular case of support 
eq.price_hour[,2] = eq.price_hour[,2] + 17 # prenium
eq.price_hour[,10] = 25 # feed in tariff

revenues_hour = matrix(0,nrow = N_simulations, ncol = 17)
for (i in 1:N_simulations){
  revenues_hour[i,] = eq.price_hour[i,]*new_dispatch_hour[i,]
}

# reset graphical parameters
# png('images/hourly_revenues_from_wind_DK12.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(5, 5,4, 2))
plot(1:N_simulations,(revenues_hour[,1]+revenues_hour[,2])/1000, type = 'l', xlab = "Time [h]", ylab = "Revenues [k€]")
lines(1:N_simulations,(revenues_hour[,9]+revenues_hour[,10])/1000, type = 'l', xlab = "Time [h]", ylab = "Revenues [k€]", col = "blue", lwd = 1.5)
legend("topleft", legend= c("DK1","DK2"), col = c("black", "blue"), lty = 1, cex = 0.75, lwd = 2)
title(main = "Hourly revenues from wind production")
# dev.off()

##  
# Generators revenues when low wind penetration 
# png('images/generator_dispatch_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, new_dispatch_hour[120,], type = "h", lwd = 5, xlab = "", xaxt ="n", ylab = "[MWh]" )
axis(1, at=1:20, labels = new_participants[1:20], las = 2, cex.axis = 0.8)
title(main="Generators dispatch when low wind penetration")
# dev.off()

png('images/revenues_dispatch_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, revenues_hour[120,1:17]/1000, type = "h", lwd = 5, ylab = "[k€]", xlab = "", xaxt ="n")
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 0.8)
title(main="Generators revenues when low wind penetration")
mtext(paste0("Eq. price (€) in DK1 - DK2 resp. : ", price_hour[120, 1], " - ", price_hour[120,2]), 1, line=7, col = "blue")
dev.off()

### Generators revenues when high wind penetration 
## Notes : BlueWater_DK2 produces 258 MWh but makes no revenues
## Notes : same for EW1_DK2 who produces 325 MWh but doesn't have any support therefore makes no revenues
png('images/generator_dispatch_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:17, new_dispatch_hour[650,], type = "h", lwd = 5, xlab = "", xaxt ="n", ylab = "[MWh]" )
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 0.8)
title(main="Generators dispatch when high wind penetration")
dev.off()

png('images/revenues_dispatch_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, revenues_hour[650,]/1000, type = "h", lwd = 5, ylab = "[k€]", xlab = "", xaxt ="n")
lines(9,revenues_hour[650,9]/1000, type = "h", col = "red", lwd = 5)
lines(16,revenues_hour[650,16]/1000, type = "h", col = "red", lwd = 5)
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 0.8)
title(main="Generators revenues when high wind penetration")
mtext(paste0("Eq. price (€) in DK1 - DK2 resp. : ", price_hour[650, 1], " - ", price_hour[650,2]), 1, line=7, col = "blue")
dev.off()
